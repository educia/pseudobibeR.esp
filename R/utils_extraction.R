# Utility functions for feature extraction

#' Normalize feature counts to per-1000-word rates
#'
#' f_43_type_token (un ratio) y f_44_mean_word_length (longitud media en
#' caracteres) son metricas derivadas, NO conteos: multiplicarlas por
#' 1000/tot_counts las corrompe (p.ej. longitud media 9 -> 714). Se excluyen
#' de la normalizacion. n_tokens / n_lex_tokens tampoco se normalizan.
#'
#' @param counts A data frame with feature counts and a tot_counts column
#' @return A data frame with normalized counts (tot_counts column removed)
#' @keywords internal
normalize_counts <- function(counts) {
  no_norm <- c("f_43_type_token", "f_44_mean_word_length",
               "n_tokens", "n_lex_tokens", "tot_counts")
  counts %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric) & !dplyr::any_of(no_norm),
      ~ 1000 * . / tot_counts
    )) %>%
    dplyr::select(-"tot_counts")
}

#' Replace NAs with zeros in numeric columns of a data frame
#'
#' @param x A data frame
#' @return Data frame with NAs in numeric columns replaced by 0
#' @keywords internal
replace_nas <- function(x) {
  dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), ~ dplyr::coalesce(., 0L)))
}

#' Extract a specific morphological feature value from UD feats string
#'
#' @param feats Character vector of UD morphological features
#' @param key The feature name to extract (e.g., "Tense", "VerbForm")
#' @return Character vector of extracted values
#' @keywords internal
extract_morph_value <- function(feats, key) {
  purrr::map_chr(feats, function(f) {
    if (is.na(f) || f == "") return(NA_character_)
    parts <- stringr::str_split(f, "\\|")[[1]]
    match <- parts[stringr::str_detect(parts, paste0("^", key, "="))]
    if (length(match) == 0) return(NA_character_)
    stringr::str_remove(match[1], paste0("^", key, "="))
  })
}

#' Get a named word list from the word_lists data
#'
#' @param word_lists_lookup The word_lists object
#' @param name Name of the list to retrieve
#' @return Character vector of terms
#' @keywords internal
get_word_list <- function(word_lists_lookup, name) {
  if (!name %in% names(word_lists_lookup)) {
    warning(paste0("Word list '", name, "' not found"))
    return(character(0))
  }
  word_lists_lookup[[name]]
}

#' Normalize terms by converting to lowercase and replacing Unicode apostrophes
#'
#' @param values Character vector of terms
#' @return Normalized character vector
#' @keywords internal
normalize_terms <- function(values) {
  stringr::str_to_lower(values) %>%
    stringr::str_replace_all("\u2019", "'")
}

#' Flag tokens that participate in a multi-word expression
#'
#' Marca con `in_mwe = TRUE` cada token que pertenece a una secuencia
#' contigua que coincide (case-insensitive) con uno de los patrones
#' multi-token. Mecanismo paralelo a `quanteda::tokens_compound()`: opera
#' sobre el dataframe parseado por UDPipe para que los bloques de rasgos
#' sint\u00e1cticos puedan excluir tokens que ya est\u00e1n "absorbidos" por una
#' locuci\u00f3n multi-palabra (p. ej. "sea" en "o sea", "es" en "es decir").
#'
#' @param tokens Data frame parseado (debe tener doc_id, sentence_id, token).
#' @param multiword_patterns Vector char con patrones tipo `o_sea`, `sin_embargo`.
#' @return `tokens` con columna `in_mwe` (logical).
#' @keywords internal
flag_mwe_tokens <- function(tokens, multiword_patterns) {
  if (!"in_mwe" %in% names(tokens)) {
    tokens$in_mwe <- FALSE
  }
  if (length(multiword_patterns) == 0 || nrow(tokens) == 0) return(tokens)

  patterns <- multiword_patterns %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish()
  patterns <- patterns[stringr::str_detect(patterns, " ")]
  if (length(patterns) == 0) return(tokens)

  pattern_words <- stringr::str_split(patterns, " ")

  tok_lc   <- stringr::str_to_lower(tokens$token)
  doc_sent <- paste(tokens$doc_id, tokens$sentence_id, sep = "\x1f")
  N <- nrow(tokens)

  for (k in seq_along(patterns)) {
    words <- pattern_words[[k]]
    n <- length(words)
    if (n > N) next

    starts <- seq_len(N - n + 1L)
    match  <- tok_lc[starts] == words[1L]
    if (n > 1L) {
      for (j in 2:n) {
        match <- match &
          tok_lc[starts + (j - 1L)] == words[j] &
          doc_sent[starts] == doc_sent[starts + (j - 1L)]
      }
    }

    hits <- starts[which(match)]
    if (length(hits) == 0) next
    for (s in hits) {
      tokens$in_mwe[s:(s + n - 1L)] <- TRUE
    }
  }
  tokens
}

#' Extract lemmas from a dictionary entry
#'
#' @param dict_lookup The dict object
#' @param feature Feature name
#' @return Character vector of lemmas
#' @keywords internal
dictionary_to_lemmas <- function(dict_lookup, feature, head_word = FALSE) {
  if (!feature %in% names(dict_lookup)) {
    return(character(0))
  }

  patterns <- dict_lookup[[feature]]

  if (head_word) {
    # Modo "head_word": extrae el primer token de cada locución
    # multi-palabra. Útil para perífrasis modales (haber_de, tener_que,
    # hay_que → haber, tener, hay) donde el primer token es el verbo
    # cabeza que se busca en el árbol sintáctico.
    lemmas <- patterns %>%
      stringr::str_extract("^[^_]+") %>%
      stringr::str_to_lower() %>%
      unique()
  } else {
    # Modo por defecto: solo entradas SINGLE-WORD pasan a la rama de
    # matching por lemma. Las locuciones multi-token (a_menudo,
    # de_vez_en_cuando, sin_embargo, tal_vez) se manejan en la rama
    # quanteda (tokens_lookup), que las captura como compounds.
    # Extraer el primer token de las locuciones (a, de, al, sin, tal)
    # introduce preposiciones y palabras comunes, causando sobreconteo
    # masivo en f_04, f_05, f_11, f_46, f_47.
    single_word <- !stringr::str_detect(patterns, "_")
    lemmas <- patterns[single_word] %>%
      stringr::str_to_lower() %>%
      unique()
  }

  lemmas
}
