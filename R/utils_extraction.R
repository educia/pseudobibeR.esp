# Utility functions for feature extraction

#' Normalize feature counts to per-1000-word rates
#'
#' @param counts A data frame with feature counts and a tot_counts column
#' @return A data frame with normalized counts (tot_counts column removed)
#' @keywords internal
normalize_counts <- function(counts) {
  counts %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ 1000 * . / tot_counts)) %>%
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

#' Extract lemmas from a dictionary entry
#'
#' @param dict_lookup The dict object
#' @param feature Feature name
#' @return Character vector of lemmas
#' @keywords internal
dictionary_to_lemmas <- function(dict_lookup, feature) {
  if (!feature %in% names(dict_lookup)) {
    return(character(0))
  }

  patterns <- dict_lookup[[feature]]

  # Solo entradas SINGLE-WORD pasan a la rama de matching por lemma.
  # Las locuciones multi-token (p.ej. "a_menudo", "de_vez_en_cuando",
  # "sin_embargo", "tal_vez") se manejan en la rama quanteda (tokens_lookup),
  # que las captura como compounds. Extraer el primer token de las
  # locuciones (p.ej. "a", "de", "al", "sin", "tal") introduciría
  # preposiciones y palabras comunes en la lista de lemmas, causando
  # sobreconteo masivo en f_04, f_05, f_11, f_46, f_47.
  single_word <- !stringr::str_detect(patterns, "_")
  lemmas <- patterns[single_word] %>%
    stringr::str_to_lower() %>%
    unique()

  lemmas
}
