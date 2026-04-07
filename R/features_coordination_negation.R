# Coordination, contractions, stranded prepositions, split structures,
# negation, and lexical membership features for Spanish
#
# NOTA LINGÜÍSTICA — negación española vs. francesa:
#
#   Francés: negación discontinua  ne … pas/jamais/rien/plus
#     → f_66 sintética = pronombres/adverbios negativos (personne, rien…)
#     → f_67 analítica  = ne + pas/jamais combinados
#
#   Español: negación con morfema ÚNICO
#     → f_66 SINTÉTICA = pronombres/adverbios negativos que niegan SIN "no"
#       preverbal: nadie llegó, nada importa, nunca viene, jamás lo hará
#       (el pronombre/adverbio ocupa la posición preverbal y es el único marcador)
#     → f_67 ANALÍTICA  = "no" preverbal + verbo  (el caso más frecuente)
#       con refuerzo opcional: no … nunca, no … nadie, no … nada
#
#   Implementación UD:
#     - "no" (y variantes) → dep_rel = "advmod", morph_polarity = "Neg"
#       o lemma %in% negation_part_lemmas
#     - pronombres negativos → lemma %in% neg_synthetic_terms, pos = PRON/ADV,
#       dep_rel != advmod (para excluir el "no" mismo)
#     - negación sintética = el pronombre/adverbio negativo aparece SIN "no"
#       preverbal en la misma oración
#     - negación analítica  = "no" preverbal ligado a un VERB/AUX por advmod

# ─────────────────────────────────────────────────────────────────────────────
# block_contractions_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract contraction features (Spanish)
#'
#' Spanish does not productively use apostrophe contractions like French;
#' this feature is kept for structural parity but will typically return 0.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @return Data frame with f_59_contractions
#' @keywords internal
block_contractions_es <- function(tokens, doc_ids) {
  tibble::tibble(doc_id = doc_ids$doc_id, f_59_contractions = 0L)
}

# ─────────────────────────────────────────────────────────────────────────────
# block_stranded_split_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract stranded preposition and split infinitive features (Spanish)
#'
#' The UD-based logic is largely language-agnostic and reused here.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @return Data frame with f_61_stranded_preposition and f_62_split_infinitive
#' @keywords internal
block_stranded_split_es <- function(tokens, doc_ids) {
  tokens_ctx <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(.data$sentence_id, .data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      lag1_pos   = dplyr::lag(.data$pos),
      lag1_lemma = dplyr::lag(.data$lemma),
      lag1_sent  = dplyr::lag(.data$sentence_id),
      lag2_pos   = dplyr::lag(.data$pos, 2),
      lag2_lemma = dplyr::lag(.data$lemma, 2),
      lag2_sent  = dplyr::lag(.data$sentence_id, 2),
      lag3_pos   = dplyr::lag(.data$pos, 3),
      lag3_lemma = dplyr::lag(.data$lemma, 3),
      lag3_sent  = dplyr::lag(.data$sentence_id, 3),
      lag4_pos   = dplyr::lag(.data$pos, 4),
      lag4_lemma = dplyr::lag(.data$lemma, 4),
      lag4_sent  = dplyr::lag(.data$sentence_id, 4),
      lead1_pos      = dplyr::lead(.data$pos),
      lead1_lemma    = dplyr::lead(.data$lemma),
      lead1_sent     = dplyr::lead(.data$sentence_id),
      lead1_prontype = dplyr::lead(.data$morph_prontype)
    ) %>%
    dplyr::ungroup()

  stranded_pronouns <- c("quien", "quienes", "que")

  f61 <- tokens_ctx %>%
    dplyr::filter(
      .data$pos == "ADP",
      !is.na(.data$lead1_sent),
      .data$lead1_sent == .data$sentence_id,
      .data$lead1_pos  == "PRON",
      .data$lead1_lemma %in% stranded_pronouns,
      stringr::str_detect(
        dplyr::coalesce(.data$lead1_prontype, ""),
        "Rel|Int"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_61_stranded_preposition = "n")

  inf_prepositions <- c("a", "al", "del", "de", "por", "para")
  filler_pos       <- c("ADV", "PART", "PRON", "DET")

  candidate_inf <- tokens_ctx %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      !is.na(.data$morph_verbform),
      .data$morph_verbform == "Inf"
    ) %>%
    dplyr::mutate(
      lag1_same  = !is.na(.data$lag1_sent) & .data$lag1_sent == .data$sentence_id,
      lag2_same  = !is.na(.data$lag2_sent) & .data$lag2_sent == .data$sentence_id,
      lag3_same  = !is.na(.data$lag3_sent) & .data$lag3_sent == .data$sentence_id,
      lag4_same  = !is.na(.data$lag4_sent) & .data$lag4_sent == .data$sentence_id,
      filler1_ok = .data$lag1_same & .data$lag1_pos %in% filler_pos,
      filler2_ok = .data$lag2_same & .data$lag2_pos %in% filler_pos,
      filler3_ok = .data$lag3_same & .data$lag3_pos %in% filler_pos,
      adv12  = (.data$lag1_same & .data$lag1_pos == "ADV") |
               (.data$lag2_same & .data$lag2_pos == "ADV"),
      adv123 = (.data$lag1_same & .data$lag1_pos == "ADV") |
               (.data$lag2_same & .data$lag2_pos == "ADV") |
               (.data$lag3_same & .data$lag3_pos == "ADV"),
      has_split2 = .data$lag2_same &
        .data$lag2_pos   == "ADP" &
        .data$lag2_lemma %in% inf_prepositions &
        .data$lag1_same  &
        .data$lag1_pos   == "ADV",
      has_split3 = .data$lag3_same &
        .data$lag3_pos   == "ADP" &
        .data$lag3_lemma %in% inf_prepositions &
        .data$filler1_ok & .data$filler2_ok & .data$adv12,
      has_split4 = .data$lag4_same &
        .data$lag4_pos   == "ADP" &
        .data$lag4_lemma %in% inf_prepositions &
        .data$filler1_ok & .data$filler2_ok & .data$filler3_ok & .data$adv123
    ) %>%
    dplyr::filter(.data$has_split2 | .data$has_split3 | .data$has_split4)

  f62 <- candidate_inf %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_62_split_infinitive = "n")

  doc_ids %>%
    dplyr::left_join(f61, by = "doc_id") %>%
    dplyr::left_join(f62, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

# ─────────────────────────────────────────────────────────────────────────────
# block_split_coordination_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract coordination features (Spanish)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param token_lookup Token lookup table
#' @param subject_heads Subject head lookup
#' @param head_lookup Head token lookup
#' @param negation_part_lemmas Negation particle lemmas
#' @return Data frame with f_63_split_auxiliary, f_64_phrasal_coordination,
#'   f_65_clausal_coordination
#' @keywords internal
block_split_coordination_es <- function(tokens, doc_ids, token_lookup,
                                         subject_heads, head_lookup,
                                         negation_part_lemmas) {
  adverbial_interveners <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV" |
        (.data$pos == "PART" & .data$lemma %in% negation_part_lemmas)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      adv_token_id_int = .data$token_id_int
    )

  aux_dependencies <- tokens %>%
    dplyr::filter(stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux")) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      .data$head_pos %in% c("VERB", "AUX"),
      !is.na(.data$token_id_int),
      !is.na(.data$head_token_id_int),
      .data$token_id_int != .data$head_token_id_int
    ) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$head_token_id_int),
      span_max = pmax(.data$token_id_int, .data$head_token_id_int)
    )

  split_auxiliary_tokens <- aux_dependencies %>%
    dplyr::left_join(adverbial_interveners, by = c("doc_id", "sentence_id")) %>%
    dplyr::filter(
      !is.na(.data$adv_token_id_int),
      .data$adv_token_id_int > .data$span_min,
      .data$adv_token_id_int < .data$span_max
    ) %>%
    dplyr::distinct(.data$doc_id, .data$token_id_int)

  split_aux_counts <- split_auxiliary_tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_63_split_auxiliary = dplyr::n(), .groups = "drop")

  cc_tokens <- tokens %>%
    dplyr::filter(.data$pos == "CCONJ", .data$dep_rel == "cc") %>%
    dplyr::left_join(
      token_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::rename(
      conj_pos              = "token_pos",
      conj_dep_rel          = "token_dep_rel",
      conj_head_token_id_int = "token_head_token_id_int",
      conj_morph_verbform   = "token_morph_verbform"
    ) %>%
    dplyr::left_join(
      token_lookup %>%
        dplyr::select("doc_id", "sentence_id", "token_id_int",
                      first_conj_pos = "token_pos"),
      by = c("doc_id", "sentence_id", "conj_head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(
      subject_heads,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "clause_head_token_id_int")
    ) %>%
    dplyr::mutate(has_subject = dplyr::coalesce(.data$has_subject, FALSE)) %>%
    dplyr::select(-dplyr::any_of("clause_head_token_id_int"))

  f64 <- cc_tokens %>%
    dplyr::filter(
      .data$conj_dep_rel == "conj",
      .data$conj_pos %in% c("NOUN", "PROPN", "ADJ", "ADV"),
      !is.na(.data$first_conj_pos),
      .data$first_conj_pos == .data$conj_pos,
      !.data$has_subject
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_64_phrasal_coordination = dplyr::n(), .groups = "drop")

  f65 <- cc_tokens %>%
    dplyr::filter(
      .data$conj_dep_rel == "conj",
      .data$conj_pos %in% c("VERB", "AUX"),
      .data$has_subject,
      is.na(.data$conj_morph_verbform) |
        !.data$conj_morph_verbform %in% c("Inf", "Ger", "Part")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_65_clausal_coordination = dplyr::n(), .groups = "drop")

  doc_ids %>%
    dplyr::left_join(split_aux_counts, by = "doc_id") %>%
    dplyr::left_join(f64, by = "doc_id") %>%
    dplyr::left_join(f65, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

# ─────────────────────────────────────────────────────────────────────────────
# block_negation_es  (reescrito)
# ─────────────────────────────────────────────────────────────────────────────

#' Extract negation features (Spanish)
#'
#' f_66  NEGACIÓN SINTÉTICA — pronombre/adverbio negativo preverbal SIN "no"
#'   Ejemplos: "Nadie llegó", "Nada importa", "Nunca viene", "Jamás lo hará"
#'   Condiciones UD:
#'     - lemma %in% neg_synthetic_terms
#'     - pos PRON o ADV
#'     - dep_rel = nsubj | advmod | obj | nmod (el token niega por sí solo)
#'     - en la misma oración NO existe un token con dep_rel = advmod y
#'       morph_polarity = Neg cuyo head sea el mismo verbo (= no hay "no" previo)
#'
#' f_67  NEGACIÓN ANALÍTICA — "no" preverbal ligado al verbo
#'   Ejemplos: "no llegó", "no lo veo nunca", "no hay nadie"
#'   Condiciones UD:
#'     - token con lemma %in% negation_part_lemmas (no, tampoco, ni)
#'     - dep_rel = advmod
#'     - head pos VERB o AUX
#'   Contamos ocurrencias del marcador "no" (no del verbo) para mantener
#'   compatibilidad con la convención de Biber et al. (1988).
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param neg_synthetic_terms Synthetic negation term lemmas
#'   (nadie, nada, ninguno/a, nunca, jamás, tampoco en posición preverbal)
#' @param negation_part_lemmas Negation particle lemmas (no, ni, tampoco)
#' @param negation_adverbs Negation adverb lemmas (nunca, jamás, tampoco, etc.)
#' @return Data frame with f_66_neg_synthetic and f_67_neg_analytic
#' @keywords internal
block_negation_es <- function(tokens, doc_ids, neg_synthetic_terms,
                               negation_part_lemmas, negation_adverbs) {

  # ── Paso 1: identificar cláusulas con "no" analítico ─────────────────────
  # Un token es "no analítico" cuando: lemma %in% negation_part_lemmas,
  # dep_rel = advmod, y su head es VERB/AUX.
  analytic_neg_heads <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      dplyr::coalesce(.data$dep_rel, "") == "advmod",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::select(
          doc_id_h      = .data$doc_id,
          sentence_id_h = .data$sentence_id,
          head_token_id_int = .data$token_id_int,
          head_pos      = .data$pos
        ),
      by = c(
        "doc_id"      = "doc_id_h",
        "sentence_id" = "sentence_id_h",
        "head_token_id_int"
      )
    ) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX")) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      neg_head_id = .data$head_token_id_int,
      is_analytic = TRUE
    ) %>%
    dplyr::distinct()

  # ── f_67  Negación analítica ──────────────────────────────────────────────
  # Contamos los tokens "no/ni/tampoco" que cumplen las condiciones de arriba.
  f67 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      dplyr::coalesce(.data$dep_rel, "") == "advmod",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::inner_join(
      analytic_neg_heads %>%
        dplyr::rename(head_token_id_int = .data$neg_head_id),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_67_neg_analytic = "n")

  # ── f_66  Negación sintética ──────────────────────────────────────────────
  # Pronombre/adverbio negativo que niega SIN "no" preverbal en la misma
  # cláusula (mismo verbo-head).
  #
  # Estrategia:
  #   (a) tomar todos los tokens neg_synthetic (nadie, nada, nunca, jamás…)
  #       con dep_rel informativo (nsubj, advmod, obj, nmod, obl)
  #   (b) obtener el head verbal de cada uno
  #   (c) excluir aquellos cuyo head verbal ya tiene un "no" analítico
  #       registrado en analytic_neg_heads

  synthetic_dep_rels <- c("nsubj", "advmod", "obj", "iobj", "nmod", "obl",
                           "nsubj:pass", "obl:agent")

  candidate_synthetic <- tokens %>%
    dplyr::filter(
      .data$lemma %in% neg_synthetic_terms,
      .data$pos %in% c("PRON", "ADV", "DET"),
      dplyr::coalesce(.data$dep_rel, "") %in% synthetic_dep_rels,
      !is.na(.data$head_token_id_int)
    )

  f66 <- candidate_synthetic %>%
    dplyr::anti_join(
      analytic_neg_heads %>%
        dplyr::rename(head_token_id_int = .data$neg_head_id),
      by = c(
        "doc_id", "sentence_id",
        "head_token_id_int"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_66_neg_synthetic = "n")

  doc_ids %>%
    dplyr::left_join(f66, by = "doc_id") %>%
    dplyr::left_join(f67, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

# ─────────────────────────────────────────────────────────────────────────────
# block_lexical_membership_es
# ─────────────────────────────────────────────────────────────────────────────

#' Extract lexical membership features (Spanish)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param word_lists_lookup Word lists lookup
#' @return Data frame with f_10_demonstrative_pronoun through f_51_demonstratives
#' @keywords internal
block_lexical_membership_es <- function(tokens, doc_ids, word_lists_lookup) {
  pronoun_terms <- get_word_list(word_lists_lookup, "pronoun_matchlist")

  f10 <- tokens %>%
    dplyr::filter(
      .data$token %in% pronoun_terms,
      .data$pos == "PRON",
      (.data$morph_prontype == "Dem" | is.na(.data$morph_prontype))
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_10_demonstrative_pronoun = "n")

  nominalization_suffixes <- get_word_list(word_lists_lookup, "nominalization_suffixes")

  nominalization_pattern <- if (length(nominalization_suffixes) > 0) {
    escaped <- stringr::str_replace_all(nominalization_suffixes, "([\\W])", "\\\\\\1")
    paste0("(", paste(escaped, collapse = "|"), ")$")
  } else {
    "^$"
  }

  nominal_stoplist <- normalize_terms(
    get_word_list(word_lists_lookup, "nominalization_stoplist")
  )

  f14 <- tokens %>%
    dplyr::filter(
      .data$pos == "NOUN",
      stringr::str_detect(.data$lemma, nominalization_pattern),
      !.data$lemma %in% nominal_stoplist
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_14_nominalizations = "n")

  gerund_stoplist <- normalize_terms(
    get_word_list(word_lists_lookup, "gerund_stoplist")
  )

  # Gerundios por morfología UD (VerbForm=Ger o Part+Tense=Pres)
  f15_morph <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      !is.na(.data$morph_verbform),
      .data$morph_verbform == "Ger" |
        (.data$morph_verbform == "Part" & .data$morph_tense == "Pres")
    ) %>%
    dplyr::filter(!.data$lemma %in% gerund_stoplist)

  # Fallback: NOUN/PROPN con lema en -ando/-iendo precedidos de "en" (ADP)
  # que el parser etiquetó mal (gerundio de "en + gerundio" = construcción
  # temporal/condicional muy frecuente en español escrito).
  f15_fallback <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      stringr::str_detect(.data$lemma, "ando$|iendo$"),
      dplyr::lag(.data$token, default = "") == "en",
      dplyr::lag(.data$pos,   default = "") == "ADP",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(nmod|obl|advcl)"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!.data$lemma %in% gerund_stoplist)

  # Unir y deduplicar; el fallback no debe solaparse con los morfológicos
  f15_tokens <- dplyr::bind_rows(f15_morph, f15_fallback) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int,
                    .keep_all = TRUE)

  # Conteo de gerundios mal parseados como NOUN (para descontar de f_16)
  gerunds_n <- f15_tokens %>%
    dplyr::filter(.data$pos %in% c("NOUN", "PROPN")) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(gerunds_n = "n")

  f15 <- f15_tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_15_gerunds = "n")

  # f_16  Otros sustantivos (excluye nominalizaciones y gerundios-NOUN)
  f16_raw <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      !stringr::str_detect(.data$token, "-")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_16_other_nouns = "n")

  f16 <- f16_raw %>%
    dplyr::left_join(f14,       by = "doc_id") %>%
    dplyr::left_join(gerunds_n, by = "doc_id") %>%
    replace_nas() %>%
    dplyr::mutate(
      f_16_other_nouns = .data$f_16_other_nouns -
        .data$f_14_nominalizations -
        .data$gerunds_n
    ) %>%
    dplyr::transmute(.data$doc_id, f_16_other_nouns = .data$f_16_other_nouns)

  f51 <- tokens %>%
    dplyr::filter(
      .data$token %in% pronoun_terms,
      .data$dep_rel == "det",
      .data$pos %in% c("DET", "PRON")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_51_demonstratives = "n")

  doc_ids %>%
    dplyr::left_join(f10, by = "doc_id") %>%
    dplyr::left_join(f14, by = "doc_id") %>%
    dplyr::left_join(f15, by = "doc_id") %>%
    dplyr::left_join(f16, by = "doc_id") %>%
    dplyr::left_join(f51, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
