# features_modals_verbs.R
# Adjective, preposition, adverb, modal, and specialized-verb features (Spanish)
# f_39-f_42, f_52-f_58
#
# NOTA LINGUISTICA -- modales espanoles:
#   El espanol no tiene auxiliares modales monolexicos como el ingles.
#   Los modales son PERIFRASIS VERBALES:
#     f_52  Posibilidad:  poder + INF, caber + INF
#     f_53  Necesidad:    deber + INF, tener_que + INF,
#                         haber_de + INF, haber_que + INF
#     f_54  Predictivo:   futuro sintetico (Tense=Fut,VerbForm=Fin)
#                         + ir_a + INF (perifrase progresivo-futuro)
#
#   Para evitar falsos positivos ("el poder", "el deber" como sustantivos)
#   exigimos que el verbo modal tenga al menos un dependiente con
#   VerbForm=Inf enlazado por xcomp / ccomp / advcl / aux / obj.
#
# NOTA -- extract_feat() y count_feature() se definen en
#   features_tense_pronouns.R y son visibles en el mismo namespace del paquete.

# -----------------------------------------------------------------------------
# 0.  Helper: perifrasis modal (verbo-cabeza + infinitivo-dependiente)
# -----------------------------------------------------------------------------

count_modal_periphrasis <- function(tokens, lemmas, feature_name = NULL) {
  # Phase 2i: el retorno depende de feature_name.
  #   - feature_name = NULL: comportamiento legacy, devuelve tibble(doc_id, n).
  #     Conservado para callers que aun esperan ese shape.
  #   - feature_name = string: retorna block_result(counts con columna nombrada,
  #     evidence E1 con feature etiquetado).
  if (length(lemmas) == 0) {
    if (is.null(feature_name)) {
      return(tibble::tibble(doc_id = character(), n = integer()))
    }
    return(make_block_result(
      tibble::tibble(doc_id = character(), !!feature_name := integer())
    ))
  }

  modal_toks <- tokens %>%
    dplyr::filter(
      .data$lemma %in% lemmas,
      .data$pos %in% c("VERB", "AUX"),
      !stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(nsubj|obj|iobj|nmod|det|appos)$"
      )
    )

  # Case 1: modal is the head, INF is a dependent (dep_rel = xcomp/ccomp/...)
  inf_deps <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Inf",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(xcomp|ccomp|advcl|obj)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_inf_dep = TRUE
    ) %>%
    dplyr::distinct()

  case1 <- modal_toks %>%
    dplyr::left_join(
      inf_deps,
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(!is.na(.data$has_inf_dep))

  # Case 2: modal is AUX dependent whose head has VerbForm=Inf
  # (canonical UD Spanish structure: INF is root, modal is aux)
  inf_heads <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      head_verbform = extract_feat(.data$feats, "VerbForm")
    )

  case2 <- modal_toks %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      inf_heads,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$head_verbform, "") == "Inf")

  modal_cols <- c("doc_id", "sentence_id", "token_id_int",
                  "token", "lemma", "pos", "feats", "head_token_id_int")
  combined <- dplyr::bind_rows(
    case1 %>% dplyr::select(dplyr::all_of(modal_cols)),
    case2 %>% dplyr::select(dplyr::all_of(modal_cols))
  )

  if (is.null(feature_name)) {
    # Legacy shape para callers no migrados
    return(
      combined %>%
        dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
        dplyr::group_by(.data$doc_id) %>%
        dplyr::tally()
    )
  }

  count_feature_traced(combined, feature_name)
}

# -----------------------------------------------------------------------------
# 1.  block_adj_prep_adv_es   f_39-f_42
# -----------------------------------------------------------------------------

#' Adjective, preposition, and adverb features (Spanish)
#'
#' f_39  Prepositions (dep_rel = case | fixed)
#' f_40  Attributive adjectives (dep_rel = amod; fallback: ADJ before NOUN)
#' f_41  Predicative adjectives (dep_rel = xcomp | acomp | cop target)
#' f_42  General adverbs (excludes stance/hedge/negation adverbs)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup (yaml list loaded by parse_functions)
#' @param word_lists_lookup Word lists lookup
#' @param negation_adverbs Character vector of negation adverb lemmas
#' @return Data frame: one row per doc, columns f_39 - f_42
#' @keywords internal
block_adj_prep_adv_es <- function(tokens, doc_ids, dict_lookup,
                                   word_lists_lookup, negation_adverbs) {

  # f_39  Preposiciones — conteo total de tokens ADP (Biber 1985 las cuenta
  # todas como medida de densidad nominal/informativa). UDPipe spanish-gsd
  # asigna dep_rel="mark" a algunas preposiciones que introducen cláusulas
  # no finitas (Para obtener, antes de comenzar), por lo que filtrar por
  # dep_rel="case" causaba subconteo. Ver biber_espanol_completo.md F_39.
  f39 <- tokens %>%
    dplyr::filter(.data$pos == "ADP") %>%
    count_feature_traced("f_39_prepositions")

  # f_40  Adjetivo atributivo
  has_amod <- any(
    !is.na(tokens$dep_rel) & tokens$dep_rel == "amod" & tokens$pos == "ADJ"
  )

  if (has_amod) {
    f40 <- tokens %>%
      dplyr::filter(
        .data$pos == "ADJ",
        dplyr::coalesce(.data$dep_rel, "") == "amod",
        !stringr::str_detect(.data$token, "-")
      ) %>%
      count_feature_traced("f_40_adj_attr")
  } else {
    # Fallback: ADJ inmediatamente antes de NOUN dentro de la misma oracion
    f40 <- tokens %>%
      dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
      dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
      dplyr::filter(
        .data$pos == "ADJ",
        !stringr::str_detect(.data$token, "-"),
        dplyr::lead(.data$pos, default = "") %in% c("NOUN", "PROPN", "ADJ")
      ) %>%
      dplyr::ungroup() %>%
      count_feature_traced("f_40_adj_attr")
  }

  # f_41  Adjetivo predicativo
  # Lista de verbos copulativos/pseudocopulativos segun biber_espanol_completo.md:
  # ser, estar, parecer, volverse, quedarse, ponerse, resultar, permanecer.
  # En UD el lema de "quedarse" es "quedar"; de "volverse" puede ser "volver".
  # Se excluyen "hacerse" y "tornarse" (no mencionados en el documento).
  linking_verbs <- c("ser", "estar", "parecer", "resultar",
                     "quedar",       # quedarse
                     "volver",       # volverse
                     "poner",        # ponerse
                     "permanecer")

  f41_dep <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::coalesce(.data$dep_rel, "") %in% c("xcomp", "acomp")
    )

  # ADJ que tiene como dependiente un verbo copulativo (cop):
  # "es positivo" → "positivo" es root con cop=ser.
  # En UD espanol el ADJ predicativo es el HEAD de la oracion copulativa.
  f41_cop_head <- tokens %>%
    dplyr::filter(.data$pos == "ADJ") %>%
    dplyr::inner_join(
      tokens %>%
        dplyr::filter(
          dplyr::coalesce(.data$dep_rel, "") == "cop",
          .data$lemma %in% linking_verbs
        ) %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          token_id_int = .data$head_token_id_int   # el ADJ es el head del cop
        ),
      by = c("doc_id", "sentence_id", "token_id_int")
    )

  # ADJ dependiente de verbo copulativo (patrón alternativo menos frecuente)
  f41_cop_dep <- tokens %>%
    dplyr::filter(
      .data$pos == "ADJ",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$token_id_int,
          head_lemma = .data$lemma
        ),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(.data$head_lemma %in% linking_verbs)

  # Phase 2h: preserve E1 columns para evidencia. count_feature_traced
  # dedupea con .keep_all = TRUE internamente.
  adj_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                         "token", "lemma", "pos", "feats", "head_token_id_int")
  f41 <- dplyr::bind_rows(
    f41_dep      %>% dplyr::select(dplyr::all_of(adj_evidence_cols)),
    f41_cop_head %>% dplyr::select(dplyr::all_of(adj_evidence_cols)),
    f41_cop_dep  %>% dplyr::select(dplyr::all_of(adj_evidence_cols))
  ) %>% count_feature_traced("f_41_adj_pred")

  # f_42  Adverbios generales (excluye los ya capturados en otros rasgos:
  # f_04 lugar, f_05 tiempo, f_23 wh-, f_45 conjuncts, f_46 downtoners,
  # f_47 hedges, f_48 amplifiers, f_49 emphatics, f_50 discourse, f_67 'no')
  excluded_adv_lemmas <- unique(c(
    dictionary_to_lemmas(dict_lookup, "f_04_place_adverbials"),
    dictionary_to_lemmas(dict_lookup, "f_05_time_adverbials"),
    dictionary_to_lemmas(dict_lookup, "f_45_conjuncts"),
    dictionary_to_lemmas(dict_lookup, "f_46_downtoners"),
    dictionary_to_lemmas(dict_lookup, "f_47_hedges"),
    dictionary_to_lemmas(dict_lookup, "f_48_amplifiers"),
    dictionary_to_lemmas(dict_lookup, "f_49_emphatics"),
    dictionary_to_lemmas(dict_lookup, "f_50_discourse_particles"),
    # Wh-adverbios interrogativos indirectos (f_23)
    "dónde", "cuándo", "cómo", "cuánto", "cuánta", "cuántos", "cuántas",
    "donde", "cuando", "como",
    negation_adverbs
  ))

  f42 <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV",
      !.data$lemma %in% excluded_adv_lemmas
    ) %>%
    count_feature_traced("f_42_adverbs")

  counts <- doc_ids %>%
    dplyr::left_join(f39$counts, by = "doc_id") %>%
    dplyr::left_join(f40$counts, by = "doc_id") %>%
    dplyr::left_join(f41$counts, by = "doc_id") %>%
    dplyr::left_join(f42$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f39$evidence, f40$evidence, f41$evidence, f42$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 2.  block_modals_es   f_52-f_54
# -----------------------------------------------------------------------------

#' Modal verb perifrastic features (Spanish)
#'
#' f_52  Posibilidad:  poder + INF, caber + INF
#' f_53  Necesidad:    deber + INF, tener_que + INF,
#'                     haber_de + INF, haber_que + INF
#' f_54  Predictivo:   futuro sintetico (Tense=Fut,VerbForm=Fin)
#'                     + ir_a + INF
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup
#' @return Data frame: one row per doc, columns f_52 - f_54
#' @keywords internal
block_modals_es <- function(tokens, doc_ids, dict_lookup) {

  # f_52  Posibilidad
  # head_word=TRUE para modales: extrae el verbo cabeza de las perífrasis
  # (haber_de → haber, tener_que → tener, hay_que → hay). El verbo cabeza
  # se busca en el árbol sintáctico con el infinitivo como dependiente.
  poss_lemmas <- dictionary_to_lemmas(dict_lookup, "f_52_modal_possibility",
                                      head_word = TRUE)
  f52 <- count_modal_periphrasis(tokens, poss_lemmas, "f_52_modal_possibility")

  # f_53  Necesidad
  #   deber + INF (sin "de" = necesidad deontica)
  #   deber_de + INF (probabilidad epistemica; incluido por convencion)
  #   tener_que, haber_de, haber_que vienen compuestos del tokenizer
  nec_lemmas <- dictionary_to_lemmas(dict_lookup, "f_53_modal_necessity",
                                     head_word = TRUE)
  f53 <- count_modal_periphrasis(tokens, nec_lemmas, "f_53_modal_necessity")

  # f_54  Predictivo
  # Segun biber_espanol_completo.md:
  #   (a) Futuro sintetico: Tense=Fut + VerbForm=Fin
  #   (b) Condicional simple y compuesto: Mood=Cnd + VerbForm=Fin
  #   (c) Ir a + INF: perifr. futura (incluida por ser equivalente funcional de "will")
  # Nota: en UDPipe spanish-gsd el condicional lleva Mood=Cnd (hablaria, habria).

  f54_fut <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"),    "") == "Fut",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    )

  #   (a2) Condicional (Mood=Cnd + VerbForm=Fin): hablaria, habria hablado, etc.
  f54_cnd <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),     "") == "Cnd",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    )

  #   (b) ir_a + INF  (ir con dep_rel aux cuyo head tiene VerbForm=Inf)
  # 2026-04-20: Filtro de Tense anadido para excluir desplazamiento fisico.
  # Sin filtro, "fue a buscarlo" (Tense=Past) se contaba como futuro perifrastico.
  # Decision: se aceptan Tense=Pres (va a hacer) y Tense=Imp (iba a hacer,
  # futuro del pasado / condicional). Se excluyen Tense=Past (fue a + INF =
  # movimiento fisico dirigido) y Tense=Fut (ira a + INF = futuro doble, raro).
  # Ver: docs/DECISIONES_ES.md ?f_54.
  ir_a_inf <- tokens %>%
    dplyr::filter(
      .data$lemma == "ir",
      .data$pos   %in% c("AUX", "VERB"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$token_id_int,
          head_verbform = extract_feat(.data$feats, "VerbForm")
        ),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_verbform, "") == "Inf",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^aux"
      ),
      # Filtro de tiempo: solo futuro proximo (Pres) y futuro del pasado (Imp).
      # Se excluye Tense=Past (fue a + INF = desplazamiento fisico, no modalidad).
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") %in% c("Pres", "Imp")
    )

  modal_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                           "token", "lemma", "pos", "feats", "head_token_id_int")
  f54 <- dplyr::bind_rows(
    f54_fut  %>% dplyr::select(dplyr::all_of(modal_evidence_cols)),
    f54_cnd  %>% dplyr::select(dplyr::all_of(modal_evidence_cols)),
    ir_a_inf %>% dplyr::select(dplyr::all_of(modal_evidence_cols))
  ) %>% count_feature_traced("f_54_modal_predictive")

  counts <- doc_ids %>%
    dplyr::left_join(f52$counts, by = "doc_id") %>%
    dplyr::left_join(f53$counts, by = "doc_id") %>%
    dplyr::left_join(f54$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f52$evidence, f53$evidence, f54$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 3.  block_specialized_verbs_es   f_55-f_58
# -----------------------------------------------------------------------------

#' Specialized verb class features (Spanish)
#'
#' f_55  Verbos publicos  (decir, afirmar, senalar, indicar, declarar?)
#' f_56  Verbos privados  (creer, pensar, saber, sentir, suponer?)
#' f_57  Verbos suasivos  (pedir, exigir, recomendar, sugerir, ordenar?)
#' f_58  Verbos "seem"    (parecer, resultar, aparecer, semejarse?)
#'
#' Las listas de lemas se leen de dict.yaml (secciones f_55_verb_public,
#' f_56_verb_private, f_57_verb_suasive, f_58_verb_seem).
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param dict_lookup Dictionary lookup
#' @return Data frame: one row per doc, columns f_55 - f_58
#' @keywords internal
block_specialized_verbs_es <- function(tokens, doc_ids, dict_lookup) {

  count_verb_class <- function(lemmas, col_name) {
    if (length(lemmas) == 0) {
      return(make_block_result(
        tibble::tibble(doc_id = character(), !!col_name := integer())
      ))
    }
    tokens %>%
      dplyr::filter(
        .data$lemma %in% lemmas,
        .data$pos   %in% c("VERB", "AUX")
      ) %>%
      count_feature_traced(col_name)
  }

  f55 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_55_verb_public"),  "f_55_verb_public")
  # Excluir verbos de f_58 (parecer/resultar) para evitar doble conteo:
  # parecer es evidencial epistémico (f_58), no verbo de proceso mental.
  # biber_espanol_completo.md F_58.
  private_lemmas <- setdiff(
    dictionary_to_lemmas(dict_lookup, "f_56_verb_private"),
    dictionary_to_lemmas(dict_lookup, "f_58_verb_seem")
  )
  f56 <- count_verb_class(private_lemmas, "f_56_verb_private")
  f57 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_57_verb_suasive"), "f_57_verb_suasive")
  f58 <- count_verb_class(
    dictionary_to_lemmas(dict_lookup, "f_58_verb_seem"),    "f_58_verb_seem")

  counts <- doc_ids %>%
    dplyr::left_join(f55$counts, by = "doc_id") %>%
    dplyr::left_join(f56$counts, by = "doc_id") %>%
    dplyr::left_join(f57$counts, by = "doc_id") %>%
    dplyr::left_join(f58$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f55$evidence, f56$evidence, f57$evidence, f58$evidence)
  make_block_result(counts = counts, evidence = evidence)
}
