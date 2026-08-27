# features_coordination_negation.R
# Coordination, split structures, negation, and lexical features (Spanish)
#
# RASGOS PRODUCIDOS: f_10, f_14, f_16, f_51 (nouns/demonstratives),
#                    f_63-f_65 (split aux, coordination),
#                    f_66-f_67 (negation)
#
# RASGOS ELIMINADOS (intraducibles):
#   f_59 (contractions): morfologia inexistente en espanol estandar
#   f_61 (stranded prepositions): agramatical en espanol
#   f_62 (split infinitives): requiere marcador preverbal "to" inexistente
#   f_15 (gerunds as nouns): gerundio espanol no es nominal
# Ver biber_espanol_completo.md para justificacion completa.
#
# NOTA LINGUISTICA -- negacion espanola:
#
#   Espanol: morfema UNICO "no" preverbal + pronombres/adverbios negativos.
#
#   f_66  NEGACION SINTETICA = pronombre/adverbio negativo ocupa posicion
#         preverbal SIN co-ocurrencia de "no" en la misma clausula.
#         Ejemplos: "Nadie llego", "Nada importa", "Nunca viene".
#
#   f_67  NEGACION ANALITICA = "no" (o variantes: ni, tampoco) en posicion
#         preverbal ligado al verbo por dep_rel=advmod.
#         Ejemplos: "no llego", "no lo veo nunca".
#
# BUG CORREGIDO respecto a la version anterior:
#   El left_join de analytic_neg_heads usaba .data$ dentro del vector
#   by = c(...) lo que produce error en dplyr. Corregido con rename previo.

# -----------------------------------------------------------------------------
# Rasgos f_59 (contracciones), f_61 (preposición varada) y f_62 (infinitivo
# escindido) ELIMINADOS por intraducibles (biber_espanol_completo.md §F_59,
# §F_61, §F_62). Antes existían stubs block_contractions_es /
# block_stranded_split_es que devolvían doc_ids sin columnas; retirados en
# la auditoría Fase 1 (Sección C2) por código vestigial.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 1.  block_split_coordination_es   f_63-f_65
# -----------------------------------------------------------------------------

#' Split auxiliary and coordination features (Spanish)
#'
#' f_63  Auxiliar escindido: ADV interviene entre auxiliar y verbo principal
#' f_64  Coordinacion sintagmatica: CCONJ entre sintagmas no clausales
#' f_65  Coordinacion clausal:      CCONJ entre clausulas con sujeto
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param token_lookup Token-level attribute lookup table
#' @param subject_heads Table of clause heads that have an explicit subject
#' @param head_lookup Head-token attribute table
#' @param negation_part_lemmas Negation particle lemmas (no, ni, tampoco)
#' @return Data frame with f_63, f_64, f_65
#' @keywords internal
block_split_coordination_es <- function(tokens, doc_ids, token_lookup,
                                         subject_heads, head_lookup,
                                         negation_part_lemmas) {

  adv_interveners <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV" |
        (.data$pos == "PART" & .data$lemma %in% negation_part_lemmas)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      adv_tok = .data$token_id_int
    )

  aux_deps <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux")
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX"),
      !is.na(.data$token_id_int),
      !is.na(.data$head_token_id_int),
      .data$token_id_int != .data$head_token_id_int
    ) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$head_token_id_int),
      span_max = pmax(.data$token_id_int, .data$head_token_id_int)
    )

  # REVISION HERNAN (Fase 5): además del auxiliar propio (dep_rel=aux), cubrir
  # la PERÍFRASIS MODAL, donde spanish-gsd hace al modal la cabeza (VERB root)
  # y al verbo auxiliado un infinitivo xcomp/obj ('podría [fácilmente]
  # resolver'). El span va del modal (head) al infinitivo; el ADV interpuesto
  # cuenta en f_63.
  modal_deps <- tokens %>%
    dplyr::filter(
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Inf",
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(xcomp|obj|ccomp)"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX"),
      .data$token_id_int != .data$head_token_id_int
    ) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$head_token_id_int),
      span_max = pmax(.data$token_id_int, .data$head_token_id_int)
    )
  aux_deps <- dplyr::bind_rows(aux_deps, modal_deps)

  # f_63  Phase 2k: preservar columnas para evidencia.
  f63 <- aux_deps %>%
    dplyr::left_join(adv_interveners, by = c("doc_id", "sentence_id")) %>%
    dplyr::filter(
      !is.na(.data$adv_tok),
      .data$adv_tok > .data$span_min,
      .data$adv_tok < .data$span_max
    ) %>%
    count_feature_traced("f_63_split_auxiliary")

  # cc_tokens: CCONJ con dep_rel=cc + atributos de su conjuncion hermana
  cc_tokens <- tokens %>%
    dplyr::filter(
      .data$pos == "CCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "cc"
    ) %>%
    # atributos del head del CCONJ (= segundo conjunto)
    dplyr::left_join(
      token_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::rename(
      conj_pos            = "token_pos",
      conj_dep_rel        = "token_dep_rel",
      conj_head_id        = "token_head_token_id_int",
      conj_verbform       = "token_morph_verbform"
    ) %>%
    # atributos del primer conjunto (head del head)
    dplyr::left_join(
      token_lookup %>%
        dplyr::select(
          "doc_id", "sentence_id", "token_id_int",
          first_conj_pos = "token_pos"
        ),
      by = c("doc_id", "sentence_id", "conj_head_id" = "token_id_int")
    ) %>%
    # sujeto explicito en la clausula
    dplyr::left_join(
      subject_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "clause_head_token_id_int")
    ) %>%
    dplyr::mutate(
      has_subject = dplyr::coalesce(.data$has_subject, FALSE)
    )

  # f_64  Coordinacion sintagmatica
  # REVISION HERNAN (Fase 5): cubrir las CUATRO categorías del rasgo inglés:
  # nombres, adjetivos, adverbios Y verbos (coordinación de SV con sujeto
  # compartido, p. ej. 'lee y escribe'). El guard !has_subject separa la
  # coordinación sintagmática (SV compartido) de la clausal (f_65, sujetos
  # propios), evitando el doble conteo.
  f64 <- cc_tokens %>%
    dplyr::filter(
      dplyr::coalesce(.data$conj_dep_rel, "") == "conj",
      dplyr::coalesce(.data$conj_pos,     "") %in%
        c("NOUN", "PROPN", "ADJ", "ADV", "VERB", "AUX"),
      !is.na(.data$first_conj_pos),
      .data$first_conj_pos == .data$conj_pos,
      !.data$has_subject
    ) %>%
    count_feature_traced("f_64_phrasal_coordination")

  # f_65  Coordinacion clausal
  #  (a) rama dependency: "y/e" con conj cuyo conjunto es VERB/AUX finito
  #      y la clausula tiene sujeto explicito (coordinacion intra-oracion)
  f65_dep <- cc_tokens %>%
    dplyr::filter(
      dplyr::coalesce(.data$conj_dep_rel, "") == "conj",
      dplyr::coalesce(.data$conj_pos,     "") %in% c("VERB", "AUX"),
      .data$has_subject,
      is.na(.data$conj_verbform) |
        !.data$conj_verbform %in% c("Inf", "Ger", "Part")
    )

  #  (b) rama posicional (biber_espanol_completo.md §f_65): "y/e" en
  #      posición inicial de cláusula, precedida por puntuación final de
  #      oración O al inicio absoluto del documento. En Spanish-GSD una
  #      oración que empieza con "Y" la etiqueta CCONJ/cc cuyo head es el
  #      root de su propia oración (no hay relación conj entre oraciones).
  #      Excluye pero/mas (espejo de Biber). token_id_int==1 = inicial.
  f65_initial <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in% c("y", "e"),
      .data$pos == "CCONJ",
      .data$token_id_int == 1L
    )

  coord_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                           "token", "lemma", "pos", "feats", "head_token_id_int")
  f65 <- dplyr::bind_rows(
    f65_dep     %>% dplyr::select(dplyr::all_of(coord_evidence_cols)),
    f65_initial %>% dplyr::select(dplyr::all_of(coord_evidence_cols))
  ) %>% count_feature_traced("f_65_clausal_coordination")

  counts <- doc_ids %>%
    dplyr::left_join(f63$counts, by = "doc_id") %>%
    dplyr::left_join(f64$counts, by = "doc_id") %>%
    dplyr::left_join(f65$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f63$evidence, f64$evidence, f65$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 4.  block_negation_es   f_66-f_67
# -----------------------------------------------------------------------------

#' Negation features (Spanish)
#'
#' f_66  Negacion sintetica -- pronombre/adverbio negativo preverbal SIN <<no>>
#' f_67  Negacion analitica  -- <<no>> (o ni/tampoco) preverbal + verbo
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param neg_synthetic_terms Lemmas de pronombres/adverbios negativos
#'   (nadie, nada, ninguno, ninguna, nunca, jamas)
#' @param negation_part_lemmas Lemmas de particulas negativas (no, ni, tampoco)
#' @return Data frame with f_66_neg_synthetic, f_67_neg_analytic
#' @keywords internal
block_negation_es <- function(tokens, doc_ids,
                               neg_synthetic_terms,
                               negation_part_lemmas,
                               negation_adverbs = NULL) {

  # En español, los adverbios "nunca", "jamás", "tampoco" son negación
  # SINTÉTICA según Biber (1985) y biber_espanol_completo.md F_66, no
  # analítica. Los unimos a la lista para detectarlos en f_66.
  neg_synthetic_terms <- unique(c(
    neg_synthetic_terms,
    "nunca", "jamás", "jamas", "tampoco"
  ))

  # -- Tabla auxiliar de heads negados analiticamente ------------------------
  # Columnas: doc_id, sentence_id, neg_head_id
  # REVISION HERNAN (Fase 5): f_67 se amplía más allá de la anteposición al
  # verbo. 'no' como adverbio de foco sobre otros constituyentes también cuenta:
  # 'una respuesta no definitiva' (head ADJ), 'no muy lejos' (head ADV). El
  # filtro dep_rel=advmod ya excluye 'no' como respuesta independiente (INTJ/
  # root) y como sustantivo ('el no del comité' -> NOUN, no advmod).
  verb_pos <- c("VERB", "AUX", "ADJ", "ADV")

  analytic_heads <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      dplyr::coalesce(.data$dep_rel, "") == "advmod",
      !is.na(.data$head_token_id_int)
    ) %>%
    # join para verificar que el head es VERB/AUX
    dplyr::rename(neg_head_id = "head_token_id_int") %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          doc_id2      = .data$doc_id,
          sentence_id2 = .data$sentence_id,
          neg_head_id  = .data$token_id_int,
          head_pos     = .data$pos
        ),
      by = c(
        "doc_id"      = "doc_id2",
        "sentence_id" = "sentence_id2",
        "neg_head_id"
      )
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$head_pos, "") %in% verb_pos) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$neg_head_id)

  # -- f_67  Negacion analitica ----------------------------------------------
  # Phase 2l: restauramos head_token_id_int (originalmente renombrado a
  # neg_head_id para el join) para que count_feature_traced pueda construir
  # evidencia con el schema E1.
  f67 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      dplyr::coalesce(.data$dep_rel, "") == "advmod",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::inner_join(
      analytic_heads %>%
        dplyr::rename(head_token_id_int = "neg_head_id"),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    count_feature_traced("f_67_neg_analytic")

  # -- f_66  Negacion sintetica ----------------------------------------------
  # Pronombre/adverbio negativo con funcion sintactica real (nsubj, advmod,
  # obj, obl?) cuyo head verbal NO aparece en analytic_heads.
  synth_dep_rels <- c(
    "nsubj", "nsubj:pass", "advmod", "obj", "iobj",
    "nmod", "obl", "obl:agent"
  )

  # Aceptar también CCONJ para "ni" (UDPipe lo etiqueta como conjunción).
  # No excluir cuando coexiste con "no" preverbal: en español la
  # concordancia negativa ("no vino nadie") es obligatoria y Biber cuenta
  # ambos elementos. biber_espanol_completo.md F_66.
  f66 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% neg_synthetic_terms,
      .data$pos %in% c("PRON", "ADV", "DET", "CCONJ")
    ) %>%
    count_feature_traced("f_66_neg_synthetic")

  counts <- doc_ids %>%
    dplyr::left_join(f66$counts, by = "doc_id") %>%
    dplyr::left_join(f67$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f66$evidence, f67$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 5.  block_lexical_membership_es   f_10 (dem.), f_14-f_16 (nouns)
# -----------------------------------------------------------------------------

#' Demonstrative pronoun, nominalization, and noun features (Spanish)
#'
#' f_10  Pronombres demostrativos (este, ese, aquel + formas)
#' f_14  Nominalizaciones (sustantivos con sufijos productivos)
#' f_15  ELIMINADO (intraducible): el gerundio espanol no funciona como
#'       sustantivo; las funciones nominales del -ing ingles se realizan
#'       mediante infinitivos (f_24) y nominalizaciones (f_14).
#'       biber_espanol_completo.md sec. F_15.
#' f_16  Otros sustantivos (= total NOUN/PROPN - f_14; sin resta de f_15)
#' f_51  Demostrativos determinantes (mismo este/ese/aquel en funcion DET)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param word_lists_lookup Word lists lookup
#' @return Data frame with f_10, f_14, f_16, f_51
#' @keywords internal
block_lexical_membership_es <- function(tokens, doc_ids, word_lists_lookup) {

  # 2026-04-20: pronoun_matchlist no contiene demostrativos (este/ese/aquel).
  # Se carga demonstrative_matchlist (anadida en word_lists.yaml) para f_10 y f_51.
  # pronoun_matchlist se conserva para otros usos genericos dentro del bloque.
  # Ver: docs/DECISIONES_ES.md ?f_10-f_51.
  pronoun_terms         <- get_word_list(word_lists_lookup, "pronoun_matchlist")
  demonstr_terms        <- get_word_list(word_lists_lookup, "demonstrative_matchlist")
  nominalization_sfx    <- get_word_list(word_lists_lookup, "nominalization_suffixes")
  nominalization_stop   <- normalize_terms(
    get_word_list(word_lists_lookup, "nominalization_stoplist")
  )
  # gerund_stop eliminado: f_15 es intraducible, gerundio espanol no es nominal.

  # -- f_10  Pronombres demostrativos ----------------------------------------
  # 2026-04-20: cambiado de pronoun_terms a demonstr_terms.
  # pronoun_matchlist carece de formas demostrativas (este/ese/aquel);
  # usarla aqui hacia que f_10 contara pronombres personales/posesivos.
  f10 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in%
        stringr::str_to_lower(demonstr_terms),
      .data$pos == "PRON"
    ) %>%
    count_feature_traced("f_10_demonstrative_pronoun")

  # -- f_51  Demostrativos determinantes -------------------------------------
  # 2026-04-20: cambiado de pronoun_terms a demonstr_terms.
  # pronoun_matchlist solo tenia posesivos atonos (mi/tu/su); esos no son
  # demostrativos. demonstrative_matchlist contiene este/ese/aquel + formas.
  f51 <- tokens %>%
    dplyr::filter(
      stringr::str_to_lower(.data$token) %in%
        stringr::str_to_lower(demonstr_terms),
      .data$pos == "DET",
      dplyr::coalesce(.data$dep_rel, "") == "det"
    ) %>%
    count_feature_traced("f_51_demonstratives")

  # -- f_14  Nominalizaciones ------------------------------------------------
  if (length(nominalization_sfx) > 0) {
    sfx_pat <- paste0(
      "(", paste(nominalization_sfx, collapse = "|"), ")$"
    )
  } else {
    sfx_pat <- "^$"
  }

  # -- f_14  Nominalizaciones ------------------------------------------------
  # Phase 2m: identificamos los tokens nominalizados primero (para que tanto
  # f_14 como f_16 puedan usar el mismo conjunto -- f_14 los cuenta, f_16
  # los excluye anti_join para evitar la sustraccion post-hoc del count).
  f14_rows <- tokens %>%
    dplyr::filter(.data$pos == "NOUN") %>%
    dplyr::mutate(lem_lower = stringr::str_to_lower(.data$lemma)) %>%
    dplyr::filter(
      stringr::str_detect(.data$lem_lower, sfx_pat),
      !.data$lem_lower %in% nominalization_stop
    ) %>%
    dplyr::select(-"lem_lower")

  f14 <- count_feature_traced(f14_rows, "f_14_nominalizations")

  # f_15 (gerundios) ELIMINADO: intraducible.

  # -- f_16  Otros sustantivos -----------------------------------------------
  # En el codigo legacy esto se computaba como (total NOUN/PROPN) - f_14. Para
  # preservar la invariante count == nrow(evidence), aqui anti-joineamos los
  # tokens de f_14 antes de contar; resultado numerico identico.
  f16_rows <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      !stringr::str_detect(.data$token, "-")
    ) %>%
    dplyr::anti_join(
      f14_rows %>% dplyr::select("doc_id", "sentence_id", "token_id_int"),
      by = c("doc_id", "sentence_id", "token_id_int")
    )
  f16 <- count_feature_traced(f16_rows, "f_16_other_nouns")

  counts <- doc_ids %>%
    dplyr::left_join(f10$counts,  by = "doc_id") %>%
    dplyr::left_join(f51$counts,  by = "doc_id") %>%
    dplyr::left_join(f14$counts,  by = "doc_id") %>%
    dplyr::left_join(f16$counts,  by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f10$evidence, f51$evidence, f14$evidence, f16$evidence)
  make_block_result(counts = counts, evidence = evidence)
}
