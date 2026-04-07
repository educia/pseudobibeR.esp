# features_subordination.R
# Subordination and clause-embedding features for Spanish (f_21–f_38)
#
# ESTRATEGIA GENERAL:
#   Reescritura completa sin depender de flags pre-computados
#   (is_infinitive, is_relative_subject, etc.) que no siempre existen.
#   Toda la lógica trabaja directamente sobre columnas UD estándar:
#     pos (UPOS), dep_rel, feats, lemma, token, head_token_id_int.
#   Se usa extract_feat() de features_tense_pronouns.R para leer
#   rasgos morfológicos del campo `feats`.
#
# MAPA DE FEATURES:
#   f_21  that-complementizador tras verbo       ("dijo que...")
#   f_22  that-complementizador tras adjetivo    ("seguro de que...")
#   f_23  Cláusula-wh (relativa/interrogativa indirecta)
#   f_24  Infinitivos (VerbForm=Inf como núcleo clausal)
#   f_25  Gerundio adverbial / complemento       (VerbForm=Ger, dep_rel=advcl|ccomp)
#   f_26  Participio adverbial / absoluto        (VerbForm=Part, dep_rel=advcl|ccomp|acl)
#   f_27  Participio postnominal (whiz-deletion) (VerbForm=Part, dep_rel=acl, head=NOUN)
#   f_28  Gerundio postnominal  (whiz-deletion)  (VerbForm=Ger,  dep_rel=acl, head=NOUN)
#   f_29  Relativa de sujeto con «que»
#   f_30  Relativa de objeto con «que»
#   f_31  Relativa de sujeto con pronombre-wh    (quien, cual)
#   f_32  Relativa de objeto con pronombre-wh
#   f_33  Pied-piping (prep + pronombre relativo)
#   f_34  Relativa oracional (eso que, lo que)
#   f_35  Subordinada causal (porque, ya que, puesto que…)
#   f_36  Subordinada concesiva (aunque, si bien, a pesar de que…)
#   f_37  Subordinada condicional (si, en caso de que…)
#   f_38  Otros subordinadores adverbiales
#   f_60  That-deletion (elipsis de «que» complementizador)

# ─────────────────────────────────────────────────────────────────────────────
# 1.  block_participial_clauses_es   f_24–f_28
# ─────────────────────────────────────────────────────────────────────────────

#' Participial and infinitive clause features (Spanish)
#'
#' @param tokens Annotated token data frame (UD format)
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @return Data frame: one row per doc, columns f_24 – f_28
#' @keywords internal
block_participial_clauses_es <- function(tokens, doc_ids, head_lookup) {

  # Pre-computar verbform por token (evita llamar extract_feat() repetidamente)
  tokens <- tokens %>%
    dplyr::mutate(
      .vf = dplyr::coalesce(
        extract_feat(.data$feats, "VerbForm"), ""
      )
    )

  # ── f_24  Infinitivos como núcleo clausal ─────────────────────────────────
  # Un infinitivo cuenta cuando:
  #   - VerbForm = Inf
  #   - dep_rel es xcomp, ccomp, advcl, acl, obj (complement clausal)
  #   - NO es el infinitivo de una perífrasis modal (aux de un VERB finito)
  f24 <- tokens %>%
    dplyr::filter(
      .data$pos  %in% c("VERB", "AUX"),
      .data$.vf  == "Inf",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(xcomp|ccomp|advcl|acl|obj)"
      )
    ) %>%
    count_feature("f_24_infinitives")

  # ── f_25  Gerundio adverbial / complemento ────────────────────────────────
  # VerbForm=Ger con dep_rel=advcl|ccomp, NO postnominal
  f25 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      .data$.vf  == "Ger",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(advcl|ccomp)"
      )
    ) %>%
    count_feature("f_25_present_participle")

  # ── f_26  Participio adverbial / absoluto ─────────────────────────────────
  # VerbForm=Part + Voice≠Pass con dep_rel=advcl|ccomp|acl
  # (participio absoluto: «llegado el momento…», «terminada la reunión…»)
  f26 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "ADJ"),
      .data$.vf  == "Part",
      dplyr::coalesce(extract_feat(.data$feats, "Voice"), "") != "Pass",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(advcl|ccomp|acl)"
      )
    ) %>%
    count_feature("f_26_past_participle")

  # ── f_27  Participio postnominal (whiz-deletion) ──────────────────────────
  # VerbForm=Part, dep_rel=acl, head es NOUN/PROPN
  f27 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "ADJ"),
      .data$.vf  == "Part",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^acl"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("NOUN", "PROPN")
    ) %>%
    count_feature("f_27_past_participle_whiz")

  # ── f_28  Gerundio postnominal (whiz-deletion) ────────────────────────────
  # VerbForm=Ger, dep_rel=acl, head es NOUN/PROPN
  f28 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      .data$.vf  == "Ger",
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^acl"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("NOUN", "PROPN")
    ) %>%
    count_feature("f_28_present_participle_whiz")

  doc_ids %>%
    dplyr::left_join(f24, by = "doc_id") %>%
    dplyr::left_join(f25, by = "doc_id") %>%
    dplyr::left_join(f26, by = "doc_id") %>%
    dplyr::left_join(f27, by = "doc_id") %>%
    dplyr::left_join(f28, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 2.  block_relatives_es   f_29–f_34
# ─────────────────────────────────────────────────────────────────────────────

#' Relative clause features (Spanish)
#'
#' Estrategia UD pura:
#'   - Cláusulas relativas = tokens con dep_rel que comienza por "acl"
#'     (acl, acl:relcl) cuyo marcador (dep_rel=mark o ref) es un pronombre
#'     relativo o wh.
#'   - Tipo de relativa (sujeto/objeto) se determina por la función UD
#'     del pronombre relativo dentro de la cláusula subordinada.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @return Data frame: one row per doc, columns f_29 – f_34
#' @keywords internal
block_relatives_es <- function(tokens, doc_ids, head_lookup) {

  # Tabla de pronombres/adverbios relativos con su función en la relativa
  # ── pronombres relativos en UD español
  rel_pronouns_all <- c(
    "que", "quien", "quienes",
    "cual", "cuales",
    "cuyo", "cuya", "cuyos", "cuyas",
    "donde", "cuando", "como"
  )
  rel_subj_roles <- c("nsubj", "nsubj:pass")
  rel_obj_roles  <- c("obj", "iobj", "obl", "obl:agent")

  # Tokens que son pronombres relativos (PronType=Rel o dep_rel=ref|mark
  # y lemma en lista)
  rel_tokens <- tokens %>%
    dplyr::filter(
      .data$lemma %in% rel_pronouns_all,
      .data$pos   %in% c("PRON", "ADV", "DET", "ADJ", "SCONJ"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(ref|mark|nsubj|obj|iobj|obl|acl|nmod)"
      ) |
        stringr::str_detect(
          dplyr::coalesce(extract_feat(.data$feats, "PronType"), ""),
          "Rel"
        )
    )

  # Head de la cláusula relativa: el token cuya dep_rel = acl*
  # y cuya sentence_id coincide
  acl_heads <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^acl")
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      acl_head_id   = .data$token_id_int,
      acl_head_pos  = .data$pos,
      antecedent_id = .data$head_token_id_int
    )

  # Antecedentes de las relativas (NOUN/PROPN que preceden al acl_head)
  antecedents <- tokens %>%
    dplyr::filter(.data$pos %in% c("NOUN", "PROPN")) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      antecedent_id = .data$token_id_int,
      ant_pos = .data$pos
    )

  # ── f_29  Relativa de sujeto con «que» ────────────────────────────────────
  f29 <- rel_tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      dplyr::coalesce(.data$dep_rel, "") %in% rel_subj_roles
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    count_feature("f_29_that_subj")

  # ── f_30  Relativa de objeto con «que» ────────────────────────────────────
  f30 <- rel_tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      dplyr::coalesce(.data$dep_rel, "") %in% rel_obj_roles
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    count_feature("f_30_that_obj")

  wh_pronouns <- c("quien", "quienes", "cual", "cuales")

  # ── f_31  Relativa de sujeto con pronombre-wh ─────────────────────────────
  f31 <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_pronouns,
      dplyr::coalesce(.data$dep_rel, "") %in% rel_subj_roles
    ) %>%
    count_feature("f_31_wh_subj")

  # ── f_32  Relativa de objeto con pronombre-wh ─────────────────────────────
  f32 <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_pronouns,
      dplyr::coalesce(.data$dep_rel, "") %in% rel_obj_roles
    ) %>%
    count_feature("f_32_wh_obj")

  # ── f_33  Pied-piping (preposición + pronombre relativo) ──────────────────
  # El pronombre relativo tiene dep_rel = obl* o nmod y su head inmediato
  # en UD es el verbo de la relativa; la preposición lo gobierna.
  # Proxy: token ADP inmediatamente antes del pronombre relativo en la oración
  f33 <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::filter(
      .data$lemma %in% rel_pronouns_all,
      .data$pos   %in% c("PRON", "DET"),
      dplyr::lag(.data$pos, default = "") == "ADP"
    ) %>%
    dplyr::ungroup() %>%
    count_feature("f_33_pied_piping")

  # ── f_34  Relativa oracional (lo que, eso que, lo cual) ───────────────────
  # El antecedente es un pronombre neutro (eso, esto, ello, lo)
  sentence_rel_antecedents <- c("eso", "esto", "ello", "lo")

  f34 <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% c("que", "cual", "cuales"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          antecedent_id = .data$token_id_int,
          ant_lemma = .data$lemma
        ),
      by = c("doc_id", "sentence_id", "antecedent_id")
    ) %>%
    dplyr::filter(
      .data$ant_lemma %in% sentence_rel_antecedents
    ) %>%
    count_feature("f_34_sentence_relatives")

  doc_ids %>%
    dplyr::left_join(f29, by = "doc_id") %>%
    dplyr::left_join(f30, by = "doc_id") %>%
    dplyr::left_join(f31, by = "doc_id") %>%
    dplyr::left_join(f32, by = "doc_id") %>%
    dplyr::left_join(f33, by = "doc_id") %>%
    dplyr::left_join(f34, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 3.  block_clause_embedding_es   f_21–f_23, f_35–f_38, f_60
# ─────────────────────────────────────────────────────────────────────────────

#' Complementizer, subordinator, and clause-embedding features (Spanish)
#'
#' f_21  That-complementizador tras VERB ("dijo que", "sabe que")
#' f_22  That-complementizador tras ADJ  ("seguro de que", "feliz de que")
#' f_23  Cláusula-wh (relativa/interrogativa indirecta)
#' f_35  Subordinada causal    (porque, ya_que, puesto_que, dado_que…)
#' f_36  Subordinada concesiva (aunque, si_bien, a_pesar_de_que…)
#' f_37  Subordinada condicional (si, en_caso_de_que, siempre_que…)
#' f_38  Otros subordinadores adverbiales (cuando, mientras, antes_de_que…)
#' f_60  That-deletion (ccomp/xcomp sin «que» complementizador)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @param dict_lookup Dictionary lookup (para listas de subordinadores)
#' @return Data frame: one row per doc, columns f_21–f_23, f_35–f_38, f_60
#' @keywords internal
block_clause_embedding_es <- function(tokens, doc_ids, head_lookup,
                                       dict_lookup = NULL) {

  # ── f_21  «que» complementizador tras VERB ────────────────────────────────
  # Condiciones UD:
  #   - token lemma = "que", pos = SCONJ
  #   - dep_rel = mark
  #   - head del mark tiene pos = VERB o AUX
  f21 <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX")
    ) %>%
    count_feature("f_21_that_verb_comp")

  # ── f_22  «que» complementizador tras ADJ ────────────────────────────────
  # El head inmediato de la cláusula marcada con «que» es un ADJ
  f22 <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") == "ADJ"
    ) %>%
    count_feature("f_22_that_adj_comp")

  # ── f_23  Cláusula-wh (interrogativa/relativa indirecta) ──────────────────
  wh_lemmas <- c(
    "quien", "quienes", "que", "cual", "cuales",
    "donde", "cuando", "como",
    "cuanto", "cuanta", "cuantos", "cuantas",
    "por_qu\u00e9", "por_que"
  )

  f23 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_lemmas,
      .data$pos   %in% c("PRON", "ADV", "DET", "ADJ", "SCONJ"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(obj|obl|nsubj|iobj|mark|advmod|nmod)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX", "ADJ")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_23_wh_clause")

  # ── f_35  Causal ──────────────────────────────────────────────────────────
  causal_lemmas <- c(
    "porque", "ya_que", "puesto_que", "dado_que",
    "pues", "como",   # «como» causal: "Como llueve, me quedo"
    "en_vista_de_que", "habida_cuenta_de_que"
  )

  f35 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% causal_lemmas,
      .data$pos   %in% c("SCONJ", "CCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc|advmod)"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_35_because")

  # ── f_36  Concesiva ───────────────────────────────────────────────────────
  concessive_lemmas <- c(
    "aunque", "si_bien", "a_pesar_de_que", "aun_cuando",
    "por_m\u00e1s_que", "por_mucho_que", "pese_a_que",
    "con_todo", "sin_embargo"  # conectores concesivos
  )

  f36 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% concessive_lemmas,
      .data$pos   %in% c("SCONJ", "CCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc|advmod)"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_36_though")

  # ── f_37  Condicional ─────────────────────────────────────────────────────
  conditional_lemmas <- c(
    "si", "en_caso_de_que", "siempre_que", "siempre_y_cuando",
    "con_tal_de_que", "a_condici\u00f3n_de_que", "a_menos_que",
    "a_no_ser_que", "salvo_que", "excepto_que"
  )

  f37 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% conditional_lemmas,
      .data$pos   %in% c("SCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^mark"
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_37_if")

  # ── f_38  Otros subordinadores adverbiales ────────────────────────────────
  # Todo SCONJ con dep_rel=mark que NO sea ya contado en f_21/f_22/f_35/f_36/f_37
  counted_sub <- unique(c(
    "que",
    causal_lemmas, concessive_lemmas, conditional_lemmas
  ))

  f38 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("SCONJ", "ADP", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^mark"
      ),
      !.data$lemma %in% counted_sub
    ) %>%
    count_feature("f_38_other_adv_sub")

  # ── f_60  That-deletion ───────────────────────────────────────────────────
  # Cláusulas ccomp/xcomp sin marcador «que»
  # Paso 1: cláusulas que SÍ tienen «que» como mark
  has_que_mark <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      clause_head_id = .data$head_token_id_int,
      has_que = TRUE
    ) %>%
    dplyr::distinct()

  # Paso 2: cláusulas complemento cuyo head es VERB/AUX/ADJ Y no tienen «que»
  f60 <- tokens %>%
    dplyr::filter(
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(ccomp|xcomp)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX", "ADJ")
    ) %>%
    dplyr::left_join(
      has_que_mark,
      by = c(
        "doc_id", "sentence_id",
        "token_id_int" = "clause_head_id"
      )
    ) %>%
    dplyr::filter(is.na(.data$has_que)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    count_feature("f_60_that_deletion")

  doc_ids %>%
    dplyr::left_join(f21, by = "doc_id") %>%
    dplyr::left_join(f22, by = "doc_id") %>%
    dplyr::left_join(f23, by = "doc_id") %>%
    dplyr::left_join(f35, by = "doc_id") %>%
    dplyr::left_join(f36, by = "doc_id") %>%
    dplyr::left_join(f37, by = "doc_id") %>%
    dplyr::left_join(f38, by = "doc_id") %>%
    dplyr::left_join(f60, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
