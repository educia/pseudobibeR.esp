# features_subordination.R
# Subordination and clause-embedding features for Spanish (f_21-f_38)
#
# ESTRATEGIA GENERAL:
#   Reescritura completa sin depender de flags pre-computados
#   (is_infinitive, is_relative_subject, etc.) que no siempre existen.
#   Toda la logica trabaja directamente sobre columnas UD estandar:
#     pos (UPOS), dep_rel, feats, lemma, token, head_token_id_int.
#   Se usa extract_feat() de features_tense_pronouns.R para leer
#   rasgos morfologicos del campo `feats`.
#
# MAPA DE FEATURES:
#   f_21  that-complementizador tras verbo       ("dijo que...")
#   f_22  that-complementizador tras adjetivo    ("seguro de que...")
#   f_23  Clausula-wh (relativa/interrogativa indirecta)
#   f_24  Infinitivos (VerbForm=Inf como nucleo clausal)
#   f_25  Gerundio adverbial / complemento       (VerbForm=Ger, dep_rel=advcl|ccomp)
#   f_26  Participio adverbial / absoluto        (VerbForm=Part, dep_rel=advcl|ccomp|acl)
#   f_27  Participio postnominal (whiz-deletion) (VerbForm=Part, dep_rel=acl, head=NOUN)
#   f_28  ELIMINADO (intraducible): gerundio no puede ser modificador nominal
#   f_29  Relativa sujeto con <<que>>/quien/cual  [fusion f_29+f_31]
#   f_30  Relativa objeto con <<que>>/quien/cual  [fusion f_30+f_32]
#   f_31  ABSORBIDO en f_29
#   f_32  ABSORBIDO en f_30
#   f_33  Pied-piping (prep + pronombre relativo)
#   f_34  Relativa oracional (eso que, lo que)
#   f_35  Subordinada causal (solo: porque)
#   f_36  Subordinada concesiva (solo: aunque)
#   f_37  Subordinada condicional (si, a_menos_que, salvo_que)
#   f_38  Otros subordinadores adverbiales
#   f_60  ELIMINADO (intraducible): <<que>> obligatorio en espanol

# -----------------------------------------------------------------------------
# 1.  block_participial_clauses_es   f_24-f_27 (f_28 eliminado)
# -----------------------------------------------------------------------------

#' Participial and infinitive clause features (Spanish)
#'
#' @param tokens Annotated token data frame (UD format)
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @return Data frame: one row per doc, columns f_24 - f_27
#' @keywords internal
block_participial_clauses_es <- function(tokens, doc_ids, head_lookup) {

  # Pre-computar verbform por token (evita llamar extract_feat() repetidamente)
  tokens <- tokens %>%
    dplyr::mutate(
      .vf = dplyr::coalesce(
        extract_feat(.data$feats, "VerbForm"), ""
      )
    )

  # -- f_24  Infinitivos como nucleo clausal ---------------------------------
  # Spec §f_24 Include: VerbForm=Inf en función de complemento (xcomp/ccomp/
  # obj) + perífrasis verbales (ir a, empezar a, dejar de, volver a,
  # acabar de). Exclude: substantivized infinitives e imperative.
  #
  # En la práctica el infinitivo aparece también como root cuando hay un
  # AUX modal finito ('Se debe seguir' → seguir=root, debe=AUX). Esos sí
  # cuentan.
  #
  # Problema observado: UDPipe spanish-gsd mis-etiqueta verbos finitos como
  # VerbForm=Inf (ej. "Quiero terminar... para poder descansar." marca
  # *Quiero* root|VerbForm=Inf cuando es Mood=Ind|VerbForm=Fin). El filtro
  # debe distinguir el infinitivo root LEGÍTIMO (perífrasis con AUX hijo)
  # del MISPARSE (root Inf sin AUX hijo).
  #
  # Implementación: VerbForm=Inf + pos=VERB, con regla específica para
  # dep_rel=root → solo si tiene un AUX hijo finito (perífrasis modal).
  aux_fin_children <- tokens %>%
    dplyr::filter(
      .data$pos == "AUX",
      stringr::str_detect(
        dplyr::coalesce(.data$feats, ""), "VerbForm=Fin"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    head_token_id_int = .data$head_token_id_int) %>%
    dplyr::mutate(has_aux_fin_child = TRUE)

  f24 <- tokens %>%
    dplyr::filter(
      .data$pos == "VERB",
      .data$.vf == "Inf"
    ) %>%
    dplyr::left_join(
      aux_fin_children,
      by = c("doc_id", "sentence_id",
             "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(
      # root → solo si hay AUX finito hijo (perífrasis); excluye misparse
      dplyr::coalesce(.data$dep_rel, "") != "root" |
        dplyr::coalesce(.data$has_aux_fin_child, FALSE)
    ) %>%
    count_feature_traced("f_24_infinitives")

  # -- f_25  Gerundio adverbial / complemento --------------------------------
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
    count_feature_traced("f_25_present_participle")

  # -- f_26  Participio adverbial / absoluto ---------------------------------
  # VerbForm=Part + Voice?Pass con dep_rel=advcl|ccomp|acl
  # (participio absoluto: <<llegado el momento?>>, <<terminada la reunion?>>)
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
    count_feature_traced("f_26_past_participle")

  # -- f_27  Participio postnominal (whiz-deletion) --------------------------
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
    count_feature_traced("f_27_past_participle_whiz")

  # f_28 (gerundio postnominal / whiz-deletion) ELIMINADO:
  # El gerundio espanol no puede funcionar como modificador nominal postnominal;
  # su uso en esa posicion es agramatical en espanol normativo. La funcion
  # equivalente se realiza mediante clausulas de relativo completas (f_29/f_30).
  # Mapear f_28 a esas relativas supondria doblar el conteo. Intraducible.
  # Ver biber_espanol_completo.md sec. F_28.

  counts <- doc_ids %>%
    dplyr::left_join(f24$counts, by = "doc_id") %>%
    dplyr::left_join(f25$counts, by = "doc_id") %>%
    dplyr::left_join(f26$counts, by = "doc_id") %>%
    dplyr::left_join(f27$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f24$evidence, f25$evidence, f26$evidence, f27$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 2.  block_relatives_es   f_29-f_34
# -----------------------------------------------------------------------------

#' Relative clause features (Spanish)
#'
#' Estrategia UD pura:
#'   - Clausulas relativas = tokens con dep_rel que comienza por "acl"
#'     (acl, acl:relcl) cuyo marcador (dep_rel=mark o ref) es un pronombre
#'     relativo o wh.
#'   - Tipo de relativa (sujeto/objeto) se determina por la funcion UD
#'     del pronombre relativo dentro de la clausula subordinada.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @return Data frame: one row per doc, columns f_29 - f_34
#' @keywords internal
block_relatives_es <- function(tokens, doc_ids, head_lookup) {

  # Tabla de pronombres/adverbios relativos con su funcion en la relativa
  # -- pronombres relativos en UD espanol
  rel_pronouns_all <- c(
    "que", "quien", "quienes",
    "cual", "cuales",
    "cuyo", "cuya", "cuyos", "cuyas",
    "donde", "cuando", "como"
  )
  rel_subj_roles <- c("nsubj", "nsubj:pass")
  # "nmod" se añade para capturar "quien/cual" tras preposición (dep_rel=nmod
  # en Spanish-GSD cuando el pronombre relativo es el objeto de una PP).
  rel_obj_roles  <- c("obj", "iobj", "obl", "obl:agent", "nmod")

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

  # Head de la clausula relativa: el token cuya dep_rel = acl*
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

  # Discriminador sujeto/objeto de la relativa con "que" (Spanish-GSD anota
  # el relativo como SCONJ/mark, sin distinción de caso). Regla operacional
  # de biber_espanol_completo.md §f_29: "the relative is the subject when,
  # within the acl:relcl clause, no other nsubj is present" → su inversa
  # define f_30: si el verbo de la relativa TIENE un nsubj externo explícito
  # (otro es el sujeto), el relativo "que" llena el hueco de OBJETO.
  # Nota: el refinamiento por persona morfológica se descartó porque
  # Spanish-GSD mis-etiqueta el pretérito 1ª sg ("compré") como
  # Person=3|Tense=Fut; en pro-drop puro el hueco no es distinguible y
  # queda en f_29 (limitación documentada §2.5/§6/§f_29).
  relcl_obj_ids <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj"),
      .data$lemma != "que",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    acl_head_id = .data$head_token_id_int) %>%
    dplyr::mutate(is_obj_relcl = TRUE)

  # -- f_29  Relativa de sujeto (que + quien/cual fusionados) ----------------
  # FUSION f_29 + f_31 segun biber_espanol_completo.md:
  # En espanol "que" cubre tanto "that" (f_29) como "who/which" (f_31) en
  # posicion de sujeto de relativa especificativa. "Quien/quienes" y
  # "cual/cuales" son variantes formales que se absorben en f_29.
  # El colapso formal hace imposible distinguir f_29 y f_31 en superficie.

  # Pronombres relativos que pueden ser sujeto de relativa (que + quien/cual)
  rel_subj_lemmas <- c("que", "quien", "quienes", "cual", "cuales")

  f29_que <- rel_tokens %>%
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
                      by = c("doc_id", "sentence_id", "antecedent_id"))

  # Rama adicional para Spanish-GSD: "que" relativo etiquetado como SCONJ/mark
  # cuyo head inmediato es un verbo con dep_rel=acl* (relativa especificativa).
  # En Spanish-GSD el pronombre relativo "que" se anota como SCONJ/mark,
  # no como PRON/nsubj. Esta rama captura ambas posiciones (sujeto y objeto)
  # puesto que en espanol pro-drop no es posible distinguirlas fiablemente.
  f29_que_sconj <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::filter(!is.na(.data$antecedent_id)) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    # Solo SUJETO: descartar relativas cuyo verbo es objeto-tipo
    dplyr::left_join(
      relcl_obj_ids,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::filter(!dplyr::coalesce(.data$is_obj_relcl, FALSE))

  wh_pronouns <- c("quien", "quienes", "cual", "cuales")

  f29_wh <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_pronouns,
      dplyr::coalesce(.data$dep_rel, "") %in% rel_subj_roles
    )

  # Phase 2f: select() preserva las columnas E1 antes del bind_rows; el
  # dedup y la evidencia los hace count_feature_traced (mismo dedup que
  # el distinct anterior, ahora con .keep_all = TRUE para conservar
  # token/lemma/pos/feats).
  rel_evidence_cols <- c("doc_id", "sentence_id", "token_id_int",
                         "token", "lemma", "pos", "feats", "head_token_id_int")
  f29 <- dplyr::bind_rows(
    f29_que       %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f29_que_sconj %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f29_wh        %>% dplyr::select(dplyr::all_of(rel_evidence_cols))
  ) %>%
    count_feature_traced("f_29_that_subj")

  # -- f_30  Relativa de objeto (que + quien/cual fusionados) ---------------
  # FUSION f_30 + f_32 segun biber_espanol_completo.md:
  # Mismo razonamiento que f_29+f_31 para posicion de objeto.

  f30_que <- rel_tokens %>%
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
                      by = c("doc_id", "sentence_id", "antecedent_id"))

  # Rama SCONJ/mark para "que" objeto: cuando el verbo acl:relcl tiene
  # sujeto explícito (no pro-dropped), el gap corresponde al objeto.
  # En la práctica, f_30 captura qui/cual en posición objeto (detectable)
  # y "que"/SCONJ/mark cuya cláusula relativa tiene nsubj explícito.
  f30_que_obj <- tokens %>%
    dplyr::filter(
      .data$lemma == "que",
      .data$pos   == "SCONJ",
      dplyr::coalesce(.data$dep_rel, "") == "mark",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      acl_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    ) %>%
    dplyr::filter(!is.na(.data$antecedent_id)) %>%
    dplyr::inner_join(antecedents,
                      by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    # Solo OBJETO: relativas cuyo verbo tiene nsubj externo o es 1ª/2ª persona
    # (sujeto pro-dropped → el relativo "que" llena el hueco de objeto).
    # biber_espanol_completo.md §f_30.
    dplyr::inner_join(
      relcl_obj_ids,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "acl_head_id")
    )

  f30_wh <- rel_tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_pronouns,
      dplyr::coalesce(.data$dep_rel, "") %in% rel_obj_roles
    )

  f30 <- dplyr::bind_rows(
    f30_que     %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f30_que_obj %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f30_wh      %>% dplyr::select(dplyr::all_of(rel_evidence_cols))
  ) %>%
    count_feature_traced("f_30_that_obj")

  # f_31 y f_32 absorbidos en f_29 y f_30 respectivamente.
  # No se generan columnas independientes en el output.

  # -- f_33  Pied-piping (preposicion + pronombre relativo) ------------------
  # En español el patrón típico de pied-piping en UDPipe Spanish-GSD es:
  #   ADP + DET + PRON_rel  (con los que, en el que, por los cuales)
  #   ADP + PRON_rel        (con quien, en cual)
  # Aceptar también que el "que" relativo aparezca como SCONJ/mark (UDPipe
  # spanish-gsd suele etiquetarlo así dentro de relativas con preposición).
  # Heurística: el pronombre/conjunción relativo está precedido por ADP
  # 1 o 2 posiciones antes en la misma oración, y el head es un VERB en
  # cláusula relativa (acl:relcl).
  f33 <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::filter(
      .data$lemma %in% rel_pronouns_all,
      .data$pos %in% c("PRON", "SCONJ"),
      (dplyr::lag(.data$pos, n = 1L, default = "") == "ADP") |
        (dplyr::lag(.data$pos, n = 2L, default = "") == "ADP" &
         dplyr::lag(.data$pos, n = 1L, default = "") == "DET")
    ) %>%
    dplyr::ungroup() %>%
    count_feature_traced("f_33_pied_piping")

  # -- f_34  Relativa oracional (lo que, eso que, lo cual) -------------------
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
    )

  # Rama heurística posicional (biber_espanol_completo.md §f_34): "coma +
  # lo que / lo cual en posición inicial de cláusula" es señal fuerte de
  # relativa oracional. Spanish-GSD adjunta el verbo de la relativa con
  # dep_rel=parataxis/advcl/conj (no acl) y "cual"/"que" cuelga de ese
  # verbo, cuyo head es a su vez un VERB (antecedente clausal, no nominal).
  # El requisito de coma previa excluye las relativas libres ("Lo que
  # quieras está bien", sin coma, inicial).
  verb_heads_lkp <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      vh_id  = .data$token_id_int,
      vh_pos = .data$pos
    )
  f34_locual <- tokens %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(
      prev_pos   = dplyr::lag(.data$pos),
      prev_tok   = dplyr::lag(.data$token),
      next_tok   = dplyr::lead(stringr::str_to_lower(.data$token))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      # "lo" clítico se lematiza como "él" en Spanish-GSD → usar superficie
      stringr::str_to_lower(.data$token) == "lo",
      dplyr::coalesce(.data$prev_pos, "") == "PUNCT" |
        dplyr::coalesce(.data$prev_tok, "") == ",",
      dplyr::coalesce(.data$next_tok, "") %in% c("cual", "que"),
      !is.na(.data$head_token_id_int)
    ) %>%
    # el "lo" cuelga del relativo (cual/que); ese relativo cuelga del verbo
    # de la relativa; ese verbo cuelga de otro VERB (cláusula antecedente)
    dplyr::left_join(
      tokens %>% dplyr::transmute(
        .data$doc_id, .data$sentence_id,
        rel_id = .data$token_id_int,
        rel_head = .data$head_token_id_int
      ),
      by = c("doc_id", "sentence_id", "head_token_id_int" = "rel_id")
    ) %>%
    dplyr::left_join(
      tokens %>% dplyr::transmute(
        .data$doc_id, .data$sentence_id,
        rv_id = .data$token_id_int,
        rv_head = .data$head_token_id_int
      ),
      by = c("doc_id", "sentence_id", "rel_head" = "rv_id")
    ) %>%
    dplyr::left_join(
      verb_heads_lkp,
      by = c("doc_id", "sentence_id", "rv_head" = "vh_id")
    ) %>%
    dplyr::filter(dplyr::coalesce(.data$vh_pos, "") == "VERB")

  f34 <- dplyr::bind_rows(
    f34        %>% dplyr::select(dplyr::all_of(rel_evidence_cols)),
    f34_locual %>% dplyr::select(dplyr::all_of(rel_evidence_cols))
  ) %>%
    count_feature_traced("f_34_sentence_relatives")

  # f_31 y f_32 han sido absorbidos en f_29 y f_30 respectivamente
  # (fusion segun biber_espanol_completo.md); no se generan columnas separadas.
  counts <- doc_ids %>%
    dplyr::left_join(f29$counts, by = "doc_id") %>%
    dplyr::left_join(f30$counts, by = "doc_id") %>%
    dplyr::left_join(f33$counts, by = "doc_id") %>%
    dplyr::left_join(f34$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
  evidence <- bind_evidence(f29$evidence, f30$evidence, f33$evidence, f34$evidence)
  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 3.  block_clause_embedding_es   f_21-f_23, f_35-f_38 (f_60 eliminado)
# -----------------------------------------------------------------------------

#' Complementizer, subordinator, and clause-embedding features (Spanish)
#'
#' f_21  That-complementizador tras VERB ("dijo que", "sabe que")
#' f_22  That-complementizador tras ADJ  ("seguro de que", "feliz de que")
#' f_23  Clausula-wh (relativa/interrogativa indirecta)
#' f_35  Subordinada causal    (solo: porque)
#' f_36  Subordinada concesiva (solo: aunque)
#' f_37  Subordinada condicional (si, a_menos_que, salvo_que)
#' f_38  Otros subordinadores adverbiales (cuando, mientras, antes_de_que?)
#'
#' f_60 (that-deletion) ELIMINADO: intraducible. En espanol el complementante
#' <<que>> es practicamente obligatorio; su omision es agramatical.
#' biber_espanol_completo.md sec. F_60.
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @param dict_lookup Dictionary lookup (para listas de subordinadores)
#' @return Data frame: one row per doc, columns f_21-f_23, f_35-f_38
#' @keywords internal
block_clause_embedding_es <- function(tokens, doc_ids, head_lookup,
                                       dict_lookup = NULL) {

  # -- f_21  <<que>> complementizador tras VERB --------------------------------
  # Condiciones UD:
  #   - token lemma = "que", pos = SCONJ
  #   - dep_rel = mark
  #   - head del mark tiene pos = VERB o AUX
  # Tabla dep_rel del head: distingue complemento (ccomp/xcomp/…) de
  # relativa (acl:relcl). biber_espanol_completo.md §f_21 (EXCLUDE): "que"
  # como pronombre relativo en acl:relcl va a f_29/f_30, NO a f_21.
  head_deprel <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      head_dep_rel = .data$dep_rel
    )

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
    dplyr::left_join(
      head_deprel,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX"),
      !stringr::str_detect(dplyr::coalesce(.data$head_dep_rel, ""), "^acl")
    ) %>%
    count_feature_traced("f_21_that_verb_comp")

  # -- f_22  <<que>> complementizador tras ADJ --------------------------------
  # El head inmediato de la clausula marcada con <<que>> es un ADJ
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
    count_feature_traced("f_22_that_adj_comp")

  # -- f_23  Clausula-wh (interrogativa indirecta) ---------------------------
  # Spec \u00a7f_23 Include: solo wh ACENTUADOS en posici\u00f3n subordinada/argumental.
  # Spec \u00a7f_23 Exclude: "Relative uses of the same words without accents".
  #
  # UDPipe spanish-gsd marca "que" relativo como PronType=Int,Rel
  # (ambivalente) y a veces normaliza la tilde en el lema. Por tanto:
  # (a) Exigir tilde en la FORMA SUPERFICIAL del token (no solo en el lema).
  # (b) Excluir heads cuya dep_rel sea acl/acl:relcl (claramente relativas).
  wh_adv_tilde <- c("d\u00f3nde", "cu\u00e1ndo", "c\u00f3mo",
                    "cu\u00e1nto", "cu\u00e1nta",
                    "cu\u00e1ntos", "cu\u00e1ntas")
  wh_pron_tilde <- c("qu\u00e9", "qui\u00e9n", "qui\u00e9nes",
                     "cu\u00e1l", "cu\u00e1les")
  # Lemas sin tilde que aceptamos SOLO si el token superficial trae tilde
  # (compensa la inconsistencia de lematizaci\u00f3n de UDPipe spanish-gsd).
  wh_pron_lemmas <- c(wh_pron_tilde,
                      "que", "quien", "quienes", "cual", "cuales")

  is_int_pron <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("PRON", "DET", "ADJ"),
      .data$lemma %in% wh_pron_lemmas,
      stringr::str_detect(
        dplyr::coalesce(.data$feats, ""), "PronType=[^|]*Int"
      ),
      # Tilde en la forma superficial \u2014 descarta "que/quien/cual" relativos
      tolower(.data$token) %in% wh_pron_tilde |
        .data$lemma %in% wh_pron_tilde
    )

  is_int_adv <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV",
      .data$lemma %in% wh_adv_tilde
    )

  # dep_rel del verbo que encabeza la cláusula-wh: si es "root" la cláusula
  # es principal → pregunta DIRECTA (va a f_13, NO a f_23). La interrogativa
  # indirecta tiene el verbo subordinado (ccomp/xcomp/advcl/acl…).
  # biber_espanol_completo.md §f_23 (EXCLUDE): "Direct questions → f_13".
  wh_head_deprel <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int = .data$token_id_int,
      wh_head_dep = .data$dep_rel
    )

  f23 <- dplyr::bind_rows(is_int_pron, is_int_adv) %>%
    dplyr::filter(
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(obj|obl|nsubj|iobj|advmod|nmod|ccomp)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(
      wh_head_deprel,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_pos, "") %in% c("VERB", "AUX", "ADJ"),
      dplyr::coalesce(.data$wh_head_dep, "") != "root",
      # Spec §f_23 Exclude: relativas. UDPipe marca el head como acl/acl:relcl
      # cuando el wh introduce una relativa (caso "El equipo que fue asignado").
      !dplyr::coalesce(.data$wh_head_dep, "") %in% c("acl", "acl:relcl")
    ) %>%
    count_feature_traced("f_23_wh_clause")

  # -- f_35  Causal ----------------------------------------------------------
  # Solo "porque" -- restriccion lexica estrecha paralela a "because" en Biber
  # (1985), que tampoco incluye since/as causales. Se excluyen ya_que,
  # puesto_que, dado_que, pues, como causal. biber_espanol_completo.md F_35.
  causal_lemmas <- c("porque")

  f35 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% causal_lemmas,
      .data$pos   %in% c("SCONJ", "CCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc|advmod)"
      )
    ) %>%
    count_feature_traced("f_35_because")

  # -- f_36  Concesiva -------------------------------------------------------
  # Solo "aunque" -- cubre tanto "although" como "though" de Biber (1985).
  # Se excluyen si_bien, aun_cuando, a_pesar_de_que, pese_a_que.
  # biber_espanol_completo.md F_36.
  concessive_lemmas <- c("aunque")

  f36 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% concessive_lemmas,
      .data$pos   %in% c("SCONJ", "CCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc|advmod)"
      )
    ) %>%
    count_feature_traced("f_36_though")

  # -- f_37  Condicional -----------------------------------------------------
  # "si" condicional + "a_menos_que" y "salvo_que" como equivalentes de
  # "unless". Se excluyen en_caso_de_que, siempre_que, siempre_y_cuando,
  # con_tal_de_que (estructuralmente paralelo a excluir provided/as_long_as
  # en Biber). biber_espanol_completo.md F_37.
  conditional_lemmas <- c("si", "a_menos_que", "salvo_que")

  # biber_espanol_completo.md §f_37 (EXCLUDE crítico): "si" como
  # complementante interrogativo indirecto NO cuenta. El discriminador es el
  # verbo regente: si el abuelo de "si" (head del verbo subordinado que
  # "si" marca) es verbo de comunicación/cognición → interrogativa indirecta
  # ("preguntó si vendría", "no sé si vino"), se excluye.
  interrog_governors <- c(
    "preguntar", "saber", "ignorar", "dudar", "desconocer", "averiguar",
    "comprobar", "ver", "decidir", "recordar", "contar", "decir",
    "explicar", "entender", "comprender", "olvidar", "consultar",
    "investigar", "examinar", "determinar", "aclarar", "cuestionar"
  )
  # Tabla verbo-subordinado → su head (abuelo). Claves sin colisión:
  # mid_id  = id del verbo que "si" marca (head de "si")
  # gp_id   = head de ese verbo (abuelo de "si")
  mid_to_gp <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      mid_id = .data$token_id_int,
      gp_id  = .data$head_token_id_int
    )
  gp_lemma_tbl <- tokens %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      gp_id    = .data$token_id_int,
      gp_lemma = .data$lemma
    )

  f37 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% conditional_lemmas,
      .data$pos   %in% c("SCONJ", "ADV"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^mark"
      )
    ) %>%
    dplyr::mutate(mid_id = .data$head_token_id_int) %>%
    dplyr::left_join(mid_to_gp,
                     by = c("doc_id", "sentence_id", "mid_id")) %>%
    dplyr::left_join(gp_lemma_tbl,
                     by = c("doc_id", "sentence_id", "gp_id")) %>%
    dplyr::filter(
      !dplyr::coalesce(.data$gp_lemma, "") %in% interrog_governors
    ) %>%
    count_feature_traced("f_37_if")

  # -- f_38  Otros subordinadores adverbiales --------------------------------
  # Todo SCONJ con dep_rel=mark que NO sea ya contado en f_21/f_22/f_35/f_36/f_37
  counted_sub <- unique(c(
    "que",
    causal_lemmas, concessive_lemmas, conditional_lemmas
  ))

  # biber_espanol_completo.md §f_38 incluye "mientras" (y mientras_que) como
  # subordinador adverbial. Spanish-GSD lo etiqueta de forma inconsistente
  # como SCONJ o CCONJ (en "mientras que" → mientras=CCONJ/mark, que=SCONJ/
  # fixed). Se añade rama CCONJ restringida a una allowlist de lemas
  # subordinantes para no capturar coordinantes (y/o/pero/ni).
  adv_sub_cconj <- c("mientras", "conforme", "según", "segun")

  f38 <- tokens %>%
    dplyr::filter(
      (
        .data$pos %in% c("SCONJ", "ADP", "ADV") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark")
      ) | (
        .data$pos == "CCONJ" &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark") &
          stringr::str_to_lower(.data$lemma) %in% adv_sub_cconj
      ),
      !.data$lemma %in% counted_sub
    ) %>%
    count_feature_traced("f_38_other_adv_sub")

  # f_60 (that-deletion) ELIMINADO: intraducible.
  # Ver docstring y biber_espanol_completo.md sec. F_60.

  counts <- doc_ids %>%
    dplyr::left_join(f21$counts, by = "doc_id") %>%
    dplyr::left_join(f22$counts, by = "doc_id") %>%
    dplyr::left_join(f23$counts, by = "doc_id") %>%
    dplyr::left_join(f35$counts, by = "doc_id") %>%
    dplyr::left_join(f36$counts, by = "doc_id") %>%
    dplyr::left_join(f37$counts, by = "doc_id") %>%
    dplyr::left_join(f38$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )

  evidence <- bind_evidence(
    f21$evidence, f22$evidence, f23$evidence,
    f35$evidence, f36$evidence, f37$evidence, f38$evidence
  )

  make_block_result(counts = counts, evidence = evidence)
}
