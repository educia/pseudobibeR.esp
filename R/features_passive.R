# Passive voice, copular and existential features for Spanish
#
# NOTA LINGUISTICA -- tres tipos de pasiva en espanol:
#
#   (A) Pasiva PERIFRASTICA (ser/estar + participio)
#       <<El libro fue escrito por Galdos.>>
#       El parser UD marca el auxiliar con dep_rel = aux:pass.
#       Detectada por passive_rel_values (igual que el frances).
#
#   (B) Pasiva REFLEJA / SE-PASIVA
#       <<Se publico el informe.>>
#       El parser UD marca el se con dep_rel = expl:pass o expl.
#       El verbo lleva Voice=Pass en feats o VerbForm=Fin sin aux:pass.
#       NO la detecta la heuristica francesa -- requiere logica propia.
#
#   (C) Pasiva ESTATIVA (estar + participio)
#       <<El puente esta cerrado.>>
#       El participo lleva Voice=Pass o Aspect=Perf. En UD suele
#       etiquetarse el participio como ADJ con dep_rel = amod/nmod.
#       Se incluye en (A) cuando estar lleva aux:pass.
#
# f_17  Pasiva agentiva sin agente explicito  (A+B sin <<por>>-frase)
# f_18  Pasiva con agente <<por>>               (A+B con <<por>>-frase)
# f_19  Ser/estar como verbo principal (copula nominal/adjetival, no aux)
# f_20  Haber existencial impersonal           (hay, habia, hubo?)

#' Extract passive voice, copular and existential features (Spanish)
#'
#' Detecta la pasiva perifistica (ser/estar + participo, aux:pass) y la
#' pasiva refleja (se + VerbForm=Part/Fin, expl:pass). Excluye haber
#' perfect de f_17/f_18. Distingue ser/estar copulativo (f_19) del
#' auxiliar de pasiva. Detecta haber impersonal-existencial (f_20).
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param passive_rel_values Dependency relation values for passive aux
#'   ("aux:pass" for udpipe; c("auxpass","aux:pass") for spaCy)
#' @return Data frame con f_17 a f_20
#' @keywords internal
block_passive_voice_es <- function(tokens, doc_ids, head_lookup,
                                   passive_rel_values) {

  # -----------------------------------------------------------------------
  # (A) Pasiva perifistica -- aux marcado como aux:pass
  # -----------------------------------------------------------------------
  perifrastic_passive <- tokens %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values,
                  .data$lemma %in% c("ser", "estar"),
                  !is.na(.data$head_token_id_int)) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(.data$head_pos == "VERB") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$head_token_id_int, .keep_all = TRUE)

  # -----------------------------------------------------------------------
  # (B) Pasiva refleja / impersonal con "se"
  #
  # UDPipe Spanish-GSD NO usa expl:pass para "se" pasivo: lo etiqueta como
  # PRON con dep_rel "iobj"/"obj"/"obl"/"expl" según el caso, y el lemma
  # suele ser "él" (no "se"). Tampoco marca Voice=Pass en el verbo. Por
  # eso detectamos por superficie: token "se" + verbo finito como head.
  #
  # Esta heurística captura tanto pasivas reflejas (se publicaron los
  # hallazgos) como impersonales con se (se debe seguir, se recomienda),
  # que comparten la función comunicativa de pasiva sin agente. Acepta
  # algún ruido por reflexivos genuinos (se levantó), pero en registros
  # académicos/instruccionales el "se" + V es mayoritariamente impersonal.
  # -----------------------------------------------------------------------
  se_passive <- tokens %>%
    dplyr::filter(
      tolower(.data$token) == "se",
      .data$pos == "PRON",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(
      .data$head_pos %in% c("VERB", "AUX"),
      # Verbo finito (excluye reflexivos en infinitivo/gerundio/participio)
      dplyr::coalesce(.data$head_morph_verbform, "") == "Fin",
      # Tercera persona (excluye "te miras", "me miro" reflexivos personales)
      stringr::str_detect(
        dplyr::coalesce(.data$head_feats, ""), "Person=3"
      )
    ) %>%
    # Excluir cuando ya hay un aux:pass (ya contado en perifrástica)
    dplyr::anti_join(
      perifrastic_passive %>%
        dplyr::select("doc_id", "sentence_id", "head_token_id_int"),
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    .data$head_token_id_int, .keep_all = TRUE)

  # -----------------------------------------------------------------------
  # Union A + B
  # -----------------------------------------------------------------------
  all_passive <- dplyr::bind_rows(
    perifrastic_passive %>%
      dplyr::select("doc_id", "sentence_id", "head_token_id_int",
                    "passive_agent_next2", "passive_agent_next3"),
    se_passive %>%
      dplyr::select("doc_id", "sentence_id", "head_token_id_int",
                    "passive_agent_next2", "passive_agent_next3")
  ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int,
                    .keep_all = TRUE)

  # f_17  Agentless passives (sin frase <<por>> en las dos posiciones siguientes)
  f17 <- all_passive %>%
    dplyr::filter(!.data$passive_agent_next2,
                  !.data$passive_agent_next3) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_17_agentless_passives = "n")

  # f_18  By-passives (con frase <<por>>)
  f18 <- all_passive %>%
    dplyr::filter(.data$passive_agent_next2 | .data$passive_agent_next3) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_18_by_passives = "n")

  # -----------------------------------------------------------------------
  # f_19  Ser/estar como verbo principal (copula nominal/adjetival)
  #       Excluye: (1) uso como aux de pasiva perifistica,
  #                (2) uso como aux de aspecto progresivo (estar + gerundio)
  # -----------------------------------------------------------------------
  # Token IDs que ya sabemos son aux:pass
  aux_pass_ids <- tokens %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values,
                  .data$lemma %in% c("ser", "estar")) %>%
    dplyr::select("doc_id", "sentence_id", "token_id_int")

  # Token IDs que son aux de progresivo (estar + gerundio)
  progressive_aux_ids <- tokens %>%
    dplyr::filter(.data$lemma == "estar",
                  .data$pos %in% c("AUX", "VERB"),
                  stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""),
                                      "^aux"),
                  !is.na(.data$head_token_id_int)) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::select("doc_id", "sentence_id",
                      "token_id_int", "morph_verbform") %>%
        dplyr::rename(head_vf = "morph_verbform"),
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_vf == "Ger") %>%
    dplyr::select("doc_id", "sentence_id", "token_id_int")

  f19 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("ser", "estar"),
      # dep_rel NO debe ser aux de ningun tipo
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux")
    ) %>%
    dplyr::anti_join(aux_pass_ids,
                     by = c("doc_id", "sentence_id", "token_id_int")) %>%
    dplyr::anti_join(progressive_aux_ids,
                     by = c("doc_id", "sentence_id", "token_id_int")) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_19_be_main_verb = "n")

  # -----------------------------------------------------------------------
  # f_20  Haber existencial impersonal (hay, habia, hubo, habra?)
  #       Condicion: lemma = haber, VERB/AUX, dep_rel root/ccomp/xcomp/advcl
  #       y SIN nsubj dependiente (eso lo distingue del haber perfecto).
  #       Se normaliza el token para capturar formas con y sin tilde.
  # -----------------------------------------------------------------------
  existential_forms <- c(
    "hay",
    "habia", "hab\u00eda",
    "hubo",
    "habra", "habr\u00e1",
    "habria", "habr\u00eda",
    "haya",                    # subjuntivo presente
    "hubiera", "hubiese",      # subjuntivo pasado
    "habiendo"                 # gerundio existencial
  )

  haber_nodes <- tokens %>%
    dplyr::filter(
      .data$lemma == "haber",
      .data$pos %in% c("VERB", "AUX"),
      stringr::str_to_lower(
        stringr::str_replace_all(.data$token, "[\u00e0-\u00fc]",
          function(x) iconv(x, to = "ASCII//TRANSLIT"))
      ) %in% existential_forms |
        .data$token %in% existential_forms,
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(root|ccomp|xcomp|advcl|acl|parataxis)"
      )
    ) %>%
    # Excluir cuando tiene un nsubj dependiente (= haber perfecto con sujeto)
    dplyr::anti_join(
      tokens %>%
        dplyr::filter(
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj")
        ) %>%
        dplyr::transmute(.data$doc_id, .data$sentence_id,
                         head_token_id_int = .data$head_token_id_int),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    )

  f20 <- haber_nodes %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_20_existential_there = "n")

  # -----------------------------------------------------------------------
  # Ensamblar
  # -----------------------------------------------------------------------
  doc_ids %>%
    dplyr::left_join(f17, by = "doc_id") %>%
    dplyr::left_join(f18, by = "doc_id") %>%
    dplyr::left_join(f19, by = "doc_id") %>%
    dplyr::left_join(f20, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
