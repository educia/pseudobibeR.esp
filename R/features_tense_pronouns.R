# Tense, aspect, and pronoun features for Spanish
#
# NOTA LINGÜÍSTICA — pro-drop:
#   El español es una lengua de sujeto nulo. Los pronombres personales
#   explícitos (yo, tú, él…) son marcadamente informativos: señalan
#   contraste, énfasis o desambiguación. Su frecuencia distingue registros
#   de manera más fuerte que en francés. Por eso contamos:
#     f_06  pronombres de 1ª persona explícitos (yo, me, mí, conmigo, nos,
#           nosotros/as)
#     f_07  pronombres de 2ª persona explícitos (tú, vos, te, ti, contigo,
#           vosotros/as, usted, ustedes)
#     f_08  pronombres de 3ª persona explícitos (él, ella, ello, ellos,
#           ellas, le, lo, la, les, los, las, consigo)
#   Excluimos los clíticos reflexivos puros (se, me, te como reflexivos)
#   cuando su dep_rel es "expl:pv" o "expl:impers" o "expl".
#
# NOTA LINGÜÍSTICA — aspecto perfecto (f_02):
#   En español el aspecto perfecto es HABER + participio (he llegado).
#   ESTAR + participio es voz pasiva de estado (la puerta está cerrada),
#   ya contada en f_17/f_18. La dificultad es que los modelos UD españoles
#   a veces no asignan Voice=Pass a estar+Part copulativo, etiquetando
#   el auxiliar solo como AUX sin rasgo de voz. Para evitar el falso
#   positivo excluimos estar cuando su head recibe dep_rel = "cop" o
#   cuando estar mismo tiene dep_rel = "cop".

# ─────────────────────────────────────────────────────────────────────────────
# 1.  block_aux_tense_es  (f_02 aspecto perfecto, f_12 verbo pro-verbal hacer)
# ─────────────────────────────────────────────────────────────────────────────

#' Extract tense and auxiliary-related features (Spanish)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param proverb_pronouns Vector of proverb pronoun lemmas
#' @return Data frame with f_02_perfect_aspect and f_12_proverb_do
#' @keywords internal
block_aux_tense_es <- function(tokens, doc_ids, head_lookup, proverb_pronouns) {

  # ── f_02  Aspecto perfecto: HABER + participio (excluye estar copulativo) ──
  #
  # Exclusiones:
  #   1. estar con dep_rel = "cop"  (estar es cópula del participio-adjetivo)
  #   2. estar cuyo head tiene dep_rel = "cop" en otro token de la misma or.
  #      (doble seguridad para parsers que asignan cop al participio/adjetivo)
  #   3. head con Voice=Pass explícito (cubre los casos bien anotados)
  #
  # Solo HABER como auxiliar perfectivo es inequívoco en UD español.
  # Conservamos estar como fallback únicamente si su head es VerbForm=Part
  # SIN Voice=Pass Y SIN dep_rel copulativa — caso raro pero posible en
  # participios de verbos inacusativos ("está llegado" en dialectos).

  # Participios-cabeza que son pasivas de estado con estar:
  # son los que tienen un token "estar" con dep_rel = cop
  estar_cop_heads <- tokens %>%
    dplyr::filter(
      .data$lemma == "estar",
      .data$pos %in% c("AUX", "VERB"),
      dplyr::coalesce(.data$dep_rel, "") == "cop",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      cop_head_id = .data$head_token_id_int
    ) %>%
    dplyr::distinct()

  perfect_candidates <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("AUX", "VERB"),
      .data$lemma %in% c("haber", "estar"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(aux|cop)"),
      # excluir estar cuando es cópula (ser/estar predicativo)
      !(.data$lemma == "estar" &
          dplyr::coalesce(.data$dep_rel, "") == "cop"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    # excluir cuando el head-participio ya tiene estar como cop (pasiva estado)
    dplyr::anti_join(
      estar_cop_heads,
      by = c(
        "doc_id", "sentence_id",
        "head_token_id_int" = "cop_head_id"
      )
    ) %>%
    dplyr::mutate(
      head_is_participle =
        (.data$head_pos %in% c("VERB", "AUX") &
           dplyr::coalesce(.data$head_morph_verbform, "") == "Part" &
           dplyr::coalesce(.data$head_morph_voice, "")    != "Pass") |
        (.data$head_pos %in% c("ADJ", "NOUN") &
           stringr::str_detect(
             dplyr::coalesce(.data$head_feats, ""), "VerbForm=Part"
           ) &
           dplyr::coalesce(.data$head_morph_voice, "") != "Pass")
    ) %>%
    dplyr::filter(.data$head_is_participle) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE)

  f02 <- perfect_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_02_perfect_aspect = "n")

  # ── f_12  Verbo pro-verbal: hacer + clítico objeto referencial ────────────
  # Ej.: "Lo hago", "¿Lo hace usted?"
  proverb_objects <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$lemma %in% proverb_pronouns,
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^obj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      head_token_id_int  = .data$head_token_id_int,
      has_proverb_object = TRUE
    ) %>%
    dplyr::distinct()

  f12 <- tokens %>%
    dplyr::filter(
      .data$lemma == "hacer",
      .data$pos %in% c("VERB", "AUX"),
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !is.na(.data$token_id_int)
    ) %>%
    dplyr::left_join(
      proverb_objects,
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(.data$has_proverb_object) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_12_proverb_do = "n")

  doc_ids %>%
    dplyr::left_join(f02, by = "doc_id") %>%
    dplyr::left_join(f12, by = "doc_id") %>%
    dplyr::mutate(
      f_02_perfect_aspect = dplyr::coalesce(.data$f_02_perfect_aspect, 0L),
      f_12_proverb_do     = dplyr::coalesce(.data$f_12_proverb_do,     0L)
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 2.  block_personal_pronouns_es
#     f_06 1ª persona  |  f_07 2ª persona  |  f_08 3ª persona
#     f_09 pronombre expletivo/impersonal   |  f_13 pregunta-qué
# ─────────────────────────────────────────────────────────────────────────────

#' Extract personal pronoun and question features (Spanish)
#'
#' f_06/f_07/f_08: conteo simplificado por lemma + exclusión de reflexivos.
#' No se usa bind_rows redundante; un único filtro por lemma es suficiente
#' porque en UD español los pronombres personales tónicos y átonos tienen
#' lemmas inequívocos. morph_person se usa solo como desempate cuando
#' un lemma es ambiguo (p.ej. "nos" puede ser 1ª o—raramente—vocativo).
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param head_lookup Head token lookup table
#' @param de_markers De marker lookup table
#' @param que_markers Que marker lookup table
#' @param clause_complements Clause complement lookup table
#' @param weather_lemmas Weather verb lemmas
#' @param raising_verbs Raising verb lemmas
#' @param wh_question_lemmas WH question word lemmas
#' @return Data frame con f_06 a f_09 y f_13
#' @keywords internal
block_personal_pronouns_es <- function(
    tokens,
    doc_ids,
    head_lookup,
    de_markers,
    que_markers,
    clause_complements,
    weather_lemmas = c(
      "llover", "nevar", "granizar", "lloviznar", "tronar",
      "amanecer", "anochecer", "atardecer"
    ),
    raising_verbs = c(
      "parecer", "resultar", "continuar", "seguir",
      "bastar", "convenir", "quedar"
    ),
    wh_question_lemmas = c(
      "quien",  "qui\u00e9n",
      "que",    "qu\u00e9",
      "cual",   "cu\u00e1l",  "cuales", "cu\u00e1les",
      "donde",  "d\u00f3nde",
      "cuando", "cu\u00e1ndo",
      "como",   "c\u00f3mo",
      "cuanto", "cu\u00e1nto",  "cuanta", "cu\u00e1nta",
      "cuantos","cu\u00e1ntos", "cuantas","cu\u00e1ntas",
      "por_que","por_qu\u00e9"
    )) {

  # dep_rel que indican uso reflexivo/impersonal — excluidos en f_06-f_08
  reflexive_deps <- c("expl:pv", "expl:impers", "expl")

  # Helper: contar pronombres de una persona dada por lista de lemmas
  count_person_pronouns <- function(lemma_list) {
    tokens %>%
      dplyr::filter(
        .data$pos == "PRON",
        .data$lemma %in% lemma_list,
        !dplyr::coalesce(.data$dep_rel, "") %in% reflexive_deps
      ) %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)
  }

  # ── f_06  1ª persona ──────────────────────────────────────────────────
  first_person_lemmas <- c(
    "yo", "nosotros", "nosotras",
    "me", "nos",
    "m\u00ed", "conmigo"
  )
  f06 <- count_person_pronouns(first_person_lemmas) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_06_first_person_pronouns = "n")

  # ── f_07  2ª persona ──────────────────────────────────────────────────
  # "te" es ambiguo: puede ser 2ª sing. átono o parte de "te" impersonal.
  # Lo incluimos pero la exclusión de reflexive_deps filtra los casos
  # expl:pv / expl más problemáticos.
  second_person_lemmas <- c(
    "t\u00fa", "vos", "vosotros", "vosotras",
    "usted", "ustedes",
    "te", "ti", "contigo", "os"
  )
  f07 <- count_person_pronouns(second_person_lemmas) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_07_second_person_pronouns = "n")

  # ── f_08  3ª persona ──────────────────────────────────────────────────
  # "se" con dep_rel reflexivo va a f_09 — aquí lo excluimos explícitamente
  # por lemma además de por reflexive_deps para mayor seguridad.
  third_person_lemmas <- c(
    "\u00e9l", "ella", "ello", "ellos", "ellas",
    "le", "lo", "la", "les", "los", "las",
    "consigo"
  )
  f08 <- count_person_pronouns(third_person_lemmas) %>%
    # excluir "ello" cuando va a f_09 (sujeto impersonal)
    # se maneja en f_09 por separado; no restamos aquí para no perder
    # usos referenciales de "ello" ("ello nos preocupó" referencial)
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_08_third_person_pronouns = "n")

  # ── f_09  Pronombre expletivo / impersonal ─────────────────────────────
  # Análogo al "il" expletivo francés. En español:
  #   (a) "ello" como sujeto impersonal ("Ello implica que…")
  #   (b) "se" con dep_rel expl:impers o expl
  #   (c) haber impersonal (hay, hubo, habrá…) sin nsubj explícito
  pronoun_it_candidates <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$lemma %in% c("ello", "se"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(de_markers,         by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(que_markers,        by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(clause_complements, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::mutate(
      has_de_marker   = dplyr::coalesce(.data$has_de_marker,   FALSE),
      has_que_marker  = dplyr::coalesce(.data$has_que_marker,  FALSE),
      has_clause_comp = dplyr::coalesce(.data$has_clause_comp, FALSE),
      is_weather         = .data$head_lemma %in% weather_lemmas,
      is_raising_verb    = .data$head_lemma %in% raising_verbs,
      has_control_marker = .data$has_de_marker | .data$has_que_marker | .data$has_clause_comp
    ) %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^expl") |
        (stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj") &
           .data$lemma == "ello" &
           (.data$is_weather |
              (.data$head_pos == "ADJ" & .data$has_control_marker) |
              (.data$head_pos %in% c("VERB", "AUX") &
                 (.data$is_raising_verb | .data$has_control_marker))))
    )

  # haber impersonal: sin nsubj colgando de este token
  haber_impersonal <- tokens %>%
    dplyr::filter(
      .data$lemma == "haber",
      .data$pos %in% c("VERB", "AUX"),
      .data$dep_rel %in% c("root", "ccomp", "xcomp", "advcl") |
        is.na(.data$dep_rel)
    ) %>%
    dplyr::anti_join(
      tokens %>%
        dplyr::filter(
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj")
        ) %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$head_token_id_int
        ),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    )

  f09 <- dplyr::bind_rows(
    pronoun_it_candidates %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int),
    haber_impersonal %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)
  ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_09_pronoun_it = "n")

  # ── f_13  Preguntas-qué ───────────────────────────────────────────────────
  question_sentences <- tokens %>%
    dplyr::filter(.data$token == "?") %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id, has_question = TRUE) %>%
    dplyr::distinct()

  f13 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_question_lemmas,
      .data$pos %in% c("ADV", "PRON", "DET", "ADJ")
    ) %>%
    dplyr::left_join(question_sentences, by = c("doc_id", "sentence_id")) %>%
    dplyr::filter(!is.na(.data$has_question)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_13_wh_question = "n")

  # ── Ensamblar ──────────────────────────────────────────────────────────────────────
  doc_ids %>%
    dplyr::left_join(f06, by = "doc_id") %>%
    dplyr::left_join(f07, by = "doc_id") %>%
    dplyr::left_join(f08, by = "doc_id") %>%
    dplyr::left_join(f09, by = "doc_id") %>%
    dplyr::left_join(f13, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}
