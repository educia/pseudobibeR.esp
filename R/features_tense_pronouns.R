# features_tense_pronouns.R
# Tense, aspect, pronoun, and adverbial features for Spanish (f_01-f_13)
#
# NOTA LINGUISTICA -- pro-drop:
#   El espanol es lengua de sujeto nulo. Los pronombres personales explicitos
#   son marcadamente informativos (contraste, enfasis, desambiguacion).
#   Contamos SOLO pronombres explicitos; los sujetos nulos no se cuentan.
#
# NOTA -- aspecto perfecto (f_02):
#   HABER + participio = perfecto compuesto (he llegado).
#   ESTAR + participio = pasiva de estado (esta cerrada) -> excluida via
#   anti_join sobre estar_cop_heads.
#
# CAMPOS UD REQUERIDOS EN tokens:
#   doc_id, sentence_id, token_id_int, head_token_id_int,
#   token, lemma, pos (UPOS), dep_rel, feats,
#   morph_tense, morph_mood, morph_verbform, morph_voice, morph_person,
#   morph_number

# -----------------------------------------------------------------------------
# 0.  Helpers internos
# -----------------------------------------------------------------------------

# Extrae un rasgo morfologico concreto de la columna `feats` (formato UD).
# Ej.: extract_feat("Tense=Past|VerbForm=Fin", "Tense") -> "Past"
# Segura con NAs: usa stringr::str_match() que devuelve NA para no-coincidencias,
# siempre con la misma longitud que feats_vec.
extract_feat <- function(feats_vec, feat_name) {
  pattern <- paste0("(?:^|\\|)", feat_name, "=([^|]+)")
  m <- stringr::str_match(dplyr::coalesce(feats_vec, ""), pattern)
  m[, 2L]
}

# Phase 5: count_feature() (retornaba data.frame de conteos) eliminada
# tras la migracion de Phase 2. Toda llamada productiva ahora pasa por
# count_feature_traced() en R/evidence_helpers.R, que retorna
# list(counts, evidence) con identica logica de dedup MWT-safe.

# -----------------------------------------------------------------------------
# 1.  block_tense_es
#     f_01  preterito indefinido         f_02  aspecto perfecto
#     f_03  tiempo presente              f_04  adv. de lugar
#     f_05  adv. de tiempo               f_11  pronombres indefinidos
#     f_71  preterito imperfecto (ext. espanola)
#     f_12  ELIMINADO (intraducible): pro-verbo "do" inexistente en espanol
# -----------------------------------------------------------------------------

#' Tense, aspect, adverbial, and indefinite-pronoun features (Spanish)
#'
#' @param tokens Annotated token data frame (UD format)
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @param place_adverbials Character vector of place-adverbial lemmas (f_04)
#' @param time_adverbials  Character vector of time-adverbial lemmas (f_05)
#' @param indefinite_pronouns Character vector of indefinite pronoun lemmas (f_11)
#' @return Data frame: one row per doc, columns f_01-f_05, f_11, f_71
#' @keywords internal
block_tense_es <- function(
    tokens,
    doc_ids,
    head_lookup,
    place_adverbials,
    time_adverbials,
    indefinite_pronouns
) {

  # Phase 2b migration: cada rasgo se computa via count_feature_traced(),
  # que devuelve list(counts, evidence). El bloque agrega counts via
  # left_join (como antes) y apila evidence via bind_evidence; retorna
  # un block_result para que parse_biber_features() pueda recolectar
  # ambas dimensiones cuando traced=TRUE.

  # -- f_01  Tiempos de pasado de indicativo y subjuntivo --------------------
  # REVISION HERNAN (Fase 1): el "past tense" ingles abarca varios tiempos y
  # aspectos del espanol en indicativo Y subjuntivo. f_01 cuenta todo verbo
  # finito con Tense in {Past, Imp, Pqp}, SIN filtrar por Mood:
  #   - perfecto simple ind. (corri)      -> Tense=Past
  #   - imperfecto ind. (corria)          -> Tense=Imp
  #   - imperfecto subj. (corriera)       -> Tense=Imp
  #   - pluscuamperfecto ind. (habia ...) -> aux 'habia' con Tense=Imp
  #   - pluscuamperf. subj. (hubiera ...) -> aux 'hubiera' con Tense=Imp
  # NOTA UD (verificado, test-udpipe-tag-verification.R): spanish-gsd NO emite
  # Tense=Pqp; el pluscuamperfecto sale con Tense=Imp en el auxiliar. Se deja
  # "Pqp" por robustez ante otros modelos, pero es rama muerta con spanish-gsd.
  # VerbForm=Fin se conserva a proposito: excluye el participio (corrido, que
  # lleva Tense=Past|VerbForm=Part) para no contar el compuesto dos veces aqui.
  # Solapamiento DELIBERADO con f_02: el auxiliar 'habia'/'hubiera' cuenta en
  # f_01 y ademas en f_02 (haber+participio), replicando "had written" del
  # ingles. Ver INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md, Fase 1 / f_01.
  f01 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") %in% c("Past", "Imp", "Pqp"),
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature_traced("f_01_past_tense")

  # -- f_71  Preterito imperfecto (extension espanola) ----------------------
  # Tense=Imp, Mood=Ind, VerbForm=Fin (caminaba, decia, era).
  # No existe en el catalogo original de Biber (1985).
  # Se mantiene como rasgo extendido del espanol; ver biber_espanol_completo.md.
  # (NOTA: f_71 es eliminada del output final en parse_biber_features; su
  #  evidencia se filtra alli automaticamente al no sobrevivir el nombre
  #  como columna del counts final.)
  f71 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") == "Imp",
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),  "") == "Ind",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature_traced("f_71_preterit")

  # -- f_02  Aspecto perfecto: HABER + participio ----------------------------
  # Excluye ESTAR copulativo (pasiva de estado).
  estar_cop_heads <- tokens %>%
    dplyr::filter(
      .data$lemma == "estar",
      .data$pos   %in% c("AUX", "VERB"),
      dplyr::coalesce(.data$dep_rel, "") == "cop",
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id, .data$sentence_id,
      cop_head_id = .data$head_token_id_int
    ) %>%
    dplyr::distinct()

  f02 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("haber", "estar"),
      .data$pos   %in% c("AUX", "VERB"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !(.data$lemma == "estar" &
          dplyr::coalesce(.data$dep_rel, "") == "cop"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::anti_join(
      estar_cop_heads,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "cop_head_id")
    ) %>%
    dplyr::filter(
      dplyr::coalesce(.data$head_morph_verbform,
                      extract_feat(.data$head_feats, "VerbForm"), "") == "Part",
      dplyr::coalesce(.data$head_morph_voice,
                      extract_feat(.data$head_feats, "Voice"),    "") != "Pass"
    ) %>%
    count_feature_traced("f_02_perfect_aspect")

  # -- f_03  Tiempo presente -------------------------------------------------
  # Presente de indicativo simple (Tense=Pres, Mood=Ind, VerbForm=Fin).
  # No incluye el presente de subjuntivo (Mood=Sub) ni las formas no
  # personales (infinitivo, gerundio, participio).
  f03 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"),    "") == "Pres",
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),     "") == "Ind",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature_traced("f_03_present_tense")

  # -- f_04  Adverbiales de lugar ---------------------------------------------
  # Matching por lemma sobre lista lexica; POS = ADV o ADP.
  f04 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% place_adverbials,
      .data$pos   %in% c("ADV", "ADP", "NOUN")
    ) %>%
    count_feature_traced("f_04_place_adverbials")

  # -- f_05  Adverbiales de tiempo -------------------------------------------
  f05 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% time_adverbials,
      .data$pos   %in% c("ADV", "NOUN", "ADP")
    ) %>%
    count_feature_traced("f_05_time_adverbials")

  # -- f_11  Pronombres indefinidos ------------------------------------------
  # biber_espanol_completo.md §f_11 (EXCLUDE crítico): un/una/unos/unas son
  # artículos (DET, PronType=Art), no pronombres. UDPipe los lematiza como
  # "uno"; el filtro PronType!=Art descarta el artículo y conserva el uso
  # pronominal (uno PRON PronType=Ind).
  # NOTA: count_feature_traced usa el nombre "f_11_indefinite_pronoun"
  # (sin la 's' final) porque parse_biber_features() lo renombra a
  # f_11_indefinite_pronouns mas tarde. Hacemos lo mismo aqui para
  # mantener paridad. La evidencia se etiqueta con el mismo nombre y
  # se relabela en parse_biber_features tras el rename.
  f11 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% indefinite_pronouns,
      .data$pos   %in% c("PRON", "DET"),
      !stringr::str_detect(dplyr::coalesce(.data$feats, ""), "PronType=Art")
    ) %>%
    count_feature_traced("f_11_indefinite_pronoun")

  # f_12 (pro-verb do): INTRADUCIBLE en espanol.
  # El espanol resuelve la anafora verbal mediante elision, sin pro-verbo
  # equivalente a "do". Ver biber_espanol_completo.md sec. F_12.
  # Columna eliminada del output desde la auditoria de Fase 2.

  # -- Ensamblar counts ------------------------------------------------------
  counts <- doc_ids %>%
    dplyr::left_join(f01$counts, by = "doc_id") %>%
    dplyr::left_join(f71$counts, by = "doc_id") %>%
    dplyr::left_join(f02$counts, by = "doc_id") %>%
    dplyr::left_join(f03$counts, by = "doc_id") %>%
    dplyr::left_join(f04$counts, by = "doc_id") %>%
    dplyr::left_join(f05$counts, by = "doc_id") %>%
    dplyr::left_join(f11$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )

  # -- Ensamblar evidence ----------------------------------------------------
  evidence <- bind_evidence(
    f01$evidence, f71$evidence, f02$evidence, f03$evidence,
    f04$evidence, f05$evidence, f11$evidence
  )

  make_block_result(counts = counts, evidence = evidence)
}

# -----------------------------------------------------------------------------
# 2.  block_personal_pronouns_es
#     f_06  1a persona   f_07  2a persona   f_08  3a persona
#     f_13  pregunta-que
#     f_09  ELIMINADO (intraducible): espanol es lengua de sujeto nulo, no hay "it" expletivo
# -----------------------------------------------------------------------------

#' Personal pronoun, expletive, and WH-question features (Spanish)
#'
#' @param tokens Annotated token data frame
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @param de_markers  Table: (doc_id, sentence_id, head_token_id_int, has_de_marker)
#' @param que_markers Table: (doc_id, sentence_id, head_token_id_int, has_que_marker)
#' @param clause_complements Table: (doc_id, sentence_id, head_token_id_int, has_clause_comp)
#' @param weather_lemmas Impersonal weather verb lemmas (default provided)
#' @param raising_verbs  Raising / impers-tendency verb lemmas (default provided)
#' @param wh_question_lemmas WH-word lemmas (default provided)
#' @return Data frame: one row per doc, columns f_06-f_08, f_13
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

  # dep_rel que senalan uso reflexivo/impersonal -- excluidos en f_06-f_08
  reflexive_deps <- c("expl:pv", "expl:impers", "expl")

  # Helper: filtrar pronombres de una persona por lista de lemmas.
  # En UDPipe Spanish-GSD, "se" se lematiza como "él" con Reflex=Yes y
  # dep_rel = "iobj"/"obj" (no expl:pass). Excluimos esos casos para que
  # el "se" pasivo/impersonal/reflexivo NO cuente como pronombre referencial.
  # Phase 2c: solo filtra (sin colapsar columnas); el dedup+tally+evidencia
  # lo hace count_feature_traced abajo.
  filter_person_pronouns <- function(lemma_list) {
    tokens %>%
      dplyr::filter(
        .data$pos == "PRON",
        .data$lemma %in% lemma_list,
        !dplyr::coalesce(.data$dep_rel, "") %in% reflexive_deps,
        !(tolower(.data$token) == "se" &
          stringr::str_detect(dplyr::coalesce(.data$feats, ""), "Reflex=Yes"))
      )
  }

  # -- Posesivos: rutado por Person morfologico (REVISION HERNAN, Fase 1) -----
  # El rasgo ingles incluye los posesivos (our/your/their). spanish-gsd los
  # etiqueta como DET con Poss=Yes y Person fiable (verificado en
  # test-udpipe-tag-verification.R): mi/nuestro -> Person=1, tu/vuestro ->
  # Person=2, su -> Person=3. 'su' de 'usted' concuerda en 3a y se asigna a
  # f_08 por criterio morfologico (documentado). Los tonicos sustantivados
  # (mio, tuyo, suyo...) salen NOUN SIN Poss=Yes y se capturan por lema.
  # Sin doble conteo: count_feature_traced dedupe por posicion y la pasada
  # lexica excluye Poss=Yes.
  possessive_by_person <- function(person) {
    tokens %>%
      dplyr::filter(
        stringr::str_detect(dplyr::coalesce(.data$feats, ""), "Poss=Yes"),
        dplyr::coalesce(extract_feat(.data$feats, "Person"), "") == person
      )
  }
  possessive_lexical <- function(lemma_list) {
    tokens %>%
      dplyr::filter(
        .data$lemma %in% lemma_list,
        !stringr::str_detect(dplyr::coalesce(.data$feats, ""), "Poss=Yes"),
        .data$pos %in% c("NOUN", "PRON", "ADJ")
      )
  }
  # Voseo: spanish-gsd suele etiquetar "vos" como NOUN (lema "tu"), perdiendolo
  # en la pasada pronominal (pos==PRON). Rescate por token; el dedup evita el
  # doble conteo cuando si viene como PRON.
  voseo_forms <- function() {
    tokens %>% dplyr::filter(tolower(.data$token) == "vos")
  }

  # -- f_06  1a persona (pronombres + posesivos) ----------------------------
  f06 <- dplyr::bind_rows(
    filter_person_pronouns(c("yo", "nosotros", "nosotras", "me", "nos",
                             "m\u00ed", "conmigo")),
    possessive_by_person("1"),
    possessive_lexical(c("m\u00edo", "nuestro"))
  ) %>% count_feature_traced("f_06_first_person_pronouns")

  # -- f_07  2a persona (pronombres + posesivos + voseo) --------------------
  # "te" puede ser 2a atono o parte de construccion impersonal;
  # la exclusion de reflexive_deps filtra los casos expl mas claros.
  f07 <- dplyr::bind_rows(
    filter_person_pronouns(c("t\u00fa", "vos", "vosotros", "vosotras",
                             "usted", "ustedes", "te", "ti", "contigo", "os")),
    possessive_by_person("2"),
    possessive_lexical(c("tuyo", "vuestro")),
    voseo_forms()
  ) %>% count_feature_traced("f_07_second_person_pronouns")

  # -- f_08  3a persona (pronombres + posesivos) ----------------------------
  f08 <- dplyr::bind_rows(
    filter_person_pronouns(c("\u00e9l", "ella", "ello", "ellos", "ellas",
                             "le", "lo", "la", "les", "los", "las",
                             "consigo", "s\u00ed")),
    possessive_by_person("3"),
    possessive_lexical(c("suyo"))
  ) %>% count_feature_traced("f_08_third_person_pronouns")

  # f_09 (pronombre it): INTRADUCIBLE en espanol.
  # El espanol es lengua de sujeto nulo; no existe expletivo equivalente a "it".
  # El haber impersonal (hay, habia...) se captura en f_20.
  # Ver biber_espanol_completo.md sec. F_09.
  # Columna eliminada del output desde la auditoria de Fase 2.

  # -- f_13  Preguntas con palabra interrogativa ---------------------------
  question_sentences <- tokens %>%
    dplyr::filter(.data$token == "?") %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id,
                     has_question = TRUE) %>%
    dplyr::distinct()

  f13 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_question_lemmas,
      .data$pos   %in% c("ADV", "PRON", "DET", "ADJ")
    ) %>%
    dplyr::left_join(question_sentences,
                     by = c("doc_id", "sentence_id")) %>%
    dplyr::filter(!is.na(.data$has_question)) %>%
    count_feature_traced("f_13_wh_question")

  # -- Ensamblar counts ------------------------------------------------------
  counts <- doc_ids %>%
    dplyr::left_join(f06$counts, by = "doc_id") %>%
    dplyr::left_join(f07$counts, by = "doc_id") %>%
    dplyr::left_join(f08$counts, by = "doc_id") %>%
    dplyr::left_join(f13$counts, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"),
                    ~ dplyr::coalesce(., 0L))
    )

  # -- Ensamblar evidence ----------------------------------------------------
  evidence <- bind_evidence(f06$evidence, f07$evidence, f08$evidence, f13$evidence)

  make_block_result(counts = counts, evidence = evidence)
}
