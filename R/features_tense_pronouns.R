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

# Cuenta ocurrencias distintas (doc, sent, tok) y agrega a nivel doc_id.
# 2026-04-21: UDPipe MWT rows (e.g. "al"/"del" with token_id "4-5") have
# token_id_int=NA; two MWTs in the same sentence would both get NA and collapse
# to one row under distinct(), undercounting by 1 per extra MWT.
# Fix: assign unique negative integers to NA positions before deduplication
# so each MWT row is treated as a distinct token.
count_feature <- function(tbl, col_name) {
  tbl %>%
    dplyr::mutate(
      .tid_dedup = dplyr::if_else(
        is.na(.data$token_id_int),
        -.Machine$integer.max + dplyr::row_number(),
        .data$token_id_int
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$.tid_dedup) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(!!col_name := "n")
}

# -----------------------------------------------------------------------------
# 1.  block_tense_es
#     f_01  tiempo pasado (imperfecto)   f_02  aspecto perfecto
#     f_03  tiempo presente              f_04  adv. de lugar
#     f_05  adv. de tiempo              f_11  pronombres indefinidos
#     f_12  pro-verbo hacer             f_71  preterito indefinido (ext. espanola)
# -----------------------------------------------------------------------------

#' Tense, aspect, adverbial, and indefinite-pronoun features (Spanish)
#'
#' @param tokens Annotated token data frame (UD format)
#' @param doc_ids One-column data frame with column `doc_id`
#' @param head_lookup Pre-built head-token attribute table
#' @param place_adverbials Character vector of place-adverbial lemmas (f_04)
#' @param time_adverbials  Character vector of time-adverbial lemmas (f_05)
#' @param indefinite_pronouns Character vector of indefinite pronoun lemmas (f_11)
#' @return Data frame: one row per doc, columns f_01-f_05, f_11, f_12, f_71
#' @keywords internal
block_tense_es <- function(
    tokens,
    doc_ids,
    head_lookup,
    place_adverbials,
    time_adverbials,
    indefinite_pronouns
) {

  # -- f_01  Tiempo pasado (imperfecto) -------------------------------------
  # f_01 DECISION DE DISE?O (2026-04-20):
  # En pseudobibeR.es, f_01 captura SOLO el preterito imperfecto (Tense=Imp).
  # El preterito indefinido vive en f_71; el perfecto compuesto en f_02.
  # Esto difiere de pseudobibeR.fr donde f_01 agrega TODOS los pasados.
  # Justificacion: en espanol imperfecto e indefinido tienen valores
  # aspectuales distintos (imperfectivo vs perfectivo); mantenerlos separados
  # aumenta el poder discriminativo del analisis. Para comparaciones
  # translingues FR?ES, usar f_01 + f_02 + f_71 como proxy de "total pasado".
  # Ver: docs/DECISIONES_ES.md ?f_01.
  f01 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") == "Imp",
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),  "") == "Ind",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature("f_01_past_tense")

  # -- f_71  Preterito indefinido (extension espanola) ----------------------
  # Tense=Past, Mood=Ind, VerbForm=Fin.
  # No existe en pseudobibeR.fr ni en el catalogo original de Biber (1985).
  # Se anade como rasgo extendido del espanol (f_71) siguiendo
  # Davies et al. (2006) Dimension 5.
  f71 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(extract_feat(.data$feats, "Tense"), "") == "Past",
      dplyr::coalesce(extract_feat(.data$feats, "Mood"),  "") == "Ind",
      dplyr::coalesce(extract_feat(.data$feats, "VerbForm"), "") == "Fin"
    ) %>%
    count_feature("f_71_preterit")

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
    count_feature("f_02_perfect_aspect")

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
    count_feature("f_03_present_tense")

  # -- f_04  Adverbiales de lugar ---------------------------------------------
  # Matching por lemma sobre lista lexica; POS = ADV o ADP.
  f04 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% place_adverbials,
      .data$pos   %in% c("ADV", "ADP", "NOUN")
    ) %>%
    count_feature("f_04_place_adverbials")

  # -- f_05  Adverbiales de tiempo -------------------------------------------
  f05 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% time_adverbials,
      .data$pos   %in% c("ADV", "NOUN", "ADP")
    ) %>%
    count_feature("f_05_time_adverbials")

  # -- f_11  Pronombres indefinidos ------------------------------------------
  f11 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% indefinite_pronouns,
      .data$pos   %in% c("PRON", "DET")
    ) %>%
    count_feature("f_11_indefinite_pronoun")

  # -- f_12  Pro-verbo "hacer" -----------------------------------------------
  # Equivalente espanol de Biber's "pro-verb do".
  # Detectamos "hacer" como VERB (no AUX) cuando tiene como dependiente
  # directo un clitico de objeto (lo/la/los/las) con dep_rel obj/iobj/expl.
  # Este patron captura usos proverbales como "lo hace", "hazlo", etc.
  # Se excluye "hacer" en locuciones fijas (hacer_falta, hacer_caso?) que
  # el tokenizador puede dejar como token unico (handled by semi_join scope).
  hacer_clitic_deps <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("lo", "la", "los", "las"),
      .data$pos   == "PRON",
      dplyr::coalesce(.data$dep_rel, "") %in% c("obj", "iobj", "expl"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::select("doc_id", "sentence_id",
                  token_id_int = "head_token_id_int")

  f12 <- tokens %>%
    dplyr::filter(
      .data$lemma == "hacer",
      .data$pos   == "VERB"
    ) %>%
    dplyr::semi_join(
      hacer_clitic_deps,
      by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    count_feature("f_12_proverb_do")

  # -- Ensamblar --------------------------------------------------------------
  doc_ids %>%
    dplyr::left_join(f01,  by = "doc_id") %>%
    dplyr::left_join(f71,  by = "doc_id") %>%
    dplyr::left_join(f02,  by = "doc_id") %>%
    dplyr::left_join(f03,  by = "doc_id") %>%
    dplyr::left_join(f04,  by = "doc_id") %>%
    dplyr::left_join(f05,  by = "doc_id") %>%
    dplyr::left_join(f11,  by = "doc_id") %>%
    dplyr::left_join(f12,  by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L))
    )
}

# -----------------------------------------------------------------------------
# 2.  block_personal_pronouns_es
#     f_06  1a persona   f_07  2a persona   f_08  3a persona
#     f_09  expletivo/impersonal            f_13  pregunta-que
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
#' @return Data frame: one row per doc, columns f_06 ? f_09, f_13
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

  # Helper: filtrar pronombres de una persona por lista de lemmas
  count_person_pronouns <- function(lemma_list) {
    tokens %>%
      dplyr::filter(
        .data$pos == "PRON",
        .data$lemma %in% lemma_list,
        !dplyr::coalesce(.data$dep_rel, "") %in% reflexive_deps
      ) %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)
  }

  # -- f_06  1a persona -----------------------------------------------------
  f06 <- count_person_pronouns(c(
    "yo", "nosotros", "nosotras",
    "me", "nos",
    "m\u00ed", "conmigo"
  )) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_06_first_person_pronouns = "n")

  # -- f_07  2a persona -----------------------------------------------------
  # "te" puede ser 2a atono o parte de construccion impersonal;
  # la exclusion de reflexive_deps filtra los casos expl mas claros.
  f07 <- count_person_pronouns(c(
    "t\u00fa", "vos", "vosotros", "vosotras",
    "usted", "ustedes",
    "te", "ti", "contigo", "os"
  )) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_07_second_person_pronouns = "n")

  # -- f_08  3a persona -----------------------------------------------------
  f08 <- count_person_pronouns(c(
    "\u00e9l", "ella", "ello", "ellos", "ellas",
    "le", "lo", "la", "les", "los", "las",
    "consigo"
  )) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_08_third_person_pronouns = "n")

  # -- f_09  Pronombre expletivo / impersonal -------------------------------
  # Cubre:
  #   (a) "ello" como sujeto impersonal ("Ello implica que?")
  #   (b) "se" con dep_rel expl:impers o expl (se impersonal)
  #   (c) haber impersonal (hay, habia?) sin sujeto explicito
  pronoun_it_candidates <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$lemma %in% c("ello", "se"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id",
             "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(de_markers,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int")) %>%
    dplyr::left_join(que_markers,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int")) %>%
    dplyr::left_join(clause_complements,
                     by = c("doc_id", "sentence_id",
                            "head_token_id_int")) %>%
    dplyr::mutate(
      has_de_marker   = dplyr::coalesce(.data$has_de_marker,   FALSE),
      has_que_marker  = dplyr::coalesce(.data$has_que_marker,  FALSE),
      has_clause_comp = dplyr::coalesce(.data$has_clause_comp, FALSE),
      is_weather          = .data$head_lemma %in% weather_lemmas,
      is_raising_verb     = .data$head_lemma %in% raising_verbs,
      has_control_marker  =
        .data$has_de_marker | .data$has_que_marker | .data$has_clause_comp
    ) %>%
    dplyr::filter(
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""), "^expl"
      ) |
        (stringr::str_detect(
          dplyr::coalesce(.data$dep_rel, ""), "^nsubj"
        ) &
           .data$lemma == "ello" &
           (.data$is_weather |
              (.data$head_pos == "ADJ" & .data$has_control_marker) |
              (.data$head_pos %in% c("VERB", "AUX") &
                 (.data$is_raising_verb |
                    .data$has_control_marker))))
    )

  # haber impersonal sin nsubj dependiente
  haber_impersonal <- tokens %>%
    dplyr::filter(
      .data$lemma == "haber",
      .data$pos   %in% c("VERB", "AUX"),
      dplyr::coalesce(.data$dep_rel, "") %in%
        c("root", "ccomp", "xcomp", "advcl", "parataxis", "")
    ) %>%
    dplyr::anti_join(
      tokens %>%
        dplyr::filter(
          stringr::str_detect(
            dplyr::coalesce(.data$dep_rel, ""), "^nsubj"
          )
        ) %>%
        dplyr::transmute(
          .data$doc_id, .data$sentence_id,
          head_token_id_int = .data$head_token_id_int
        ),
      by = c("doc_id", "sentence_id",
             "token_id_int" = "head_token_id_int")
    )

  f09 <- dplyr::bind_rows(
    pronoun_it_candidates %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id,
                      .data$token_id_int),
    haber_impersonal %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id,
                      .data$token_id_int)
  ) %>%
    dplyr::distinct() %>%
    count_feature("f_09_pronoun_it")

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
    count_feature("f_13_wh_question")

  # -- Ensamblar -------------------------------------------------------------
  doc_ids %>%
    dplyr::left_join(f06, by = "doc_id") %>%
    dplyr::left_join(f07, by = "doc_id") %>%
    dplyr::left_join(f08, by = "doc_id") %>%
    dplyr::left_join(f09, by = "doc_id") %>%
    dplyr::left_join(f13, by = "doc_id") %>%
    dplyr::mutate(
      dplyr::across(-dplyr::any_of("doc_id"),
                    ~ dplyr::coalesce(., 0L))
    )
}
