# app.R — pseudobibeR.es Shiny interface
# Annota texto español con UDPipe y extrae los rasgos de Biber (1985/1988).
#
# Ejecutar con: shiny::runApp() desde el directorio del paquete.

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(udpipe)
})

# ─── Cargar el paquete local ─────────────────────────────────────────────────
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::load_all(quiet = TRUE)

# ─── Modelo UDPipe (carga única al iniciar) ───────────────────────────────────
MODEL_PATH <- "spanish-gsd-ud-2.5-191206.udpipe"
if (!file.exists(MODEL_PATH))
  stop("Modelo UDPipe no encontrado: ", MODEL_PATH,
       "\nDescárgalo con: udpipe::udpipe_download_model('spanish-gsd')")
message("Cargando modelo UDPipe…")
udmodel <- udpipe_load_model(MODEL_PATH)
message("Modelo listo.")

# ─── Catálogo de rasgos ───────────────────────────────────────────────────────
# 74 filas: 67 rasgos de Biber (1985) + 7 extensiones españolas (f_68–f_71)
feature_labels <- data.frame(
  code = c(
    "f_01_past_tense", "f_02_perfect_aspect", "f_03_present_tense",
    "f_04_place_adverbials", "f_05_time_adverbials",
    "f_06_first_person_pronouns", "f_07_second_person_pronouns",
    "f_08_third_person_pronouns", "f_09_pronoun_it",
    "f_10_demonstrative_pronoun", "f_11_indefinite_pronouns", "f_12_proverb_do",
    "f_13_wh_question",
    "f_14_nominalizations", "f_15_gerunds", "f_16_other_nouns",
    "f_17_agentless_passives", "f_18_by_passives",
    "f_19_be_main_verb", "f_20_existential_there",
    "f_21_that_verb_comp", "f_22_that_adj_comp", "f_23_wh_clause",
    "f_24_infinitives", "f_25_present_participle", "f_26_past_participle",
    "f_27_past_participle_whiz", "f_28_present_participle_whiz",
    "f_29_that_subj", "f_30_that_obj", "f_31_wh_subj", "f_32_wh_obj",
    "f_33_pied_piping", "f_34_sentence_relatives",
    "f_35_because", "f_36_though", "f_37_if", "f_38_other_adv_sub",
    "f_39_prepositions", "f_40_adj_attr", "f_41_adj_pred", "f_42_adverbs",
    "f_43_type_token", "f_44_mean_word_length",
    "f_45_conjuncts", "f_46_downtoners", "f_47_hedges", "f_48_amplifiers",
    "f_49_emphatics", "f_50_discourse_particles", "f_51_demonstratives",
    "f_52_modal_possibility", "f_53_modal_necessity", "f_54_modal_predictive",
    "f_55_verb_public", "f_56_verb_private", "f_57_verb_suasive", "f_58_verb_seem",
    "f_59_contractions", "f_60_that_deletion",
    "f_61_stranded_preposition", "f_62_split_infinitive", "f_63_split_auxiliary",
    "f_64_phrasal_coordination", "f_65_clausal_coordination",
    "f_66_neg_synthetic", "f_67_neg_analytic",
    "f_68_nominalization", "f_68_nominalization_rate",
    "f_69_mente_adverbs", "f_69_mente_adverbs_rate",
    "f_70_long_words", "f_70_long_words_rate",
    "f_71_preterit"
  ),
  grupo = c(
    "Tiempo verbal", "Tiempo verbal", "Tiempo verbal",
    "Adverbiales", "Adverbiales",
    "Pronombres", "Pronombres", "Pronombres", "Pronombres",
    "Pronombres", "Pronombres", "Pronombres",
    "Interrogativas",
    "Sustantivos", "Sustantivos", "Sustantivos",
    "Voz pasiva", "Voz pasiva",
    "Construcciones existenciales", "Construcciones existenciales",
    "Subordinación", "Subordinación", "Subordinación",
    "Subordinación", "Subordinación", "Subordinación",
    "Cláusulas relativas", "Cláusulas relativas",
    "Cláusulas relativas", "Cláusulas relativas",
    "Cláusulas relativas", "Cláusulas relativas",
    "Cláusulas relativas", "Cláusulas relativas",
    "Subordinación adverbial", "Subordinación adverbial",
    "Subordinación adverbial", "Subordinación adverbial",
    "SN elaborado", "SN elaborado", "SN elaborado",
    "Adverbios",
    "Complejidad léxica", "Complejidad léxica",
    "Marcadores discursivos", "Marcadores discursivos",
    "Marcadores discursivos", "Marcadores discursivos",
    "Marcadores discursivos", "Marcadores discursivos",
    "Pronombres",
    "Modalidad", "Modalidad", "Modalidad",
    "Verbos especializados", "Verbos especializados",
    "Verbos especializados", "Verbos especializados",
    "Estructuras marcadas", "Estructuras marcadas",
    "Estructuras marcadas", "Estructuras marcadas", "Estructuras marcadas",
    "Coordinación", "Coordinación",
    "Negación", "Negación",
    "Extensión española", "Extensión española",
    "Extensión española", "Extensión española",
    "Extensión española", "Extensión española",
    "Extensión española"
  ),
  descripcion = c(
    "Imperfecto de indicativo",
    "Aspecto perfecto (haber + part.)",
    "Presente de indicativo",
    "Adverbiales de lugar",
    "Adverbiales de tiempo",
    "Pronombres 1.\u00aa pers.",
    "Pronombres 2.\u00aa pers.",
    "Pronombres 3.\u00aa pers.",
    "Pronombre impersonal",
    "Pronombres demostrativos",
    "Pronombres indefinidos",
    "Pro-verbo hacer",
    "Interrogativas con pronombre QU-",
    "Nominalizaciones",
    "Gerundios",
    "Otros sustantivos",
    "Pasiva sin agente",
    "Pasiva con agente (por)",
    "Ser/estar como verbo principal",
    "Existencial (hay)",
    "Compl. de verbo con que",
    "Compl. de adj. con que",
    "Cl\u00e1usula WH",
    "Infinitivos",
    "Cl\u00e1usula de participio presente",
    "Cl\u00e1usula de participio pasado",
    "Relativa reducida (part. pasado)",
    "Relativa reducida (part. presente)",
    "Relativa de sujeto con que",
    "Relativa de objeto con que",
    "Relativa QU- de sujeto",
    "Relativa QU- de objeto",
    "Pied-piping",
    "Relativas sentenciales",
    "Causal (porque)",
    "Concesiva (aunque)",
    "Condicional (si)",
    "Otras sub. adverbiales",
    "Preposiciones",
    "Adjetivo atributivo",
    "Adjetivo predicativo",
    "Adverbios generales",
    "TTR (type-token ratio)",
    "Longitud media de palabra",
    "Conjunciones textuales",
    "Atenuadores (casi, apenas\u2026)",
    "Marcadores de duda (quiz\u00e1s\u2026)",
    "Amplificadores (muy, totalmente\u2026)",
    "Enf\u00e1ticos (realmente, de hecho\u2026)",
    "Part\u00edculas discursivas",
    "Demostrativos determinantes",
    "Modal de posibilidad (poder + inf.)",
    "Modal de necesidad (deber/tener que)",
    "Modal predictivo (ir a + inf. / futuro)",
    "Verbos p\u00fablicos (decir, afirmar\u2026)",
    "Verbos privados (creer, pensar\u2026)",
    "Verbos suasorios (pedir, exigir\u2026)",
    "Verbos de apariencia (parecer, resultar\u2026)",
    "Contracciones (al, del)",
    "Supresi\u00f3n de que",
    "Preposici\u00f3n varada",
    "Infinitivo escindido",
    "Auxiliar escindido",
    "Coordinaci\u00f3n sintagm\u00e1tica",
    "Coordinaci\u00f3n clausal",
    "Negaci\u00f3n sint\u00e9tica (nadie, nunca\u2026)",
    "Negaci\u00f3n anal\u00edtica (no + verbo)",
    "Nominalizaciones (recuento)",
    "Nominalizaciones (tasa por 100 pal.)",
    "Adverbios en -mente (recuento)",
    "Adverbios en -mente (tasa por 100 pal.)",
    "Palabras largas \u22657 letras (recuento)",
    "Palabras largas \u22657 letras (tasa por 100 pal.)",
    "Pret\u00e9rito indefinido"
  ),
  tipo = c(
    rep("Biber original", 67),
    rep("Extensi\u00f3n espa\u00f1ola", 7)
  ),
  stringsAsFactors = FALSE
)

# ─── UI ──────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "pseudobibeR.es \u2014 Rasgos de Biber",

  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body {
        font-family: 'Georgia', serif;
        background: #f8f7f2;
        color: #1a1a1a;
        margin: 0; padding: 0;
      }
      .app-header {
        background: #1c3d5a; color: #fff;
        padding: 18px 32px 14px;
        border-bottom: 3px solid #e8a020;
      }
      .app-header h1 { margin: 0 0 4px; font-size: 1.55rem; font-weight: normal; }
      .app-header p  { margin: 0; font-size: .85rem; color: #b8cfe0; font-family: Arial, sans-serif; }
      .main-container { max-width: 1180px; margin: 28px auto; padding: 0 20px 60px; }
      .panel {
        background: #fff; border: 1px solid #d4cfc4; border-radius: 4px;
        padding: 20px 24px; margin-bottom: 22px;
        box-shadow: 0 1px 3px rgba(0,0,0,.07);
      }
      .panel h2 {
        font-size: .9rem; margin: 0 0 12px; color: #1c3d5a;
        font-family: Arial, sans-serif; font-weight: bold;
        text-transform: uppercase; letter-spacing: .07em;
      }
      textarea {
        width: 100% !important; font-family: 'Courier New', monospace !important;
        font-size: .9rem !important; background: #fdfcf9 !important;
        border: 1px solid #ccc !important; border-radius: 3px !important;
        padding: 10px !important; resize: vertical !important;
        color: #222 !important; box-sizing: border-box !important;
      }
      textarea:focus { outline: none !important; border-color: #1c3d5a !important; }
      #analizar {
        background: #1c3d5a; color: #fff; border: none; border-radius: 3px;
        padding: 10px 28px; font-size: .95rem; font-family: Arial, sans-serif;
        cursor: pointer; margin-top: 12px; transition: background .15s;
      }
      #analizar:hover  { background: #255278; }
      #analizar:active { background: #142c42; }
      #status_msg {
        display: inline-block; margin-left: 14px;
        font-family: Arial, sans-serif; font-size: .88rem;
        color: #666; font-style: italic; vertical-align: middle;
      }
      .results-meta {
        font-family: Arial, sans-serif; font-size: .82rem; color: #555;
        margin-bottom: 14px; padding-bottom: 8px; border-bottom: 1px solid #eee;
      }
      .results-meta strong { color: #1c3d5a; }
      .filter-bar {
        display: flex; gap: 6px; flex-wrap: wrap; align-items: flex-end;
        margin-bottom: 12px; font-family: Arial, sans-serif; font-size: .85rem;
      }
      .filter-bar .form-group { margin-bottom: 0; }
      .filter-bar label { color: #444; font-weight: bold; font-size: .82rem; }
      .filter-bar select, .filter-bar .form-control {
        border: 1px solid #ccc; border-radius: 3px; padding: 4px 8px;
        font-family: Arial, sans-serif; font-size: .85rem; background: #fdfcf9;
        height: auto;
      }
      .placeholder-msg {
        text-align: center; padding: 50px 20px;
        color: #999; font-family: Arial, sans-serif; font-size: .95rem;
      }
      .placeholder-msg .icon { font-size: 2.5rem; margin-bottom: 10px; }
      .app-footer {
        text-align: center; font-family: Arial, sans-serif;
        font-size: .78rem; color: #aaa; margin-top: 40px;
        padding-top: 12px; border-top: 1px solid #e0ddd6;
      }
      /* DT overrides */
      .dataTables_wrapper { font-family: Arial, sans-serif; font-size: .84rem; }
      table.dataTable thead th {
        background: #1c3d5a !important; color: #fff !important;
        font-weight: normal !important; border-bottom: none !important;
        white-space: nowrap;
      }
      table.dataTable tbody tr:nth-child(even) { background: #f8f7f2; }
      table.dataTable tbody tr:hover           { background: #edf4fa !important; }
      .row-ext td { background: #f5f0ea !important; color: #5a3a00 !important; }
      .row-ext:hover td { background: #ede4d6 !important; }
    "))
  ),

  div(class = "app-header",
    h1("pseudobibeR.es"),
    p("Extractor de rasgos l\u00e9xico-gramaticales de Biber (1985/1988) para el espa\u00f1ol")
  ),

  div(class = "main-container",

    # ── Panel de entrada ──
    div(class = "panel",
      h2("Texto a analizar"),
      textAreaInput(
        "texto", label = NULL, value = "", rows = 10,
        placeholder = paste0(
          "Pega aqu\u00ed el texto en espa\u00f1ol (m\u00ednimo recomendado: 200 palabras).\u2026"
        ),
        width = "100%"
      ),
      div(
        actionButton("analizar", "Analizar", icon = icon("magnifying-glass")),
        span(id = "status_msg", textOutput("status_msg", inline = TRUE))
      )
    ),

    # ── Panel de resultados ──
    div(class = "panel",
      h2("Rasgos de Biber"),
      uiOutput("results_area")
    ),

    div(class = "app-footer",
      HTML("pseudobibeR.es &mdash; Biber (1985/1988) para espa\u00f1ol
            &mdash; Anotaci\u00f3n: UDPipe + Spanish-GSD")
    )
  )
)

# ─── Extracción de evidencia (palabras marcadas) ─────────────────────────────
# Devuelve un named character vector: feature_code → "palabra1, palabra2, …"
extract_evidence <- function(raw_tokens) {
  data("dict",       package = "pseudobibeR.es", envir = environment())
  data("word_lists", package = "pseudobibeR.es", envir = environment())

  toks <- raw_tokens
  # UDPipe raw output uses "upos"; feature functions expect "pos"
  if ("upos" %in% colnames(toks) && !"pos" %in% colnames(toks))
    toks$pos <- toks$upos
  toks$lemma_lc <- tolower(toks$lemma)
  toks$token_lc <- tolower(toks$token)

  ef <- function(feats, name) extract_feat(feats, name)

  # Helper: recoge tokens únicos que cumplen una máscara lógica
  collect <- function(mask, col = "token", max_n = 8) {
    mask[is.na(mask)] <- FALSE
    if (sum(mask) == 0) return("")
    words <- unique(toks[[col]][mask])
    if (length(words) > max_n)
      paste0(paste(words[seq_len(max_n)], collapse = ", "), ", \u2026")
    else
      paste(words, collapse = ", ")
  }

  # Helper: lemas del diccionario (solo entradas de una palabra;
  #         las multi-word se detectan por quanteda, no por token suelto)
  dict_lemmas <- function(key) {
    if (!key %in% names(dict)) return(character(0))
    single <- dict[[key]][!grepl("_", dict[[key]])]
    unique(tolower(single))
  }

  ev <- list()

  # — Grupo 1: Morfológicos (tiempo, aspecto) —
  ev[["f_01_past_tense"]] <- collect(
    toks$pos %in% c("VERB","AUX") &
    dplyr::coalesce(ef(toks$feats, "Tense"), "") == "Imp" &
    dplyr::coalesce(ef(toks$feats, "Mood"), "")  == "Ind" &
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin"
  )
  ev[["f_02_perfect_aspect"]] <- collect(
    toks$lemma_lc %in% c("haber") &
    toks$pos %in% c("AUX","VERB") &
    grepl("^aux", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_03_present_tense"]] <- collect(
    toks$pos %in% c("VERB","AUX") &
    dplyr::coalesce(ef(toks$feats, "Tense"), "") == "Pres" &
    dplyr::coalesce(ef(toks$feats, "Mood"), "")  == "Ind" &
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin"
  )
  ev[["f_71_preterit"]] <- collect(
    toks$pos %in% c("VERB","AUX") &
    dplyr::coalesce(ef(toks$feats, "Tense"), "") == "Past" &
    dplyr::coalesce(ef(toks$feats, "Mood"), "")  == "Ind" &
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin"
  )

  # — Grupo 2: Dict-based (lemma lookup) —
  dict_features <- c(
    "f_04_place_adverbials", "f_05_time_adverbials",
    "f_11_indefinite_pronoun",
    "f_45_conjuncts", "f_46_downtoners", "f_47_hedges",
    "f_48_amplifiers", "f_49_emphatics", "f_50_discourse_particles",
    "f_55_verb_public", "f_56_verb_private",
    "f_57_verb_suasive", "f_58_verb_seem"
  )
  for (feat in dict_features) {
    lems <- dict_lemmas(feat)
    # El código de feature sin el prefijo del dict puede diferir
    code <- feat
    # f_11 en dict es "f_11_indefinite_pronoun" pero en output "f_11_indefinite_pronouns"
    if (feat == "f_11_indefinite_pronoun") code <- "f_11_indefinite_pronouns"
    ev[[code]] <- collect(toks$lemma_lc %in% lems)
  }

  # — Grupo 3: Pronombres por persona —
  pron_06 <- dict_lemmas("f_06_first_person_pronouns")
  pron_07 <- dict_lemmas("f_07_second_person_pronouns")
  pron_08 <- dict_lemmas("f_08_third_person_pronouns")
  ev[["f_06_first_person_pronouns"]]  <- collect(toks$lemma_lc %in% pron_06 & toks$pos == "PRON")
  ev[["f_07_second_person_pronouns"]] <- collect(toks$lemma_lc %in% pron_07 & toks$pos == "PRON")
  ev[["f_08_third_person_pronouns"]]  <- collect(toks$lemma_lc %in% pron_08 & toks$pos == "PRON")

  # f_09: impersonal se/ello
  ev[["f_09_pronoun_it"]] <- collect(
    toks$lemma_lc %in% c("ello","se") & toks$pos == "PRON" &
    grepl("^expl", dplyr::coalesce(toks$dep_rel, ""))
  )

  # f_10, f_51: demostrativos (pronombre vs determinante)
  dem_lemmas <- dict_lemmas("f_51_demonstratives")
  ev[["f_10_demonstrative_pronoun"]] <- collect(toks$lemma_lc %in% dem_lemmas & toks$pos == "PRON")
  ev[["f_51_demonstratives"]]        <- collect(toks$lemma_lc %in% dem_lemmas & toks$pos == "DET")

  # f_12: pro-verbo hacer
  ev[["f_12_proverb_do"]] <- collect(toks$lemma_lc == "hacer" & toks$pos == "VERB")

  # f_13: interrogativas WH
  wh <- c("qu\u00e9", "qui\u00e9n", "qui\u00e9nes", "cu\u00e1l", "cu\u00e1les",
          "cu\u00e1nto", "cu\u00e1nta", "cu\u00e1ntos", "cu\u00e1ntas",
          "cu\u00e1ndo", "d\u00f3nde", "c\u00f3mo")
  ev[["f_13_wh_question"]] <- collect(toks$lemma_lc %in% wh & toks$pos %in% c("PRON","ADV","DET","ADJ"))

  # f_14: nominalizaciones (sufijo)
  nom_suffixes <- word_lists$nominalization_suffixes
  if (length(nom_suffixes) > 0) {
    nom_regex <- paste0("(", paste(nom_suffixes, collapse = "|"), ")$")
    ev[["f_14_nominalizations"]] <- collect(
      toks$pos == "NOUN" & grepl(nom_regex, toks$lemma_lc)
    )
  } else ev[["f_14_nominalizations"]] <- ""

  # f_15: gerundios
  ev[["f_15_gerunds"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Ger"
  )

  # f_16: otros sustantivos
  ev[["f_16_other_nouns"]] <- collect(toks$pos %in% c("NOUN","PROPN"), max_n = 6)

  # f_17/f_18: pasivas
  ev[["f_17_agentless_passives"]] <- collect(
    grepl("^(aux:pass|expl:pass)", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_18_by_passives"]] <- ""

  # f_19: ser/estar copulativo
  ev[["f_19_be_main_verb"]] <- collect(
    toks$lemma_lc %in% c("ser","estar") &
    !grepl("^aux", dplyr::coalesce(toks$dep_rel, ""))
  )

  # f_20: existencial hay
  ev[["f_20_existential_there"]] <- collect(
    toks$lemma_lc == "haber" &
    toks$token_lc %in% c("hay","hab\u00eda","habr\u00e1","habr\u00eda","hubo","habido")
  )

  # f_21, f_22: que complementante (se muestra "que" genéricamente)
  ev[["f_21_that_verb_comp"]] <- collect(
    toks$lemma_lc == "que" & toks$pos == "SCONJ" &
    dplyr::coalesce(toks$dep_rel, "") == "mark"
  )
  ev[["f_22_that_adj_comp"]] <- ev[["f_21_that_verb_comp"]]

  # f_23: cláusulas WH indirectas
  ev[["f_23_wh_clause"]] <- collect(
    toks$lemma_lc %in% c("qu\u00e9","qui\u00e9n","qui\u00e9nes","cu\u00e1l","cu\u00e1les",
                          "cu\u00e1ndo","d\u00f3nde","c\u00f3mo","cu\u00e1nto") &
    grepl("^(obj|obl|nsubj|iobj|mark|advmod|nmod)", dplyr::coalesce(toks$dep_rel, ""))
  )

  # f_24: infinitivos clausales
  ev[["f_24_infinitives"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Inf" &
    grepl("^(xcomp|ccomp|advcl|acl|obj)", dplyr::coalesce(toks$dep_rel, ""))
  )

  # f_25: participios presentes
  ev[["f_25_present_participle"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Ger" &
    grepl("^(advcl|ccomp)", dplyr::coalesce(toks$dep_rel, ""))
  )

  # f_26: participios pasados
  ev[["f_26_past_participle"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Part" &
    grepl("^(advcl|ccomp|acl)", dplyr::coalesce(toks$dep_rel, ""))
  )

  # f_27, f_28: whiz-deletion relativas
  ev[["f_27_past_participle_whiz"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Part" &
    dplyr::coalesce(toks$dep_rel, "") == "acl"
  )
  ev[["f_28_present_participle_whiz"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Ger" &
    dplyr::coalesce(toks$dep_rel, "") == "acl"
  )

  # f_29–f_34: relativas
  ev[["f_29_that_subj"]] <- collect(toks$lemma_lc == "que" & grepl("^nsubj", dplyr::coalesce(toks$dep_rel, "")))
  ev[["f_30_that_obj"]]  <- collect(toks$lemma_lc == "que" & grepl("^(obj|iobj|obl)", dplyr::coalesce(toks$dep_rel, "")))
  rel_pron <- c("quien","quienes","cual","cuales")
  ev[["f_31_wh_subj"]] <- collect(toks$lemma_lc %in% rel_pron & grepl("^nsubj", dplyr::coalesce(toks$dep_rel, "")))
  ev[["f_32_wh_obj"]]  <- collect(toks$lemma_lc %in% rel_pron & grepl("^(obj|iobj|obl)", dplyr::coalesce(toks$dep_rel, "")))
  ev[["f_33_pied_piping"]] <- collect(toks$lemma_lc %in% c("que","quien","quienes","cual","cuales") & toks$pos %in% c("PRON","DET"))
  ev[["f_34_sentence_relatives"]] <- ""

  # f_35–f_38: subordinación adverbial
  causal_lems <- dict_lemmas("f_35_because")
  if (length(causal_lems) == 0) causal_lems <- c("porque","pues","como")
  ev[["f_35_because"]] <- collect(toks$lemma_lc %in% causal_lems & toks$pos %in% c("SCONJ","CCONJ","ADV"))

  conc_lems <- dict_lemmas("f_36_though")
  if (length(conc_lems) == 0) conc_lems <- c("aunque")
  ev[["f_36_though"]] <- collect(toks$lemma_lc %in% conc_lems & toks$pos %in% c("SCONJ","CCONJ","ADV"))

  cond_lems <- dict_lemmas("f_37_if")
  if (length(cond_lems) == 0) cond_lems <- c("si")
  ev[["f_37_if"]] <- collect(toks$lemma_lc %in% cond_lems & toks$pos %in% c("SCONJ","ADV"))

  ev[["f_38_other_adv_sub"]] <- collect(
    toks$pos %in% c("SCONJ","ADP","ADV") &
    dplyr::coalesce(toks$dep_rel, "") == "mark" &
    !(toks$lemma_lc %in% c("que", causal_lems, conc_lems, cond_lems))
  )

  # f_39–f_42: preposiciones, adjetivos, adverbios
  ev[["f_39_prepositions"]] <- collect(toks$pos == "ADP" & dplyr::coalesce(toks$dep_rel, "") %in% c("case","fixed"))
  ev[["f_40_adj_attr"]]     <- collect(toks$pos == "ADJ" & dplyr::coalesce(toks$dep_rel, "") == "amod")
  ev[["f_41_adj_pred"]]     <- collect(toks$pos == "ADJ" & dplyr::coalesce(toks$dep_rel, "") %in% c("xcomp","acomp"))
  ev[["f_42_adverbs"]]      <- collect(toks$pos == "ADV", max_n = 6)

  # f_43, f_44: métricas computadas
  ev[["f_43_type_token"]]     <- "(m\u00e9trica: TTR)"
  ev[["f_44_mean_word_length"]] <- "(m\u00e9trica: long. media)"

  # f_52–f_54: modales
  mod_pos <- dict_lemmas("f_52_modal_possibility")
  mod_nec <- dict_lemmas("f_53_modal_necessity")
  ev[["f_52_modal_possibility"]] <- collect(toks$lemma_lc %in% mod_pos & toks$pos %in% c("VERB","AUX"))
  ev[["f_53_modal_necessity"]]   <- collect(toks$lemma_lc %in% mod_nec & toks$pos %in% c("VERB","AUX"))
  ev[["f_54_modal_predictive"]]  <- collect(
    (dplyr::coalesce(ef(toks$feats, "Tense"), "") == "Fut" &
     dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin") |
    (toks$lemma_lc == "ir" & grepl("^aux", dplyr::coalesce(toks$dep_rel, "")))
  )

  # f_59: contracciones
  ev[["f_59_contractions"]] <- collect(toks$token_lc %in% c("del","al"))

  # f_60: supresión de que
  ev[["f_60_that_deletion"]] <- ""

  # f_61–f_63: estructuras marcadas
  ev[["f_61_stranded_preposition"]] <- ""
  ev[["f_62_split_infinitive"]]     <- ""
  ev[["f_63_split_auxiliary"]]      <- ""

  # f_64–f_65: coordinación
  ev[["f_64_phrasal_coordination"]] <- collect(toks$pos == "CCONJ" & dplyr::coalesce(toks$dep_rel, "") == "cc")
  ev[["f_65_clausal_coordination"]] <- collect(toks$pos == "CCONJ" & dplyr::coalesce(toks$dep_rel, "") == "cc")

  # f_66–f_67: negación
  neg_syn <- c("nadie","nada","ninguno","ninguna","nunca","jam\u00e1s")
  neg_ana <- tolower(word_lists$negation_particles)
  ev[["f_66_neg_synthetic"]] <- collect(toks$lemma_lc %in% neg_syn)
  ev[["f_67_neg_analytic"]]  <- collect(toks$lemma_lc %in% neg_ana & dplyr::coalesce(toks$dep_rel, "") == "advmod")

  # f_68–f_70: extensiones españolas
  if (length(nom_suffixes) > 0) {
    ev[["f_68_nominalization"]]      <- ev[["f_14_nominalizations"]]
    ev[["f_68_nominalization_rate"]]  <- ev[["f_14_nominalizations"]]
  } else {
    ev[["f_68_nominalization"]] <- ""
    ev[["f_68_nominalization_rate"]] <- ""
  }

  ev[["f_69_mente_adverbs"]]      <- collect(toks$pos == "ADV" & grepl("mente$", toks$token_lc))
  ev[["f_69_mente_adverbs_rate"]] <- ev[["f_69_mente_adverbs"]]

  ev[["f_70_long_words"]]      <- collect(
    toks$pos %in% c("NOUN","VERB","ADJ","ADV","PROPN") & nchar(toks$token) >= 7,
    max_n = 6
  )
  ev[["f_70_long_words_rate"]] <- ev[["f_70_long_words"]]

  ev
}

# ─── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    resultado  = NULL,
    tokens_raw = NULL,
    procesando = FALSE,
    error_msg  = NULL
  )

  output$status_msg <- renderText({
    if (rv$procesando)
      "Procesando\u2026 (puede tardar 10\u201330 s)"
    else if (!is.null(rv$error_msg))
      paste("Error:", rv$error_msg)
    else ""
  })

  observeEvent(input$analizar, {
    texto <- trimws(input$texto)
    if (nchar(texto) == 0) {
      showNotification("Introduce un texto antes de analizar.",
                       type = "warning", duration = 4)
      return()
    }
    rv$procesando  <- TRUE
    rv$error_msg   <- NULL
    rv$resultado   <- NULL
    rv$tokens_raw  <- NULL

    resultado <- tryCatch({
      parsed <- udpipe_annotate(
        object = udmodel, x = texto,
        tagger = "default", parser = "default"
      )
      parsed_df <- as.data.frame(parsed)
      rv$tokens_raw <- parsed_df
      biber_es(parsed_df, measure = "none", normalize = FALSE)
    }, error = function(e) {
      rv$error_msg <- conditionMessage(e)
      NULL
    })

    rv$procesando <- FALSE
    rv$resultado  <- resultado
  })

  # ── Área de resultados ──
  output$results_area <- renderUI({
    if (rv$procesando) {
      div(class = "placeholder-msg",
        div(class = "icon", "\u23f3"),
        p("Anotando y extrayendo rasgos\u2026")
      )
    } else if (!is.null(rv$error_msg)) {
      div(class = "placeholder-msg",
        div(class = "icon", "\u26a0\ufe0f"),
        p(strong("Error:"), code(rv$error_msg))
      )
    } else if (is.null(rv$resultado)) {
      div(class = "placeholder-msg",
        div(class = "icon", "\ud83d\udcdd"),
        p("Pega un texto arriba y pulsa", strong("Analizar"), ".")
      )
    } else {
      res <- rv$resultado
      n_tok <- if ("n_tokens"     %in% names(res)) res$n_tokens[1]     else NA
      n_lex <- if ("n_lex_tokens" %in% names(res)) res$n_lex_tokens[1] else NA

      grupo_choices <- c("Todos los grupos" = "",
                         stats::setNames(
                           sort(unique(feature_labels$grupo)),
                           sort(unique(feature_labels$grupo))
                         ))
      tipo_choices <- c(
        "Biber original + Extensiones" = "",
        "Solo Biber original (f_01\u2013f_67)" = "Biber original",
        "Solo extensiones espa\u00f1olas (f_68\u2013f_71)" = "Extensi\u00f3n espa\u00f1ola"
      )

      tagList(
        div(class = "results-meta",
          HTML(sprintf(
            "<strong>%s</strong> tokens totales &nbsp;|&nbsp;
             <strong>%s</strong> tokens l\u00e9xicos",
            format(n_tok, big.mark = "."),
            format(n_lex, big.mark = ".")
          ))
        ),
        div(class = "filter-bar",
          div(style = "display:inline-block; width:220px;",
            selectInput("fil_grupo", "Filtrar por grupo:",
                        choices = grupo_choices, selected = "")
          ),
          div(style = "display:inline-block; width:300px; margin-left:10px;",
            selectInput("fil_tipo", "Tipo:",
                        choices = tipo_choices, selected = "")
          )
        ),
        DTOutput("tabla_rasgos")
      )
    }
  })

  # ── Datos filtrados para la tabla ──
  tabla_data <- reactive({
    req(rv$resultado)
    res <- rv$resultado

    feat_cols <- grep("^f_", names(res), value = TRUE)
    valores   <- as.numeric(unlist(res[1, feat_cols, drop = TRUE]))

    df_raw <- data.frame(code = feat_cols, valor = valores,
                         stringsAsFactors = FALSE)

    df <- merge(df_raw, feature_labels, by = "code", all.x = TRUE, sort = FALSE)
    df <- df[match(feat_cols, df$code), ]
    df$grupo[is.na(df$grupo)]              <- "Otros"
    df$descripcion[is.na(df$descripcion)]  <- df$code[is.na(df$descripcion)]
    df$tipo[is.na(df$tipo)]                <- "Biber original"

    # Evidencia de palabras marcadas (solo para rasgos con valor > 0)
    ev <- if (!is.null(rv$tokens_raw)) {
      tryCatch(extract_evidence(rv$tokens_raw), error = function(e) list())
    } else list()
    df$palabras <- vapply(seq_len(nrow(df)), function(i) {
      code <- df$code[i]
      val  <- df$valor[i]
      if (!is.na(val) && val != 0 && code %in% names(ev)) ev[[code]] else ""
    }, character(1))

    # Aplicar filtros
    grp  <- input$fil_grupo
    tipo <- input$fil_tipo
    if (!is.null(grp) && nchar(grp) > 0)
      df <- df[df$grupo == grp, ]
    if (!is.null(tipo) && nchar(tipo) > 0)
      df <- df[df$tipo == tipo, ]

    df
  })

  # ── Tabla ──
  output$tabla_rasgos <- renderDT({
    df <- tabla_data()

    tabla <- data.frame(
      Codigo      = df$code,
      Grupo       = df$grupo,
      Descripcion = df$descripcion,
      Tipo        = df$tipo,
      Valor       = df$valor,
      Palabras    = df$palabras,
      stringsAsFactors = FALSE
    )
    colnames(tabla) <- c(
      "C\u00f3digo", "Grupo", "Descripci\u00f3n", "Tipo", "Valor", "Palabras"
    )

    datatable(
      tabla,
      rownames  = FALSE,
      escape    = FALSE,
      class     = "compact stripe hover",
      extensions = "Buttons",
      options   = list(
        pageLength    = 80,
        dom           = "Bfrtip",
        buttons       = list(
          list(extend = "csv",   text = "Exportar CSV"),
          list(extend = "excel", text = "Exportar Excel")
        ),
        scrollY       = "540px",
        scrollCollapse = TRUE,
        columnDefs    = list(
          list(width = "120px", targets = 0),
          list(width = "150px", targets = 1),
          list(width = "250px", targets = 2),
          list(width = "120px", targets = 3),
          list(width = "65px",  targets = 4, className = "dt-right"),
          list(width = "260px", targets = 5)
        ),
        rowCallback = JS(
          "function(row, data) {",
          "  if (data[3] === 'Extensi\u00f3n espa\u00f1ola') {",
          "    $(row).addClass('row-ext');",
          "  }",
          "}"
        ),
        language = list(
          search     = "Buscar:",
          info       = "Rasgos _START_\u2013_END_ de _TOTAL_",
          infoEmpty  = "Sin resultados",
          paginate   = list(first = "\u00ab", last = "\u00bb",
                            previous = "\u2039", `next` = "\u203a"),
          zeroRecords = "No se encontraron rasgos"
        )
      )
    ) %>%
      formatStyle(
        "Tipo",
        target     = "cell",
        fontStyle  = styleEqual("Extensi\u00f3n espa\u00f1ola", "italic"),
        color      = styleEqual("Extensi\u00f3n espa\u00f1ola", "#7a4000")
      ) %>%
      formatStyle(
        "Valor",
        target     = "cell",
        color      = styleInterval(c(0, 1e-9),
                                   c("#bbb", "#bbb", "#005a22")),
        fontWeight = styleInterval(0, c("normal", "bold"))
      ) %>%
      formatStyle(
        "Palabras",
        target    = "cell",
        color     = "#555",
        fontSize  = "0.82em",
        fontStyle = "italic"
      )
  }, server = FALSE)
}

shinyApp(ui = ui, server = server)
