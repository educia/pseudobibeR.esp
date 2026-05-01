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
# 57 rasgos de Biber (1985/1988) adaptados al español.
# Eliminados (intraducibles): f_09, f_12, f_15, f_28, f_31, f_32,
#   f_59, f_60, f_61, f_62, f_68.
# Fusiones: f_29+f_31 → f_29_that_subj; f_30+f_32 → f_30_that_obj.
# Extensiones propias (f_69-f_71) calculadas por biber_es() pero no mostradas.
feature_labels <- data.frame(
  code = c(
    # A. Tiempo y aspecto
    "f_01_past_tense", "f_02_perfect_aspect", "f_03_present_tense",
    # B. Adverbiales de lugar y tiempo
    "f_04_place_adverbials", "f_05_time_adverbials",
    # C. Pronombres
    "f_06_first_person_pronouns", "f_07_second_person_pronouns",
    "f_08_third_person_pronouns",
    "f_10_demonstrative_pronoun", "f_11_indefinite_pronouns",
    # D. Interrogativas
    "f_13_wh_question",
    # E. Formas nominales
    "f_14_nominalizations", "f_16_other_nouns",
    # F. Pasivas
    "f_17_agentless_passives", "f_18_by_passives",
    # G. Formas estativas
    "f_19_be_main_verb", "f_20_existential_there",
    # H. Subordinación
    "f_21_that_verb_comp", "f_22_that_adj_comp", "f_23_wh_clause",
    "f_24_infinitives", "f_25_present_participle", "f_26_past_participle",
    "f_27_past_participle_whiz",
    "f_29_that_subj", "f_30_that_obj",
    "f_33_pied_piping", "f_34_sentence_relatives",
    "f_35_because", "f_36_though", "f_37_if", "f_38_other_adv_sub",
    # I. Preposiciones, adjetivos y adverbios
    "f_39_prepositions", "f_40_adj_attr", "f_41_adj_pred", "f_42_adverbs",
    # J. Especificidad léxica
    "f_43_type_token", "f_44_mean_word_length",
    # K. Clases léxicas
    "f_45_conjuncts", "f_46_downtoners", "f_47_hedges", "f_48_amplifiers",
    "f_49_emphatics", "f_50_discourse_particles", "f_51_demonstratives",
    # L. Modales
    "f_52_modal_possibility", "f_53_modal_necessity", "f_54_modal_predictive",
    # M. Verbos especializados
    "f_55_verb_public", "f_56_verb_private", "f_57_verb_suasive", "f_58_verb_seem",
    # N. Estructuras reducidas
    "f_63_split_auxiliary",
    # O. Coordinación
    "f_64_phrasal_coordination", "f_65_clausal_coordination",
    # P. Negación
    "f_66_neg_synthetic", "f_67_neg_analytic"
  ),
  grupo = c(
    "A. Tiempo y aspecto", "A. Tiempo y aspecto", "A. Tiempo y aspecto",
    "B. Adverbiales lugar/tiempo", "B. Adverbiales lugar/tiempo",
    "C. Pronombres", "C. Pronombres", "C. Pronombres",
    "C. Pronombres", "C. Pronombres",
    "D. Preguntas",
    "E. Formas nominales", "E. Formas nominales",
    "F. Pasivas", "F. Pasivas",
    "G. Formas estativas", "G. Formas estativas",
    "H. Subordinación", "H. Subordinación", "H. Subordinación",
    "H. Subordinación", "H. Subordinación", "H. Subordinación",
    "H. Subordinación",
    "H. Subordinación", "H. Subordinación",
    "H. Subordinación", "H. Subordinación",
    "H. Subordinación", "H. Subordinación", "H. Subordinación", "H. Subordinación",
    "I. Prep., adj. y adv.", "I. Prep., adj. y adv.",
    "I. Prep., adj. y adv.", "I. Prep., adj. y adv.",
    "J. Especificidad léxica", "J. Especificidad léxica",
    "K. Clases léxicas", "K. Clases léxicas", "K. Clases léxicas",
    "K. Clases léxicas", "K. Clases léxicas", "K. Clases léxicas", "K. Clases léxicas",
    "L. Modales", "L. Modales", "L. Modales",
    "M. Verbos especializados", "M. Verbos especializados",
    "M. Verbos especializados", "M. Verbos especializados",
    "N. Estructuras reducidas",
    "O. Coordinación", "O. Coordinación",
    "P. Negación", "P. Negación"
  ),
  descripcion = c(
    # A
    "Pretérito indefinido (pasado simple)",
    "Aspecto perfecto (haber + part.)",
    "Presente de indicativo",
    # B
    "Adverbiales de lugar",
    "Adverbiales de tiempo",
    # C
    "Pronombres 1.ª persona (yo, nosotros…)",
    "Pronombres 2.ª persona (tú, usted…)",
    "Pronombres 3.ª persona (él, ella, le…)",
    "Pronombres demostrativos (este, ese…)",
    "Pronombres indefinidos (alguien, nadie…)",
    # D
    "Interrogativas con pronombre QU-",
    # E
    "Nominalizaciones (-ción, -idad, -miento…)",
    "Otros sustantivos",
    # F
    "Pasiva sin agente",
    "Pasiva con agente (por)",
    # G
    "Ser/estar como verbo principal",
    "Existencial (hay)",
    # H
    "Compl. de verbo con que (dijo que…)",
    "Compl. de adj. con que (es importante que…)",
    "Cláusula WH indirecta (no sé quién…)",
    "Infinitivos clausales",
    "Gerundio adverbial/complemento",
    "Participio adverbial/absoluto",
    "Relativa reducida (participio postnominal)",
    "Relativa de sujeto/objeto con que/quien [f_29+f_31]",
    "Relativa con quien/cual (obl/obj) [f_30+f_32]",
    "Pied-piping (prep. + pronombre relativo)",
    "Relativas sentenciales (lo que, eso que…)",
    "Subordinada causal (porque)",
    "Subordinada concesiva (aunque)",
    "Subordinada condicional (si…)",
    "Otras subordinadas adverbiales",
    # I
    "Preposiciones",
    "Adjetivo atributivo (amod)",
    "Adjetivo predicativo (cop/xcomp)",
    "Adverbios generales",
    # J
    "TTR (type-token ratio)",
    "Longitud media de palabra (tokens léxicos)",
    # K
    "Conjunciones textuales (sin embargo, además…)",
    "Atenuadores (casi, apenas, algo…)",
    "Marcadores de duda (quizás, tal vez…)",
    "Amplificadores (muy, totalmente, enormemente…)",
    "Enfáticos (de hecho, sin duda, realmente…)",
    "Partículas discursivas",
    "Demostrativos determinantes (este, ese, aquel)",
    # L
    "Modal de posibilidad (poder + inf.)",
    "Modal de necesidad (deber / tener que)",
    "Modal predictivo (ir a + inf. / futuro sint.)",
    # M
    "Verbos públicos (decir, afirmar, anunciar…)",
    "Verbos privados (creer, pensar, saber…)",
    "Verbos suasorios (pedir, recomendar, exigir…)",
    "Verbos de apariencia (parecer, resultar…)",
    # N
    "Auxiliar escindido",
    # O
    "Coordinación sintagmática",
    "Coordinación clausal",
    # P
    "Negación sintética (nadie, nunca, nada…)",
    "Negación analítica (no + verbo)"
  ),
  stringsAsFactors = FALSE
)

# ─── UI ──────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "pseudobibeR.es — Rasgos de Biber",

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
    "))
  ),

  div(class = "app-header",
    h1("pseudobibeR.es"),
    p("Extractor de rasgos léxico-gramaticales de Biber (1985/1988) para el español")
  ),

  div(class = "main-container",

    # ── Panel de entrada ──
    div(class = "panel",
      h2("Texto a analizar"),
      textAreaInput(
        "texto", label = NULL, value = "", rows = 10,
        placeholder = paste0(
          "Pega aquí el texto en español (mínimo recomendado: 200 palabras).\n",
          "Se anotará con UDPipe (modelo spanish-gsd) y se extraerán los ",
          "rasgos de Biber (1985/1988) adaptados al español."
        ),
        width = "100%"
      ),
      div(
        actionButton("analizar", "Analizar", icon = icon("magnifying-glass")),
        actionButton("cargar_ejemplo", "Cargar ejemplo",
                     icon = icon("file-lines"),
                     style = "background:#f0ece3; color:#1c3d5a; border:1px solid #d4cfc4; margin-left:8px;"),
        actionButton("limpiar", "Limpiar",
                     icon = icon("xmark"),
                     style = "background:#f0ece3; color:#666; border:1px solid #d4cfc4; margin-left:8px;"),
        span(id = "status_msg", textOutput("status_msg", inline = TRUE))
      )
    ),

    # ── Panel de resultados ──
    div(class = "panel",
      h2("Rasgos de Biber"),
      uiOutput("results_area")
    ),

    div(class = "app-footer",
      HTML(paste0(
        "<strong>pseudobibeR.es</strong> &mdash; ",
        "Extractor de rasgos léxico-gramaticales de Biber (1985/1988) para español<br>",
        "<span style='font-size:.75rem;'>",
        "Anotación: <code>UDPipe</code> + modelo <code>spanish-gsd</code> &middot; ",
        "57 rasgos de Biber adaptados al español &middot; ",
        "11 rasgos intraducibles eliminados (f_09, f_12, f_15, f_28, f_31, f_32, f_59&ndash;f_62, f_68)",
        "</span><br>",
        "<span style='font-size:.72rem; color:#999; font-style:italic; margin-top:6px; display:inline-block;'>",
        "Cita: Biber, D. (1988). <em>Variation across Speech and Writing</em>. Cambridge University Press.",
        "</span>"
      ))
    )
  )
)

# ─── Extracción de evidencia (palabras marcadas) ─────────────────────────────
# Devuelve un named character vector: feature_code → "palabra1, palabra2, …"
extract_evidence <- function(raw_tokens) {
  data("dict",       package = "pseudobibeR.es", envir = environment())
  data("word_lists", package = "pseudobibeR.es", envir = environment())

  toks <- raw_tokens
  if ("upos" %in% colnames(toks) && !"pos" %in% colnames(toks))
    toks$pos <- toks$upos
  toks$lemma_lc <- tolower(toks$lemma)
  toks$token_lc <- tolower(toks$token)

  # Extrae un atributo morfosintáctico del campo feats de UDPipe
  ef <- function(feats_vec, attr) {
    pattern <- paste0("(?:^|\\|)", attr, "=([^|]+)")
    m <- regmatches(
      dplyr::coalesce(feats_vec, ""),
      regexpr(pattern, dplyr::coalesce(feats_vec, ""), perl = TRUE)
    )
    ifelse(lengths(regmatches(feats_vec, gregexpr(pattern, dplyr::coalesce(feats_vec, ""), perl = TRUE))) > 0,
           sub(paste0(".*", attr, "=([^|]+).*"), "\\1",
               regmatches(dplyr::coalesce(feats_vec, ""),
                          regexpr(pattern, dplyr::coalesce(feats_vec, ""), perl = TRUE))),
           NA_character_)
  }
  # Versión segura usando stringr si disponible, o fallback
  extract_feat_safe <- function(feats_vec, attr) {
    pat <- paste0("(?:^|[|])", attr, "=([^|]+)")
    sapply(dplyr::coalesce(feats_vec, ""), function(f) {
      m <- regmatches(f, regexpr(pat, f, perl = TRUE))
      if (length(m) == 0 || m == "") return(NA_character_)
      sub(paste0(".*", attr, "="), "", m)
    }, USE.NAMES = FALSE)
  }
  ef <- extract_feat_safe

  # Recoge tokens únicos que cumplen una máscara lógica
  collect <- function(mask, col = "token", max_n = 8) {
    mask[is.na(mask)] <- FALSE
    if (sum(mask) == 0) return("")
    words <- unique(toks[[col]][mask])
    if (length(words) > max_n)
      paste0(paste(words[seq_len(max_n)], collapse = ", "), ", …")
    else
      paste(words, collapse = ", ")
  }

  # Lemas del diccionario (solo entradas de una palabra)
  dict_lemmas <- function(key) {
    if (!key %in% names(dict)) return(character(0))
    single <- dict[[key]][!grepl("_", dict[[key]])]
    unique(tolower(single))
  }

  ev <- list()

  # ── A. Tiempo y aspecto ──────────────────────────────────────────────────
  # f_01: pretérito indefinido (Tense=Past, Mood=Ind, VerbForm=Fin)
  ev[["f_01_past_tense"]] <- collect(
    toks$pos %in% c("VERB", "AUX") &
    dplyr::coalesce(ef(toks$feats, "Tense"),    "") == "Past" &
    dplyr::coalesce(ef(toks$feats, "Mood"),     "") == "Ind"  &
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin"
  )
  # f_02: aspecto perfecto (haber como AUX de participio)
  ev[["f_02_perfect_aspect"]] <- collect(
    toks$lemma_lc %in% c("haber") &
    toks$pos %in% c("AUX", "VERB") &
    grepl("^aux", dplyr::coalesce(toks$dep_rel, ""))
  )
  # f_03: presente de indicativo (Tense=Pres, Mood=Ind, VerbForm=Fin)
  ev[["f_03_present_tense"]] <- collect(
    toks$pos %in% c("VERB", "AUX") &
    dplyr::coalesce(ef(toks$feats, "Tense"),    "") == "Pres" &
    dplyr::coalesce(ef(toks$feats, "Mood"),     "") == "Ind"  &
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin"
  )
  # f_71 (extensión española): pretérito imperfecto (Tense=Imp, Mood=Ind)
  ev[["f_71_preterit"]] <- collect(
    toks$pos %in% c("VERB", "AUX") &
    dplyr::coalesce(ef(toks$feats, "Tense"),    "") == "Imp" &
    dplyr::coalesce(ef(toks$feats, "Mood"),     "") == "Ind" &
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin"
  )

  # ── B. Adverbiales ───────────────────────────────────────────────────────
  for (feat in c("f_04_place_adverbials", "f_05_time_adverbials")) {
    lems <- dict_lemmas(feat)
    ev[[feat]] <- collect(toks$lemma_lc %in% lems)
  }

  # ── C. Pronombres ────────────────────────────────────────────────────────
  reflexive_deps <- c("expl:pv", "expl:impers", "expl")
  non_refl <- !dplyr::coalesce(toks$dep_rel, "") %in% reflexive_deps

  pron_06 <- tolower(c("yo", "nosotros", "nosotras", "me", "nos", "mí", "conmigo"))
  pron_07 <- tolower(c("tú", "vos", "vosotros", "vosotras", "usted", "ustedes",
                        "te", "ti", "contigo", "os"))
  pron_08 <- tolower(c("él", "ella", "ello", "ellos", "ellas",
                        "le", "lo", "la", "les", "los", "las", "consigo"))
  ev[["f_06_first_person_pronouns"]]  <- collect(toks$lemma_lc %in% pron_06 & toks$pos == "PRON" & non_refl)
  ev[["f_07_second_person_pronouns"]] <- collect(toks$lemma_lc %in% pron_07 & toks$pos == "PRON" & non_refl)
  ev[["f_08_third_person_pronouns"]]  <- collect(toks$lemma_lc %in% pron_08 & toks$pos == "PRON" & non_refl)

  dem_lemmas <- dict_lemmas("f_51_demonstratives")
  ev[["f_10_demonstrative_pronoun"]] <- collect(toks$lemma_lc %in% dem_lemmas & toks$pos == "PRON")

  indef_lems <- dict_lemmas("f_11_indefinite_pronoun")
  ev[["f_11_indefinite_pronouns"]] <- collect(toks$lemma_lc %in% indef_lems & toks$pos %in% c("PRON", "DET"))

  # ── D. Interrogativas ────────────────────────────────────────────────────
  wh_q <- c("qué", "quién", "quiénes", "cuál", "cuáles",
             "cuánto", "cuánta", "cuántos", "cuántas",
             "cuándo", "dónde", "cómo")
  ev[["f_13_wh_question"]] <- collect(toks$lemma_lc %in% tolower(wh_q) &
                                       toks$pos %in% c("PRON", "ADV", "DET", "ADJ"))

  # ── E. Formas nominales ──────────────────────────────────────────────────
  nom_suffixes <- word_lists$nominalization_suffixes
  if (length(nom_suffixes) > 0) {
    nom_regex <- paste0("(", paste(nom_suffixes, collapse = "|"), ")$")
    ev[["f_14_nominalizations"]] <- collect(toks$pos == "NOUN" & grepl(nom_regex, toks$lemma_lc))
  } else {
    ev[["f_14_nominalizations"]] <- ""
  }
  ev[["f_16_other_nouns"]] <- collect(toks$pos %in% c("NOUN", "PROPN"), max_n = 6)

  # ── F. Pasivas ───────────────────────────────────────────────────────────
  ev[["f_17_agentless_passives"]] <- collect(
    grepl("^aux:pass", dplyr::coalesce(toks$dep_rel, ""))
  )
  # f_18: buscar "por" en función obl:agent tras participio pasivo
  ev[["f_18_by_passives"]] <- collect(
    toks$lemma_lc == "por" &
    dplyr::coalesce(toks$dep_rel, "") %in% c("case", "obl:agent")
  )

  # ── G. Formas estativas ───────────────────────────────────────────────────
  ev[["f_19_be_main_verb"]] <- collect(
    toks$lemma_lc %in% c("ser", "estar") &
    toks$pos %in% c("VERB", "AUX")
  )
  ev[["f_20_existential_there"]] <- collect(
    toks$lemma_lc == "haber" &
    toks$token_lc %in% c("hay", "había", "habrá", "habría", "hubo", "haya")
  )

  # ── H. Subordinación ─────────────────────────────────────────────────────
  ev[["f_21_that_verb_comp"]] <- collect(
    toks$lemma_lc == "que" & toks$pos == "SCONJ" &
    dplyr::coalesce(toks$dep_rel, "") == "mark"
  )
  ev[["f_22_that_adj_comp"]]  <- ev[["f_21_that_verb_comp"]]  # misma forma superficial
  ev[["f_23_wh_clause"]] <- collect(
    toks$lemma_lc %in% tolower(c("qué", "quién", "quiénes", "cuál", "cuáles",
                                   "cuándo", "dónde", "cómo", "cuánto")) &
    grepl("^(obj|obl|nsubj|iobj|mark|advmod|nmod)", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_24_infinitives"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Inf" &
    grepl("^(xcomp|ccomp|advcl|acl|obj)", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_25_present_participle"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Ger" &
    grepl("^(advcl|ccomp)", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_26_past_participle"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Part" &
    grepl("^(advcl|ccomp|acl)", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_27_past_participle_whiz"]] <- collect(
    dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Part" &
    dplyr::coalesce(toks$dep_rel, "") == "acl" &
    toks$pos %in% c("VERB", "ADJ")
  )
  # f_29: "que" relativo (SCONJ/mark + acl:relcl head) O pronombres QU- sujeto
  ev[["f_29_that_subj"]] <- collect(
    (toks$lemma_lc == "que" & toks$pos == "SCONJ" &
     dplyr::coalesce(toks$dep_rel, "") == "mark") |
    (toks$lemma_lc %in% c("quien", "quienes", "cual", "cuales") &
     toks$pos == "PRON" &
     grepl("^nsubj", dplyr::coalesce(toks$dep_rel, "")))
  )
  # f_30: pronombres QU- en posición objeto/oblicuo
  ev[["f_30_that_obj"]] <- collect(
    toks$lemma_lc %in% c("quien", "quienes", "cual", "cuales") &
    toks$pos == "PRON" &
    grepl("^(obj|iobj|obl|nmod)", dplyr::coalesce(toks$dep_rel, ""))
  )
  ev[["f_33_pied_piping"]] <- collect(
    toks$lemma_lc %in% c("que", "quien", "quienes", "cual", "cuales") &
    toks$pos %in% c("PRON", "DET")
  )
  ev[["f_34_sentence_relatives"]] <- collect(
    toks$lemma_lc %in% c("lo", "eso", "esto", "ello") & toks$pos == "PRON"
  )

  causal_lems <- dict_lemmas("f_35_because"); if (!length(causal_lems)) causal_lems <- "porque"
  conc_lems   <- dict_lemmas("f_36_though");  if (!length(conc_lems))   conc_lems   <- "aunque"
  cond_lems   <- dict_lemmas("f_37_if");      if (!length(cond_lems))   cond_lems   <- "si"

  ev[["f_35_because"]] <- collect(toks$lemma_lc %in% causal_lems & toks$pos %in% c("SCONJ", "CCONJ", "ADV"))
  ev[["f_36_though"]]  <- collect(toks$lemma_lc %in% conc_lems   & toks$pos %in% c("SCONJ", "CCONJ", "ADV"))
  ev[["f_37_if"]]      <- collect(toks$lemma_lc %in% cond_lems   & toks$pos %in% c("SCONJ", "CCONJ", "ADV"))
  ev[["f_38_other_adv_sub"]] <- collect(
    toks$pos %in% c("SCONJ", "ADP", "ADV") &
    dplyr::coalesce(toks$dep_rel, "") == "mark" &
    !toks$lemma_lc %in% c("que", causal_lems, conc_lems, cond_lems)
  )

  # ── I. Prep., adj. y adv. ─────────────────────────────────────────────────
  ev[["f_39_prepositions"]] <- collect(toks$pos == "ADP" & dplyr::coalesce(toks$dep_rel, "") %in% c("case", "fixed"))
  ev[["f_40_adj_attr"]]     <- collect(toks$pos == "ADJ" & dplyr::coalesce(toks$dep_rel, "") == "amod")
  ev[["f_41_adj_pred"]]     <- collect(
    toks$pos == "ADJ" & (
      dplyr::coalesce(toks$dep_rel, "") %in% c("xcomp", "acomp") |
      dplyr::coalesce(toks$dep_rel, "") == "root"   # ADJ como raíz en oración copulativa
    )
  )
  ev[["f_42_adverbs"]]      <- collect(toks$pos == "ADV", max_n = 6)

  # ── J. Especificidad léxica ───────────────────────────────────────────────
  ev[["f_43_type_token"]]       <- "(métrica: TTR)"
  ev[["f_44_mean_word_length"]] <- "(métrica: longitud media)"

  # ── K. Clases léxicas ────────────────────────────────────────────────────
  for (feat in c("f_45_conjuncts", "f_46_downtoners", "f_47_hedges",
                 "f_48_amplifiers", "f_49_emphatics", "f_50_discourse_particles")) {
    lems <- dict_lemmas(feat)
    ev[[feat]] <- collect(toks$lemma_lc %in% lems)
  }
  ev[["f_51_demonstratives"]] <- collect(toks$lemma_lc %in% dem_lemmas & toks$pos == "DET")

  # ── L. Modales ────────────────────────────────────────────────────────────
  mod_pos <- dict_lemmas("f_52_modal_possibility")
  mod_nec <- dict_lemmas("f_53_modal_necessity")
  ev[["f_52_modal_possibility"]] <- collect(toks$lemma_lc %in% mod_pos & toks$pos %in% c("VERB", "AUX"))
  ev[["f_53_modal_necessity"]]   <- collect(toks$lemma_lc %in% mod_nec & toks$pos %in% c("VERB", "AUX"))
  ev[["f_54_modal_predictive"]]  <- collect(
    (dplyr::coalesce(ef(toks$feats, "Tense"), "") == "Fut" &
     dplyr::coalesce(ef(toks$feats, "VerbForm"), "") == "Fin") |
    (toks$lemma_lc == "ir" & grepl("^aux", dplyr::coalesce(toks$dep_rel, "")))
  )

  # ── M. Verbos especializados ──────────────────────────────────────────────
  for (feat in c("f_55_verb_public", "f_56_verb_private",
                 "f_57_verb_suasive", "f_58_verb_seem")) {
    lems <- dict_lemmas(feat)
    ev[[feat]] <- collect(toks$lemma_lc %in% lems & toks$pos %in% c("VERB", "AUX"))
  }

  # ── N. Estructuras reducidas ──────────────────────────────────────────────
  ev[["f_63_split_auxiliary"]] <- collect(
    toks$pos %in% c("AUX") &
    grepl("^aux", dplyr::coalesce(toks$dep_rel, ""))
  )

  # ── O. Coordinación ───────────────────────────────────────────────────────
  ev[["f_64_phrasal_coordination"]] <- collect(
    toks$pos == "CCONJ" & dplyr::coalesce(toks$dep_rel, "") == "cc"
  )
  ev[["f_65_clausal_coordination"]] <- ev[["f_64_phrasal_coordination"]]

  # ── P. Negación ───────────────────────────────────────────────────────────
  neg_syn  <- c("nadie", "nada", "ninguno", "ninguna", "nunca", "jamás", "tampoco")
  neg_part <- tolower(c("no", "ni", "tampoco"))
  ev[["f_66_neg_synthetic"]] <- collect(toks$lemma_lc %in% neg_syn & toks$pos %in% c("PRON", "ADV", "DET"))
  ev[["f_67_neg_analytic"]]  <- collect(
    toks$lemma_lc %in% neg_part &
    dplyr::coalesce(toks$dep_rel, "") == "advmod"
  )

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
      "Procesando… (puede tardar 10–30 s)"
    else if (!is.null(rv$error_msg))
      paste("Error:", rv$error_msg)
    else ""
  })

  # ── Texto de ejemplo ──
  observeEvent(input$cargar_ejemplo, {
    texto_ejemplo <- paste0(
      "Este es un texto de prueba. Fue escrito por un estudiante. ",
      "El trabajo fue revisado por el profesor y corregido con cuidado. ",
      "Los cambios habían sido propuestos la semana anterior.\n\n",
      "Hay muchos estudios que han analizado la variación de registros. ",
      "En la lingüística de corpus, diversas investigaciones muestran ",
      "que los textos académicos presentan una alta densidad de ",
      "nominalizaciones y pasivas sin agente. Se han identificado también ",
      "diferencias sistemáticas entre el registro hablado y el escrito, ",
      "tal como se reporta en la bibliografía especializada. ",
      "Existen múltiples dimensiones funcionales que permiten clasificar ",
      "los textos según su propósito comunicativo."
    )
    updateTextAreaInput(session, "texto", value = texto_ejemplo)
  })

  # ── Botón Limpiar ──
  observeEvent(input$limpiar, {
    updateTextAreaInput(session, "texto", value = "")
    rv$resultado  <- NULL
    rv$tokens_raw <- NULL
    rv$error_msg  <- NULL
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
      parsed_df     <- as.data.frame(parsed)
      rv$tokens_raw <- parsed_df
      biber_es(parsed_df, measure = "MATTR", normalize = FALSE)
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
        div(class = "icon", "⏳"),
        p("Anotando y extrayendo rasgos…")
      )
    } else if (!is.null(rv$error_msg)) {
      div(class = "placeholder-msg",
        div(class = "icon", "⚠️"),
        p(strong("Error:"), code(rv$error_msg))
      )
    } else if (is.null(rv$resultado)) {
      div(class = "placeholder-msg",
        div(class = "icon", "📝"),
        p("Pega un texto arriba y pulsa", strong("Analizar"), ".")
      )
    } else {
      res <- rv$resultado
      n_tok <- if ("n_tokens"          %in% names(res)) res$n_tokens[1]          else NA
      n_lex <- if ("n_lex_tokens"      %in% names(res)) res$n_lex_tokens[1]      else NA
      ttr   <- if ("f_43_type_token"   %in% names(res)) res$f_43_type_token[1]   else NA
      mwl   <- if ("f_44_mean_word_length" %in% names(res)) res$f_44_mean_word_length[1] else NA

      grupos_all   <- unique(feature_labels$grupo)
      grupos_biber <- sort(grupos_all[grepl("^[A-P]\\. ", grupos_all)])
      grupos_ext   <- grupos_all[!grepl("^[A-P]\\. ", grupos_all)]
      grupos_ord   <- c(grupos_biber, grupos_ext)
      grupo_choices <- c("Todos los grupos" = "",
                         stats::setNames(grupos_ord, grupos_ord))

      tagList(
        div(class = "results-meta",
          HTML(sprintf(
            paste0("<strong>%s</strong> tokens totales &nbsp;|&nbsp; ",
                   "<strong>%s</strong> tokens léxicos &nbsp;|&nbsp; ",
                   "TTR: <strong>%.3f</strong> &nbsp;|&nbsp; ",
                   "Long. media: <strong>%.2f</strong> car."),
            format(n_tok, big.mark = "."),
            format(n_lex, big.mark = "."),
            as.numeric(ttr),
            as.numeric(mwl)
          ))
        ),
        div(class = "filter-bar",
          div(style = "display:inline-block; width:240px;",
            selectInput("fil_grupo", "Filtrar por grupo:",
                        choices = grupo_choices, selected = "")
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
    feat_cols <- feat_cols[!grepl("^f_(69|70|71)_", feat_cols)]  # ocultar extensiones
    valores   <- as.numeric(unlist(res[1, feat_cols, drop = TRUE]))

    df_raw <- data.frame(code = feat_cols, valor = valores, stringsAsFactors = FALSE)
    df <- merge(df_raw, feature_labels, by = "code", all.x = TRUE, sort = FALSE)
    df <- df[match(feat_cols, df$code), ]
    df$grupo[is.na(df$grupo)]             <- "Otros"
    df$descripcion[is.na(df$descripcion)] <- df$code[is.na(df$descripcion)]

    ev <- if (!is.null(rv$tokens_raw)) {
      tryCatch(extract_evidence(rv$tokens_raw), error = function(e) list())
    } else list()
    df$palabras <- vapply(seq_len(nrow(df)), function(i) {
      code <- df$code[i]
      val  <- df$valor[i]
      if (!is.na(val) && val != 0 && code %in% names(ev)) ev[[code]] else ""
    }, character(1))

    grp <- input$fil_grupo
    if (!is.null(grp) && nchar(grp) > 0) df <- df[df$grupo == grp, ]

    df
  })

  # ── Tabla ──
  output$tabla_rasgos <- renderDT({
    df <- tabla_data()

    tabla <- data.frame(
      Codigo      = df$code,
      Grupo       = df$grupo,
      Descripcion = df$descripcion,
      Valor       = df$valor,
      Palabras    = df$palabras,
      stringsAsFactors = FALSE
    )
    colnames(tabla) <- c("Código", "Grupo", "Descripción", "Valor", "Palabras")

    datatable(
      tabla,
      rownames   = FALSE,
      escape     = FALSE,
      class      = "compact stripe hover",
      extensions = "Buttons",
      options    = list(
        pageLength     = 80,
        dom            = "Bfrtip",
        buttons        = list(
          list(extend = "csv",   text = "Exportar CSV"),
          list(extend = "excel", text = "Exportar Excel")
        ),
        scrollY        = "540px",
        scrollCollapse = TRUE,
        columnDefs     = list(
          list(width = "160px", targets = 0),
          list(width = "160px", targets = 1),
          list(width = "300px", targets = 2),
          list(width = "65px",  targets = 3, className = "dt-right"),
          list(width = "240px", targets = 4)
        ),
        language = list(
          search      = "Buscar:",
          info        = "Rasgos _START_–_END_ de _TOTAL_",
          infoEmpty   = "Sin resultados",
          paginate    = list(first = "«", last = "»",
                             previous = "‹", `next` = "›"),
          zeroRecords = "No se encontraron rasgos"
        )
      )
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
