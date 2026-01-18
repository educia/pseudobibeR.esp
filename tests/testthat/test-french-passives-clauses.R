# nolint start: line_length_linter, object_name_linter

test_that("passive counts handle aux:pass and French agents", {
  passive_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docE", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, NA_character_,
    "docE", 1L, 2L, "résultats", "résultat", "NOUN", "NOUN", "nsubj:pass", 4L, NA_character_,
    "docE", 1L, 3L, "sont", "être", "AUX", "AUX", "aux:pass", 4L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docE", 1L, 4L, "publiés", "publier", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur|Voice=Pass",
    "docE", 1L, 5L, "souvent", "souvent", "ADV", "ADV", "advmod", 4L, NA_character_,
    "docE", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 4L, NA_character_,
    "docF", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, NA_character_,
    "docF", 1L, 2L, "documents", "document", "NOUN", "NOUN", "nsubj:pass", 4L, NA_character_,
    "docF", 1L, 3L, "ont", "avoir", "AUX", "AUX", "aux:pass", 4L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docF", 1L, 4L, "été", "être", "AUX", "AUX", "aux:pass", 5L, "VerbForm=Part|Tense=Past|Voice=Pass",
    "docF", 1L, 5L, "validés", "valider", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur|Voice=Pass",
    "docF", 1L, 6L, "par", "par", "ADP", "ADP", "case", 7L, NA_character_,
    "docF", 1L, 7L, "l'équipe", "équipe", "NOUN", "NOUN", "obl", 5L, NA_character_,
    "docF", 1L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 5L, NA_character_
  )

  passive_tokens$doc_id <- stringr::str_to_lower(passive_tokens$doc_id)
  passive_tokens$token <- stringr::str_to_lower(passive_tokens$token)
  passive_tokens$lemma <- stringr::str_to_lower(passive_tokens$lemma)
  passive_tokens$tag <- stringr::str_to_upper(passive_tokens$tag)

  class(passive_tokens) <- c("spacyr_parsed", class(passive_tokens))

  features <- biber(passive_tokens, measure = "none", normalize = FALSE)

  docE <- features[features$doc_id == "doce", , drop = FALSE]
  docF <- features[features$doc_id == "docf", , drop = FALSE]

  expect_equal(docE$f_17_agentless_passives, 1)
  expect_equal(docE$f_18_by_passives, 0)

  expect_equal(docF$f_17_agentless_passives, 0)
  expect_equal(docF$f_18_by_passives, 1)
})


test_that("être main verb excludes auxiliaries", {
  etre_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docG", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=3|Number=Plur",
    "docG", 1L, 2L, "sont", "être", "AUX", "AUX", "cop", 3L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docG", 1L, 3L, "heureux", "heureux", "ADJ", "ADJ", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docG", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docH", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj:pass", 4L, "PronType=Prs|Person=3|Number=Plur",
    "docH", 1L, 2L, "sont", "être", "AUX", "AUX", "aux:pass", 4L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docH", 1L, 3L, "été", "être", "AUX", "AUX", "aux:pass", 4L, "VerbForm=Part|Tense=Past|Voice=Pass",
    "docH", 1L, 4L, "aidés", "aider", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur|Voice=Pass",
    "docH", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 4L, NA_character_
  )

  etre_tokens$doc_id <- stringr::str_to_lower(etre_tokens$doc_id)
  etre_tokens$token <- stringr::str_to_lower(etre_tokens$token)
  etre_tokens$lemma <- stringr::str_to_lower(etre_tokens$lemma)
  etre_tokens$tag <- stringr::str_to_upper(etre_tokens$tag)

  class(etre_tokens) <- c("spacyr_parsed", class(etre_tokens))

  features <- biber(etre_tokens, measure = "none", normalize = FALSE)

  docG <- features[features$doc_id == "docg", , drop = FALSE]
  docH <- features[features$doc_id == "doch", , drop = FALSE]

  expect_equal(docG$f_19_be_main_verb, 1)
  expect_equal(docH$f_19_be_main_verb, 0)
})


test_that("French complementizers map to verb and adjective complements", {
  que_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docI", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docI", 1L, 2L, "crois", "croire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docI", 1L, 3L, "que", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docI", 1L, 4L, "nous", "nous", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=1|Number=Plur",
    "docI", 1L, 5L, "partons", "partir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docI", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docJ", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=3|Number=Plur",
    "docJ", 1L, 2L, "sont", "être", "AUX", "AUX", "cop", 3L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docJ", 1L, 3L, "heureux", "heureux", "ADJ", "ADJ", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docJ", 1L, 4L, "que", "que", "SCONJ", "SCONJ", "mark", 6L, NA_character_,
    "docJ", 1L, 5L, "vous", "vous", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=2|Number=Plur",
    "docJ", 1L, 6L, "veniez", "venir", "VERB", "VERB", "ccomp", 3L, "Mood=Sub|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin",
    "docJ", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docK", 1L, 1L, "Il", "il", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docK", 1L, 2L, "dit", "dire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docK", 1L, 3L, "qu'", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docK", 1L, 4L, "il", "il", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docK", 1L, 5L, "part", "partir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docK", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  que_tokens$doc_id <- stringr::str_to_lower(que_tokens$doc_id)
  que_tokens$token <- stringr::str_to_lower(que_tokens$token)
  que_tokens$lemma <- stringr::str_to_lower(que_tokens$lemma)
  que_tokens$tag <- stringr::str_to_upper(que_tokens$tag)

  class(que_tokens) <- c("spacyr_parsed", class(que_tokens))

  features <- biber(que_tokens, measure = "none", normalize = FALSE)

  docI <- features[features$doc_id == "doci", , drop = FALSE]
  docJ <- features[features$doc_id == "docj", , drop = FALSE]
  docK <- features[features$doc_id == "dock", , drop = FALSE]

  expect_equal(docI$f_21_that_verb_comp, 1)
  expect_equal(docI$f_22_that_adj_comp, 0)

  expect_equal(docJ$f_21_that_verb_comp, 0)
  expect_equal(docJ$f_22_that_adj_comp, 1)

  expect_equal(docK$f_21_that_verb_comp, 1)
  expect_equal(docK$f_22_that_adj_comp, 0)
})


test_that("French wh clauses counted for f_23", {
  wh_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docL", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docL", 1L, 2L, "sais", "savoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docL", 1L, 3L, "qui", "qui", "PRON", "PRON", "nsubj", 4L, "PronType=Int|Number=Sing",
    "docL", 1L, 4L, "vient", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docL", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docM", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docM", 1L, 2L, "dis", "dire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docM", 1L, 3L, "que", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docM", 1L, 4L, "tu", "tu", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=2|Number=Sing",
    "docM", 1L, 5L, "viens", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docM", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  wh_tokens$doc_id <- stringr::str_to_lower(wh_tokens$doc_id)
  wh_tokens$token <- stringr::str_to_lower(wh_tokens$token)
  wh_tokens$lemma <- stringr::str_to_lower(wh_tokens$lemma)
  wh_tokens$tag <- stringr::str_to_upper(wh_tokens$tag)

  class(wh_tokens) <- c("spacyr_parsed", class(wh_tokens))

  features <- biber(wh_tokens, measure = "none", normalize = FALSE)

  docL <- features[features$doc_id == "docl", , drop = FALSE]
  docM <- features[features$doc_id == "docm", , drop = FALSE]

  expect_equal(docL$f_23_wh_clause, 1)
  expect_equal(docM$f_23_wh_clause, 0)
})


test_that("French adverbial subordinators counted", {
  sub_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docSR", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=3|Number=Plur",
    "docSR", 1L, 2L, "se", "se", "PRON", "PRON", "expl", 3L, "PronType=Prs|Reflex=Yes",
    "docSR", 1L, 3L, "taisent", "taire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docSR", 1L, 4L, ",", ",", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docSR", 1L, 5L, "ce", "ce", "PRON", "PRON", "obj", 7L, "PronType=Dem|Number=Sing",
    "docSR", 1L, 6L, "qui", "qui", "PRON", "PRON", "nsubj", 7L, "PronType=Rel|Number=Sing",
    "docSR", 1L, 7L, "est", "être", "VERB", "VERB", "ccomp", 3L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docSR", 1L, 8L, "surprenant", "surprenant", "ADJ", "ADJ", "xcomp", 7L, "Gender=Masc|Number=Sing",
    "docSR", 1L, 9L, ".", ".", "PUNCT", "PUNCT", "punct", 7L, NA_character_,
    "docBC", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Plur",
    "docBC", 1L, 2L, "partent", "partir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 1L, 3L, "parce", "parce", "SCONJ", "SCONJ", "mark", 6L, NA_character_,
    "docBC", 1L, 4L, "qu'", "que", "SCONJ", "SCONJ", "mark", 6L, NA_character_,
    "docBC", 1L, 5L, "il", "il", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=3|Number=Sing",
    "docBC", 1L, 6L, "pleut", "pleuvoir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docBC", 2L, 8L, "Ils", "ils", "PRON", "PRON", "nsubj", 9L, "PronType=Prs|Person=3|Number=Plur",
    "docBC", 2L, 9L, "restent", "rester", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 2L, 10L, "car", "car", "CCONJ", "CCONJ", "cc", 9L, NA_character_,
    "docBC", 2L, 11L, "il", "il", "PRON", "PRON", "nsubj", 12L, "PronType=Prs|Person=3|Number=Sing",
    "docBC", 2L, 12L, "adore", "adorer", "VERB", "VERB", "conj", 9L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 2L, 13L, "la", "le", "DET", "DET", "det", 14L, "Definite=Def|Gender=Fem|Number=Sing|PronType=Art",
    "docBC", 2L, 14L, "pluie", "pluie", "NOUN", "NOUN", "obj", 12L, "Gender=Fem|Number=Sing",
    "docBC", 2L, 15L, ".", ".", "PUNCT", "PUNCT", "punct", 9L, NA_character_,
    "docCONC", 1L, 1L, "Bien", "bien", "ADV", "ADV", "mark", 4L, "Degree=Pos",
    "docCONC", 1L, 2L, "qu'", "que", "SCONJ", "SCONJ", "mark", 4L, NA_character_,
    "docCONC", 1L, 3L, "il", "il", "PRON", "PRON", "nsubj", 4L, "PronType=Prs|Person=3|Number=Sing",
    "docCONC", 1L, 4L, "soit", "être", "VERB", "VERB", "advcl", 8L, "Mood=Sub|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 1L, 5L, "tard", "tard", "ADV", "ADV", "xcomp", 4L, NA_character_,
    "docCONC", 1L, 6L, ",", ",", "PUNCT", "PUNCT", "punct", 4L, NA_character_,
    "docCONC", 1L, 7L, "ils", "ils", "PRON", "PRON", "nsubj", 8L, "PronType=Prs|Person=3|Number=Plur",
    "docCONC", 1L, 8L, "continuent", "continuer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 1L, 9L, ".", ".", "PUNCT", "PUNCT", "punct", 8L, NA_character_,
    "docCONC", 2L, 10L, "Quoique", "quoique", "SCONJ", "SCONJ", "mark", 12L, NA_character_,
    "docCONC", 2L, 11L, "elle", "elle", "PRON", "PRON", "nsubj", 12L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docCONC", 2L, 12L, "doute", "douter", "VERB", "VERB", "advcl", 15L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 2L, 13L, ",", ",", "PUNCT", "PUNCT", "punct", 12L, NA_character_,
    "docCONC", 2L, 14L, "elle", "elle", "PRON", "PRON", "nsubj", 15L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docCONC", 2L, 15L, "avance", "avancer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 2L, 16L, ".", ".", "PUNCT", "PUNCT", "punct", 15L, NA_character_,
    "docCONC", 3L, 17L, "Même", "même", "ADV", "ADV", "mark", 20L, NA_character_,
    "docCONC", 3L, 18L, "si", "si", "SCONJ", "SCONJ", "mark", 20L, NA_character_,
    "docCONC", 3L, 19L, "tu", "tu", "PRON", "PRON", "nsubj", 20L, "PronType=Prs|Person=2|Number=Sing",
    "docCONC", 3L, 20L, "pars", "partir", "VERB", "VERB", "advcl", 23L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCONC", 3L, 21L, ",", ",", "PUNCT", "PUNCT", "punct", 20L, NA_character_,
    "docCONC", 3L, 22L, "je", "je", "PRON", "PRON", "nsubj", 23L, "PronType=Prs|Person=1|Number=Sing",
    "docCONC", 3L, 23L, "reste", "rester", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docCONC", 3L, 24L, ".", ".", "PUNCT", "PUNCT", "punct", 23L, NA_character_,
    "docCOND", 1L, 1L, "Si", "si", "SCONJ", "SCONJ", "mark", 3L, NA_character_,
    "docCOND", 1L, 2L, "tu", "tu", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=2|Number=Sing",
    "docCOND", 1L, 3L, "viens", "venir", "VERB", "VERB", "advcl", 6L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCOND", 1L, 4L, ",", ",", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docCOND", 1L, 5L, "on", "on", "PRON", "PRON", "nsubj", 6L, "PronType=Ind|Number=Sing",
    "docCOND", 1L, 6L, "partira", "partir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Fut|VerbForm=Fin",
    "docCOND", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_,
    "docCOND", 2L, 8L, "À", "à", "ADP", "ADP", "case", 9L, NA_character_,
    "docCOND", 2L, 9L, "moins", "moins", "ADV", "ADV", "mark", 12L, NA_character_,
    "docCOND", 2L, 10L, "que", "que", "SCONJ", "SCONJ", "mark", 12L, NA_character_,
    "docCOND", 2L, 11L, "tu", "tu", "PRON", "PRON", "nsubj", 12L, "PronType=Prs|Person=2|Number=Sing",
    "docCOND", 2L, 12L, "refuses", "refuser", "VERB", "VERB", "advcl", 15L, "Mood=Sub|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCOND", 2L, 13L, ",", ",", "PUNCT", "PUNCT", "punct", 12L, NA_character_,
    "docCOND", 2L, 14L, "nous", "nous", "PRON", "PRON", "nsubj", 15L, "PronType=Prs|Person=1|Number=Plur",
    "docCOND", 2L, 15L, "irons", "aller", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Fut|VerbForm=Fin",
    "docCOND", 2L, 16L, ".", ".", "PUNCT", "PUNCT", "punct", 15L, NA_character_,
    "docCOND", 3L, 17L, "À", "à", "ADP", "ADP", "case", 18L, NA_character_,
    "docCOND", 3L, 18L, "condition", "condition", "NOUN", "NOUN", "mark", 21L, NA_character_,
    "docCOND", 3L, 19L, "que", "que", "SCONJ", "SCONJ", "mark", 21L, NA_character_,
    "docCOND", 3L, 20L, "tu", "tu", "PRON", "PRON", "nsubj", 21L, "PronType=Prs|Person=2|Number=Sing",
    "docCOND", 3L, 21L, "répondes", "répondre", "VERB", "VERB", "advcl", 24L, "Mood=Sub|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCOND", 3L, 22L, ",", ",", "PUNCT", "PUNCT", "punct", 21L, NA_character_,
    "docCOND", 3L, 23L, "je", "je", "PRON", "PRON", "nsubj", 24L, "PronType=Prs|Person=1|Number=Sing",
    "docCOND", 3L, 24L, "signe", "signer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docCOND", 3L, 25L, ".", ".", "PUNCT", "PUNCT", "punct", 24L, NA_character_,
    "docMISC", 1L, 1L, "Il", "il", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Sing",
    "docMISC", 1L, 2L, "sourit", "sourire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docMISC", 1L, 3L, "lorsque", "lorsque", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docMISC", 1L, 4L, "tu", "tu", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=2|Number=Sing",
    "docMISC", 1L, 5L, "arrives", "arriver", "VERB", "VERB", "advcl", 2L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docMISC", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  sub_tokens$doc_id <- stringr::str_to_lower(sub_tokens$doc_id)
  sub_tokens$token <- stringr::str_to_lower(sub_tokens$token)
  sub_tokens$lemma <- stringr::str_to_lower(sub_tokens$lemma)
  sub_tokens$tag <- stringr::str_to_upper(sub_tokens$tag)

  class(sub_tokens) <- c("spacyr_parsed", class(sub_tokens))

  features <- biber(sub_tokens, measure = "none", normalize = FALSE)

  docsr <- features[features$doc_id == "docsr", , drop = FALSE]
  docbc <- features[features$doc_id == "docbc", , drop = FALSE]
  docconc <- features[features$doc_id == "docconc", , drop = FALSE]
  doccond <- features[features$doc_id == "doccond", , drop = FALSE]
  docmisc <- features[features$doc_id == "docmisc", , drop = FALSE]

  expect_equal(docsr$f_34_sentence_relatives, 1)
  expect_equal(docsr$f_35_because, 0)

  expect_equal(docbc$f_35_because, 2)
  expect_equal(docbc$f_36_though, 0)

  expect_equal(docconc$f_36_though, 3)
  expect_equal(docconc$f_37_if, 1)

  expect_equal(doccond$f_37_if, 3)
  expect_equal(doccond$f_35_because, 0)

  expect_equal(docmisc$f_38_other_adv_sub, 1)
  expect_equal(docmisc$f_35_because, 0)

  # nolint end
})
