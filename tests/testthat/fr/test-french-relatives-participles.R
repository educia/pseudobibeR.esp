# nolint start: line_length_linter, object_name_linter
if (!requireNamespace("pseudobibeR.fr", quietly = TRUE)) skip("French-only test: pseudobibeR.fr not installed")


test_that("French infinitives and participles counted", {
  clause_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docN", 1L, 1L, "Elle", "elle", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docN", 1L, 2L, "veut", "vouloir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docN", 1L, 3L, "partir", "partir", "VERB", "VERB", "xcomp", 2L, "VerbForm=Inf",
    "docN", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docO", 1L, 1L, "Parlant", "parler", "VERB", "VERB", "advcl", 4L, "VerbForm=Part|Tense=Pres|Gender=Masc|Number=Sing",
    "docO", 1L, 2L, ",", ",", "PUNCT", "PUNCT", "punct", 1L, NA_character_,
    "docO", 1L, 3L, "il", "il", "PRON", "PRON", "nsubj", 4L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docO", 1L, 4L, "écoute", "écouter", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docO", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 4L, NA_character_,
    "docP", 1L, 1L, "Né", "naître", "VERB", "VERB", "advcl", 6L, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "docP", 1L, 2L, "en", "en", "ADP", "ADP", "case", 3L, NA_character_,
    "docP", 1L, 3L, "1990", "1990", "NUM", "NUM", "obl", 1L, "NumType=Card",
    "docP", 1L, 4L, ",", ",", "PUNCT", "PUNCT", "punct", 1L, NA_character_,
    "docP", 1L, 5L, "il", "il", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docP", 1L, 6L, "vit", "vivre", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docP", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_,
    "docQ", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docQ", 1L, 2L, "documents", "document", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docQ", 1L, 3L, "publiés", "publier", "VERB", "VERB", "acl", 2L, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur",
    "docQ", 1L, 4L, "hier", "hier", "ADV", "ADV", "advmod", 3L, NA_character_,
    "docQ", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docR", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docR", 1L, 2L, "étudiants", "étudiant", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docR", 1L, 3L, "travaillant", "travailler", "VERB", "VERB", "acl", 2L, "VerbForm=Part|Tense=Pres|Gender=Masc|Number=Plur",
    "docR", 1L, 4L, "tard", "tard", "ADV", "ADV", "advmod", 3L, NA_character_,
    "docR", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  clause_tokens$doc_id <- stringr::str_to_lower(clause_tokens$doc_id)
  clause_tokens$token <- stringr::str_to_lower(clause_tokens$token)
  clause_tokens$lemma <- stringr::str_to_lower(clause_tokens$lemma)
  clause_tokens$tag <- stringr::str_to_upper(clause_tokens$tag)

  class(clause_tokens) <- c("spacyr_parsed", class(clause_tokens))

  features <- biber(clause_tokens, measure = "none", normalize = FALSE)

  docN <- features[features$doc_id == "docn", , drop = FALSE]
  docO <- features[features$doc_id == "doco", , drop = FALSE]
  docP <- features[features$doc_id == "docp", , drop = FALSE]
  docQ <- features[features$doc_id == "docq", , drop = FALSE]
  docR <- features[features$doc_id == "docr", , drop = FALSE]

  expect_equal(docN$f_24_infinitives, 1)
  expect_equal(docN$f_25_present_participle, 0)
  expect_equal(docN$f_26_past_participle, 0)

  expect_equal(docO$f_24_infinitives, 0)
  expect_equal(docO$f_25_present_participle, 1)
  expect_equal(docO$f_26_past_participle, 0)

  expect_equal(docP$f_24_infinitives, 0)
  expect_equal(docP$f_25_present_participle, 0)
  expect_equal(docP$f_26_past_participle, 1)

  expect_equal(docQ$f_27_past_participle_whiz, 1)
  expect_equal(docQ$f_28_present_participle_whiz, 0)

  expect_equal(docR$f_27_past_participle_whiz, 0)
  expect_equal(docR$f_28_present_participle_whiz, 1)
})


test_that("French relative clauses counted", {
  relative_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docS", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docS", 1L, 2L, "etudiants", "etudiant", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docS", 1L, 3L, "qui", "qui", "PRON", "PRON", "nsubj", 4L, "PronType=Rel|Number=Plur",
    "docS", 1L, 4L, "arrivent", "arriver", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docS", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docT", 1L, 1L, "le", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Sing|PronType=Art",
    "docT", 1L, 2L, "livre", "livre", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Sing",
    "docT", 1L, 3L, "que", "que", "PRON", "PRON", "obj", 5L, "PronType=Rel",
    "docT", 1L, 4L, "nous", "nous", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=1|Number=Plur",
    "docT", 1L, 5L, "lisons", "lire", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docT", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docU", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docU", 1L, 2L, "projets", "projet", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docU", 1L, 3L, "lesquels", "lequel", "PRON", "PRON", "nsubj", 4L, "PronType=Rel|Number=Plur",
    "docU", 1L, 4L, "avancent", "avancer", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docU", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docV", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docV", 1L, 2L, "tests", "test", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docV", 1L, 3L, "lesquels", "lequel", "PRON", "PRON", "obj", 5L, "PronType=Rel|Number=Plur",
    "docV", 1L, 4L, "nous", "nous", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=1|Number=Plur",
    "docV", 1L, 5L, "choisissons", "choisir", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docV", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docW", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docW", 1L, 2L, "salles", "salle", "NOUN", "NOUN", "root", NA_integer_, "Gender=Fem|Number=Plur",
    "docW", 1L, 3L, "dans", "dans", "ADP", "ADP", "case", 4L, NA_character_,
    "docW", 1L, 4L, "lesquelles", "lequel", "PRON", "PRON", "obl", 6L, "PronType=Rel|Number=Plur",
    "docW", 1L, 5L, "nous", "nous", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=1|Number=Plur",
    "docW", 1L, 6L, "travaillons", "travailler", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docW", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  relative_tokens$doc_id <- stringr::str_to_lower(relative_tokens$doc_id)
  relative_tokens$token <- stringr::str_to_lower(relative_tokens$token)
  relative_tokens$lemma <- stringr::str_to_lower(relative_tokens$lemma)
  relative_tokens$tag <- stringr::str_to_upper(relative_tokens$tag)

  class(relative_tokens) <- c("spacyr_parsed", class(relative_tokens))

  features <- biber(relative_tokens, measure = "none", normalize = FALSE)

  docS <- features[features$doc_id == "docs", , drop = FALSE]
  docT <- features[features$doc_id == "doct", , drop = FALSE]
  docU <- features[features$doc_id == "docu", , drop = FALSE]
  docV <- features[features$doc_id == "docv", , drop = FALSE]
  docW <- features[features$doc_id == "docw", , drop = FALSE]

  expect_equal(docS$f_29_that_subj, 1)
  expect_equal(docS$f_31_wh_subj, 0)

  expect_equal(docT$f_30_that_obj, 1)
  expect_equal(docT$f_32_wh_obj, 0)

  expect_equal(docU$f_29_that_subj, 0)
  expect_equal(docU$f_31_wh_subj, 1)

  expect_equal(docV$f_30_that_obj, 0)
  expect_equal(docV$f_32_wh_obj, 1)

  expect_equal(docW$f_33_pied_piping, 1)
  expect_equal(docW$f_32_wh_obj, 0)
})

  # nolint end
