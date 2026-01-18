# nolint start: line_length_linter, object_name_linter

test_that("synthetic French tokens cover f01-f08", {
  synthetic_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "doc1", 1L, 1L, "Nous", "nous", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=1|Number=Plur",
    "doc1", 1L, 2L, "avons", "avoir", "AUX", "AUX", "aux", 3L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "doc1", 1L, 3L, "fini", "finir", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "doc1", 1L, 4L, "loin", "loin", "ADV", "ADV", "advmod", 3L, NA_character_,
    "doc1", 2L, 5L, "Autrefois", "autrefois", "ADV", "ADV", "advmod", 7L, NA_character_,
    "doc1", 2L, 6L, "nous", "nous", "PRON", "PRON", "nsubj", 7L, "PronType=Prs|Person=1|Number=Plur",
    "doc1", 2L, 7L, "mangions", "manger", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Imp|VerbForm=Fin",
    "doc1", 2L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 7L, NA_character_,
    "doc2", 1L, 1L, "Vous", "vous", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=2|Number=Plur",
    "doc2", 1L, 2L, "allez", "aller", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin",
    "doc2", 1L, 3L, "demain", "demain", "ADV", "ADV", "advmod", 2L, NA_character_,
    "doc2", 1L, 4L, "et", "et", "CCONJ", "CCONJ", "cc", 2L, NA_character_,
    "doc2", 1L, 5L, "elle", "elle", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "doc2", 1L, 6L, "parle", "parler", "VERB", "VERB", "conj", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "doc2", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_
  )

  synthetic_tokens$token <- stringr::str_to_lower(synthetic_tokens$token)
  synthetic_tokens$lemma <- stringr::str_to_lower(synthetic_tokens$lemma)
  synthetic_tokens$tag <- stringr::str_to_upper(synthetic_tokens$tag)

  class(synthetic_tokens) <- c("spacyr_parsed", class(synthetic_tokens))

  features <- biber(synthetic_tokens, measure = "none", normalize = FALSE)

  doc1 <- features[features$doc_id == "doc1", , drop = FALSE]
  doc2 <- features[features$doc_id == "doc2", , drop = FALSE]

  expect_equal(doc1$f_01_past_tense, 1)
  expect_equal(doc1$f_02_perfect_aspect, 1)
  expect_equal(doc1$f_03_present_tense, 0)
  expect_equal(doc1$f_04_place_adverbials, 1)
  expect_equal(doc1$f_05_time_adverbials, 1)
  expect_equal(doc1$f_06_first_person_pronouns, 2)
  expect_equal(doc1$f_07_second_person_pronouns, 0)
  expect_equal(doc1$f_08_third_person_pronouns, 0)

  expect_equal(doc2$f_01_past_tense, 0)
  expect_equal(doc2$f_02_perfect_aspect, 0)
  expect_equal(doc2$f_03_present_tense, 2)
  expect_equal(doc2$f_04_place_adverbials, 0)
  expect_equal(doc2$f_05_time_adverbials, 1)
  expect_equal(doc2$f_06_first_person_pronouns, 0)
  expect_equal(doc2$f_07_second_person_pronouns, 1)
  expect_equal(doc2$f_08_third_person_pronouns, 1)
})


test_that("spaCy morph column fills feats when absent", {
  morph_only_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~morph,
    "docA", 1L, 1L, "Nous", "nous", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=1|Number=Plur",
    "docA", 1L, 2L, "avons", "avoir", "AUX", "AUX", "aux", 3L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docA", 1L, 3L, "fini", "finir", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "docA", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docA", 2L, 5L, "Nous", "nous", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=1|Number=Plur",
    "docA", 2L, 6L, "travaillons", "travailler", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docA", 2L, 7L, "encore", "encore", "ADV", "ADV", "advmod", 6L, NA_character_,
    "docA", 2L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_,
    "docB", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Plur",
    "docB", 1L, 2L, "chantaient", "chanter", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Imp|VerbForm=Fin",
    "docB", 1L, 3L, "hier", "hier", "ADV", "ADV", "advmod", 2L, NA_character_,
    "docB", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  morph_only_tokens$doc_id <- stringr::str_to_lower(morph_only_tokens$doc_id)
  morph_only_tokens$token <- stringr::str_to_lower(morph_only_tokens$token)
  morph_only_tokens$lemma <- stringr::str_to_lower(morph_only_tokens$lemma)
  morph_only_tokens$tag <- stringr::str_to_upper(morph_only_tokens$tag)

  class(morph_only_tokens) <- c("spacyr_parsed", class(morph_only_tokens))

  features <- biber(morph_only_tokens, measure = "none", normalize = FALSE)

  docA <- features[features$doc_id == "doca", , drop = FALSE]
  docB <- features[features$doc_id == "docb", , drop = FALSE]

  expect_equal(docA$f_01_past_tense, 0)
  expect_equal(docA$f_02_perfect_aspect, 1)
  expect_equal(docA$f_03_present_tense, 1)

  expect_equal(docB$f_01_past_tense, 1)
  expect_equal(docB$f_02_perfect_aspect, 0)
  expect_equal(docB$f_03_present_tense, 0)
})


test_that("demonstrative pronouns require morph dem tag", {
  demo_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docC", 1L, 1L, "Cela", "cela", "PRON", "PRON", "nsubj", 2L, "PronType=Dem|Person=3|Number=Sing",
    "docC", 1L, 2L, "fonctionne", "fonctionner", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docC", 1L, 3L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docD", 1L, 1L, "Nous", "nous", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Plur",
    "docD", 1L, 2L, "avons", "avoir", "AUX", "AUX", "aux", 3L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docD", 1L, 3L, "fini", "finir", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "docD", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_
  )

  demo_tokens$doc_id <- stringr::str_to_lower(demo_tokens$doc_id)
  demo_tokens$token <- stringr::str_to_lower(demo_tokens$token)
  demo_tokens$lemma <- stringr::str_to_lower(demo_tokens$lemma)
  demo_tokens$tag <- stringr::str_to_upper(demo_tokens$tag)

  class(demo_tokens) <- c("spacyr_parsed", class(demo_tokens))

  features <- biber(demo_tokens, measure = "none", normalize = FALSE)

  docC <- features[features$doc_id == "docc", , drop = FALSE]
  docD <- features[features$doc_id == "docd", , drop = FALSE]

  expect_equal(docC$f_10_demonstrative_pronoun, 1)
  expect_equal(docC$f_06_first_person_pronouns, 0)

  expect_equal(docD$f_10_demonstrative_pronoun, 0)
  expect_equal(docD$f_06_first_person_pronouns, 1)
})


test_that("French impersonal, existential, and nominal features counted", {
  tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docIMP", 1L, 1L, "Il", "il", "PRON", "PRON", "expl", 2L, "PronType=Prs|Person=3|Number=Sing",
    "docIMP", 1L, 2L, "pleut", "pleuvoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docIMP", 1L, 3L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docIMP", 2L, 1L, "Il", "il", "PRON", "PRON", "expl", 3L, "PronType=Prs|Person=3|Number=Sing",
    "docIMP", 2L, 2L, "y", "y", "PRON", "PRON", "obj", 3L, "PronType=Prs",
    "docIMP", 2L, 3L, "a", "avoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docIMP", 2L, 4L, "trois", "trois", "NUM", "NUM", "nummod", 5L, "NumType=Card",
    "docIMP", 2L, 5L, "ans", "an", "NOUN", "NOUN", "obl", 3L, "Gender=Masc|Number=Plur",
    "docIMP", 2L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docNOM", 1L, 1L, "La", "le", "DET", "DET", "det", 2L, "Definite=Def|Gender=Fem|Number=Sing|PronType=Art",
    "docNOM", 1L, 2L, "création", "création", "NOUN", "NOUN", "nsubj", 3L, "Gender=Fem|Number=Sing",
    "docNOM", 1L, 3L, "progresse", "progresser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docNOM", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docNOM", 2L, 1L, "En", "en", "ADP", "ADP", "mark", 2L, NA_character_,
    "docNOM", 2L, 2L, "travaillant", "travailler", "VERB", "VERB", "advcl", 5L, "VerbForm=Ger|Tense=Pres",
    "docNOM", 2L, 3L, ",", ",", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docNOM", 2L, 4L, "il", "il", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=3|Number=Sing",
    "docNOM", 2L, 5L, "progresse", "progresser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docNOM", 2L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 5L, NA_character_
  )

  tokens$doc_id <- stringr::str_to_lower(tokens$doc_id)
  tokens$token <- stringr::str_to_lower(tokens$token)
  tokens$lemma <- stringr::str_to_lower(tokens$lemma)
  tokens$tag <- stringr::str_to_upper(tokens$tag)

  class(tokens) <- c("spacyr_parsed", class(tokens))

  features <- biber(tokens, measure = "none", normalize = FALSE)

  docimp <- features[features$doc_id == "docimp", , drop = FALSE]
  docnom <- features[features$doc_id == "docnom", , drop = FALSE]

  expect_equal(docimp$f_09_pronoun_it, 2)
  expect_equal(docimp$f_12_proverb_do, 0)
  expect_equal(docimp$f_20_existential_there, 1)

  expect_equal(docnom$f_14_nominalizations, 1)
  expect_equal(docnom$f_15_gerunds, 1)
})

  # nolint end
