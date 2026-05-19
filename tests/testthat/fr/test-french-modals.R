# nolint start: line_length_linter, object_name_linter
if (!requireNamespace("pseudobibeR.fr", quietly = TRUE)) skip("French-only test: pseudobibeR.fr not installed")


test_that("modal detection covers lemma and periphrastic forms", {
  modal_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docPWR", 1L, 1L, "il", "il", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Sing",
    "docPWR", 1L, 2L, "peut", "pouvoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docPWR", 1L, 3L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,

    "docPOS", 1L, 1L, "nous", "nous", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Plur",
    "docPOS", 1L, 2L, "avons", "avoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docPOS", 1L, 3L, "la", "le", "DET", "DET", "det", 4L, "Definite=Def|Gender=Fem|Number=Sing|PronType=Art",
    "docPOS", 1L, 4L, "possibilité", "possibilité", "NOUN", "NOUN", "obj", 2L, "Gender=Fem|Number=Sing",
    "docPOS", 1L, 5L, "de", "de", "ADP", "ADP", "mark", 6L, NA_character_,
    "docPOS", 1L, 6L, "partir", "partir", "VERB", "VERB", "xcomp", 2L, "VerbForm=Inf",
    "docPOS", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,

    "docRIS", 1L, 1L, "il", "il", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Sing",
    "docRIS", 1L, 2L, "risque", "risquer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docRIS", 1L, 3L, "de", "de", "ADP", "ADP", "mark", 4L, NA_character_,
    "docRIS", 1L, 4L, "partir", "partir", "VERB", "VERB", "xcomp", 2L, "VerbForm=Inf",
    "docRIS", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,

    "docNEC", 1L, 1L, "ils", "ils", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Plur",
    "docNEC", 1L, 2L, "sont", "être", "AUX", "AUX", "cop", 3L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docNEC", 1L, 3L, "obligés", "obliger", "ADJ", "ADJ", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docNEC", 1L, 4L, "de", "de", "ADP", "ADP", "mark", 5L, NA_character_,
    "docNEC", 1L, 5L, "venir", "venir", "VERB", "VERB", "xcomp", 3L, "VerbForm=Inf",
    "docNEC", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,

    "docSUS", 1L, 1L, "elle", "elle", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docSUS", 1L, 2L, "est", "être", "AUX", "AUX", "cop", 3L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docSUS", 1L, 3L, "susceptible", "susceptible", "ADJ", "ADJ", "root", NA_integer_, "Gender=Fem|Number=Sing",
    "docSUS", 1L, 4L, "de", "de", "ADP", "ADP", "mark", 5L, NA_character_,
    "docSUS", 1L, 5L, "gagner", "gagner", "VERB", "VERB", "xcomp", 3L, "VerbForm=Inf",
    "docSUS", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,

    "docNEG", 1L, 1L, "ils", "ils", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Plur",
    "docNEG", 1L, 2L, "ne", "ne", "PART", "PART", "advmod", 3L, NA_character_,
    "docNEG", 1L, 3L, "peuvent", "pouvoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docNEG", 1L, 4L, "plus", "plus", "ADV", "ADV", "advmod", 3L, NA_character_,
    "docNEG", 1L, 5L, "s'", "se", "PRON", "PRON", "obj", 6L, "Reflex=Yes|Person=3|Number=Plur",
    "docNEG", 1L, 6L, "en", "en", "PRON", "PRON", "obj", 3L, "PronType=Prs|Person=3|Number=Sing",
    "docNEG", 1L, 7L, "passer", "passer", "VERB", "VERB", "xcomp", 3L, "VerbForm=Inf",
    "docNEG", 1L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,

    "docFUT", 1L, 1L, "ce", "ce", "PRON", "PRON", "nsubj", 2L, "PronType=Dem|Number=Sing",
    "docFUT", 1L, 2L, "sera", "être", "AUX", "AUX", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Fut|VerbForm=Fin",
    "docFUT", 1L, 3L, "simple", "simple", "ADJ", "ADJ", "xcomp", 2L, NA_character_,
    "docFUT", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  modal_tokens$doc_id <- stringr::str_to_lower(modal_tokens$doc_id)
  modal_tokens$token <- stringr::str_to_lower(modal_tokens$token)
  modal_tokens$lemma <- stringr::str_to_lower(modal_tokens$lemma)
  modal_tokens$tag <- stringr::str_to_upper(modal_tokens$tag)

  class(modal_tokens) <- c("spacyr_parsed", class(modal_tokens))

  features <- biber(modal_tokens, measure = "none", normalize = FALSE)

  get_doc <- function(id) {
    features[features$doc_id == stringr::str_to_lower(id), , drop = FALSE]
  }

  expect_equal(get_doc("docPWR")$f_52_modal_possibility, 1)
  expect_equal(get_doc("docPWR")$f_54_modal_predictive, 0)

  expect_equal(get_doc("docPOS")$f_52_modal_possibility, 1)

  # risquer de + inf is predictive, not possibility
  expect_equal(get_doc("docRIS")$f_52_modal_possibility, 0)
  expect_equal(get_doc("docRIS")$f_54_modal_predictive, 1)

  expect_equal(get_doc("docNEC")$f_53_modal_necessity, 1)

  expect_equal(get_doc("docSUS")$f_54_modal_predictive, 1)

  expect_equal(get_doc("docNEG")$f_52_modal_possibility, 1)

  expect_equal(get_doc("docFUT")$f_54_modal_predictive, 1)
})

# nolint end
