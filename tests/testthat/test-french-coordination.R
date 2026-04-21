# nolint start: line_length_linter, object_name_linter
if (!requireNamespace("pseudobibeR.fr", quietly = TRUE)) skip("French-only test: pseudobibeR.fr not installed")


test_that("French complementizer omission and coordination features behave", {
  deletion_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docDEL", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docDEL", 1L, 2L, "pense", "penser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docDEL", 1L, 3L, "il", "il", "PRON", "PRON", "nsubj", 4L, "PronType=Prs|Person=3|Number=Sing",
    "docDEL", 1L, 4L, "vient", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docDEL", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docQUE", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docQUE", 1L, 2L, "pense", "penser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docQUE", 1L, 3L, "qu'", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docQUE", 1L, 4L, "il", "il", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=3|Number=Sing",
    "docQUE", 1L, 5L, "vient", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docQUE", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  coord_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docPHRAS", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docPHRAS", 1L, 2L, "chats", "chat", "NOUN", "NOUN", "nsubj", 5L, "Gender=Masc|Number=Plur",
    "docPHRAS", 1L, 3L, "et", "et", "CCONJ", "CCONJ", "cc", 4L, NA_character_,
    "docPHRAS", 1L, 4L, "chiens", "chien", "NOUN", "NOUN", "conj", 2L, "Gender=Masc|Number=Plur",
    "docPHRAS", 1L, 5L, "jouent", "jouer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docPHRAS", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 5L, NA_character_,
    "docCLAUS", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docCLAUS", 1L, 2L, "chante", "chanter", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docCLAUS", 1L, 3L, "et", "et", "CCONJ", "CCONJ", "cc", 5L, NA_character_,
    "docCLAUS", 1L, 4L, "tu", "tu", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=2|Number=Sing",
    "docCLAUS", 1L, 5L, "danses", "danser", "VERB", "VERB", "conj", 2L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCLAUS", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  all_tokens <- dplyr::bind_rows(deletion_tokens, coord_tokens)

  all_tokens$doc_id <- stringr::str_to_lower(all_tokens$doc_id)
  all_tokens$token <- stringr::str_to_lower(all_tokens$token)
  all_tokens$lemma <- stringr::str_to_lower(all_tokens$lemma)
  all_tokens$tag <- stringr::str_to_upper(all_tokens$tag)

  class(all_tokens) <- c("spacyr_parsed", class(all_tokens))

  features <- biber(all_tokens, measure = "none", normalize = FALSE)

  docdel <- features[features$doc_id == "docdel", , drop = FALSE]
  docque <- features[features$doc_id == "docque", , drop = FALSE]
  docphras <- features[features$doc_id == "docphras", , drop = FALSE]
  docclaus <- features[features$doc_id == "docclaus", , drop = FALSE]

  expect_equal(docdel$f_60_that_deletion, 1)
  expect_equal(docque$f_60_that_deletion, 0)

  expect_equal(docdel$f_61_stranded_preposition, 0)
  expect_equal(docque$f_62_split_infinitive, 0)

  expect_equal(docphras$f_64_phrasal_coordination, 1)
  expect_equal(docclaus$f_65_clausal_coordination, 1)
})


test_that("Stranded prepositions and split infinitives counted", {
  tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docPREP", 1L, 1L, "C'", "ce", "PRON", "PRON", "nsubj", 2L, "PronType=Dem",
    "docPREP", 1L, 2L, "est", "etre", "AUX", "AUX", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docPREP", 1L, 3L, "la", "le", "DET", "DET", "det", 4L, "Definite=Def|Gender=Fem|Number=Sing|PronType=Art",
    "docPREP", 1L, 4L, "personne", "personne", "NOUN", "NOUN", "nsubj", 2L, "Gender=Fem|Number=Sing",
    "docPREP", 1L, 5L, "avec", "avec", "ADP", "ADP", "case", 6L, NA_character_,
    "docPREP", 1L, 6L, "qui", "qui", "PRON", "PRON", "obl", 8L, "PronType=Rel",
    "docPREP", 1L, 7L, "je", "je", "PRON", "PRON", "nsubj", 8L, "PronType=Prs|Person=1|Number=Sing",
    "docPREP", 1L, 8L, "parle", "parler", "VERB", "VERB", "acl:relcl", 4L, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docPREP", 1L, 9L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docSPLIT", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docSPLIT", 1L, 2L, "essaie", "essayer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docSPLIT", 1L, 3L, "de", "de", "ADP", "ADP", "mark", 5L, NA_character_,
    "docSPLIT", 1L, 4L, "vraiment", "vraiment", "ADV", "ADV", "advmod", 5L, NA_character_,
    "docSPLIT", 1L, 5L, "comprendre", "comprendre", "VERB", "VERB", "xcomp", 2L, "VerbForm=Inf",
    "docSPLIT", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  tokens$doc_id <- stringr::str_to_lower(tokens$doc_id)
  tokens$token <- stringr::str_to_lower(tokens$token)
  tokens$lemma <- stringr::str_to_lower(tokens$lemma)
  tokens$tag <- stringr::str_to_upper(tokens$tag)

  class(tokens) <- c("spacyr_parsed", class(tokens))

  features <- biber(tokens, measure = "none", normalize = FALSE)

  docprep <- dplyr::filter(features, .data$doc_id == "docprep")
  docsplit <- dplyr::filter(features, .data$doc_id == "docsplit")

  expect_equal(docprep$f_61_stranded_preposition, 1)
  expect_equal(docsplit$f_62_split_infinitive, 1)
})


test_that("Contractions counted (not elisions)", {
  # Contractions are informal reductions (p'tit, m'sieur)
  # NOT grammatical elisions (c', l', qu', d')
  # Note: Properly tokenized as real parsers would do (apostrophe tokens split)
  tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    # doc1: "Un p'tit garçon" - 1 contraction (p'tit = ADJ)
    "docCON1", 1L, 1L, "Un", "un", "DET", "DET", "det", 3L, "Definite=Ind|Gender=Masc|Number=Sing",
    "docCON1", 1L, 2L, "p'tit", "petit", "ADJ", "ADJ", "amod", 3L, "Gender=Masc|Number=Sing",
    "docCON1", 1L, 3L, "garçon", "garçon", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Sing",
    "docCON1", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    # doc2: "Bonjour m'sieur" - 1 contraction (m'sieur = NOUN or PROPN)
    "docCON2", 1L, 1L, "Bonjour", "bonjour", "INTJ", "INTJ", "root", NA_integer_, NA_character_,
    "docCON2", 1L, 2L, "m'sieur", "monsieur", "NOUN", "NOUN", "vocative", 1L, "Gender=Masc|Number=Sing",
    "docCON2", 1L, 3L, ".", ".", "PUNCT", "PUNCT", "punct", 1L, NA_character_,
    # doc3: "C'est l'ami qu'il aime" - 0 contractions (all apostrophes are grammatical elisions)
    # Properly split: C' (PRON) + est (AUX) + l' (DET) + ami (NOUN) + qu' (PRON) + il (PRON) + aime (VERB)
    "docCON3", 1L, 1L, "C'", "ce", "PRON", "PRON", "nsubj", 2L, "Gender=Masc|Number=Sing",
    "docCON3", 1L, 2L, "est", "être", "AUX", "AUX", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCON3", 1L, 3L, "l'", "le", "DET", "DET", "det", 4L, "Definite=Def|Number=Sing",
    "docCON3", 1L, 4L, "ami", "ami", "NOUN", "NOUN", "nsubj", 2L, "Gender=Masc|Number=Sing",
    "docCON3", 1L, 5L, "qu'", "que", "PRON", "PRON", "obj", 7L, NA_character_,
    "docCON3", 1L, 6L, "il", "il", "PRON", "PRON", "nsubj", 7L, "Gender=Masc|Number=Sing|Person=3",
    "docCON3", 1L, 7L, "aime", "aimer", "VERB", "VERB", "acl:relcl", 4L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCON3", 1L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  tokens$doc_id <- stringr::str_to_lower(tokens$doc_id)
  tokens$token <- stringr::str_to_lower(tokens$token)
  tokens$lemma <- stringr::str_to_lower(tokens$lemma)
  tokens$tag <- stringr::str_to_upper(tokens$tag)

  class(tokens) <- c("spacyr_parsed", class(tokens))

  features <- biber(tokens, measure = "none", normalize = FALSE)

  doc1 <- dplyr::filter(features, .data$doc_id == "doccon1")
  doc2 <- dplyr::filter(features, .data$doc_id == "doccon2")
  doc3 <- dplyr::filter(features, .data$doc_id == "doccon3")

  expect_equal(doc1$f_59_contractions, 1)  # p'tit (ADJ, not filtered)
  expect_equal(doc2$f_59_contractions, 1)  # m'sieur (NOUN, not filtered)
  expect_equal(doc3$f_59_contractions, 0)  # C', l', qu' have POS=PRON/DET (filtered as elisions)
})

# nolint end
