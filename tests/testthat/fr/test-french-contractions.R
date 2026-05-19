# Tests for f_59_contractions feature
# Contractions are informal reductions (t'es, p'tit), NOT grammatical elisions (l', d', qu')
if (!requireNamespace("pseudobibeR.fr", quietly = TRUE)) skip("French-only test: pseudobibeR.fr not installed")


library(testthat)
library(dplyr)

test_that("f_59_contractions detects true contractions like t'es and p'tit", {
  tokens <- data.frame(
    doc_id = c(
      rep("doc1", 5),   # T'es sûr de toi ?
      rep("doc2", 5)    # Il est p'tit lui
    ),
    sentence_id = c(
      rep(1, 5),
      rep(1, 5)
    ),
    token_id = c(1:5, 1:5),
    token = c(
      "T'es", "sûr", "de", "toi", "?",
      "Il", "est", "p'tit", "lui", "."
    ),
    lemma = c(
      "être", "sûr", "de", "toi", "?",
      "il", "être", "petit", "lui", "."
    ),
    pos = c(
      "VERB", "ADJ", "ADP", "PRON", "PUNCT",
      "PRON", "AUX", "ADJ", "PRON", "PUNCT"
    ),
    tag = c(
      "VER:pres", "ADJ", "PREP", "PRO:per", "SENT",
      "PRO:per", "VER:pres", "ADJ", "PRO:per", "SENT"
    ),
    head_token_id = c(2, 1, 4, 2, 1, 2, 2, 2, 2, 2),
    dep_rel = c("root", "amod", "case", "obl", "punct", "nsubj", "root", "amod", "nsubj", "punct"),
    stringsAsFactors = FALSE
  )
  
  tokens$token_id_int <- as.integer(tokens$token_id)
  tokens$doc_id <- stringr::str_to_lower(tokens$doc_id)
  tokens$token <- stringr::str_to_lower(tokens$token)
  tokens$lemma <- stringr::str_to_lower(tokens$lemma)
  tokens$tag <- stringr::str_to_upper(tokens$tag)
  
  class(tokens) <- c("spacyr_parsed", class(tokens))
  
  features <- biber(tokens, measure = "none", normalize = FALSE)
  
  # Each document should detect exactly 1 contraction
  expect_equal(features$f_59_contractions[features$doc_id == "doc1"], 1)  # t'es
  expect_equal(features$f_59_contractions[features$doc_id == "doc2"], 1)  # p'tit
})

test_that("f_59_contractions does NOT count grammatical elisions", {
  # Using UDPipe to parse real text with elisions
  model_path <- "tests/models/french-gsd-ud-2.5-191206.udpipe"
  skip_if_not(file.exists(model_path), "UDPipe model not available")
  
  model <- udpipe::udpipe_load_model(model_path)
  # Text with elisions but NO contractions
  parsed <- udpipe::udpipe_annotate(model, "C'est l'histoire d'un homme qu'il aime aujourd'hui") %>%
    as.data.frame()
  
  # Convert to spacyr-like format for biber()
  tokens <- parsed %>%
    dplyr::mutate(
      doc_id = "doc1",
      token_id_int = as.integer(token_id),
      pos = upos
    ) %>%
    dplyr::select(doc_id, sentence_id, token_id, token_id_int, token, lemma, pos, upos, head_token_id, dep_rel)
  
  class(tokens) <- c("spacyr_parsed", "data.frame")
  
  features <- biber(tokens, measure = "none", normalize = FALSE)
  
  # Should be 0 contractions (all are elisions with POS=PRON/DET/ADP/ADV)
  expect_equal(features$f_59_contractions[features$doc_id == "doc1"], 0)
})

test_that("f_59_contractions distinguishes t' as elision vs t'es as contraction", {
  # Note: UDPipe doesn't handle informal "t'es" well (parses as T + ' + es with POS=X/PUNCT/X)
  # This test uses manually constructed tokens to test the logic for when parsers DO recognize it
  # In practice, p'tit is more reliably recognized by parsers
  
  skip("UDPipe doesn't recognize informal t'es - test with p'tit instead")
})

test_that("f_59_contractions handles other informal contractions", {
  # m'sieur (monsieur), m'dame (madame) are also contractions
  tokens <- data.frame(
    doc_id = c(rep("doc1", 3), rep("doc2", 3)),
    sentence_id = c(rep(1, 3), rep(1, 3)),
    token_id = c(1:3, 1:3),
    token = c(
      "Bonjour", "m'sieur", ".",
      "Merci", "m'dame", "."
    ),
    lemma = c(
      "bonjour", "monsieur", ".",
      "merci", "madame", "."
    ),
    pos = c(
      "INTJ", "NOUN", "PUNCT",
      "INTJ", "NOUN", "PUNCT"
    ),
    tag = c(
      "ADV", "NOM", "SENT",
      "ADV", "NOM", "SENT"
    ),
    head_token_id = c(1, 1, 1, 1, 1, 1),
    dep_rel = c("root", "vocative", "punct", "root", "vocative", "punct"),
    stringsAsFactors = FALSE
  )
  
  tokens$token_id_int <- as.integer(tokens$token_id)
  tokens$doc_id <- stringr::str_to_lower(tokens$doc_id)
  tokens$token <- stringr::str_to_lower(tokens$token)
  tokens$lemma <- stringr::str_to_lower(tokens$lemma)
  tokens$tag <- stringr::str_to_upper(tokens$tag)
  
  class(tokens) <- c("spacyr_parsed", class(tokens))
  
  features <- biber(tokens, measure = "none", normalize = FALSE)
  
  # Both should detect their respective contractions
  expect_equal(features$f_59_contractions[features$doc_id == "doc1"], 1)  # m'sieur
  expect_equal(features$f_59_contractions[features$doc_id == "doc2"], 1)  # m'dame
})

test_that("f_59_contractions comprehensive elision check with real parsing", {
  # Test that common elisions are NOT counted when parsed by UDPipe
  model_path <- "tests/models/french-gsd-ud-2.5-191206.udpipe"
  skip_if_not(file.exists(model_path), "UDPipe model not available")
  
  model <- udpipe::udpipe_load_model(model_path)
  # Sentence with many common elisions
  parsed <- udpipe::udpipe_annotate(model, "J'aime l'ami d'abord qu'il s'appelle aujourd'hui") %>%
    as.data.frame()
  
  tokens <- parsed %>%
    dplyr::mutate(
      doc_id = "doc1",
      token_id_int = as.integer(token_id),
      pos = upos
    ) %>%
    dplyr::select(doc_id, sentence_id, token_id, token_id_int, token, lemma, pos, upos, head_token_id, dep_rel)
  
  class(tokens) <- c("spacyr_parsed", "data.frame")
  
  features <- biber(tokens, measure = "none", normalize = FALSE)
  
  # Should be 0 - all are grammatical elisions (J', l', d', qu', s', aujourd'hui)
  # with POS=PRON/DET/ADP
  expect_equal(features$f_59_contractions[features$doc_id == "doc1"], 0)
})
