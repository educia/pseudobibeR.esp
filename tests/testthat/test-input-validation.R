test_that("biber rejects NULL input", {
  expect_error(
    biber(NULL),
    "no applicable method"
  )
})

test_that("biber.spacyr_parsed rejects empty data frame", {
  empty_df <- data.frame()
  class(empty_df) <- c("spacyr_parsed", "data.frame")
  
  expect_error(
    biber(empty_df),
    "'tokens' is empty \\(0 rows\\)"
  )
})

test_that("biber.spacyr_parsed rejects missing dep_rel column", {
  bad_df <- data.frame(
    doc_id = "1",
    token = "test",
    lemma = "test",
    pos = "NOUN",
    tag = "NN"
  )
  class(bad_df) <- c("spacyr_parsed", "data.frame")
  
  expect_error(
    biber(bad_df),
    "Column 'dep_rel' not found.*dependency = TRUE"
  )
})

test_that("biber.spacyr_parsed rejects missing tag column", {
  bad_df <- data.frame(
    doc_id = "1",
    token = "test",
    lemma = "test",
    pos = "NOUN",
    dep_rel = "nsubj"
  )
  class(bad_df) <- c("spacyr_parsed", "data.frame")
  
  expect_error(
    biber(bad_df),
    "Column 'tag' not found.*tag = TRUE"
  )
})

test_that("biber.spacyr_parsed rejects missing pos column", {
  bad_df <- data.frame(
    doc_id = "1",
    token = "test",
    lemma = "test",
    tag = "NN",
    dep_rel = "nsubj"
  )
  class(bad_df) <- c("spacyr_parsed", "data.frame")
  
  expect_error(
    biber(bad_df),
    "Column 'pos' not found.*pos = TRUE"
  )
})

test_that("biber.spacyr_parsed rejects wrong input type", {
  expect_error(
    biber.spacyr_parsed("not a data frame"),
    "'tokens' must be a data frame"
  )
})

test_that("biber handles single token document", {
  # Create minimal valid spacyr_parsed data matching actual structure
  single_token <- data.frame(
    doc_id = "doc1",
    sentence_id = 1L,
    token_id = 1L,
    token = "bonjour",
    lemma = "bonjour",
    pos = "INTJ",
    tag = "INTJ",
    head_token_id = 0,
    dep_rel = "root",
    morph = NA_character_,
    stringsAsFactors = FALSE
  )
  class(single_token) <- c("spacyr_parsed", "data.frame")
  
  result <- biber(single_token, normalize = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$doc_id, "doc1")
})

test_that("biber handles document with only punctuation", {
  # Create punctuation-only document
  punct_only <- data.frame(
    doc_id = "doc1",
    sentence_id = c(1L, 1L, 1L),
    token_id = 1:3,
    token = c(".", "!", "?"),
    lemma = c(".", "!", "?"),
    pos = c("PUNCT", "PUNCT", "PUNCT"),
    tag = c("PUNCT", "PUNCT", "PUNCT"),
    head_token_id = c(0, 0, 0),
    dep_rel = c("punct", "punct", "punct"),
    morph = NA_character_,
    stringsAsFactors = FALSE
  )
  class(punct_only) <- c("spacyr_parsed", "data.frame")
  
  result <- biber(punct_only, normalize = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("biber handles multiple documents with varying lengths", {
  # Create multiple documents with different lengths
  varying_docs <- data.frame(
    doc_id = c(rep("doc1", 2), rep("doc2", 5), "doc3"),
    sentence_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
    token_id = c(1:2, 1:5, 1L),
    token = c("je", "parle", "tu", "parles", "très", "bien", "français", "bonjour"),
    lemma = c("je", "parler", "tu", "parler", "très", "bien", "français", "bonjour"),
    pos = c("PRON", "VERB", "PRON", "VERB", "ADV", "ADV", "NOUN", "INTJ"),
    tag = c("PRON", "VERB", "PRON", "VERB", "ADV", "ADV", "NOUN", "INTJ"),
    head_token_id = c(2, 0, 2, 0, 4, 4, 4, 0),
    dep_rel = c("nsubj", "root", "nsubj", "root", "advmod", "advmod", "obj", "root"),
    morph = NA_character_,
    stringsAsFactors = FALSE
  )
  class(varying_docs) <- c("spacyr_parsed", "data.frame")
  
  result <- biber(varying_docs, normalize = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$doc_id, c("doc1", "doc2", "doc3"))
})
