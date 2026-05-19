if (!requireNamespace("pseudobibeR.fr", quietly = TRUE)) skip("French-only test: pseudobibeR.fr not installed")

test_that("udpipe connector handles missing xpos", {
  skip_if_not_installed("udpipe")

  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "French UD model not available")

  model <- udpipe::udpipe_load_model(model_path)
  on.exit(rm(model), add = TRUE)

  annotation <- udpipe::udpipe_annotate(
    model,
    x = "Nous avons fini le travail.",
    doc_id = "ud_doc"
  )

  features <- biber(annotation, measure = "none", normalize = FALSE)

  doc <- features[features$doc_id == "ud_doc", , drop = FALSE]

  expect_equal(doc$f_01_past_tense, 0)
  expect_equal(doc$f_02_perfect_aspect, 1)
  expect_equal(doc$f_03_present_tense, 0)
  expect_equal(doc$f_06_first_person_pronouns, 1)
})
