# nolint start: line_length_linter, object_name_linter

test_that("french edge cases stay stable", {
  skip_on_cran()
  skip_if_not_installed("yaml")
  skip_if_not_installed("udpipe")

  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")

  yaml_path <- testthat::test_path("..", "..", "data-raw", "french_edge_cases.yaml")
  skip_if(!file.exists(yaml_path), "french_edge_cases.yaml missing")

  edge_cases <- yaml::read_yaml(yaml_path)$edge_cases
  skip_if(length(edge_cases) == 0, "No edge cases declared")

  edge_df <- purrr::imap_dfr(edge_cases, function(sentences, category) {
    tibble::tibble(
      category = category,
      text = unname(unlist(sentences)),
      doc_id = sprintf("%s_%02d", category, seq_along(sentences))
    )
  })

  expect_gt(nrow(edge_df), 0)

  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }

  annotations <- udpipe::udpipe_annotate(
    model,
    x = edge_df$text,
    doc_id = edge_df$doc_id,
    parser = "default"
  )

  features <- withCallingHandlers(
    biber(annotations, measure = "none", normalize = FALSE),
    warning = function(w) {
      message_text <- conditionMessage(w)
      if (grepl("many-to-many relationship", message_text, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  feature_cols <- grep("^f_", names(features), value = TRUE)

  long <- tidyr::pivot_longer(
    features,
    cols = dplyr::all_of(feature_cols),
    names_to = "feature",
    values_to = "count"
  )
  long <- dplyr::filter(long, count > 0)

  lookup <- edge_df[c("doc_id", "category")]
  actual <- dplyr::left_join(long, lookup, by = "doc_id")
  actual <- actual[, c("category", "doc_id", "feature", "count")]
  actual <- actual[order(actual$category, actual$doc_id, actual$feature), ]
  actual <- as.data.frame(actual)
  rownames(actual) <- NULL

  expected_path <- testthat::test_path("fixtures", "french_edge_case_features.csv")
  expect_true(file.exists(expected_path), info = "Fixture missing; run data-raw/generate_edge_case_fixture.R")
  expected <- utils::read.csv(expected_path, stringsAsFactors = FALSE)
  expected <- expected[order(expected$category, expected$doc_id, expected$feature), ]
  rownames(expected) <- NULL

  testthat::expect_equal(actual, expected, tolerance = 1e-8)
})

# nolint end
