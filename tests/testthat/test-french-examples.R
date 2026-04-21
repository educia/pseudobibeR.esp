# nolint start: line_length_linter, object_name_linter
if (!requireNamespace("pseudobibeR.fr", quietly = TRUE)) skip("French-only test: pseudobibeR.fr not installed")


test_that("UDPipe french examples align with expected feature counts", {
  skip_if_not_installed("udpipe")
  skip_on_cran()

  # udpipe sometimes tags these features differently from spaCy, so treat them as informational only.
  relaxed_features <- c(
    "f_01_past_tense",
    "f_04_place_adverbials",
    "f_09_pronoun_it",
    "f_50_discourse_particles",
    "f_52_modal_possibility",
    "f_53_modal_necessity",
    "f_54_modal_predictive",
    "f_63_split_auxiliary"
  )

  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")

  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }

  sample_examples <- french_examples |>
    dplyr::group_by(feature) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  annotations <- udpipe::udpipe_annotate(
    model,
    x = sample_examples$example,
    doc_id = sample_examples$feature,
    parser = "default"
  )

  feature_counts <- biber(annotations, measure = "none", normalize = FALSE)

  missing_columns <- character()

  lookup_count <- function(feature_id) {
    doc_row <- dplyr::filter(feature_counts, doc_id == feature_id)
    expect_equal(nrow(doc_row), 1L, info = paste0("Missing doc row for ", feature_id))
    if (!feature_id %in% colnames(doc_row)) {
      missing_columns <<- unique(c(missing_columns, feature_id))
      return(0)
    }
    as.numeric(doc_row[[feature_id]])
  }

  comparison <- sample_examples |>
    dplyr::mutate(observed = purrr::map_dbl(.data$feature, lookup_count)) |>
    dplyr::mutate(matches = .data$observed == .data$count)

  mismatches <- comparison |>
    dplyr::filter(.data$feature %in% relaxed_features == FALSE) |>
    dplyr::filter(!.data$matches)

  ignored_mismatches <- comparison |>
    dplyr::filter(.data$feature %in% relaxed_features) |>
    dplyr::filter(!.data$matches)

  problems <- character()

  if (length(missing_columns) > 0) {
    problems <- c(
      problems,
      paste0("Missing feature columns: ", paste(missing_columns, collapse = ", "))
    )
  }

  if (nrow(mismatches) > 0) {
    details <- mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": expected ", .data$count,
          ", observed ", .data$observed
        )
      ) |>
      dplyr::pull("detail")
    problems <- c(problems, paste(c("Feature mismatches:", details), collapse = "\n  "))
  }

  message_text <- if (length(problems) == 0) "" else paste(problems, collapse = "\n  ")
  testthat::expect_true(length(problems) == 0, info = message_text)

  if (nrow(ignored_mismatches) > 0) {
    ignored_details <- ignored_mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": expected ", .data$count,
          ", observed ", .data$observed
        )
      ) |>
      dplyr::pull("detail")
    info_text <- paste(
      c(
        "Known udpipe divergences (informational only):",
        ignored_details
      ),
      collapse = "\n  "
    )
    cli::cli_inform(info_text)
  }
})

# nolint end
