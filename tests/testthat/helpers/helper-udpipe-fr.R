# nolint start: line_length_linter, object_name_linter

udpipe_model_filename <- "french-gsd-ud-2.5-191206.udpipe"

udpipe_candidate_paths <- function() {
  tests_dir <- testthat::test_path()
  package_tests <- normalizePath(file.path(tests_dir, ".."), mustWork = FALSE)
  package_root <- normalizePath(file.path(package_tests, ".."), mustWork = FALSE)
  workspace_root <- normalizePath(file.path(package_root, ".."), mustWork = FALSE)

  unique(c(
    file.path(tests_dir, "..", udpipe_model_filename),
    file.path(tests_dir, "..", "models", udpipe_model_filename),
    file.path(package_tests, udpipe_model_filename),
    file.path(package_tests, "models", udpipe_model_filename),
    file.path(package_root, "tests", udpipe_model_filename),
    file.path(package_root, "tests", "models", udpipe_model_filename),
    file.path(workspace_root, "tests", udpipe_model_filename),
    file.path(workspace_root, "tests", "models", udpipe_model_filename)
  ))
}

get_udpipe_model_path <- function() {
  candidate_paths <- udpipe_candidate_paths()
  existing_models <- candidate_paths[file.exists(candidate_paths)]
  if (length(existing_models) == 0) {
    return(NULL)
  }
  normalizePath(existing_models[[1]], mustWork = TRUE)
}

# nolint end
