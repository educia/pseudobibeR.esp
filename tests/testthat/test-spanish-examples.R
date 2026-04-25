# test-spanish-examples.R
# Ejemplos etiquetados por rasgo, validados contra biber_es() via UDPipe.
#
# Carga data-raw/spanish_examples.yaml (una fila por ejemplo con feature/example/count),
# parsea cada oración con el modelo UD spanish-gsd, ejecuta biber_es(normalize = FALSE)
# y compara el conteo observado con el esperado.
#
# Algunos rasgos divergen puntualmente entre parsers o requieren contexto discursivo:
# se marcan en `relaxed_features` y solo se reportan como información.
#
# nolint start: line_length_linter, object_name_linter

test_that("UDPipe spanish examples align with expected feature counts", {
  skip_if_not_installed("udpipe")
  skip_if_not_installed("yaml")
  skip_on_cran()

  yaml_path <- testthat::test_path("..", "..", "data-raw", "spanish_examples.yaml")
  if (!file.exists(yaml_path)) skip("spanish_examples.yaml no encontrado")

  examples_raw <- yaml::read_yaml(yaml_path)
  spanish_examples <- tibble::tibble(
    feature = vapply(examples_raw, function(x) x$feature, character(1)),
    example = vapply(examples_raw, function(x) x$example, character(1)),
    count   = vapply(examples_raw, function(x) as.numeric(x$count), numeric(1))
  )

  # Rasgos con divergencias conocidas en UDPipe spanish-gsd o dependientes de
  # contexto discursivo — se reportan como informativos.
  relaxed_features <- c(
    "f_01_past_tense",
    "f_01b_preterit",
    "f_04_place_adverbials",
    "f_05_time_adverbials",
    "f_09_pronoun_it",
    "f_45_conjuncts",
    "f_50_discourse_particles",
    "f_52_modal_possibility",
    "f_53_modal_necessity",
    "f_54_modal_predictive",
    "f_63_split_auxiliary",
    "f_71_preterit"
  )

  # Descargar modelo al cache de tests si no existe
  model_path <- tryCatch(
    {
      cache_dir <- testthat::test_path("fixtures")
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      existing <- list.files(cache_dir, pattern = "spanish-gsd.*\\.udpipe$", full.names = TRUE)
      if (length(existing) > 0) {
        existing[[1]]
      } else {
        dl <- udpipe::udpipe_download_model(language = "spanish-gsd", model_dir = cache_dir)
        dl$file_model
      }
    },
    error = function(e) NULL
  )
  skip_if(is.null(model_path) || !nzchar(model_path) || !file.exists(model_path),
          "UDPipe Spanish (spanish-gsd) no disponible")

  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }

  # Uno por rasgo (el primero) para mantener el test razonablemente rápido
  sample_examples <- spanish_examples |>
    dplyr::group_by(.data$feature) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # doc_id único por fila (feature puede aparecer una vez tras slice_head)
  annotations <- udpipe::udpipe_annotate(
    model,
    x      = sample_examples$example,
    doc_id = sample_examples$feature,
    parser = "default"
  )

  feature_counts <- biber_es(annotations, measure = "none", normalize = FALSE)

  missing_columns <- character()

  lookup_count <- function(feature_id) {
    doc_row <- dplyr::filter(feature_counts, .data$doc_id == feature_id)
    if (nrow(doc_row) != 1L) {
      return(NA_real_)
    }
    if (!feature_id %in% colnames(doc_row)) {
      missing_columns <<- unique(c(missing_columns, feature_id))
      return(0)
    }
    as.numeric(doc_row[[feature_id]])
  }

  comparison <- sample_examples |>
    dplyr::mutate(observed = purrr::map_dbl(.data$feature, lookup_count)) |>
    dplyr::mutate(matches = !is.na(.data$observed) & .data$observed == .data$count)

  strict <- comparison |> dplyr::filter(!(.data$feature %in% relaxed_features))
  mismatches <- strict |> dplyr::filter(!.data$matches)

  ignored_mismatches <- comparison |>
    dplyr::filter(.data$feature %in% relaxed_features) |>
    dplyr::filter(!.data$matches)

  problems <- character()

  if (length(missing_columns) > 0) {
    problems <- c(
      problems,
      paste0("Columnas de rasgo ausentes: ", paste(missing_columns, collapse = ", "))
    )
  }

  if (nrow(mismatches) > 0) {
    details <- mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": esperado ", .data$count,
          ", observado ", .data$observed,
          " [\"", .data$example, "\"]"
        )
      ) |>
      dplyr::pull("detail")
    problems <- c(problems, paste(c("Divergencias:", details), collapse = "\n  "))
  }

  info_text <- if (length(problems) == 0) "" else paste(problems, collapse = "\n  ")
  testthat::expect_true(length(problems) == 0, info = info_text)

  if (nrow(ignored_mismatches) > 0 && requireNamespace("cli", quietly = TRUE)) {
    ignored_details <- ignored_mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": esperado ", .data$count,
          ", observado ", .data$observed
        )
      ) |>
      dplyr::pull("detail")
    cli::cli_inform(paste(
      c("Divergencias UDPipe conocidas (informativas):", ignored_details),
      collapse = "\n  "
    ))
  }
})

# nolint end
