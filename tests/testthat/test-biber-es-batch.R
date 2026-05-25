# Tests para biber_es_batch() y write_biber_xlsx() (Phase 4).

library(testthat)

# Setup: modelo UDPipe (compartido)
model_path <- if (file.exists("spanish-gsd-ud-2.5-191206.udpipe")) {
  "spanish-gsd-ud-2.5-191206.udpipe"
} else if (file.exists("../../spanish-gsd-ud-2.5-191206.udpipe")) {
  "../../spanish-gsd-ud-2.5-191206.udpipe"
} else NA_character_

ud_model <- if (!is.na(model_path)) {
  tryCatch(udpipe::udpipe_load_model(model_path), error = function(e) NULL)
} else NULL

sample_df <- data.frame(
  doc_id = c("d1", "d2"),
  genre  = c("narr", "acad"),
  text   = c("María llegó tarde a la reunión.",
             "El método permite comparar dos modelos."),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Validacion de inputs
# ---------------------------------------------------------------------------

test_that("biber_es_batch falla sin 'model'", {
  expect_error(
    pseudobibeR.es::biber_es_batch(sample_df),
    "obligatorio"
  )
})

test_that("biber_es_batch rechaza inputs invalidos", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  expect_error(
    pseudobibeR.es::biber_es_batch(42, ud_model),
    "path a CSV o un data.frame"
  )
  expect_error(
    pseudobibeR.es::biber_es_batch("ruta_inexistente.csv", ud_model),
    "no encontrado"
  )
})

test_that("biber_es_batch detecta columna de texto faltante", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  bad <- data.frame(doc_id = "d1", contenido = "Hola.")
  expect_error(
    pseudobibeR.es::biber_es_batch(bad, ud_model),
    "no encontrada"
  )
})

test_that("biber_es_batch rechaza doc_ids duplicados", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  dup <- data.frame(doc_id = c("d1","d1"), text = c("a","b"))
  expect_error(
    pseudobibeR.es::biber_es_batch(dup, ud_model, id_column = "doc_id"),
    "duplicados"
  )
})

# ---------------------------------------------------------------------------
# Modo rapido sobre data.frame
# ---------------------------------------------------------------------------

test_that("biber_es_batch (fast, df input) produce N filas x 70+ cols", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  res <- pseudobibeR.es::biber_es_batch(sample_df, ud_model,
                                        id_column = "doc_id",
                                        normalize = FALSE)
  expect_named(res, c("counts"))
  expect_equal(nrow(res$counts), 2L)
  # 67 features + doc_id + n_tokens + n_lex_tokens + metadata (genre) = 71
  expect_true(ncol(res$counts) >= 70L)
  expect_true("genre" %in% colnames(res$counts))
})

test_that("biber_es_batch auto-genera doc_ids cuando id_column=NULL", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  df_no_id <- data.frame(text = c("Hola.", "Adios."), stringsAsFactors = FALSE)
  res <- pseudobibeR.es::biber_es_batch(df_no_id, ud_model, normalize = FALSE)
  expect_equal(res$counts$doc_id, c("doc_0001", "doc_0002"))
})

test_that("biber_es_batch propaga metadata entre doc_id y f_NN", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  res <- pseudobibeR.es::biber_es_batch(sample_df, ud_model,
                                        id_column = "doc_id",
                                        normalize = FALSE)
  cols <- colnames(res$counts)
  expect_equal(cols[1], "doc_id")
  expect_equal(cols[2], "genre")
  # Primer f_NN debe venir despues de la metadata
  first_f <- which(grepl("^f_\\d{2}_", cols))[1]
  expect_gt(first_f, 2L)
})

# ---------------------------------------------------------------------------
# Modo trace
# ---------------------------------------------------------------------------

test_that("biber_es_batch(trace=TRUE) anade evidence con schema E1", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  res <- pseudobibeR.es::biber_es_batch(sample_df, ud_model,
                                        id_column = "doc_id",
                                        trace = TRUE, normalize = FALSE)
  expect_true("evidence" %in% names(res))
  expect_setequal(
    colnames(res$evidence),
    pseudobibeR.es:::.EVIDENCE_COLS
  )
})

# ---------------------------------------------------------------------------
# Modo safe
# ---------------------------------------------------------------------------

test_that("biber_es_batch(safe=TRUE) anade failed_docs aunque este vacio", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  res <- pseudobibeR.es::biber_es_batch(sample_df, ud_model,
                                        id_column = "doc_id",
                                        safe = TRUE, progress = FALSE,
                                        normalize = FALSE)
  expect_true("failed_docs" %in% names(res))
  expect_equal(nrow(res$failed_docs), 0L)
  expect_equal(nrow(res$counts), 2L)
})

# ---------------------------------------------------------------------------
# CSV input
# ---------------------------------------------------------------------------

test_that("biber_es_batch acepta CSV path (round-trip ingest)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(sample_df, tmp, row.names = FALSE, fileEncoding = "UTF-8")

  res <- pseudobibeR.es::biber_es_batch(tmp, ud_model,
                                        id_column = "doc_id",
                                        normalize = FALSE)
  expect_equal(nrow(res$counts), 2L)
  expect_equal(sort(res$counts$doc_id), c("d1", "d2"))
})

# ---------------------------------------------------------------------------
# Consistencia con biber_es / biber_es_traced
# ---------------------------------------------------------------------------

test_that("biber_es_batch$counts (fast) coincide con biber_es directo", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  parsed <- udpipe::udpipe_annotate(ud_model, x = sample_df$text,
                                    doc_id = sample_df$doc_id)
  direct <- pseudobibeR.es::biber_es(parsed, measure = "none", normalize = FALSE)
  res <- pseudobibeR.es::biber_es_batch(sample_df, ud_model,
                                        id_column = "doc_id",
                                        measure = "none", normalize = FALSE)
  # biber_es_batch inserta metadata; comparamos sin esas cols
  batch_counts <- res$counts[, !colnames(res$counts) %in% "genre", drop = FALSE]
  # Ordenar por doc_id para asegurar misma orientacion
  batch_counts <- batch_counts[order(batch_counts$doc_id), ]
  direct      <- direct[order(direct$doc_id), ]
  rownames(batch_counts) <- NULL
  rownames(direct)       <- NULL
  expect_equal(batch_counts, direct, ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------
# write_biber_xlsx
# ---------------------------------------------------------------------------

test_that("write_biber_xlsx genera archivo round-trippable con readxl", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  res <- pseudobibeR.es::biber_es_batch(sample_df, ud_model,
                                        id_column = "doc_id",
                                        trace = TRUE, normalize = FALSE)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  pseudobibeR.es::write_biber_xlsx(res, tmp)

  expect_true(file.exists(tmp))
  sheet_names <- readxl::excel_sheets(tmp)
  expect_true(all(c("raw", "metadata", "evidence") %in% sheet_names))

  raw_back <- readxl::read_xlsx(tmp, sheet = "raw")
  # El re-import preserva orientacion tidy: docs en filas
  expect_equal(nrow(raw_back), 2L)
  expect_true("doc_id" %in% colnames(raw_back))
  expect_true(any(grepl("^f_\\d{2}_", colnames(raw_back))))
})

test_that("write_biber_xlsx rechaza input que no sea output de biber_es_batch", {
  skip_if_not_installed("writexl")
  expect_error(
    pseudobibeR.es::write_biber_xlsx(list(foo = 1), tempfile()),
    "falta \\$counts"
  )
})
