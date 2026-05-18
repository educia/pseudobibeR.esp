# Regression: contrato de salida de biber_es() para español.
#
# Bug histórico: f_69/f_70/f_71 (extensiones internas) se filtraban al
# output pese a que pseudobibeR.es es una adaptación 1:1 de los 67 rasgos
# de Biber (57 tras eliminar intraducibles + fusiones). Además f_44 y los
# *_rate sufrían doble normalización (longitud media 9 -> 714).

spanish_udpipe_model_or_skip <- function() {
  skip_if_not_installed("udpipe")
  roots <- unique(c(
    testthat::test_path("..", ".."),
    testthat::test_path("..", "..", ".."),
    "."
  ))
  cand <- file.path(roots, "spanish-gsd-ud-2.5-191206.udpipe")
  hit  <- cand[file.exists(cand)]
  if (length(hit) == 0) skip("modelo UDPipe spanish-gsd no disponible")
  udpipe::udpipe_load_model(normalizePath(hit[[1]]))
}

test_that("biber_es solo expone los 57 rasgos de Biber (sin f_69/f_70/f_71)", {
  skip_if_not_installed("udpipe")
  ud <- spanish_udpipe_model_or_skip()

  parsed <- udpipe::udpipe_annotate(
    ud,
    x = paste(
      "La producción de conocimiento requiere una evaluación rigurosa.",
      "Esos métodos fueron analizados por el comité de expertos."
    )
  ) |> as.data.frame()

  res <- biber_es(parsed, measure = "MATTR", normalize = TRUE)
  feat_cols <- grep("^f_", names(res), value = TRUE)

  # Exactamente 57 rasgos de Biber
  expect_equal(length(feat_cols), 57L)
  # Ninguna extensión interna
  expect_length(grep("^f_(69|70|71)(_|$)", feat_cols), 0L)
  # f_43 (ratio) y f_44 (longitud media) presentes
  expect_true(all(c("f_43_type_token", "f_44_mean_word_length") %in% feat_cols))
})

test_that("f_43/f_44 no se doblan al normalizar; conteos en rango sano", {
  skip_if_not_installed("udpipe")
  ud <- spanish_udpipe_model_or_skip()

  parsed <- udpipe::udpipe_annotate(
    ud,
    x = "La producción de conocimiento requiere una evaluación rigurosa y detallada."
  ) |> as.data.frame()

  rF <- biber_es(parsed, normalize = FALSE)
  rT <- biber_es(parsed, normalize = TRUE)

  # Métricas derivadas idénticas con y sin normalización
  expect_equal(rF$f_43_type_token,       rT$f_43_type_token,       tolerance = 1e-6)
  expect_equal(rF$f_44_mean_word_length, rT$f_44_mean_word_length, tolerance = 1e-6)

  # Longitud media de palabra en rango plausible (3-15 caracteres)
  expect_gt(rT$f_44_mean_word_length, 3)
  expect_lt(rT$f_44_mean_word_length, 15)

  # Ninguna tasa por 1000 supera un techo razonable (no hay doble normalización)
  feat_cols <- setdiff(grep("^f_", names(rT), value = TRUE),
                       c("f_43_type_token", "f_44_mean_word_length"))
  expect_lt(max(as.numeric(rT[1, feat_cols]), na.rm = TRUE), 1000)
})
