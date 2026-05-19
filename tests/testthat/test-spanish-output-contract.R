# Regression: contrato de salida de biber_es() para español.
#
# v0.024+: paridad superficial con pseudobibeR.fr — biber_es() expone los
# 67 nombres de rasgo de Biber (1988). Los 10 no-detectables en español
# (f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, f_62) son
# columnas constantes-cero. Los 55 detectables + 2 fusiones (f_29 absorbe
# f_31, f_30 absorbe f_32) cumplen el contrato 1:1 con Biber.
# Bug histórico bloqueado: f_69/f_70/f_71 (extensiones internas) no deben
# aparecer; f_44 no debe doblarse al normalizar.

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

test_that("biber_es expone los 67 nombres de rasgo (paridad superficial FR)", {
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

  # Inventario superficial completo: 67 rasgos
  expect_equal(length(feat_cols), 67L)
  # Ninguna extensión interna f_68+ (excluidas del catalogo Biber)
  expect_length(grep("^f_(68|69|70|71)(_|$)", feat_cols), 0L)
  # f_43 (ratio) y f_44 (longitud media) presentes
  expect_true(all(c("f_43_type_token", "f_44_mean_word_length") %in% feat_cols))
  # Los 10 rasgos no-detectables en espanol deben ser 0
  surface_zero <- c("f_09_pronoun_it", "f_12_proverb_do", "f_15_gerunds",
                    "f_28_present_participle_whiz", "f_31_wh_subj",
                    "f_32_wh_obj", "f_59_contractions", "f_60_that_deletion",
                    "f_61_stranded_preposition", "f_62_split_infinitive")
  for (sf in surface_zero) {
    expect_true(sf %in% feat_cols, info = paste(sf, "ausente"))
    expect_equal(as.numeric(res[[sf]][1]), 0, info = paste(sf, "no es 0"))
  }
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
