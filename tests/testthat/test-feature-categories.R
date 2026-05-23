# Tests del lookup interno .feature_categories.
#
# Garantiza que el mapeo rasgo->categoria definido en R/feature_categories.R
# se mantiene completo y consistente frente al output real de biber_es().
# Cualquier nuevo rasgo anadido a la salida que olvide su entrada en
# .feature_categories rompe este test.

library(testthat)

# El objeto vive en el namespace interno del paquete.
cats          <- pseudobibeR.es:::.feature_categories
zero_out      <- pseudobibeR.es:::.zero_output_features
metric_feats  <- pseudobibeR.es:::.metric_features
detectable    <- pseudobibeR.es:::.detectable_features
evidence_pool <- pseudobibeR.es:::.evidence_features

test_that(".feature_categories cubre los 67 rasgos esperados", {
  expect_length(cats, 67L)
  expect_true(all(grepl("^f_\\d{2}_", names(cats))))
  expect_false(any(duplicated(names(cats))))
})

test_that(".feature_categories tiene exactamente 16 categorias (A-P)", {
  uniq <- sort(unique(unname(cats)))
  expect_length(uniq, 16L)
  expect_true(all(grepl("^[A-P]\\. ", uniq)))
})

test_that(".feature_categories cubre cada columna f_NN_ del output de biber_es()", {
  skip_if_not_installed("udpipe")
  skip_if_not(file.exists("../../spanish-gsd-ud-2.5-191206.udpipe") ||
              file.exists("spanish-gsd-ud-2.5-191206.udpipe"),
              "modelo UDPipe no disponible para este test")

  model_path <- if (file.exists("spanish-gsd-ud-2.5-191206.udpipe")) {
    "spanish-gsd-ud-2.5-191206.udpipe"
  } else {
    "../../spanish-gsd-ud-2.5-191206.udpipe"
  }
  ud_model <- udpipe::udpipe_load_model(model_path)
  parsed   <- udpipe::udpipe_annotate(ud_model, x = "El gato come pescado.", doc_id = "d1")
  out      <- pseudobibeR.es::biber_es(parsed, measure = "none", normalize = FALSE)

  feat_cols <- grep("^f_\\d{2}_", colnames(out), value = TRUE)
  missing   <- setdiff(feat_cols, names(cats))
  extra     <- setdiff(names(cats), feat_cols)

  expect_equal(missing, character(0),
               info = "rasgos en biber_es() sin categoria asignada")
  expect_equal(extra,   character(0),
               info = "categorias asignadas a rasgos que no existen en biber_es()")
})

test_that(".zero_output_features tiene los 10 esperados y todos estan en .feature_categories", {
  expect_length(zero_out, 10L)
  expect_true(all(zero_out %in% names(cats)))
})

test_that(".metric_features cubre f_43 y f_44", {
  expect_setequal(metric_feats, c("f_43_type_token", "f_44_mean_word_length"))
  expect_true(all(metric_feats %in% names(cats)))
})

test_that(".detectable_features = 67 - 10 zero-output = 57", {
  expect_length(detectable, 57L)
  expect_true(all(detectable %in% names(cats)))
  expect_equal(length(intersect(detectable, zero_out)), 0L)
})

test_that(".evidence_features = detectable - metricas = 55", {
  expect_length(evidence_pool, 55L)
  expect_equal(length(intersect(evidence_pool, metric_feats)), 0L)
  expect_equal(length(intersect(evidence_pool, zero_out)),     0L)
})

# Topologia del path de conteo (M1: invariante relajada para dual-path)

dual_path  <- pseudobibeR.es:::.dual_path_features
dict_only  <- pseudobibeR.es:::.dict_only_features
strict_ev  <- pseudobibeR.es:::.strict_evidence_features

test_that(".dual_path_features, .dict_only_features y .strict_evidence_features son disjuntos", {
  expect_equal(length(intersect(dual_path, dict_only)), 0L)
  expect_equal(length(intersect(dual_path, strict_ev)), 0L)
  expect_equal(length(intersect(dict_only, strict_ev)), 0L)
})

test_that("la union de los tres = .evidence_features (sin huecos ni solapes)", {
  union_all <- sort(unique(c(dual_path, dict_only, strict_ev)))
  expect_setequal(union_all, evidence_pool)
})

test_that(".dual_path_features tiene los 12 esperados y todos estan en .feature_categories", {
  expect_length(dual_path, 12L)
  expect_true(all(dual_path %in% names(cats)))
})

test_that(".dict_only_features tiene los 6 esperados (categoria K)", {
  expect_length(dict_only, 6L)
  expect_true(all(dict_only %in% names(cats)))
  expect_true(all(grepl("^K\\.", cats[dict_only])))
})

test_that(".strict_evidence_features = 55 - 12 - 6 = 37", {
  expect_length(strict_ev, 37L)
})
