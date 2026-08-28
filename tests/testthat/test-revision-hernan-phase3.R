# test-revision-hernan-phase3.R
#
# Fase 3 (REVERTIDA a pedido del usuario): la revisión de Hernán había
# activado f_15_gerunds para contar el infinitivo en función nominal-sujeto
# (csubj). El usuario pidió devolver f_15 a su comportamiento original:
# columna siempre-cero, sin detección. Este archivo documenta el revert.
#
# nolint start: line_length_linter, object_name_linter

feat_p3 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_15 se mantiene siempre en 0 (comportamiento original, revertido)", {
  skip_if_not_installed("udpipe")
  # Casos que en la Fase 3 (antes del revert) disparaban f_15 = 1:
  expect_equal(feat_p3("Fumar perjudica la salud.", "f_15_gerunds"), 0)
  expect_equal(feat_p3("Me gusta nadar en el mar.", "f_15_gerunds"), 0)
  expect_equal(feat_p3("El fumar constante daña la salud.", "f_15_gerunds"), 0)
})

test_that("f_24 vuelve a contar TODOS los infinitivos, incluidos los que antes iban a f_15", {
  skip_if_not_installed("udpipe")
  # 'nadar' (antes csubj -> f_15) ahora cuenta en f_24, como en el diseño original.
  r <- run_biber("Me gusta nadar en el mar.")
  expect_equal(as.numeric(r$f_15_gerunds), 0)
  expect_equal(as.numeric(r$f_24_infinitives), 1)
  # Los casos que ya contaban en f_24 (xcomp) no cambian:
  expect_equal(feat_p3("Quiere estudiar medicina.", "f_24_infinitives"), 1)
  expect_equal(feat_p3("Prohibieron fumar en el bar.", "f_24_infinitives"), 1)
})

# nolint end
