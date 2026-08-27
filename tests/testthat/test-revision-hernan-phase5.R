# test-revision-hernan-phase5.R
#
# Fase 5: refinamientos de regla. Conteos verificados contra spanish-gsd.
# Las reglas bloqueadas por el modelo (f_18 obl:agent) NO se fuerzan y se
# documentan en test-udpipe-tag-verification.R.
#
# nolint start: line_length_linter, object_name_linter

p5 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_17: la pasiva refleja cuenta; la impersonal con se se excluye", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Se publicaron los informes.", "f_17_agentless_passives"), 1)   # refleja (nsubj)
  expect_equal(p5("Se venden casas baratas.", "f_17_agentless_passives"), 1)       # refleja
  expect_equal(p5("Se entrevistó a los candidatos.", "f_17_agentless_passives"), 0) # impersonal (obj con 'a')
  expect_equal(p5("Se recomienda leer el manual.", "f_17_agentless_passives"), 0)   # impersonal (xcomp)
  # La pasiva perifrástica no se ve afectada:
  expect_equal(p5("La tarea fue realizada.", "f_17_agentless_passives"), 1)
})

test_that("f_52: 'puede que' cuenta como posibilidad (correlato de may/might)", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Puede que llueva mañana.", "f_52_modal_possibility"), 1)
  # poder + infinitivo sigue contando:
  expect_equal(p5("Puede hacerlo fácilmente.", "f_52_modal_possibility"), 1)
})

test_that("f_64: cubre la coordinación de verbos (SV compartido), sin doblar f_65", {
  skip_if_not_installed("udpipe")
  rv <- run_biber("Lee y escribe muy bien.")
  expect_equal(as.numeric(rv$f_64_phrasal_coordination), 1)   # verbos, sujeto compartido
  expect_equal(as.numeric(rv$f_65_clausal_coordination), 0)
  expect_equal(p5("Juan y María vinieron temprano.", "f_64_phrasal_coordination"), 1) # nombres
  # Coordinación clausal (sujetos propios) -> f_65, no f_64:
  rc <- run_biber("Ella canta y él baila.")
  expect_equal(as.numeric(rc$f_65_clausal_coordination), 1)
  expect_equal(as.numeric(rc$f_64_phrasal_coordination), 0)
})

test_that("f_67: 'no' de foco sobre constituyentes no verbales cuenta; excluye sustantivo", {
  skip_if_not_installed("udpipe")
  expect_equal(p5("Una respuesta no definitiva.", "f_67_neg_analytic"), 1)  # foco sobre ADJ
  expect_equal(p5("No muy lejos de aquí.", "f_67_neg_analytic"), 1)         # foco sobre ADV
  expect_equal(p5("No corre rápido hoy.", "f_67_neg_analytic"), 1)          # preverbal (control)
  expect_equal(p5("El no del comité fue claro.", "f_67_neg_analytic"), 0)   # 'no' sustantivo
})

# nolint end
