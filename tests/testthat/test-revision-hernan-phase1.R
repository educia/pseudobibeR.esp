# test-revision-hernan-phase1.R
#
# Fase 1 de la revisión de Hernán (INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md):
# correcciones de cobertura de f_01, f_06-f_08 y f_42. Los conteos esperados
# están verificados empíricamente contra spanish-gsd; las etiquetas UD que los
# sustentan se aseveran en test-udpipe-tag-verification.R.
#
# Las oraciones evitan colocar el verbo/forma objetivo en posición inicial
# (spanish-gsd mis-etiqueta palabras capitalizadas al inicio de oración).
#
# nolint start: line_length_linter, object_name_linter

feat_count <- function(text, feature) {
  as.numeric(run_biber(text)[[feature]])
}

test_that("f_01 cuenta pasados de indicativo y subjuntivo (perfecto simple + imperfecto)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_count("Ayer corrí hasta la esquina.", "f_01_past_tense"), 1)          # perfecto simple
  expect_equal(feat_count("Cuando era joven caminaba mucho.", "f_01_past_tense"), 2)       # imperfecto ind. (era, caminaba)
  expect_equal(feat_count("Yo quería que corriera más rápido.", "f_01_past_tense"), 2)     # + imperfecto subj. (corriera)
  expect_equal(feat_count("Ya había corrido dos kilómetros.", "f_01_past_tense"), 1)       # aux 'había' del compuesto
  expect_equal(feat_count("Ella corre todos los días.", "f_01_past_tense"), 0)             # presente: no cuenta
})

test_that("f_06/f_07/f_08 incluyen posesivos ruteados por persona", {
  skip_if_not_installed("udpipe")
  r1 <- run_biber("Nuestro equipo revisó mi propuesta.")
  expect_equal(as.numeric(r1$f_06_first_person_pronouns), 2)   # nuestro + mi (Poss=Yes, Person=1)
  expect_equal(as.numeric(r1$f_07_second_person_pronouns), 0)
  expect_equal(as.numeric(r1$f_08_third_person_pronouns), 0)

  r2 <- run_biber("Usted debe entregar su informe.")
  expect_equal(as.numeric(r2$f_07_second_person_pronouns), 1)  # usted
  expect_equal(as.numeric(r2$f_08_third_person_pronouns), 1)   # su -> Person=3 (criterio morfológico)

  # Posesivos tónicos sustantivados (NOUN, sin Poss=Yes) capturados por lema.
  r3 <- run_biber("Los resultados míos superan los tuyos.")
  expect_equal(as.numeric(r3$f_06_first_person_pronouns), 1)   # míos
  expect_equal(as.numeric(r3$f_07_second_person_pronouns), 1)  # tuyos

  # Voseo: 'vos' se mis-etiqueta NOUN; se rescata por token.
  expect_equal(feat_count("Vos tenés razón siempre.", "f_07_second_person_pronouns"), 1)
})

test_that("LIMITACIÓN documentada: 'se' argumental no es separable de la pasiva refleja", {
  skip_if_not_installed("udpipe")
  # 'Ella se vio' -> cuenta solo 'ella' (=1). El 'se' reflexivo argumental NO se
  # cuenta porque es morfológicamente idéntico a la pasiva refleja / impersonal
  # (ver test-udpipe-tag-verification.R). El spec pedía 2; se documenta la
  # desviación para no inflar f_08 en corpus académicos llenos de 'se' impersonal.
  expect_equal(feat_count("Ella se vio en el espejo.", "f_08_third_person_pronouns"), 1)
  # Pasiva refleja: 'se' correctamente NO contado.
  r <- run_biber("Se publicaron los informes.")
  expect_equal(as.numeric(r$f_08_third_person_pronouns), 0)
  expect_equal(as.numeric(r$f_06_first_person_pronouns), 0)
})

test_that("f_42 cuenta TODOS los adverbios, con solapamientos deliberados", {
  skip_if_not_installed("udpipe")
  # 'no' (también f_67), 'rápidamente', 'hoy' (también f_05): los tres en f_42.
  expect_equal(feat_count("No corre rápidamente hoy.", "f_42_adverbs"), 3)
})

# nolint end
