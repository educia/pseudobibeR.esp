# test-revision-hernan-phase2.R
#
# Fase 2: reasignación de relativas. La des-fusión de f_31/f_32 (quien/el cual)
# respecto de f_29/f_30 (que) se implementó y luego se REVIRTIÓ (v2, a pedido
# explícito del usuario): quien/el cual vuelven a absorberse en f_29/f_30;
# f_31/f_32 vuelven a ser columnas siempre-cero. Conteos verificados contra
# spanish-gsd.
#
# LÍMITE del modelo (documentado, ver test-udpipe-tag-verification.R): en
# español pro-drop, sin sujeto explícito el relativo objeto se etiqueta nsubj,
# así que los objetos-relativa caen en f_29 en vez de f_30. f_30 solo dispara
# con sujeto explícito en la relativa.
#
# nolint start: line_length_linter, object_name_linter

feat_p2 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_29: relativa de 'que' en función de sujeto", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("El libro que está en la mesa es mío.", "f_29_that_subj"), 1)
})

test_that("f_30: relativa de 'que' objeto (requiere sujeto explícito en la relativa)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("El libro que María escribió es famoso.", "f_30_that_obj"), 1)
})

test_that("f_31/f_32 (REVERTIDO): quien/el cual se absorben en f_29/f_30, siempre 0", {
  skip_if_not_installed("udpipe")
  r1 <- run_biber("La autora, quien presentó el proyecto, respondió.")
  expect_equal(as.numeric(r1$f_31_wh_subj), 0)
  expect_equal(as.numeric(r1$f_29_that_subj), 1)  # absorbido

  r2 <- run_biber("El informe, el cual revisamos ayer, ya se publicó.")
  expect_equal(as.numeric(r2$f_31_wh_subj), 0)
  expect_equal(as.numeric(r2$f_32_wh_obj), 0)
  expect_equal(as.numeric(r2$f_29_that_subj), 1)  # 'cual' tagueado nsubj -> absorbido en f_29
})

test_that("f_33: pied-piping (preposición + relativo) — incluye 'a/con/por quien|cual'", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("El método por el cual se analizaron los datos falló.", "f_33_pied_piping"), 1)
  expect_equal(feat_p2("La persona con quien Juan habló se fue.", "f_33_pied_piping"), 1)
})

test_that("f_34: relativa oracional (lo que / lo cual con antecedente proposicional)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p2("No entregó el informe, lo que retrasó el proceso.", "f_34_sentence_relatives"), 1)
  # Relativa libre sin antecedente proposicional: NO es f_34.
  expect_equal(feat_p2("No entiendo lo que dices.", "f_34_sentence_relatives"), 0)
})

test_that("LÍMITE pro-drop: objeto-relativa sin sujeto explícito cae en f_29", {
  skip_if_not_installed("udpipe")
  # 'que leí' (yo pro-dropped) -> imputado a f_29, no f_30.
  r1 <- run_biber("La novela que leí era larga.")
  expect_equal(as.numeric(r1$f_30_that_obj), 0)
  expect_equal(as.numeric(r1$f_29_that_subj), 1)
})

# nolint end
