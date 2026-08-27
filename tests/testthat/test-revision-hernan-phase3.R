# test-revision-hernan-phase3.R
#
# Fase 3: f_15 (infinitivos en función nominal) y frontera con f_24.
# f_15_gerunds mantiene el nombre de columna pero pasa a contar infinitivos
# nominales. spanish-gsd solo marca de forma fiable el infinitivo SUJETO como
# csubj; el resto de usos nominales no son recuperables (documentado en
# test-udpipe-tag-verification.R y en los comentarios de features_subordination.R).
#
# nolint start: line_length_linter, object_name_linter

feat_p3 <- function(text, feature) as.numeric(run_biber(text)[[feature]])

test_that("f_15 cuenta el infinitivo en función nominal-sujeto (csubj)", {
  skip_if_not_installed("udpipe")
  expect_equal(feat_p3("Fumar perjudica la salud.", "f_15_gerunds"), 1)
  expect_equal(feat_p3("Me gusta nadar en el mar.", "f_15_gerunds"), 1)
})

test_that("f_24 cuenta el resto de infinitivos (complemento verbal, perífrasis, final)", {
  skip_if_not_installed("udpipe")
  r1 <- run_biber("Quiere estudiar medicina.")
  expect_equal(as.numeric(r1$f_24_infinitives), 1)   # xcomp
  expect_equal(as.numeric(r1$f_15_gerunds), 0)
  expect_equal(feat_p3("Prohibieron fumar en el bar.", "f_24_infinitives"), 1)  # xcomp
})

test_that("f_15 y f_24 son mutuamente excluyentes (ningún token en ambos)", {
  skip_if_not_installed("udpipe")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")))
  parsed <- as.data.frame(udpipe::udpipe_annotate(.fc_ud(), x = "Me gusta nadar en el mar."))
  ev <- pseudobibeR.es::biber_es_traced(parsed, measure = "none", normalize = FALSE)$evidence
  t15 <- ev$token_id[ev$feature == "f_15_gerunds"]
  t24 <- ev$token_id[ev$feature == "f_24_infinitives"]
  expect_gte(length(t15), 1)              # 'nadar' en f_15
  expect_length(intersect(t15, t24), 0)   # y no en f_24
})

test_that("LÍMITE documentado: infinitivo con determinante ('el fumar') no se detecta", {
  skip_if_not_installed("udpipe")
  # spanish-gsd re-etiqueta el infinitivo con determinante como NOUN (pierde
  # VerbForm=Inf); f_15 no puede captarlo. No se fuerza.
  expect_equal(feat_p3("El fumar constante daña la salud.", "f_15_gerunds"), 0)
})

# nolint end
