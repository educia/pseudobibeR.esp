# test-revision-hernan-phase4.R
#
# Fase 4: diccionarios y control sintáctico. Cubre lo implementado por código
# (sin regenerar dict.rda):
#   - f_11: control sintáctico (solo función pronominal independiente; excluye
#     el uso determinante). Pasa a code-only para que la rama de superficie no
#     anule el filtro.
#
# Nota f_04/f_05: la detección de locuciones multipalabra YA funciona (la rama
# quanteda compacta todas las locuciones del dict vía multiword_patterns, que el
# build construye como la unión de todas las entradas '_' del diccionario). El
# residual es de COBERTURA del dict (p. ej. 'en mitad de' no está) y de
# evidencia trazada, no del mecanismo.
#
# nolint start: line_length_linter, object_name_linter

f11_count <- function(text) as.numeric(run_biber(text)$f_11_indefinite_pronouns)

test_that("f_11 cuenta indefinidos en función pronominal independiente", {
  skip_if_not_installed("udpipe")
  expect_equal(f11_count("Elige cualquiera de los libros."), 1)
  expect_equal(f11_count("No vino nadie a la fiesta."), 1)
  expect_equal(f11_count("Algo falló en el motor."), 1)
  expect_equal(f11_count("Alguien llamó pero nadie respondió nada."), 3)
  expect_equal(f11_count("Lo quiero todo ahora."), 1)   # 'todo' pronominal (obj)
})

test_that("f_11 excluye el uso NO pronominal (adjetival y determinante)", {
  skip_if_not_installed("udpipe")
  expect_equal(f11_count("Es un libro cualquiera."), 0)      # ADJ/amod
  expect_equal(f11_count("Todo el día estuvo lloviendo."), 0) # DET/det ('todo el día')
})

test_that("f_46/f_47: 'casi' cuenta en f_47 (hedges), no en f_46 (downtoners)", {
  skip_if_not_installed("udpipe")
  r <- run_biber("Casi termino el informe.")
  expect_equal(as.numeric(r$f_47_hedges), 1)
  expect_equal(as.numeric(r$f_46_downtoners), 0)
  # 'apenas' permanece en f_46:
  expect_equal(as.numeric(run_biber("Apenas se nota el cambio.")$f_46_downtoners), 1)
})

test_that("f_04/f_05: locuciones añadidas al diccionario se cuentan", {
  skip_if_not_installed("udpipe")
  expect_equal(as.numeric(run_biber("Lo dejó en mitad de la sala.")$f_04_place_adverbials), 1)
  expect_equal(as.numeric(run_biber("El pueblo está más allá del río.")$f_04_place_adverbials), 1)
  expect_equal(as.numeric(run_biber("Hace poco llegaron los datos.")$f_05_time_adverbials), 1)
})

# nolint end
