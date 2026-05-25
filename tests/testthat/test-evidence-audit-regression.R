# Tests de regresion especificos para los 4 rasgos cuyo detector fue
# corregido durante el audit (commits 7479f0e..8d285a5):
#
#   f_06_first_person_pronouns -- supplement morfologico con filtro de
#                                 forma/lema conocidos
#   f_23_wh_clause             -- tilde superficial + exclusion de heads
#                                 acl/acl:relcl
#   f_24_infinitives           -- root|Inf solo cuando hay AUX|Fin hijo
#   f_47_hedges                -- locale UTF-8 forzado en biber_es para
#                                 que quanteda matchee acentos
#
# Phase 3 motivacion (M1 design): la invariante count == nrow(evidence)
# valida que el numero esta bien. Estos tests verifican ademas que la
# EVIDENCIA contiene los TOKENS ESPECIFICOS esperados. Detecta el bug
# sutil donde el detector cuenta correctamente pero captura tokens
# equivocados -- regresion que un test de invariante puro no detecta.

library(testthat)

# Locate model (relative or in package root)
model_path <- if (file.exists("spanish-gsd-ud-2.5-191206.udpipe")) {
  "spanish-gsd-ud-2.5-191206.udpipe"
} else if (file.exists("../../spanish-gsd-ud-2.5-191206.udpipe")) {
  "../../spanish-gsd-ud-2.5-191206.udpipe"
} else NA_character_

ud_model <- if (!is.na(model_path)) {
  tryCatch(udpipe::udpipe_load_model(model_path), error = function(e) NULL)
} else NULL

trace_text <- function(text, doc_id = "t1") {
  parsed <- udpipe::udpipe_annotate(ud_model, x = text, doc_id = doc_id)
  pseudobibeR.es::biber_es_traced(parsed, measure = "none", normalize = FALSE)
}

# ---------------------------------------------------------------------------
# f_06_first_person_pronouns -- audit commit bcbf08c
# El bug: el supplement morfologico aceptaba cualquier PRON con Person=1
# sin verificar que la forma/lema fuera un pronombre conocido. Verbos
# conjugados (caigamos, vivamos) con Person=1 contaban como pronombres.
# Fix: filtrar por forma o lema en una lista cerrada de pronombres.
# Regresion target: "Yo creo que nosotros podemos mejorar" debe contar yo
# y nosotros, NO los verbos.
# ---------------------------------------------------------------------------

test_that("f_06: la evidencia contiene yo y nosotros, no los verbos conjugados", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Yo creo que nosotros podemos mejorar.")
  ev_f06 <- result$evidence[result$evidence$feature == "f_06_first_person_pronouns", ]

  expect_gte(nrow(ev_f06), 2L)
  tokens_lower <- tolower(ev_f06$token)
  expect_true("yo"        %in% tokens_lower, info = "yo ausente de evidencia f_06")
  expect_true("nosotros"  %in% tokens_lower, info = "nosotros ausente de evidencia f_06")

  # Verbos conjugados Person=1 no deben aparecer (regresion del bug f_06)
  expect_false("creo"     %in% tokens_lower, info = "creo aparece como falso positivo")
  expect_false("podemos"  %in% tokens_lower, info = "podemos aparece como falso positivo")
})

# ---------------------------------------------------------------------------
# f_23_wh_clause -- audit commit 76bc5c2
# El bug: aceptaba "que" relativo (sin tilde) en cualquier dep_rel, asi
# que "El equipo que fue asignado" contaba "que" como wh-clause cuando
# es relativa. Fix: exigir tilde superficial + excluir heads acl/acl:relcl.
# Regresion target: "No se quien llamo ni cuando llego" tiene quien y
# cuando (ambos acentuados, en clausula subordinada).
# ---------------------------------------------------------------------------

test_that("f_23: la evidencia contiene quien y cuando (acentuados, no relativos)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("No sé quién llamó ni cuándo llegó.")
  ev_f23 <- result$evidence[result$evidence$feature == "f_23_wh_clause", ]

  expect_gte(nrow(ev_f23), 2L)
  tokens_lower <- tolower(ev_f23$token)
  expect_true("quién"  %in% tokens_lower, info = "quien ausente de evidencia f_23")
  expect_true("cuándo" %in% tokens_lower, info = "cuando ausente de evidencia f_23")
})

test_that("f_23: 'que' relativo (sin tilde) NO aparece en la evidencia", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("El equipo que fue asignado al proyecto comenzó ayer.")
  ev_f23 <- result$evidence[result$evidence$feature == "f_23_wh_clause", ]

  expect_equal(nrow(ev_f23), 0L,
               info = "evidencia f_23 contiene tokens cuando deberia estar vacia")
})

# ---------------------------------------------------------------------------
# f_24_infinitives -- audit commit 04e3f4b
# El bug: contaba root|Inf indiscriminadamente. UDPipe mis-etiqueta
# verbos finitos como Inf (e.g., "Quiero terminar..." con Quiero como
# root|Inf). Fix: aceptar root|Inf solo si tiene AUX|Fin hijo (perifrasis).
# Regresion target: "Quiero terminar el trabajo para poder descansar"
# debe contar terminar y descansar, NO Quiero.
# ---------------------------------------------------------------------------

test_that("f_24: terminar y descansar en evidencia; Quiero NO (audit fix 04e3f4b)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Quiero terminar el trabajo para poder descansar.")
  ev_f24 <- result$evidence[result$evidence$feature == "f_24_infinitives", ]

  expect_gte(nrow(ev_f24), 2L)
  tokens_lower <- tolower(ev_f24$token)
  expect_true("terminar"  %in% tokens_lower, info = "terminar ausente de evidencia f_24")
  expect_true("descansar" %in% tokens_lower, info = "descansar ausente de evidencia f_24")
  expect_false("quiero"   %in% tokens_lower,
               info = "quiero (root|Inf misparsed) aparece como falso positivo")
})

test_that("f_24: 'Se debe seguir' SI cuenta seguir (root|Inf con AUX|Fin hijo)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Se debe seguir el procedimiento.")
  ev_f24 <- result$evidence[result$evidence$feature == "f_24_infinitives", ]

  expect_gte(nrow(ev_f24), 1L)
  expect_true("seguir" %in% tolower(ev_f24$token),
              info = "seguir ausente: la rama root|Inf+AUX|Fin no funciona")
})

# ---------------------------------------------------------------------------
# f_47_hedges -- audit commit 8d285a5
# El bug: en locale C, quanteda::tokens borraba la marca Encoding=UTF-8
# y "quizas" quedaba como bytes crudos que no matcheaban el diccionario.
# Fix: forzar LC_CTYPE=UTF-8 en biber_es y biber_es_traced.
# Regresion target: "Quizas el resultado depende" debe contar quizas
# incluso si la sesion R esta en locale C.
#
# NOTA: f_47 es dict-only en v1 -- no produce filas de evidencia. Solo
# verificamos el count (la regresion era del CONTEO, no de la evidencia).
# ---------------------------------------------------------------------------

test_that("f_47: quizas se detecta correctamente (locale UTF-8 forzado)", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Quizás el resultado depende de otros factores.")
  expect_gte(result$counts$f_47_hedges[[1]], 1L,
             label = "f_47 count en 'Quizas el resultado depende'")
})

test_that("f_47: tambien funciona con multiples hedges acentuados", {
  skip_if(is.null(ud_model), "modelo UDPipe no disponible")
  skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
              "biber_es_traced() requerida")

  result <- trace_text("Quizás y tal vez los resultados son válidos.")
  expect_gte(result$counts$f_47_hedges[[1]], 1L)
})
