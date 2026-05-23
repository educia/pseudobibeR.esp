# Test transversal de cobertura de evidencia (Phase 0 - red de seguridad
# pre-migracion).
#
# Para cada uno de los 55 rasgos con deteccion real y no-metricos
# (.evidence_features), verifica la invariante fundamental del schema E3:
#
#   count_en_counts == nrow(filter(evidence, feature == X))
#
# Esto blinda contra el modo de falla descrito en el design: que el detector
# cuente correctamente pero la rama de evidencia regrese vacia (evidencia
# silenciosamente perdida).
#
# Estado actual: TODOS los tests estan skipped via skip_evidence(). Cada
# rasgo se activa quitando el skip cuando su bloque correspondiente se
# migra al contrato list(count, evidence) en Phase 2.
#
# Al final de la migracion, este archivo debe correr con 55 tests activos
# y 0 skips para que la migracion se considere completa.

library(testthat)

# ---------------------------------------------------------------------------
# Casos: una oracion minima por rasgo que activa count >= min_count.
# Ejemplos extraidos de data-raw/spanish_examples.yaml donde existen; el
# resto son sentencias construidas a proposito para activar el rasgo.
# ---------------------------------------------------------------------------

cases <- list(
  # A. Tiempo y aspecto
  f_01_past_tense              = list(text = "María llegó tarde a la reunión.",                              min_count = 1),
  f_02_perfect_aspect          = list(text = "He terminado el informe esta mañana.",                              min_count = 1),
  f_03_present_tense           = list(text = "El profesor explica la lección con claridad.",                       min_count = 1),
  # B. Adverbiales
  f_04_place_adverbials        = list(text = "El gato está aquí, encima de la mesa.",                              min_count = 1),
  f_05_time_adverbials         = list(text = "Ayer llegaron tarde, pero hoy están puntuales.",                     min_count = 1),
  # C. Pronombres (solo los detectables; los zero-output no entran)
  f_06_first_person_pronouns   = list(text = "Yo creo que nosotros podemos mejorar.",                              min_count = 1),
  f_07_second_person_pronouns  = list(text = "Tú debes presentar el informe mañana.",                              min_count = 1),
  f_08_third_person_pronouns   = list(text = "Él llegó primero y ella lo saludó.",                                 min_count = 1),
  f_10_demonstrative_pronoun   = list(text = "Esto es importante, pero aquello también lo es.",                    min_count = 1),
  f_11_indefinite_pronouns     = list(text = "Alguien dejó algo sobre la mesa.",                                   min_count = 1),
  # D. Interrogativas
  f_13_wh_question             = list(text = "¿Quién llamó? ¿Qué hora es?",                                          min_count = 1),
  # E. Formas nominales
  f_14_nominalizations         = list(text = "La organización del trabajo mejora la productividad.",               min_count = 1),
  f_16_other_nouns             = list(text = "El gato come pescado en la cocina.",                                 min_count = 1),
  # F. Pasivas
  f_17_agentless_passives      = list(text = "La propuesta fue aprobada sin debate.",                              min_count = 1),
  f_18_by_passives             = list(text = "La novela fue escrita por García Márquez.",                          min_count = 1),
  # G. Formas estativas
  f_19_be_main_verb            = list(text = "El libro es interesante.",                                           min_count = 1),
  f_20_existential_there       = list(text = "Hay tres errores en el documento.",                                  min_count = 1),
  # H. Subordinacion
  f_21_that_verb_comp          = list(text = "Creo que el proyecto tendrá éxito.",                                 min_count = 1),
  f_22_that_adj_comp           = list(text = "Es importante que todos participen.",                                min_count = 1),
  f_23_wh_clause               = list(text = "No sé quién llamó ni cuándo llegó.",                                 min_count = 1),
  f_24_infinitives             = list(text = "Quiero terminar el trabajo para poder descansar.",                   min_count = 1),
  f_25_present_participle      = list(text = "Caminando por el parque, encontré a mi vecino.",                     min_count = 1),
  f_26_past_participle         = list(text = "Terminado el examen, los alumnos salieron.",                         min_count = 1),
  f_27_past_participle_whiz    = list(text = "El informe redactado ayer es muy completo.",                         min_count = 1),
  f_29_that_subj               = list(text = "El estudiante que llegó tarde no pudo entrar.",                      min_count = 1),
  f_30_that_obj                = list(text = "La persona con quien hablé es muy amable.",                          min_count = 1),
  f_33_pied_piping             = list(text = "El asunto del que hablamos requiere atención.",                      min_count = 1),
  f_34_sentence_relatives      = list(text = "El resultado fue inesperado, lo que sorprendió a todos.",            min_count = 1),
  f_35_because                 = list(text = "No pudo asistir porque estaba enfermo.",                             min_count = 1),
  f_36_though                  = list(text = "Aunque llovía, salimos a caminar.",                                  min_count = 1),
  f_37_if                      = list(text = "Puedes hacerlo si quieres.",                                         min_count = 1),
  f_38_other_adv_sub           = list(text = "Cuando llegues, avísame.",                                           min_count = 1),
  # I. Sintagmas prep., adj. y adv.
  f_39_prepositions            = list(text = "El libro está sobre la mesa de la biblioteca.",                      min_count = 1),
  f_40_adj_attr                = list(text = "Un brillante estudiante presentó una nueva solución.",               min_count = 1),
  f_41_adj_pred                = list(text = "El resultado es positivo.",                                          min_count = 1),
  f_42_adverbs                 = list(text = "Habló rápidamente y respondió claramente.",                          min_count = 1),
  # K. Clases lexicas
  f_45_conjuncts               = list(text = "Sin embargo, continuaron con el proyecto.",                          min_count = 1),
  f_46_downtoners              = list(text = "El error es casi imperceptible.",                                    min_count = 1),
  f_47_hedges                  = list(text = "Quizás el resultado depende de otros factores.",                     min_count = 1),
  f_48_amplifiers              = list(text = "El rendimiento mejoró enormemente con el nuevo algoritmo.",          min_count = 1),
  f_49_emphatics               = list(text = "De hecho, los resultados confirman la hipótesis.",                   min_count = 1),
  f_50_discourse_particles     = list(text = "Bueno, pues entonces, sigamos adelante.",                            min_count = 1),
  f_51_demonstratives          = list(text = "Este libro y aquella revista son útiles.",                           min_count = 1),
  # L. Modales
  f_52_modal_possibility       = list(text = "Podemos mejorar los resultados con más datos.",                      min_count = 1),
  f_53_modal_necessity         = list(text = "Debemos revisar los datos antes de publicar.",                       min_count = 1),
  f_54_modal_predictive        = list(text = "El equipo presentará los resultados el viernes.",                    min_count = 1),
  # M. Verbos especializados
  f_55_verb_public             = list(text = "El ministro afirmó que la reforma es necesaria.",                    min_count = 1),
  f_56_verb_private            = list(text = "Creo que el análisis es correcto.",                                  min_count = 1),
  f_57_verb_suasive            = list(text = "El comité recomendó revisar el protocolo.",                          min_count = 1),
  f_58_verb_seem               = list(text = "Parece que los resultados no son concluyentes.",                     min_count = 1),
  # N. Formas reducidas
  f_63_split_auxiliary         = list(text = "Ha probablemente sido analizado por el equipo.",                     min_count = 1),
  # O. Coordinacion
  f_64_phrasal_coordination    = list(text = "Los estudiantes y los profesores asistieron.",                       min_count = 1),
  f_65_clausal_coordination    = list(text = "Llovía y hacía frío en la calle.",                                   min_count = 1),
  # P. Negacion
  f_66_neg_synthetic           = list(text = "Nadie sabe nada sobre ese asunto.",                                  min_count = 1),
  f_67_neg_analytic            = list(text = "No llegó.",                                                          min_count = 1)
)

# ---------------------------------------------------------------------------
# Validacion estructural: cases cubre exactamente los 55 .evidence_features
# ---------------------------------------------------------------------------

test_that("cases cubre exactamente los 55 .evidence_features", {
  evidence_pool <- pseudobibeR.es:::.evidence_features
  missing <- setdiff(evidence_pool, names(cases))
  extra   <- setdiff(names(cases), evidence_pool)
  expect_equal(missing, character(0),
               info = "rasgos en .evidence_features sin caso de prueba")
  expect_equal(extra, character(0),
               info = "casos de prueba para rasgos que no son evidence_features")
})

# ---------------------------------------------------------------------------
# Helper de skip: se reemplazara por la ejecucion real cuando
# biber_es_traced() exista. Cada feature se activa borrando su entrada
# de SKIPPED_FEATURES o, mas elegante, eliminando este helper cuando
# todos esten migrados.
# ---------------------------------------------------------------------------

# Set vacio = todos los tests corren. Set lleno con todos los nombres = todos skipped.
# Durante la migracion se vacia progresivamente. Cada bloque migrado
# remueve sus rasgos de este set.
#
# MIGRADOS:
#   Phase 2b -- block_tense_es: f_01, f_02, f_03, f_04, f_05, f_11
SKIPPED_FEATURES <- setdiff(names(cases), c(
  "f_01_past_tense",
  "f_02_perfect_aspect",
  "f_03_present_tense",
  "f_04_place_adverbials",
  "f_05_time_adverbials",
  "f_11_indefinite_pronouns"
))

skip_until_migrated <- function(feature) {
  if (feature %in% SKIPPED_FEATURES) {
    skip(paste0("biber_es_traced() block for ", feature, " not yet migrated"))
  }
}

# ---------------------------------------------------------------------------
# Setup del modelo UDPipe (compartido por todos los tests del archivo)
# ---------------------------------------------------------------------------

model_path <- if (file.exists("spanish-gsd-ud-2.5-191206.udpipe")) {
  "spanish-gsd-ud-2.5-191206.udpipe"
} else if (file.exists("../../spanish-gsd-ud-2.5-191206.udpipe")) {
  "../../spanish-gsd-ud-2.5-191206.udpipe"
} else {
  NA_character_
}

ud_model <- if (!is.na(model_path)) {
  tryCatch(udpipe::udpipe_load_model(model_path), error = function(e) NULL)
} else {
  NULL
}

# ---------------------------------------------------------------------------
# Tests parametrizados: uno por rasgo
# ---------------------------------------------------------------------------

dual_path  <- pseudobibeR.es:::.dual_path_features
dict_only  <- pseudobibeR.es:::.dict_only_features
strict_ev  <- pseudobibeR.es:::.strict_evidence_features

# Clasifica la invariante a aplicar segun la topologia del rasgo (M1):
#  - strict:  count == nrow(evidence) exacta
#  - relaxed: 1 <= nrow(evidence) <= count
#  - skipped: dict-only, sin evidencia posible en v1
invariant_for <- function(feature) {
  if (feature %in% dict_only) return("skip_dict_only")
  if (feature %in% dual_path) return("relaxed")
  return("strict")
}

for (feat in names(cases)) {
  local({
    feature_name <- feat
    case         <- cases[[feat]]
    mode         <- invariant_for(feature_name)

    label <- switch(mode,
      strict        = paste0(feature_name, " [strict]: count == nrow(evidence)"),
      relaxed       = paste0(feature_name, " [relaxed dual-path]: 1 <= nrow(evidence) <= count"),
      skip_dict_only= paste0(feature_name, " [dict-only]: skipped (sin evidencia v1)")
    )

    test_that(label, {
      if (mode == "skip_dict_only") {
        skip("dict-only feature: M1 difiere evidencia a v2 (ver .dict_only_features en R/feature_categories.R)")
      }
      skip_until_migrated(feature_name)
      skip_if_not_installed("udpipe")
      skip_if(is.null(ud_model), "modelo UDPipe no disponible")
      skip_if_not(exists("biber_es_traced", envir = asNamespace("pseudobibeR.es")),
                  "biber_es_traced() aun no implementada")

      parsed <- udpipe::udpipe_annotate(ud_model, x = case$text, doc_id = "t1")
      # normalize=FALSE para que counts sea integer comparable a nrow(evidence).
      # Con normalize=TRUE los counts se escalan a "por 1000 tokens" y rompen
      # la igualdad/desigualdad por definicion; la evidencia siempre es raw.
      result <- pseudobibeR.es::biber_es_traced(parsed, measure = "none", normalize = FALSE)

      # Invariante de count (independiente del modo)
      expect_true(feature_name %in% colnames(result$counts),
                  info = paste0(feature_name, " ausente de result$counts"))
      observed_count <- result$counts[[feature_name]][[1]]
      expect_gte(observed_count, case$min_count)

      # Invariante de evidencia segun topologia
      ev_rows <- sum(result$evidence$feature == feature_name)

      if (mode == "strict") {
        expect_equal(ev_rows, observed_count,
                     info = paste0(feature_name,
                                   " [strict]: count=", observed_count,
                                   " pero nrow(evidence)=", ev_rows))
      } else if (mode == "relaxed") {
        expect_gte(ev_rows, 1L,
                   label = paste0(feature_name, " [relaxed]: evidence > 0 cuando count > 0"))
        expect_lte(ev_rows, observed_count,
                   label = paste0(feature_name,
                                  " [relaxed]: nrow(evidence)=", ev_rows,
                                  " > count=", observed_count))
      }
    })
  })
}
