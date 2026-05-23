#' Mapeo rasgo -> categoria Biber (interno)
#'
#' Vector nombrado que asocia cada uno de los 67 nombres de columna del
#' output de \code{biber_es()} con su categoria Biber (1988) en espanol.
#' Fuente: \code{biber_espanol_completo.md} seccion 3 y README seccion
#' "Categorias de rasgos". Sirve como tabla de lookup para
#' \code{biber_es_traced()} y \code{biber_es_batch()} al agrupar la
#' evidencia por categoria, y para tests de consistencia que verifican
#' que ningun rasgo del output queda sin categoria.
#'
#' @keywords internal
#' @noRd
.feature_categories <- c(
  # A. Tiempo y aspecto
  f_01_past_tense              = "A. Tiempo y aspecto",
  f_02_perfect_aspect          = "A. Tiempo y aspecto",
  f_03_present_tense           = "A. Tiempo y aspecto",
  # B. Adverbiales de lugar y tiempo
  f_04_place_adverbials        = "B. Adverbiales de lugar y tiempo",
  f_05_time_adverbials         = "B. Adverbiales de lugar y tiempo",
  # C. Pronombres (incluye pro-verbo do en Biber 1988)
  f_06_first_person_pronouns   = "C. Pronombres",
  f_07_second_person_pronouns  = "C. Pronombres",
  f_08_third_person_pronouns   = "C. Pronombres",
  f_09_pronoun_it              = "C. Pronombres",
  f_10_demonstrative_pronoun   = "C. Pronombres",
  f_11_indefinite_pronouns     = "C. Pronombres",
  f_12_proverb_do              = "C. Pronombres",
  # D. Interrogativas
  f_13_wh_question             = "D. Interrogativas",
  # E. Formas nominales
  f_14_nominalizations         = "E. Formas nominales",
  f_15_gerunds                 = "E. Formas nominales",
  f_16_other_nouns             = "E. Formas nominales",
  # F. Pasivas
  f_17_agentless_passives      = "F. Pasivas",
  f_18_by_passives             = "F. Pasivas",
  # G. Formas estativas
  f_19_be_main_verb            = "G. Formas estativas",
  f_20_existential_there       = "G. Formas estativas",
  # H. Subordinacion
  f_21_that_verb_comp          = "H. Subordinacion",
  f_22_that_adj_comp           = "H. Subordinacion",
  f_23_wh_clause               = "H. Subordinacion",
  f_24_infinitives             = "H. Subordinacion",
  f_25_present_participle      = "H. Subordinacion",
  f_26_past_participle         = "H. Subordinacion",
  f_27_past_participle_whiz    = "H. Subordinacion",
  f_28_present_participle_whiz = "H. Subordinacion",
  f_29_that_subj               = "H. Subordinacion",
  f_30_that_obj                = "H. Subordinacion",
  f_31_wh_subj                 = "H. Subordinacion",
  f_32_wh_obj                  = "H. Subordinacion",
  f_33_pied_piping             = "H. Subordinacion",
  f_34_sentence_relatives      = "H. Subordinacion",
  f_35_because                 = "H. Subordinacion",
  f_36_though                  = "H. Subordinacion",
  f_37_if                      = "H. Subordinacion",
  f_38_other_adv_sub           = "H. Subordinacion",
  # I. Sintagmas prep., adj. y adv.
  f_39_prepositions            = "I. Sintagmas prep., adj. y adv.",
  f_40_adj_attr                = "I. Sintagmas prep., adj. y adv.",
  f_41_adj_pred                = "I. Sintagmas prep., adj. y adv.",
  f_42_adverbs                 = "I. Sintagmas prep., adj. y adv.",
  # J. Especificidad lexica (metricas, no conteos)
  f_43_type_token              = "J. Especificidad lexica",
  f_44_mean_word_length        = "J. Especificidad lexica",
  # K. Clases lexicas
  f_45_conjuncts               = "K. Clases lexicas",
  f_46_downtoners              = "K. Clases lexicas",
  f_47_hedges                  = "K. Clases lexicas",
  f_48_amplifiers              = "K. Clases lexicas",
  f_49_emphatics               = "K. Clases lexicas",
  f_50_discourse_particles     = "K. Clases lexicas",
  f_51_demonstratives          = "K. Clases lexicas",
  # L. Modales
  f_52_modal_possibility       = "L. Modales",
  f_53_modal_necessity         = "L. Modales",
  f_54_modal_predictive        = "L. Modales",
  # M. Verbos especializados
  f_55_verb_public             = "M. Verbos especializados",
  f_56_verb_private            = "M. Verbos especializados",
  f_57_verb_suasive            = "M. Verbos especializados",
  f_58_verb_seem               = "M. Verbos especializados",
  # N. Formas reducidas
  f_59_contractions            = "N. Formas reducidas",
  f_60_that_deletion           = "N. Formas reducidas",
  f_61_stranded_preposition    = "N. Formas reducidas",
  f_62_split_infinitive        = "N. Formas reducidas",
  f_63_split_auxiliary         = "N. Formas reducidas",
  # O. Coordinacion
  f_64_phrasal_coordination    = "O. Coordinacion",
  f_65_clausal_coordination    = "O. Coordinacion",
  # P. Negacion
  f_66_neg_synthetic           = "P. Negacion",
  f_67_neg_analytic            = "P. Negacion"
)

#' Lista de los 10 rasgos zero-output (interno)
#'
#' Rasgos que aparecen como columnas en el output de \code{biber_es()} por
#' paridad superficial con \code{pseudobibeR.fr} pero cuyo valor es siempre
#' 0 porque la propiedad linguistica subyacente no se traduce al espanol
#' (sujeto nulo, sin contracciones ortograficas, sin preposicion varada,
#' etc.) o porque fue absorbida por una fusion (f_31 -> f_29; f_32 -> f_30).
#' Documentado en \code{biber_espanol_completo.md} seccion 1 y README.
#'
#' Usado por \code{biber_es_traced()} y los tests para distinguir entre
#' "rasgo detectable que dio count=0 en este texto" (ausencia genuina) y
#' "rasgo zero-output por contrato" (no hay detector que correr).
#'
#' @keywords internal
#' @noRd
.zero_output_features <- c(
  "f_09_pronoun_it",
  "f_12_proverb_do",
  "f_15_gerunds",
  "f_28_present_participle_whiz",
  "f_31_wh_subj",
  "f_32_wh_obj",
  "f_59_contractions",
  "f_60_that_deletion",
  "f_61_stranded_preposition",
  "f_62_split_infinitive"
)

#' Lista de los 2 rasgos-metrica (interno)
#'
#' Rasgos cuyo valor en \code{biber_es()} no es un conteo de tokens sino
#' una metrica calculada sobre el documento completo:
#' \itemize{
#'   \item \code{f_43_type_token}: ratio tipos/tokens (MATTR, TTR, CTTR o MSTTR
#'         segun el argumento \code{measure}).
#'   \item \code{f_44_mean_word_length}: longitud media en caracteres de
#'         todos los tokens excepto puntuacion.
#' }
#'
#' Por convencion, las metricas no producen filas de evidencia en
#' \code{biber_es_traced()} v1: su valor esta directamente en
#' \code{result$counts$f_43_type_token} y \code{result$counts$f_44_mean_word_length}.
#' Una v2 podria anadir trazabilidad fina (que tokens entraron en el calculo).
#'
#' @keywords internal
#' @noRd
.metric_features <- c(
  "f_43_type_token",
  "f_44_mean_word_length"
)

#' Lista de los 57 rasgos con deteccion real (interno)
#'
#' Derivada por diferencia: 67 totales - 10 zero-output = 57 detectables.
#' Incluye los 2 rasgos-metrica (f_43, f_44). Para el conjunto puro de
#' "rasgos que generan filas de evidencia", restar tambien \code{.metric_features}.
#'
#' Usado por el test transversal de cobertura para iterar sobre todos
#' los rasgos que deben generar evidencia no-vacia cuando count > 0.
#'
#' @keywords internal
#' @noRd
.detectable_features <- setdiff(names(.feature_categories), .zero_output_features)

#' Rasgos que generan evidencia (interno)
#'
#' 55 rasgos: detectables - metricas. Cada uno debe satisfacer la
#' invariante \code{count == nrow(filter(evidence, feature == X))} cuando
#' count > 0.
#'
#' @keywords internal
#' @noRd
.evidence_features <- setdiff(.detectable_features, .metric_features)
