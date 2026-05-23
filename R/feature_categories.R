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
#' count > 0, MODULADA por la topologia del path de conteo (ver
#' \code{.dual_path_features} y \code{.dict_only_features}).
#'
#' @keywords internal
#' @noRd
.evidence_features <- setdiff(.detectable_features, .metric_features)

#' Rasgos dual-path: dict + block contribuyen al conteo final (interno)
#'
#' Estos rasgos reciben aportes tanto de la pipeline dict-based
#' (\code{quanteda::tokens_lookup} sobre \code{dict}) como de algun
#' \code{block_*_es()}. \code{parse_biber_features()} los combina al
#' final via \code{pmax(.x, .y)} (para los pmax_features) o
#' \code{.x + .y} (para los modales/verbos especializados restantes,
#' excepto f_52 que es code-only).
#'
#' Implicacion para \code{biber_es_traced()}: el path-block produce
#' evidencia estructural (tokens identificados por la regla sintactica
#' del detector), pero la pipeline dict no expone tokens posicionalmente
#' en v1. Por tanto, para estos rasgos vale la invariante RELAJADA:
#'
#'   \code{1 <= nrow(filter(evidence, feature == X)) <= counts[[X]]}
#'
#' La igualdad estricta no se cumple porque parte del count viene del
#' dict path. Esto se documenta en \code{biber_es_traced()} y se valida
#' en el test transversal con un test parametrizado distinto al strict.
#'
#' Listado autoritativo: derivado de \code{parse_functions.R} cruzando
#' \code{pmax_features} y \code{combine_features} con \code{dict.yaml}.
#' f_52 se excluye porque \code{code_only_features} lo fuerza a path
#' block puro (dict path suprimido).
#'
#' @keywords internal
#' @noRd
.dual_path_features <- c(
  "f_04_place_adverbials",
  "f_05_time_adverbials",
  "f_06_first_person_pronouns",
  "f_07_second_person_pronouns",
  "f_08_third_person_pronouns",
  "f_11_indefinite_pronouns",
  "f_51_demonstratives",
  "f_53_modal_necessity",
  "f_55_verb_public",
  "f_56_verb_private",
  "f_57_verb_suasive",
  "f_58_verb_seem"
)

#' Rasgos dict-only: solo path dict produce el conteo (interno)
#'
#' Categoria K (clases lexicas, f_45-f_50). Estos rasgos vienen
#' exclusivamente de \code{quanteda::tokens_lookup} sobre \code{dict};
#' no hay block estructural en v1 que emita tokens para ellos.
#'
#' Implicacion para \code{biber_es_traced()}: en v1 no es posible
#' construir evidencia para estos rasgos sin reemplazar la pipeline
#' quanteda por una variante que preserve posiciones (opcion M3 en el
#' design doc). El test transversal los marca con \code{skip()}
#' explicito hasta que v2 los aborde. Su columna en \code{result$counts}
#' tiene el valor correcto; \code{result$evidence} simplemente no
#' contiene filas para ellos.
#'
#' Si el usuario quiere reconstruir manualmente los tokens que
#' dispararon (p.ej.) f_47, puede joinear contra el dict en R:
#'
#'   \code{parsed \%>\% filter(tolower(lemma) \%in\% dict$f_47_hedges)}
#'
#' @keywords internal
#' @noRd
.dict_only_features <- c(
  "f_45_conjuncts",
  "f_46_downtoners",
  "f_47_hedges",
  "f_48_amplifiers",
  "f_49_emphatics",
  "f_50_discourse_particles"
)

#' Rasgos con limitacion conocida de UDPipe (interno)
#'
#' Rasgos cuyo detector es correcto pero UDPipe spanish-gsd subreporta
#' por errores de tagging/parsing en construcciones especificas. La
#' invariante strict/relaxed sigue valiendo logicamente (cuando UDPipe
#' acierta), pero el test transversal no puede asumir count >= 1 sobre
#' un ejemplo sintetico porque el parser ya devolvio 0 antes de que
#' el detector pudiese opinar.
#'
#' Documentado en README "Known limitations" y biber_espanol_completo.md
#' §f_22, §f_26. El test transversal los skipea con razon explicita.
#'
#' @keywords internal
#' @noRd
.udpipe_limited_features <- c(
  "f_22_that_adj_comp",
  "f_26_past_participle",
  # f_27 sufre el mismo problema que f_26: UDPipe spanish-gsd etiqueta el
  # participio postnominal de forma inconsistente (ADJ sin VerbForm en
  # algunos casos, VERB con dep_rel "amod" en otros). El detector es
  # correcto cuando UDPipe acierta, pero no podemos forzar count >= 1
  # sobre un ejemplo sintetico generico.
  "f_27_past_participle_whiz"
)

#' Rasgos strict: invariante \code{count == nrow(evidence)} se cumple exactamente (interno)
#'
#' Complemento: \code{.evidence_features - .dual_path_features - .dict_only_features}.
#' Son los rasgos que se generan exclusivamente por un \code{block_*_es()}
#' estructural, sin contribucion dict, y por tanto su block evidencia
#' captura el 100% del conteo.
#'
#' En el test transversal, estos rasgos se testean con la invariante
#' ESTRICTA. Cualquier desviacion indica un bug en la migracion.
#'
#' @keywords internal
#' @noRd
.strict_evidence_features <- setdiff(
  .evidence_features,
  c(.dual_path_features, .dict_only_features)
)
