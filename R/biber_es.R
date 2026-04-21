#' Rasgos de Biber para textos en espanol (udpipe)
#'
#' @title Extractor de rasgos lexico-gramaticales de Biber (1988) para espanol
#'
#' @description
#' Extrae los 67 rasgos de Biber adaptados al espanol a partir de la
#' salida de \code{udpipe::udpipe_annotate()} usando el modelo UD
#' \code{spanish-gsd}. Devuelve una fila por documento con conteos brutos o
#' normalizados.
#'
#' La funcion acepta dos formatos de entrada:
#' \itemize{
#'   \item \strong{udpipe_connlu}: objeto devuelto directamente por
#'         \code{udpipe::udpipe_annotate()} (la salida se convierte
#'         internamente antes de llamar al orquestador).
#'   \item \strong{data.frame / spacyr_parsed}: tabla ya convertida con
#'         columnas \code{doc_id}, \code{token}, \code{lemma}, \code{pos},
#'         \code{tag}, \code{feats}, \code{head_token_id}, \code{dep_rel}.
#' }
#'
#' @param tokens Objeto devuelto por \code{udpipe::udpipe_annotate()} o
#'   data.frame con formato \code{spacyr_parsed}. Debe contener al menos las
#'   columnas \code{doc_id}, \code{token}, \code{lemma}, \code{upos} (o
#'   \code{pos}), \code{dep_rel}.
#' @param measure Medida de diversidad lexica (TTR). Opciones:
#'   \code{"MATTR"} (Moving-Average TTR, recomendada para textos de longitud
#'   variable), \code{"TTR"} (tipo/token simple), \code{"CTTR"} (TTR
#'   corregida), \code{"MSTTR"} (TTR de segmentos), o \code{"none"} (omitir
#'   calculo de TTR). Por defecto \code{"MATTR"}.
#' @param normalize Logico. Si \code{TRUE} (por defecto), todos los conteos se
#'   normalizan a frecuencia por 1\,000 tokens lexicos (se excluyen signos de
#'   puntuacion y espacios del denominador). Si \code{FALSE} se devuelven
#'   recuentos absolutos.
#'
#' @return Un \code{data.frame} con una fila por documento y las siguientes
#'   columnas:
#'   \itemize{
#'     \item \code{doc_id}: identificador del documento (hereda el
#'           \code{doc_id} de la anotacion udpipe).
#'     \item \code{f_01_past_tense} \ldots \code{f_67_neg_analytic}: 67
#'           columnas de rasgos con el convenio de nombres
#'           \code{f_NN_nombre}, donde \code{NN} es el numero de rasgo
#'           segun Biber (1988) y \code{nombre} es un descriptor breve en
#'           ingles.
#'     \item \code{f_43_type_token}: diversidad lexica (MATTR u otra medida
#'           seleccionada).
#'     \item \code{f_44_mean_word_length}: longitud media de tokens lexicos
#'           (en caracteres).
#'   }
#'
#' @note
#' \strong{Alcance de f_01 (preterito imperfecto):} A diferencia del paquete
#' frances \code{pseudobibeR.fr} (donde \code{f_01} agrega todos los pasados),
#' en la version espanola \code{f_01} captura unicamente el preterito
#' imperfecto (\code{Tense=Imp|Mood=Ind|VerbForm=Fin}). El indefinido
#' (\emph{canto}) se recoge en \code{f_71_preterit} y el perfecto compuesto
#' (\emph{ha cantado}) en \code{f_02_perfect_aspect}. Para un proxy de
#' \dQuote{total de formas de pasado} equivalente al ingles, sumar
#' \code{f_01 + f_02 + f_71}.
#'
#' @references
#' Biber, D. (1988). \emph{Variation across speech and writing}.
#' Cambridge University Press.
#' \doi{10.1017/CBO9780511621024}
#'
#' Brown, D. W. (2024). \emph{pseudobibeR: Pseudo Biber Tagger for Text
#' Feature Extraction}. R package.
#' \url{https://github.com/browndw/pseudobibeR}
#'
#' @examples
#' \dontrun{
#' library(udpipe)
#' library(pseudobibeR.es)
#'
#' # Descargar y cargar modelo UD espanol
#' m  <- udpipe_download_model("spanish-gsd")
#' ud <- udpipe_load_model(m$file_model)
#'
#' # Anotar texto
#' parsed <- udpipe_annotate(
#'   ud,
#'   x      = "El gobierno aprob\u00f3 la ley ayer.",
#'   doc_id = "doc1"
#' )
#'
#' # Extraer rasgos (normalizados por 1000 tokens)
#' result <- biber_es(parsed, measure = "MATTR", normalize = TRUE)
#' print(result[, 1:10])
#' }
#'
#' @export
biber_es <- function(tokens,
                     measure   = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                     normalize = TRUE) {

  # Requires udpipe (same pattern as biber.udpipe_connlu())
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop(
      "El paquete 'udpipe' debe estar instalado para usar biber_es().\n",
      "Inst\u00e1lalo con: install.packages('udpipe')",
      call. = FALSE
    )
  }

  if (is.null(tokens)) {
    stop(
      "'tokens' no puede ser NULL. Pasa el resultado de udpipe_annotate().",
      call. = FALSE
    )
  }

  # Convert to internal spacyr_parsed-compatible data.frame
  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)

  if (nrow(udpipe_tks) == 0) {
    stop(
      "'tokens' est\u00e1 vac\u00edo (0 filas). ",
      "Aseg\u00farate de que udpipe_annotate() devolvi\u00f3 algo.",
      call. = FALSE
    )
  }

  required_cols <- c("doc_id", "token", "lemma", "upos", "xpos", "dep_rel")
  missing_cols  <- setdiff(required_cols, colnames(udpipe_tks))
  if (length(missing_cols) > 0) {
    stop(
      "Faltan columnas requeridas en 'tokens': ",
      paste(missing_cols, collapse = ", "),
      ". Aseg\u00farate de llamar a udpipe_annotate() con ",
      "tagger='default' y parser='default'.",
      call. = FALSE
    )
  }

  udpipe_tks <- udpipe_tks |>
    dplyr::select(
      "doc_id", "sentence_id", "token_id", "token", "lemma",
      "upos", "xpos", "feats", "head_token_id", "dep_rel"
    ) |>
    dplyr::rename(pos = "upos", tag = "xpos") |>
    dplyr::mutate(
      tag = dplyr::if_else(
        is.na(.data$tag) | .data$tag == "",
        .data$pos,
        .data$tag
      )
    )

  # The "spacyr_parsed" class is what parse_biber_features() expects
  udpipe_tks <- structure(
    udpipe_tks,
    class = c("spacyr_parsed", "data.frame")
  )

  measure <- match.arg(measure)

  # language = "es" activates all *_es blocks in parse_biber_features()
  parse_biber_features(
    tokens    = udpipe_tks,
    measure   = measure,
    normalize = normalize,
    engine    = "udpipe",
    language  = "es"
  )
}
