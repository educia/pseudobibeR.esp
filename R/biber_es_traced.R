#' Extraccion de rasgos de Biber con evidencia token-level
#'
#' @description
#' Variante de \code{biber_es()} que ademas de los conteos por documento
#' devuelve un tibble largo con los tokens que dispararon cada deteccion.
#' Util para auditoria manual, depuracion de detectores, y analisis
#' cross-feature ("que rasgos se activaron en este token?").
#'
#' El retorno es una lista de dos elementos con el contrato E3:
#' \itemize{
#'   \item \code{counts}: identico shape al output de \code{biber_es()} —
#'         una fila por documento, 70 columnas (\code{doc_id} + 67 rasgos +
#'         \code{n_tokens} + \code{n_lex_tokens}). Drop-in compatible.
#'   \item \code{evidence}: tibble largo con 9 columnas estandar
#'         (\code{doc_id, feature, sentence_id, token_id, token, lemma,
#'         upos, feats, head_token_id}). Una fila por token superviviente
#'         a la deteccion estructural de cada rasgo.
#' }
#'
#' @section Invariante de evidencia:
#' Para rasgos \strong{strict} (bloque-only, sin contribucion del path
#' dict): \code{counts[[feat]] == nrow(filter(evidence, feature == feat))}.
#'
#' Para rasgos \strong{relaxed} (dual-path dict + bloque): la evidencia
#' captura solo el aporte del bloque; el conteo total puede ser mayor.
#' Invariante: \code{1 <= nrow(evidence_de_feat) <= counts[[feat]]}.
#' Ver \code{.dual_path_features} en \code{R/feature_categories.R}.
#'
#' Para rasgos \strong{dict-only} (categoria K, f_45-f_50): en v1 no se
#' produce evidencia (path quanteda::tokens_lookup no expone posiciones).
#' El conteo en \code{counts} es correcto; \code{evidence} simplemente
#' no tendra filas para esos rasgos. Reconstruccion manual:
#' \code{parsed \%>\% filter(tolower(lemma) \%in\% dict$f_47_hedges)}.
#'
#' @inheritParams biber_es
#'
#' @return Una \code{list} con dos elementos: \code{counts} (data.frame)
#'   y \code{evidence} (tibble).
#'
#' @examples
#' \dontrun{
#' library(udpipe)
#' library(pseudobibeR.es)
#'
#' m  <- udpipe_download_model("spanish-gsd")
#' ud <- udpipe_load_model(m$file_model)
#' parsed <- udpipe_annotate(ud, x = "Quizas el resultado depende.", doc_id = "d1")
#'
#' result <- biber_es_traced(parsed, measure = "none", normalize = FALSE)
#'
#' # Conteos identicos a biber_es():
#' result$counts
#'
#' # Auditar tokens que dispararon f_47_hedges en este documento:
#' subset(result$evidence, feature == "f_47_hedges")
#' }
#'
#' @seealso \code{\link{biber_es}} para el extractor sin evidencia.
#'
#' @export
biber_es_traced <- function(tokens,
                            measure   = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                            normalize = TRUE) {

  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop(
      "El paquete 'udpipe' debe estar instalado para usar biber_es_traced().\n",
      "Instalalo con: install.packages('udpipe')",
      call. = FALSE
    )
  }

  # Mismo manejo de locale que biber_es() para evitar bugs de encoding
  # con dict de acentos (ver biber_es.R).
  old_ctype <- Sys.getlocale("LC_CTYPE")
  if (!grepl("UTF-?8", old_ctype, ignore.case = TRUE)) {
    utf8_candidates <- c("en_US.UTF-8", "C.UTF-8", "es_ES.UTF-8")
    for (lc in utf8_candidates) {
      ok <- tryCatch(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)),
                     error = function(e) "")
      if (nzchar(ok) && ok != "C") break
    }
    on.exit(Sys.setlocale("LC_CTYPE", old_ctype), add = TRUE)
  }

  if (is.null(tokens)) {
    stop("'tokens' no puede ser NULL. Pasa el resultado de udpipe_annotate().",
         call. = FALSE)
  }

  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)
  if (nrow(udpipe_tks) == 0) {
    stop("'tokens' esta vacio (0 filas).", call. = FALSE)
  }

  required_cols <- c("doc_id", "token", "lemma", "upos", "xpos", "dep_rel")
  missing_cols  <- setdiff(required_cols, colnames(udpipe_tks))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas requeridas en 'tokens': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
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

  udpipe_tks <- structure(
    udpipe_tks,
    class = c("spacyr_parsed", "data.frame")
  )

  measure <- match.arg(measure)

  # Mismo muffler de warnings que biber_es() para silenciar el ruido de
  # "input string 'X' cannot be translated from 'US-ASCII' to UTF-8"
  # generado por quanteda en locales no-UTF-8.
  withCallingHandlers(
    parse_biber_features(
      tokens    = udpipe_tks,
      measure   = measure,
      normalize = normalize,
      engine    = "udpipe",
      language  = "es",
      traced    = TRUE
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("cannot be translated", msg, fixed = TRUE) &&
          grepl("but is valid UTF-8", msg, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
