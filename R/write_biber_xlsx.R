#' Exportar resultados de biber_es_batch a XLSX
#'
#' @description
#' Serializa el output de \code{biber_es_batch()} a un archivo Excel
#' multi-hoja con orientacion tidy (docs en filas, rasgos en columnas) en
#' TODAS las hojas. Esta orientacion preserva la compatibilidad de
#' re-importacion con \code{readxl::read_xlsx()} y herramientas
#' equivalentes en otros lenguajes.
#'
#' @section Hojas generadas:
#'
#' \itemize{
#'   \item \code{raw}: conteos brutos (\code{biber_es_batch} con
#'         \code{normalize = FALSE}) o el output tal cual venga.
#'   \item \code{per_1k}: conteos normalizados por 1000 tokens lexicos.
#'         Solo presente si se invoca con \code{include_per_1k = TRUE}.
#'   \item \code{metadata}: fecha de generacion, version del paquete,
#'         dimensiones del corpus.
#'   \item \code{evidence}: tibble largo E1, solo si
#'         \code{result$evidence} esta presente.
#'   \item \code{failed_docs}: tabla de fallos individuales, solo si
#'         \code{result$failed_docs} tiene filas (modo safe).
#' }
#'
#' @param result Output de \code{biber_es_batch()}.
#' @param path Ruta del archivo .xlsx a escribir.
#' @param include_per_1k Logical. Si \code{TRUE}, anade una hoja con los
#'   conteos normalizados a /1000 tokens lexicos. Default \code{FALSE}
#'   (el usuario suele querer una u otra orientacion, no ambas).
#'
#' @return Invisible: el path escrito.
#'
#' @examples
#' \dontrun{
#' res <- biber_es_batch("corpus.csv", ud_model, trace = TRUE)
#' write_biber_xlsx(res, "output.xlsx")
#' }
#'
#' @export
write_biber_xlsx <- function(result, path, include_per_1k = FALSE) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("El paquete 'writexl' es necesario para write_biber_xlsx().\n",
         "Instalalo con: install.packages('writexl')",
         call. = FALSE)
  }

  if (!is.list(result) || !"counts" %in% names(result)) {
    stop("'result' no es un output de biber_es_batch (falta $counts).",
         call. = FALSE)
  }

  sheets <- list(raw = as.data.frame(result$counts))

  if (include_per_1k) {
    if (!"n_lex_tokens" %in% colnames(result$counts)) {
      warning("No se pudo computar per_1k: falta 'n_lex_tokens'.", call. = FALSE)
    } else {
      sheets$per_1k <- normalize_counts_per_1k(result$counts)
    }
  }

  sheets$metadata <- data.frame(
    field = c("generated_at", "package_version", "n_docs", "n_features"),
    value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      as.character(utils::packageVersion("pseudobibeR.es")),
      as.character(nrow(result$counts)),
      as.character(sum(grepl("^f_", colnames(result$counts))))
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(result$evidence) && nrow(result$evidence) > 0L) {
    sheets$evidence <- as.data.frame(result$evidence)
  }

  if (!is.null(result$failed_docs) && nrow(result$failed_docs) > 0L) {
    sheets$failed_docs <- as.data.frame(result$failed_docs)
  }

  writexl::write_xlsx(sheets, path = path)
  invisible(path)
}

#' Normaliza conteos a tasa por 1000 tokens lexicos
#'
#' @keywords internal
#' @noRd
normalize_counts_per_1k <- function(counts) {
  feature_cols <- grep("^f_\\d{2}_", colnames(counts), value = TRUE)
  # f_43 (TTR) y f_44 (mean word length) son metricas, no conteos -- no se
  # renormalizan.
  metric_cols <- c("f_43_type_token", "f_44_mean_word_length")
  count_cols  <- setdiff(feature_cols, metric_cols)

  out <- counts
  for (col in count_cols) {
    out[[col]] <- ifelse(
      out$n_lex_tokens > 0,
      round(out[[col]] / out$n_lex_tokens * 1000, 3),
      NA_real_
    )
  }
  out
}
