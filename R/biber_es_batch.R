#' Procesamiento batch de corpus para extraccion Biber
#'
#' @description
#' Wrapper de alto nivel que combina ingestion + parsing UDPipe +
#' extraccion de rasgos en una sola llamada. Acepta input polimorfico
#' (path a CSV o data.frame), maneja metadata, opcionalmente devuelve
#' evidencia token-level y/o reporta fallos individuales en modo
#' tolerante.
#'
#' @section Modos:
#'
#' \strong{Fast mode (default, \code{safe = FALSE}):} un solo
#' \code{udpipe_annotate()} sobre el vector completo de textos, luego un
#' \code{biber_es_traced()}. Falla atomica: si UDPipe rompe en algun
#' texto, falla todo. Es lo que un usuario nuevo espera y lo que conviene
#' para corpora curados.
#'
#' \strong{Safe mode (\code{safe = TRUE}):} loop documento por documento
#' con \code{tryCatch}. Los fallos individuales se acumulan en
#' \code{result$failed_docs} y los exitosos en \code{result$counts}.
#' Mas lento pero tolerante a corpora heterogeneos.
#'
#' @section Input polimorfico:
#'
#' \code{input} puede ser:
#' \itemize{
#'   \item \strong{Path a CSV}: string con extension \code{.csv}. Se
#'         lee con \code{read.csv(fileEncoding = "UTF-8")}.
#'   \item \strong{data.frame en memoria}: se usa directamente.
#' }
#'
#' Para corpora como directorios de .txt, ver la receta en el README
#' (una linea con \code{vapply(list.files(...), readLines, ...)}).
#'
#' @section Metadata propagada:
#'
#' Cualquier columna del input que no sea \code{text_column} ni
#' \code{id_column} se propaga al output entre \code{doc_id} y las
#' columnas \code{f_NN_*}. Util para metadatos como genero, ano, autor,
#' que el flujo MDA tipico necesita junto a los conteos.
#'
#' @param input Path a CSV (string) o data.frame con al menos las
#'   columnas \code{text_column} y, opcionalmente, \code{id_column}.
#' @param model Modelo UDPipe cargado con \code{udpipe::udpipe_load_model()}.
#'   Obligatorio (sin default): el usuario lo pasa explicitamente para
#'   evitar side effects de red.
#' @param text_column Nombre de la columna con el texto del documento.
#'   Default \code{"text"}.
#' @param id_column Nombre de la columna con identificadores de
#'   documento. Si \code{NULL} (default), se auto-generan ids
#'   \code{doc_0001}, \code{doc_0002}, ...
#' @param trace Logical. Si \code{TRUE}, el output incluye una columna
#'   \code{evidence} con el tibble largo E1. Default \code{FALSE}.
#' @param safe Logical. Modo robusto con tryCatch por documento. Default
#'   \code{FALSE} (modo rapido / falla atomica).
#' @param measure Medida de TTR. Pasa-through a \code{biber_es_traced()}.
#' @param normalize Si normalizar a por-1000-tokens. Default \code{TRUE}.
#' @param progress Logical. Mostrar barra de progreso en modo safe.
#'   Default \code{TRUE}. Sin efecto en modo fast.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{counts}: data.frame N filas x (1 + N_meta + 67 + 2)
#'         columnas. \code{doc_id}, columnas de metadata, los 67 rasgos
#'         y \code{n_tokens}, \code{n_lex_tokens}. Misma orientacion
#'         que \code{biber_es()} (docs en filas, rasgos en columnas).
#'   \item \code{evidence} (solo si \code{trace = TRUE}): tibble largo
#'         con el schema canonico E1.
#'   \item \code{failed_docs} (solo si \code{safe = TRUE}): tibble con
#'         \code{doc_id, error_message, stage}.
#' }
#'
#' @examples
#' \dontrun{
#' library(udpipe)
#' library(pseudobibeR.es)
#'
#' m  <- udpipe_download_model("spanish-gsd")
#' ud <- udpipe_load_model(m$file_model)
#'
#' # Desde data.frame
#' corpus <- data.frame(
#'   doc_id = c("d1","d2"),
#'   genre  = c("narr","acad"),
#'   text   = c("Maria llego.", "El metodo permite comparar.")
#' )
#' res <- biber_es_batch(corpus, ud, id_column = "doc_id")
#'
#' # Desde CSV
#' res <- biber_es_batch("corpus.csv", ud)
#'
#' # Con evidencia y modo robusto
#' res <- biber_es_batch("corpus.csv", ud, trace = TRUE, safe = TRUE)
#' }
#'
#' @seealso \code{\link{biber_es}}, \code{\link{biber_es_traced}}.
#'
#' @export
biber_es_batch <- function(input,
                           model,
                           text_column = "text",
                           id_column   = NULL,
                           trace       = FALSE,
                           safe        = FALSE,
                           measure     = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                           normalize   = TRUE,
                           progress    = TRUE) {

  if (missing(model) || is.null(model)) {
    stop("'model' es obligatorio. Pasa el resultado de udpipe::udpipe_load_model().",
         call. = FALSE)
  }
  measure <- match.arg(measure)

  # ----- 1. Ingestion polimorfica ---------------------------------------
  corpus_df <- read_corpus_input(input, text_column = text_column,
                                 id_column = id_column)

  # ----- 2. Separar metadata vs texto/id --------------------------------
  meta_cols <- setdiff(colnames(corpus_df),
                       c("doc_id", text_column))
  texts <- corpus_df[[text_column]]
  ids   <- corpus_df$doc_id

  # ----- 3. Procesamiento --------------------------------------------------
  if (safe) {
    proc <- run_batch_safe(texts, ids, model, measure, normalize,
                           trace = trace, progress = progress)
  } else {
    proc <- run_batch_fast(texts, ids, model, measure, normalize, trace = trace)
  }

  # ----- 4. Anadir metadata a counts -------------------------------------
  if (length(meta_cols) > 0L && nrow(proc$counts) > 0L) {
    meta_df <- corpus_df[, c("doc_id", meta_cols), drop = FALSE]
    proc$counts <- dplyr::left_join(meta_df, proc$counts, by = "doc_id")
  }

  # ----- 5. Aviso de tamano de evidencia ---------------------------------
  if (trace && !is.null(proc$evidence) &&
      nrow(proc$counts) * 50L > 5e6L) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_warn(c(
        "Estimacion conservadora indica que evidence podria exceder 5M filas.",
        "i" = "Para corpora muy grandes considera subset o evidence_features (v2)."
      ))
    } else {
      warning("Evidence puede ser muy grande para este corpus.", call. = FALSE)
    }
  }

  proc
}

# ============================================================================
# Helpers internos
# ============================================================================

#' Lectura polimorfica del corpus (CSV path o data.frame)
#'
#' @keywords internal
#' @noRd
read_corpus_input <- function(input, text_column, id_column) {
  # Detectar tipo
  if (is.character(input) && length(input) == 1L) {
    # Path
    if (!file.exists(input)) {
      stop("Archivo no encontrado: ", input, call. = FALSE)
    }
    df <- utils::read.csv(input, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8")
  } else if (is.data.frame(input)) {
    df <- as.data.frame(input, stringsAsFactors = FALSE)
  } else {
    stop("'input' debe ser un path a CSV o un data.frame.",
         "\nTipo recibido: ", paste(class(input), collapse = "/"),
         call. = FALSE)
  }

  if (!text_column %in% colnames(df)) {
    stop("Columna de texto '", text_column, "' no encontrada en input. ",
         "Columnas disponibles: ", paste(colnames(df), collapse = ", "),
         call. = FALSE)
  }

  # doc_id: si id_column es NULL, autogenerar; si no, renombrar a doc_id
  if (is.null(id_column)) {
    df$doc_id <- sprintf("doc_%04d", seq_len(nrow(df)))
  } else {
    if (!id_column %in% colnames(df)) {
      stop("Columna de id '", id_column, "' no encontrada en input.",
           call. = FALSE)
    }
    if (id_column != "doc_id") {
      df$doc_id <- as.character(df[[id_column]])
      df[[id_column]] <- NULL
    } else {
      df$doc_id <- as.character(df$doc_id)
    }
  }

  if (any(duplicated(df$doc_id))) {
    stop("doc_id contiene valores duplicados. Cada documento debe tener id unico.",
         call. = FALSE)
  }

  df
}

#' Modo rapido: una sola pasada UDPipe + biber_es_traced
#' @keywords internal
#' @noRd
run_batch_fast <- function(texts, ids, model, measure, normalize, trace) {
  parsed <- udpipe::udpipe_annotate(
    model, x = texts, doc_id = ids,
    tagger = "default", parser = "default"
  )

  if (trace) {
    result <- pseudobibeR.es::biber_es_traced(parsed, measure = measure,
                                              normalize = normalize)
    return(list(counts = result$counts, evidence = result$evidence))
  }

  counts <- pseudobibeR.es::biber_es(parsed, measure = measure,
                                     normalize = normalize)
  list(counts = counts)
}

#' Modo robusto: loop por documento con tryCatch
#' @keywords internal
#' @noRd
run_batch_safe <- function(texts, ids, model, measure, normalize,
                           trace, progress) {

  n <- length(texts)
  failed <- list()
  counts_list <- vector("list", n)
  evidence_list <- if (trace) vector("list", n) else NULL

  use_cli <- progress && requireNamespace("cli", quietly = TRUE)
  if (use_cli) cli::cli_progress_bar("Procesando documentos", total = n)

  for (i in seq_len(n)) {
    if (use_cli) cli::cli_progress_update()

    res <- tryCatch({
      parsed <- udpipe::udpipe_annotate(model, x = texts[i], doc_id = ids[i],
                                        tagger = "default", parser = "default")
      if (trace) {
        out <- pseudobibeR.es::biber_es_traced(parsed, measure = measure,
                                               normalize = normalize)
        list(counts = out$counts, evidence = out$evidence, error = NULL)
      } else {
        list(counts = pseudobibeR.es::biber_es(parsed, measure = measure,
                                               normalize = normalize),
             evidence = NULL, error = NULL)
      }
    }, error = function(e) {
      list(counts = NULL, evidence = NULL, error = conditionMessage(e))
    })

    if (!is.null(res$error)) {
      failed[[length(failed) + 1L]] <- list(
        doc_id        = ids[i],
        error_message = res$error,
        stage         = "annotate_or_extract"
      )
    } else {
      counts_list[[i]] <- res$counts
      if (trace) evidence_list[[i]] <- res$evidence
    }
  }

  if (use_cli) cli::cli_progress_done()

  counts <- if (length(counts_list) > 0L) {
    dplyr::bind_rows(counts_list[!vapply(counts_list, is.null, logical(1))])
  } else {
    data.frame(doc_id = character(0))
  }

  failed_df <- if (length(failed) > 0L) {
    do.call(rbind, lapply(failed, as.data.frame, stringsAsFactors = FALSE))
  } else {
    data.frame(doc_id = character(0), error_message = character(0),
               stage = character(0), stringsAsFactors = FALSE)
  }

  out <- list(counts = counts, failed_docs = failed_df)
  if (trace) {
    out$evidence <- if (length(evidence_list) > 0L) {
      do.call(bind_evidence, evidence_list)
    } else {
      empty_evidence_tibble()
    }
  }
  out
}
