# evidence_helpers.R
#
# Infraestructura para el contrato dual (counts + evidence) que adoptan
# los bloques migrados al schema E3 destinado a biber_es_traced() y
# biber_es_batch().
#
# Estado en Phase 1: estos helpers existen y estan testeados, pero
# ningun bloque productivo los usa todavia. parse_biber_features() sigue
# operando exactamente igual que antes. Esto se mantiene como red de
# seguridad: cualquier regresion durante la migracion de Phase 2 se
# detecta inmediatamente sin que estos helpers introduzcan riesgo
# antes de tiempo.

# ----------------------------------------------------------------------
# Schema canonico de evidencia (formato largo E1)
# ----------------------------------------------------------------------

#' Columnas del schema de evidencia E1
#'
#' Una fila por token que disparo una deteccion de conteo. El orden y
#' los tipos son fijos para permitir bind_rows() sin coercion.
#'
#' @keywords internal
#' @noRd
.EVIDENCE_COLS <- c(
  "doc_id", "feature", "sentence_id", "token_id",
  "token", "lemma", "upos", "feats", "head_token_id"
)

#' Tibble vacio con el schema canonico de evidencia
#'
#' Usado como valor por defecto en \code{make_block_result()} cuando un
#' bloque todavia no esta migrado (su evidencia es vacia) y como base
#' para \code{bind_evidence()} cuando no hay nada que apilar.
#'
#' @return Un tibble de 0 filas con las 9 columnas de \code{.EVIDENCE_COLS}
#'   y tipos correctos: \code{character} para doc_id/feature/token/lemma/upos/feats,
#'   \code{integer} para sentence_id/token_id/head_token_id.
#' @keywords internal
#' @noRd
empty_evidence_tibble <- function() {
  tibble::tibble(
    doc_id        = character(0),
    feature       = character(0),
    sentence_id   = integer(0),
    token_id      = integer(0),
    token         = character(0),
    lemma         = character(0),
    upos          = character(0),
    feats         = character(0),
    head_token_id = integer(0)
  )
}

# ----------------------------------------------------------------------
# Constructor y accessors del contrato block result
# ----------------------------------------------------------------------

#' Construye un block_result canonico
#'
#' El contrato que adoptan los bloques migrados en Phase 2:
#' devolver \code{list(counts, evidence)} en lugar del data.frame plano.
#' Los bloques no-migrados siguen devolviendo data.frame y se normalizan
#' via \code{as_block_result()} cuando se consumen.
#'
#' @param counts data.frame con \code{doc_id} y una o mas columnas
#'   numericas \code{f_NN_*}.
#' @param evidence tibble largo con el schema \code{.EVIDENCE_COLS}.
#'   Default: tibble vacio.
#' @return Un objeto \code{list} con clase \code{"block_result"} y dos
#'   elementos: \code{counts} y \code{evidence}.
#' @keywords internal
#' @noRd
make_block_result <- function(counts, evidence = empty_evidence_tibble()) {
  if (!is.data.frame(counts)) {
    stop("'counts' debe ser un data.frame.", call. = FALSE)
  }
  if (!is.data.frame(evidence)) {
    stop("'evidence' debe ser un data.frame.", call. = FALSE)
  }
  if (nrow(evidence) > 0L) {
    missing_cols <- setdiff(.EVIDENCE_COLS, colnames(evidence))
    if (length(missing_cols) > 0L) {
      stop(
        "'evidence' no tiene el schema canonico. Faltan columnas: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }
  structure(
    list(counts = counts, evidence = evidence),
    class = c("block_result", "list")
  )
}

#' Detecta si x es un block_result en el nuevo contrato
#'
#' @keywords internal
#' @noRd
is_block_result <- function(x) {
  inherits(x, "block_result") ||
    (is.list(x) && !is.data.frame(x) &&
       all(c("counts", "evidence") %in% names(x)))
}

#' Coerciona un retorno de bloque al contrato block_result
#'
#' Acepta dos formatos:
#' \itemize{
#'   \item Antiguo (pre-migracion): \code{data.frame} con \code{doc_id} y
#'         columnas \code{f_NN_*}. Se envuelve con evidence vacio.
#'   \item Nuevo (post-migracion): \code{list(counts, evidence)} ya en
#'         contrato. Se devuelve sin cambios.
#' }
#'
#' Esta funcion es el puente que permite migrar bloques uno-por-uno sin
#' romper \code{parse_biber_features()}.
#'
#' @keywords internal
#' @noRd
as_block_result <- function(x) {
  if (is_block_result(x)) return(x)
  if (is.data.frame(x))   return(make_block_result(counts = x))
  stop(
    "El retorno de un bloque debe ser un data.frame (formato antiguo) ",
    "o list(counts, evidence) (contrato block_result). Tipo recibido: ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}

#' Extrae el data.frame de conteos de un block_result o data.frame
#'
#' Idempotente: si \code{x} ya es un data.frame, lo devuelve sin cambios.
#'
#' @keywords internal
#' @noRd
extract_counts <- function(x) {
  if (is_block_result(x)) return(x$counts)
  if (is.data.frame(x))   return(x)
  stop("extract_counts(): tipo no reconocido: ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' Extrae el tibble de evidencia de un block_result o data.frame
#'
#' Si \code{x} es un data.frame (formato antiguo, no migrado), devuelve
#' un tibble de evidencia vacio canonico. Esto permite que el codigo
#' que consume evidencia funcione uniformemente sobre bloques migrados
#' y no-migrados.
#'
#' @keywords internal
#' @noRd
extract_evidence <- function(x) {
  if (is_block_result(x)) return(x$evidence)
  if (is.data.frame(x))   return(empty_evidence_tibble())
  stop("extract_evidence(): tipo no reconocido: ",
       paste(class(x), collapse = "/"), call. = FALSE)
}

#' Apila tibbles de evidencia en uno solo
#'
#' Wrapper sobre \code{dplyr::bind_rows()} que devuelve el tibble vacio
#' canonico cuando no hay nada que apilar, evitando NULLs intermedios.
#'
#' @param ... uno o mas tibbles con schema \code{.EVIDENCE_COLS}.
#' @keywords internal
#' @noRd
bind_evidence <- function(...) {
  args <- list(...)
  if (length(args) == 0L) return(empty_evidence_tibble())
  # Filtrar NULLs y vacios para evitar coerciones raras
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) == 0L) return(empty_evidence_tibble())
  dplyr::bind_rows(args)
}

# ----------------------------------------------------------------------
# Helper paralelo a count_feature() que retorna ambos: counts + evidence
# ----------------------------------------------------------------------

#' Variante de count_feature() que tambien devuelve evidencia
#'
#' Parametros y semantica identicos a \code{count_feature()} en
#' \code{features_tense_pronouns.R}: toma un tbl ya filtrado, dedupea
#' por (doc_id, sentence_id, token_id_int) con la misma logica para
#' MWTs con token_id NA, y agrega a nivel doc_id.
#'
#' La diferencia es el retorno: en lugar de un data.frame, devuelve un
#' \code{block_result} con \code{counts} (mismo formato que count_feature)
#' y \code{evidence} (long format E1, una fila por token superviviente
#' a la deduplicacion).
#'
#' @param tbl data.frame de tokens ya filtrados. Debe tener columnas
#'   UD estandar: doc_id, sentence_id, token_id_int, token, lemma, pos,
#'   feats, head_token_id_int.
#' @param feature_name string con el nombre canonico del rasgo
#'   (p.ej. \code{"f_01_past_tense"}). Sera el valor de la columna
#'   \code{feature} en la evidencia y el nombre de la columna de conteo.
#' @return Un \code{block_result}.
#' @keywords internal
#' @noRd
count_feature_traced <- function(tbl, feature_name) {
  # Mismo dedup que count_feature() — replica exacta de la logica para
  # preservar paridad de conteos.
  dedup <- tbl %>%
    dplyr::mutate(
      .tid_dedup = dplyr::if_else(
        is.na(.data$token_id_int),
        -.Machine$integer.max + dplyr::row_number(),
        .data$token_id_int
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$.tid_dedup, .keep_all = TRUE)

  counts <- dedup %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(!!feature_name := "n")

  evidence <- if (nrow(dedup) == 0L) {
    empty_evidence_tibble()
  } else {
    tibble::tibble(
      doc_id        = as.character(dedup$doc_id),
      feature       = feature_name,
      sentence_id   = as.integer(dedup$sentence_id),
      token_id      = as.integer(dedup$token_id_int),
      token         = as.character(dedup$token),
      lemma         = as.character(dedup$lemma),
      upos          = as.character(dedup$pos),
      feats         = as.character(dedup$feats),
      head_token_id = as.integer(dedup$head_token_id_int)
    )
  }

  make_block_result(counts = counts, evidence = evidence)
}
