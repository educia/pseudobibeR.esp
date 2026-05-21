# nolint start: line_length_linter, object_name_linter
# Helper para test-feature-coverage.R — auditoría Fase 2 (57 rasgos ES).
# En root tests/testthat/ para que testthat lo auto-cargue.

.fc_model_filename <- "spanish-gsd-ud-2.5-191206.udpipe"

.fc_model_path <- function() {
  td <- testthat::test_path()
  cands <- unique(c(
    file.path(td, "..", .fc_model_filename),
    file.path(td, "fixtures", .fc_model_filename),
    file.path(td, "..", "..", .fc_model_filename),
    file.path(td, "..", "..", "..", .fc_model_filename),
    file.path(".", .fc_model_filename)
  ))
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) return(NULL)
  normalizePath(hit[[1]], mustWork = TRUE)
}

# Carga perezosa única del modelo UDPipe
.fc_ud <- local({
  m <- NULL
  function() {
    if (is.null(m)) {
      p <- .fc_model_path()
      if (is.null(p)) testthat::skip("modelo UDPipe spanish-gsd no disponible")
      m <<- udpipe::udpipe_load_model(p)
    }
    m
  }
})

# Parsea un texto y devuelve la fila de rasgos. Por defecto normalize = FALSE
# (conteos brutos). Tests de regresión sobre métricas derivadas (f_43/f_44)
# pueden pasar normalize = TRUE para verificar invariancia.
run_biber <- function(text, normalize = FALSE) {
  skip_if_not_installed("udpipe")
  ud <- .fc_ud()
  parsed <- as.data.frame(udpipe::udpipe_annotate(ud, x = text))
  suppressMessages(biber_es(parsed, measure = "none", normalize = normalize))
}

# nolint end
