library(udpipe)
library(pseudobibeR.es)
library(dplyr)

# ── 1. Cargar modelo UD español ──────────────────────────────────────────────
ud_model <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")

# ── 2. Leer textos ───────────────────────────────────────────────────────────
genres <- c("acad", "narr", "conv", "pren")
texts  <- purrr::map_chr(genres, ~ {
  readLines(
    file.path("inst/extdata/validation", paste0(.x, ".txt")),
    encoding = "UTF-8"
  ) |> paste(collapse = " ")
})
names(texts) <- genres

# Contar tokens aproximados por género
tok_counts <- purrr::map_int(texts, ~ length(strsplit(.x, "\\s+")[[1]]))
message("Tokens aproximados por género:")
purrr::iwalk(tok_counts, ~ message(sprintf("  %-6s %d palabras", .y, .x))  )

# ── 3. Parsear ───────────────────────────────────────────────────────────────
message("\nAnotando con UDPipe…")
parsed <- udpipe_annotate(
  object  = ud_model,
  x       = texts,
  doc_id  = genres,
  tagger  = "default",
  parser  = "default"
)

# ── 4. Extraer features ──────────────────────────────────────────────────────
message("Extrayendo rasgos de Biber…")
features <- biber_es(parsed, measure = "MATTR", normalize = TRUE)

# ── 5. Mostrar resumen ───────────────────────────────────────────────────────
feat_cols <- grep("^f_", names(features), value = TRUE)
message(sprintf("\nResultado: %d documentos × %d rasgos", nrow(features), length(feat_cols)))
message("\nRasgos con valor > 0 por género:")
purrr::walk(genres, function(g) {
  n_nonzero <- sum(features[features$doc_id == g, feat_cols] > 0, na.rm = TRUE)
  message(sprintf("  %-6s %d / %d rasgos no nulos", g, n_nonzero, length(feat_cols)))
})

# ── 6. Guardar resultado ─────────────────────────────────────────────────────
saveRDS(features, "inst/extdata/validation/features_validation.rds")
write.csv(features, "inst/extdata/validation/features_validation.csv",
          row.names = FALSE)

message("\n✓ Features guardadas en inst/extdata/validation/")
