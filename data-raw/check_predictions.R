library(dplyr)
library(purrr)
library(tibble)

features <- readRDS("inst/extdata/validation/features_validation.rds")

# Helper: extraer valor de un rasgo para un género
# Si hay colisión .x/.y, toma el máximo de los dos
get_f <- function(genre, feat) {
  row <- features[features$doc_id == genre, , drop = FALSE]
  # Buscar columna exacta o variantes .x/.y
  candidates <- c(feat,
                  paste0(feat, ".x"), paste0(feat, ".y"),
                  paste0(feat, ".1"), paste0(feat, ".2"))
  cols <- intersect(candidates, names(row))
  if (length(cols) == 0) return(NA_real_)
  vals <- as.numeric(unlist(row[, cols, drop = TRUE]))
  max(vals, na.rm = TRUE)
}

results <- list()

# ── Predicciones de Biber (1988) adaptadas al español ────────────────────────

# P1: Pasado más alto en narrativa que en académico
results[["P1_past_narr_gt_acad"]] <- list(
  desc    = "f_01 (imperfecto) narr > acad",
  passed  = get_f("narr","f_01_past_tense") > get_f("acad","f_01_past_tense"),
  narr    = get_f("narr","f_01_past_tense"),
  acad    = get_f("acad","f_01_past_tense")
)

# P2: Presente más alto en académico que en narrativa
results[["P2_present_acad_gt_narr"]] <- list(
  desc    = "f_03 (presente) acad > narr",
  passed  = get_f("acad","f_03_present_tense") > get_f("narr","f_03_present_tense"),
  acad    = get_f("acad","f_03_present_tense"),
  narr    = get_f("narr","f_03_present_tense")
)

# P3: Nominalizaciones más altas en académico
results[["P3_nomin_acad_highest"]] <- list(
  desc    = "f_14 (nominalizaciones) acad es el más alto",
  passed  = get_f("acad","f_14_nominalizations") ==
            max(unlist(features[, "f_14_nominalizations"])),
  values  = setNames(features$f_14_nominalizations, features$doc_id)
)

# P4: Pronombres 1ª persona más altos en conversacional
results[["P4_1p_conv_highest"]] <- list(
  desc    = "f_06 (1ª persona) conv es el más alto",
  passed  = get_f("conv","f_06_first_person_pronouns") ==
            max(unlist(features[, "f_06_first_person_pronouns"])),
  values  = setNames(features$f_06_first_person_pronouns, features$doc_id)
)

# P5: Pronombres 2ª persona más altos en conversacional
results[["P5_2p_conv_highest"]] <- list(
  desc    = "f_07 (2ª persona) conv es el más alto",
  passed  = get_f("conv","f_07_second_person_pronouns") ==
            max(unlist(features[, "f_07_second_person_pronouns"])),
  values  = setNames(features$f_07_second_person_pronouns, features$doc_id)
)

# P6: Preposiciones más altas en académico
results[["P6_prep_acad_gt_conv"]] <- list(
  desc    = "f_39 (preposiciones) acad > conv",
  passed  = get_f("acad","f_39_prepositions") > get_f("conv","f_39_prepositions"),
  acad    = get_f("acad","f_39_prepositions"),
  conv    = get_f("conv","f_39_prepositions")
)

# P7: Pasivas sin agente más altas en académico
results[["P7_agentless_pass_acad_gt_conv"]] <- list(
  desc    = "f_17 (pasivas sin agente) acad > conv",
  passed  = get_f("acad","f_17_agentless_passives") > get_f("conv","f_17_agentless_passives"),
  acad    = get_f("acad","f_17_agentless_passives"),
  conv    = get_f("conv","f_17_agentless_passives")
)

# P8: TTR más alto en académico que en conversacional
results[["P8_ttr_acad_gt_conv"]] <- list(
  desc    = "f_43 (TTR) acad > conv",
  passed  = get_f("acad","f_43_type_token") > get_f("conv","f_43_type_token"),
  acad    = get_f("acad","f_43_type_token"),
  conv    = get_f("conv","f_43_type_token")
)

# P9: Longitud media de palabra más alta en académico
results[["P9_wordlen_acad_gt_conv"]] <- list(
  desc    = "f_44 (long. media) acad > conv",
  passed  = get_f("acad","f_44_mean_word_length") > get_f("conv","f_44_mean_word_length"),
  acad    = get_f("acad","f_44_mean_word_length"),
  conv    = get_f("conv","f_44_mean_word_length")
)

# P10: Verbos privados más altos en conversacional
results[["P10_private_verbs_conv_gt_acad"]] <- list(
  desc    = "f_56 (verbos privados) conv > acad",
  passed  = get_f("conv","f_56_verb_private") > get_f("acad","f_56_verb_private"),
  conv    = get_f("conv","f_56_verb_private"),
  acad    = get_f("acad","f_56_verb_private")
)

# P11: Partículas discursivas más altas en conversacional
results[["P11_discourse_conv_highest"]] <- list(
  desc    = "f_50 (partículas discursivas) conv es el más alto",
  passed  = get_f("conv","f_50_discourse_particles") ==
            max(unlist(features[, "f_50_discourse_particles"])),
  values  = setNames(features$f_50_discourse_particles, features$doc_id)
)

# P12: Subordinadas causales más altas en académico
results[["P12_causal_acad_gt_conv"]] <- list(
  desc    = "f_35 (causales) acad > conv",
  passed  = get_f("acad","f_35_because") > get_f("conv","f_35_because"),
  acad    = get_f("acad","f_35_because"),
  conv    = get_f("conv","f_35_because")
)

# P13: Adjetivos atributivos más altos en académico
results[["P13_attr_adj_acad_gt_conv"]] <- list(
  desc    = "f_40 (adj. atributivos) acad > conv",
  passed  = get_f("acad","f_40_adj_attr") > get_f("conv","f_40_adj_attr"),
  acad    = get_f("acad","f_40_adj_attr"),
  conv    = get_f("conv","f_40_adj_attr")
)

# P14: Ser/estar como verbo principal más alto en conversacional
results[["P14_be_main_conv_gt_acad"]] <- list(
  desc    = "f_19 (ser/estar principal) conv > acad",
  passed  = get_f("conv","f_19_be_main_verb") > get_f("acad","f_19_be_main_verb"),
  conv    = get_f("conv","f_19_be_main_verb"),
  acad    = get_f("acad","f_19_be_main_verb")
)

# P15: Negación analítica más alta en conversacional
results[["P15_neg_analytic_conv_gt_acad"]] <- list(
  desc    = "f_67 (negación analítica) conv > acad",
  passed  = get_f("conv","f_67_neg_analytic") > get_f("acad","f_67_neg_analytic"),
  conv    = get_f("conv","f_67_neg_analytic"),
  acad    = get_f("acad","f_67_neg_analytic")
)

# P16: Complementos de verbo con 'que' más altos en académico
results[["P16_that_verb_acad_gt_conv"]] <- list(
  desc    = "f_21 (comp. verb que) acad > conv",
  passed  = get_f("acad","f_21_that_verb_comp") > get_f("conv","f_21_that_verb_comp"),
  acad    = get_f("acad","f_21_that_verb_comp"),
  conv    = get_f("conv","f_21_that_verb_comp")
)

# P17: Verbos públicos más altos en periodístico (>= conversacional)
results[["P17_public_verbs_pren_highest"]] <- list(
  desc    = "f_55 (verbos públicos) pren >= conv",
  passed  = get_f("pren","f_55_verb_public") >= get_f("conv","f_55_verb_public"),
  values  = setNames(features$f_55_verb_public, features$doc_id)
)

# P18: Modales de posibilidad más altos en académico (hedging epistémico)
results[["P18_possibility_modal_acad_gt_narr"]] <- list(
  desc    = "f_52 (modales posibilidad) acad > narr",
  passed  = get_f("acad","f_52_modal_possibility") > get_f("narr","f_52_modal_possibility"),
  acad    = get_f("acad","f_52_modal_possibility"),
  narr    = get_f("narr","f_52_modal_possibility")
)

# P19: Demostrativos pronominales > 0 en al menos 3 géneros
results[["P19_demonstr_not_zero"]] <- list(
  desc    = "f_10 (dem. pronominales) > 0 en al menos 3 géneros",
  passed  = sum(features$f_10_demonstrative_pronoun > 0) >= 3,
  values  = setNames(features$f_10_demonstrative_pronoun, features$doc_id)
)

# P20: f_54 predictivos: narr < 3 × acad (sanity check post-fix)
results[["P20_f54_narr_not_absurd"]] <- list(
  desc    = "f_54 (predictivos) narr < 3 × acad (sanity check post-fix)",
  passed  = get_f("narr","f_54_modal_predictive") <
            3 * max(get_f("acad","f_54_modal_predictive"), 0.1),
  narr    = get_f("narr","f_54_modal_predictive"),
  acad    = get_f("acad","f_54_modal_predictive")
)

# ── Resumen ──────────────────────────────────────────────────────────────────
passed_n <- sum(purrr::map_lgl(results, function(r) isTRUE(r$passed)))
total    <- length(results)

message("\n── Validación Biber (1988) — pseudobibeR.es ──────────────────────")
message(sprintf("Predicciones verificadas: %d / %d\n", passed_n, total))

purrr::iwalk(results, function(r, name) {
  icon <- if (isTRUE(r$passed)) "\u2705" else "\u274c"
  message(sprintf("%s  %-45s", icon, r$desc))
  # Mostrar valores relevantes
  vals <- r[!names(r) %in% c("desc","passed")]
  if (length(vals) > 0) {
    for (nm in names(vals)) {
      v <- vals[[nm]]
      if (is.numeric(v) && length(v) == 1) {
        message(sprintf("     %-8s %.3f", nm, v))
      } else if (is.numeric(v)) {
        message(sprintf("     %s", paste(names(v), round(v, 3), sep="=", collapse="  ")))
      }
    }
  }
})

# ── Rasgos con 0 en todos los géneros ────────────────────────────────────────
# Solo columnas sin sufijo .x/.y para el análisis de ceros
feat_cols_all <- grep("^f_", names(features), value = TRUE)
# Colapsar pares .x/.y: quedarse con el nombre canónico
feat_cols <- unique(sub("\\.(x|y|1|2)$", "", feat_cols_all))
all_genres <- c("acad", "narr", "conv", "pren")
all_zero <- feat_cols[sapply(feat_cols, function(f) {
  v <- sapply(all_genres, function(g) get_f(g, f))
  all(v == 0, na.rm = TRUE)
})]
message(sprintf("\nRasgos con valor 0 en todos los géneros (%d):", length(all_zero)))
if (length(all_zero) > 0) {
  for (f in all_zero) message(sprintf("  %s", f))
} else {
  message("  Ninguno")
}

# ── Tabla de valores brutos (rasgos clave) ────────────────────────────────────
key_feats <- c(
  "f_01_past_tense", "f_03_present_tense", "f_06_first_person_pronouns",
  "f_07_second_person_pronouns", "f_14_nominalizations", "f_17_agentless_passives",
  "f_19_be_main_verb", "f_35_because", "f_39_prepositions",
  "f_40_adj_attr", "f_43_type_token", "f_44_mean_word_length",
  "f_50_discourse_particles", "f_52_modal_possibility",
  "f_55_verb_public", "f_56_verb_private", "f_67_neg_analytic",
  "f_71_preterit"
)
key_feats <- intersect(key_feats, names(features))

message("\nValores brutos — rasgos clave (por 1000 tokens, excl. TTR y long. media):")
message(sprintf("%-35s %8s %8s %8s %8s", "Rasgo", "acad", "narr", "conv", "pren"))
message(strrep("-", 67))
for (f in key_feats) {
  message(sprintf("%-35s %8.2f %8.2f %8.2f %8.2f", f,
    get_f("acad", f), get_f("narr", f),
    get_f("conv", f), get_f("pren", f)))
}

# ── Guardar reporte ──────────────────────────────────────────────────────────
report <- purrr::map_dfr(names(results), function(nm) {
  r <- results[[nm]]
  tibble::tibble(
    prediccion  = nm,
    descripcion = r$desc,
    passed      = isTRUE(r$passed)
  )
})
write.csv(report, "inst/extdata/validation/validation_report.csv", row.names = FALSE)

# Tabla completa por género (usar columnas que existen en el data.frame)
avail_cols <- intersect(c("doc_id", feat_cols), names(features))
feat_matrix <- features[, avail_cols]
write.csv(feat_matrix, "inst/extdata/validation/features_by_genre.csv", row.names = FALSE)

message(sprintf("\n✓ Reporte guardado. Resultado: %d / %d predicciones verificadas.",
                passed_n, total))
