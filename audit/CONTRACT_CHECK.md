# Fase 1 — Verificación del contrato de salida

**Fecha:** 2026-05-21
**Commit baseline:** `598d9e3` — *chore: stabilize test baseline for audit*
**Paquete:** `pseudobibeR.es` (working tree post-estabilización)
**Modelo UDPipe:** `tests/testthat/fixtures/spanish-gsd-ud-2.5-191206.udpipe` (modelo oficial del paquete, confirmado en `README.md` §requisitos y `R/biber_es.R` ejemplos)

---

## 1. Script ejecutado

```r
suppressPackageStartupMessages(devtools::load_all(".", quiet = TRUE))
ud <- udpipe::udpipe_load_model("tests/testthat/fixtures/spanish-gsd-ud-2.5-191206.udpipe")
test <- udpipe::udpipe_annotate(ud, x = "Prueba de contrato.") |> as.data.frame()
result <- biber_es(test, normalize = FALSE)

# 1. Total de columnas con prefijo f_ → debe ser 67
n_features <- length(grep("^f_", names(result)))
cat("Columnas f_*:", n_features, "(esperado: 67)\n")

# 2. Ninguna columna numerada > f_67
extras <- grep("^f_(6[89]|7[0-9]|8[0-9]|9[0-9])", names(result), value = TRUE)
cat("Columnas > f_67 (esperado: 0):", length(extras), "\n")
if (length(extras) > 0) print(extras)

# 3. Las 10 zero-output existen y devuelven 0
zero_cols <- c(
  "f_09_pronoun_it", "f_12_proverb_do", "f_15_gerunds",
  "f_28_present_participle_whiz", "f_31_wh_subj", "f_32_wh_obj",
  "f_59_contractions", "f_60_that_deletion",
  "f_61_stranded_preposition", "f_62_split_infinitive"
)
missing <- setdiff(zero_cols, names(result))
cat("Zero-output ausentes (esperado: 0):", length(missing), "\n")
if (length(missing) > 0) print(missing)
present <- zero_cols[zero_cols %in% names(result)]
nonzero_in_zero <- present[vapply(present,
  function(c) as.numeric(result[[c]][1]) != 0, logical(1))]
cat("Zero-output con valor distinto de 0 (esperado: 0):", length(nonzero_in_zero), "\n")
if (length(nonzero_in_zero) > 0) print(nonzero_in_zero)

# 4. f_43 y f_44 NO normalizados
result_norm <- biber_es(test, normalize = TRUE)
cat("f_43 normalizado:", result_norm$f_43_type_token,
    "(debe ser ratio 0-1, no escalado)\n")
cat("f_44 normalizado:", result_norm$f_44_mean_word_length,
    "(debe estar en rango 4-6)\n")

# 5. Sin duplicados con sufijos espurios
dup_patterns <- grep("_(rate|count|raw)$", names(result), value = TRUE)
cat("Columnas con sufijos espurios (esperado: 0):", length(dup_patterns), "\n")
if (length(dup_patterns) > 0) print(dup_patterns)
```

> **Nota sobre el path del modelo:** el script de tu brief usaba `"spanish-gsd-ud-2.5-191206.udpipe"` (raíz). El modelo vive físicamente en `tests/testthat/fixtures/`. Adapté la ruta para que el script sea reproducible desde repo root; el binario es el mismo modelo oficial referenciado en `README.md` líneas 84, 111 y 242.

## 2. Output literal

```
Columnas f_*: 67 (esperado: 67)
Columnas > f_67 (esperado: 0): 0
Zero-output ausentes (esperado: 0): 0
Zero-output con valor distinto de 0 (esperado: 0): 0
f_43 normalizado: 1 (debe ser ratio 0-1, no escalado)
f_44 normalizado: 7 (debe estar en rango 4-6)
Columnas con sufijos espurios (esperado: 0): 0
```

## 3. Verificación adicional — worked example de f_44 (spec §3.J)

El valor de f_44 = 7 sobre `"Prueba de contrato."` está fuera del rango esperado (4–6). Para discriminar entre ruido por oración corta vs. bug real, repetí con la oración que la spec usa como worked example:

```r
texto <- "El informe fue redactado por el equipo de investigación."
parsed <- udpipe::udpipe_annotate(ud, x = texto) |> as.data.frame()

# Manual conforme spec §3.J: todos los tokens excepto PUNCT
tok_no_punct <- parsed[parsed$upos != "PUNCT", "token"]
sum(nchar(tok_no_punct)) / length(tok_no_punct)   # → 47/9 = 5.2222

# Producido por biber_es:
biber_es(parsed, normalize = FALSE)$f_44_mean_word_length   # → 8.75
biber_es(parsed, normalize = TRUE)$f_44_mean_word_length    # → 8.75
```

**Esperado por spec §3.J:** `5.22` (47 chars / 9 tokens no-puntuación).
**Observado:** `8.75`.

**Diagnóstico:** el detector filtra por categoría léxica antes de promediar. Solo cuenta NOUN/VERB/ADJ/ADV:
- *informe* (7) + *redactado* (9) + *equipo* (6) + *investigación* (13) = **35 chars / 4 tokens = 8.75** ✓

Tu brief identifica este patrón explícitamente: *"Si devuelve 7.6 o 8.75 → bug de filtrado por categoría léxica"*. El bug está confirmado.

Como diagnóstico secundario, el `7` que dio `"Prueba de contrato."` también encaja con el mismo filtrado léxico:
- *Prueba* (6) + *contrato* (8) = 14 chars / 2 tokens léxicos = 7.0.
- Conforme spec: 6 + 2 + 8 = 16 / 3 = 5.33.

## 4. Discrepancias contra el contrato

| Check | README.md §1 / spec | Observado | Estado |
|---|---|---|---|
| Total columnas f_* | 67 | 67 | ✅ OK |
| Columnas > f_67 | 0 | 0 | ✅ OK (ninguna extensión interna f_68+ filtrada) |
| Zero-output presentes (10) | 10 | 10 | ✅ OK |
| Zero-output con valor ≠ 0 | 0 | 0 | ✅ OK |
| Sufijos espurios `_rate/_count/_raw` | 0 | 0 | ✅ OK |
| `f_43` no normalizado por normalize=TRUE | ratio igual | igual (0.8889 vs 0.8889 en worked example) | ✅ OK |
| `f_44` no normalizado por normalize=TRUE | rango 4–6 con todos los tokens excepto PUNCT | 8.75 con filtrado léxico | ❌ **FAIL** |

### Hallazgo único: `f_44_mean_word_length` viola §3.J

- **Contrato (`biber_espanol_completo.md` §3.J):** "implementación debe usar **todos los tokens excepto puntuación**, no solo léxicos, sin umbral de longitud mínima".
- **Código actual:** filtra por categoría léxica (NOUN/VERB/ADJ/ADV).
- **Worked example:** spec → 5.22; código → 8.75.
- **Severidad:** estructural. Cualquier corpus va a recibir longitudes infladas ~60-70% respecto a lo documentado, lo que distorsiona comparaciones con corpora medidas según la spec.
- **Prioridad de corrección (según tu Fase 3):** **Prioridad 2** ("f_44 si devuelve fuera del rango 4–6 (filtrado incorrecto)").

## 5. Conclusión (pre-fix)

Contrato superficial 67-columnas: **OK**.
Contrato lingüístico de `f_44`: **❌ FAIL** — discrepancia documentada arriba.

Decisión tomada: **opción 1** del menú original (arreglar `f_44` antes de pasar a Fase 2, dado que la spec lo cataloga como crítico y que arrastrarlo contaminaría las comparaciones de densidad léxica durante toda la auditoría).

---

## 5.1 Post-fix re-check

**Commit del fix:** `7479f0e` — *fix(f_44): use all non-PUNCT tokens per spec §3.J*

### Cambio aplicado

`R/features_lexical_complexity.R` §f_44:
- `lex_toks` → `toks` (el dataframe pre-filtrado por `!PUNCT_UPOS` que ya existía en el mismo scope).
- Eliminado `dplyr::filter(tok_len >= 2)`.
- `nchar(as.character(token))` → `nchar(as.character(token), type = "chars")` (explícito, blinda contra locales no-UTF-8).

`R/parse_functions.R` fallback (líneas 520-526): **sin tocar** — es el path francés legacy que solo dispara si el bloque español no se ejecutó; no aplica al contrato `biber_es()`.

### Re-ejecución del script de Fase 1

```
Columnas f_*: 67 (esperado: 67)
Columnas > f_67 (esperado: 0): 0
Zero-output ausentes (esperado: 0): 0
Zero-output con valor distinto de 0 (esperado: 0): 0
f_43 normalizado: 1 (debe ser ratio 0-1, no escalado)
f_44 normalizado: 5.333 (debe estar en rango 4-6)
Columnas con sufijos espurios (esperado: 0): 0
```

`f_44` sobre `"Prueba de contrato."` pasa de **7** → **5.333** (chars: *Prueba* 6 + *de* 2 + *contrato* 8 = 16 / 3 = 5.333). Ya dentro del rango 4–6.

### Re-ejecución del worked example oficial (spec §3.J)

`"El informe fue redactado por el equipo de investigación."`:

| | Antes | Después | Esperado spec |
|---|---|---|---|
| `f_44` (normalize=FALSE) | 8.75 | **5.222** | 5.22 ✅ |
| `f_44` (normalize=TRUE)  | 8.75 | **5.222** | 5.22 ✅ (invariante) |

### Tests de regresión añadidos

`tests/testthat/test-feature-coverage.R`:

```r
test_that("f_44 uses all non-PUNCT tokens per spec §3.J", {
  r <- run_biber("El informe fue redactado por el equipo de investigación.")
  expect_equal(round(as.numeric(r$f_44_mean_word_length), 2), 5.22,
               info = "Spec: 47 chars / 9 tokens no-PUNCT = 5.22")
})

test_that("f_44 is invariant under normalize=TRUE (ratio, not count)", {
  texto <- "El informe fue redactado por el equipo de investigación."
  r_raw  <- run_biber(texto, normalize = FALSE)
  r_norm <- run_biber(texto, normalize = TRUE)
  expect_equal(as.numeric(r_raw$f_44_mean_word_length),
               as.numeric(r_norm$f_44_mean_word_length),
               info = "f_44 no debe re-escalarse por normalize=TRUE")
})
```

`tests/testthat/helper-feature-coverage.R`: `run_biber()` extendido para aceptar `normalize = FALSE/TRUE` (default `FALSE`, sin romper los 65 tests existentes).

### Suite completa post-fix

```
devtools::test()
[ FAIL 0 | WARN 1 | SKIP 4 | PASS 299 ]
```

+2 PASS respecto al baseline post-estabilización (los dos tests de regresión añadidos). 0 regresiones.

## 6. Estado final de Fase 1

| Check | Estado |
|---|---|
| 67 columnas f_* | ✅ |
| Sin columnas > f_67 | ✅ |
| 10 zero-output presentes con valor 0 | ✅ |
| Sin sufijos espurios | ✅ |
| `f_43` no normalizado por normalize=TRUE | ✅ |
| `f_44` ∈ rango 4–6 con todos los tokens excepto PUNCT | ✅ (5.333 / worked example 5.222) |

**Contrato superficial 67-columnas + contrato lingüístico de f_43/f_44: OK.**

Procedo a Fase 2 sin esperar más confirmación, conforme tu instrucción explícita: *"actualiza el CONTRACT_CHECK.md con el output post-fix … y entonces sí, pasa a Fase 2 sin esperar más confirmación."*
