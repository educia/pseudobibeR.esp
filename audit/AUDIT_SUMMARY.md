# Audit Summary — pseudobibeR.es

**Periodo:** 2026-05-20 → 2026-05-21
**Rama:** `main`
**Commit final:** `8d285a5`

## Métricas

- **Total features audited:** 67 (57 con detección + 10 zero-output)
- **Detection features OK:** 53 / 57
- **Detection features with acceptable noise (⚠️):** 2 / 57 (`f_22`, `f_50` — autorizados por spec)
- **Detection features failing (❌):** 0 / 57 — todos los bugs detectados en Fase 2 fueron cerrados en Fase 3
- **Zero-output columns verified (0️⃣):** 10 / 10
- **Contract check (Fase 1):** ✅ 67 columnas, sin sufijos espurios, f_43/f_44 dentro de spec
- **Global validation pass rate:** **93.9 %** (170 OK + 11 TOLERANCE / 181)
- **Pass rate per text:** 100 % en cada uno de los 8 textos (FAIL=0 en todos)
- **devtools::test() final:** 0 FAIL / 371 PASS / 4 SKIP / 0 WARN

## Commits aplicados

| Commit | Tipo | Asunto |
|---|---|---|
| `598d9e3` | chore | stabilize test baseline for audit |
| `7479f0e` | fix | f_44: use all non-PUNCT tokens per spec §3.J |
| `2766bec` | docs | audit Fase 1 contract check + post-fix re-check |
| `c7dd502` | test | extender test-feature-coverage para Fase 2 |
| `bcbf08c` | fix | f_06: supplement morfológico debe filtrar por forma/lema conocido |
| `76bc5c2` | fix | f_23: exigir tilde superficial + excluir heads acl/acl:relcl |
| `04e3f4b` | fix | f_24: root\|Inf solo cuenta si tiene AUX finito hijo |
| `8d285a5` | fix | f_47: forzar LC_CTYPE=UTF-8 en biber_es para que quanteda matchee acentos |

8 commits totales. 5 fixes de rasgos críticos. 1 estabilización. 1 extensión de test suite. 1 documentación.

## Entregables generados

- `audit/CONTRACT_CHECK.md` — Fase 1: verificación del contrato de 67 columnas + fix de `f_44`.
- `audit/FEATURE_AUDIT.md` — Fase 2: tabla 67-rasgos con estado (✅/⚠️/❌/0️⃣) + diagnóstico por bug.
- `audit/VALIDATION_REPORT.md` — Fase 4: pass rate global, pass rate por texto, comparación con baseline.
- `audit/AUDIT_SUMMARY.md` — este documento.
- `tests/testthat/test-feature-coverage.R` — extendido: 10 zero-output + 4 críticos puntuales + 5 regression-guards de los fixes.
- `tests/testthat/helper-feature-coverage.R` — `run_biber()` ahora acepta `normalize = FALSE/TRUE`.

## Resumen de cambios sustantivos en el código

| Archivo | Cambio |
|---|---|
| `R/features_lexical_complexity.R` | `f_44`: todos los tokens excepto PUNCT, sin umbral de longitud mínima, `nchar(type="chars")` explícito (spec §3.J). |
| `R/features_stance.R` | `f_06–f_08` supplement morfológico: filtro adicional por forma/lema en lista conocida de pronombres. |
| `R/features_subordination.R` | `f_23`: exigir tilde en forma superficial + excluir heads `acl`/`acl:relcl`. `f_24`: `root\|Inf` solo cuenta con AUX finito hijo. |
| `R/biber_es.R` | Forzar `LC_CTYPE=UTF-8` durante la llamada con restauración via `on.exit` (necesario para que quanteda no corrompa acentos en locale C). |
| `R/parse_functions.R` | Lower-casing previo con `stringi::stri_trans_tolower` antes de pasar a quanteda. |
| `data-raw/spanish_examples.yaml` | Borrar entradas stale de `f_69`/`f_71`; corregir `f_42` count 3→2. |
| `tests/testthat/test-spanish-examples.R` | Encoding UTF-8 explícito al leer YAML; quitar `f_71` de relaxed; mecanismo `bug_skipped` con `cli_inform` (ahora vacío). |

## Verificación de entregables (checklist del brief)

- [x] `audit/CONTRACT_CHECK.md` — verificación del contrato de 67 columnas
- [x] `audit/FEATURE_AUDIT.md` — estado rasgo por rasgo (✅/⚠️/❌/🚫/0️⃣)
- [x] `audit/VALIDATION_REPORT.md` — resultado del sistema de validación
- [x] `audit/AUDIT_SUMMARY.md` — pass rate ≥ 90 %, este documento
- [x] `tests/testthat/test-feature-coverage.R` actualizado
- [x] Historial de commits granular con mensajes descriptivos
- [x] `biber_es()` devolviendo exactamente 67 columnas
- [x] 10 columnas zero-output retornando exactamente 0
- [x] `f_44` devolviendo valores en rango 4–6 con todos los tokens excepto puntuación
- [ ] `devtools::check()` sin warnings — *pendiente ejecutar; los WARN previos de "strings not representable in native encoding" desaparecieron con el fix de locale en `8d285a5`. Recomiendo ejecutar `devtools::check()` como verificación final independiente.*

## Hallazgos relevantes no listados en el brief

1. **El bug real de `f_06` era arquitectónico** (supplement morfológico sin filtro de lema/forma), no un caso aislado de pro-drop. Sin embargo, **`f_07` y `f_08` no compartían el bug** porque el supplement coincidía con la lista léxica en esos casos. El fix unificó el comportamiento añadiendo el filtro por forma/lema conocido y dejó los tres simétricos.

2. **El bug real de `f_24` no era *poder* en perífrasis modal** (mi hipótesis inicial en Fase 2). UDPipe spanish-gsd mis-etiqueta el verbo finito raíz (*Quiero*) como `VerbForm=Inf`. El detector debe distinguir entre `root|Inf` legítimo (perífrasis con AUX hijo, ej. *"Se debe seguir"*) y `root|Inf` espurio (sin AUX hijo).

3. **El bug de `f_47` era de encoding/locale**, no de diccionario. `quanteda::tokens` borra la marca `Encoding="UTF-8"` en locale "C", convirtiendo *"quizás"* en bytes crudos `"quiz<c3><a1>s"` que no matchean el dict. Afectaba a **todos los lemas con acento** en locales no-UTF-8 — no solo a *quizás*. El fix en `biber_es()` (forzar `LC_CTYPE=UTF-8`) tiene efecto colateral positivo en cualquier rasgo basado en diccionario con acentos.

4. **`f_44` filtraba por categoría léxica** (NOUN/VERB/ADJ/ADV) cuando la spec §3.J pide todos los tokens excepto puntuación. Magnitud del sesgo: ~67 % inflado en el worked example oficial (8.75 vs 5.22 esperado).

## Estado final del paquete

`pseudobibeR.es` cumple ahora su contrato superficial 67-columnas y su contrato lingüístico spec-compatible para los rasgos con detección activa. Quedan documentadas como ⚠️ las dos limitaciones autorizadas por spec (`f_22`, `f_50`) que no son bugs. La suite de tests cubre los 67 rasgos con casos positivos/negativos y 5 regression-guards específicos para los bugs cerrados en Fase 3.
