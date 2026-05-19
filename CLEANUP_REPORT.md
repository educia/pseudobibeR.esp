# CLEANUP_REPORT.md — Auditoría Fase 1 (delta)

**Paquete:** pseudobibeR.es
**Fecha:** 2026-05-18
**Modo:** audit delta (Q1=A). Los fixes de extensiones f_69/f_70/f_71 y
doble-normalización f_43/f_44 **ya se aplicaron en esta sesión** (commits
`ec88749`, `6d84dbe`); este reporte refleja el estado **post-fix** real y
se concentra en el barrido de código/archivos/tests obsoletos.

---

## Sección A — Columnas de salida actuales vs. 57 esperadas

Verificado con `biber_es(udpipe_annotate(ud,"Esto es una prueba simple."), normalize=FALSE)`:

| Métrica | Valor | Esperado | Estado |
|---|---|---|---|
| Columnas con prefijo `f_` | **57** | 57 | ✅ |
| Columnas totales | 60 | 57 + `doc_id` + `n_tokens` + `n_lex_tokens` | ✅ |
| Extensiones f_68/69/70/71 | **0** | 0 | ✅ |
| Columnas `f_31_*`/`f_32_*` (deben estar fusionadas) | **0** | 0 | ✅ |
| Columnas duplicadas `_rate` | **0** | 0 | ✅ |
| f_43/f_44 normalizadas con `normalize=TRUE` | NO (estables) | NO | ✅ |

**Conclusión:** la salida ya cumple el contrato de 57 rasgos de
`biber_espanol_completo.md §1`. La cabecera del checklist §5 (líneas
1406–1413 del documento) pasa en su totalidad.

---

## Sección B — Rasgos espurios detectados

| Rasgo espurio | Estado | Detalle |
|---|---|---|
| `f_69_mente_adverbs(_rate)` | ✅ **RESUELTO esta sesión** | Eliminado de `canonical_order` en `parse_functions.R`; filtro defensivo `grep("^f_(69|70|71)(_|$)")` añadido |
| `f_70_long_words(_rate)` | ✅ **RESUELTO esta sesión** | Íd. Eliminaba además el valor absurdo `f_70_long_words_rate=76923` (doble normalización) |
| `f_71_preterit` | ✅ **RESUELTO esta sesión** | Íd. Extensión no-Biber retirada del output |
| Doble normalización f_43/f_44 | ✅ **RESUELTO esta sesión** | `normalize_counts()` en `utils_extraction.R` excluye `f_43_type_token`, `f_44_mean_word_length`, `n_tokens`, `n_lex_tokens` |

**`data-raw/dict.yaml`:** sin entradas para rasgos eliminados
(f_09/12/15/28/31/32/59/60/61/62/68/69/70/71). Limpio. ✅

No quedan rasgos espurios en la **salida**. El trabajo restante de Fase 1
es código/tests/archivos vestigiales que ya no se ejecutan o no aplican al
paquete español.

---

## Sección C — Funciones/código obsoleto a eliminar

### C1. Rama `language == "fr"` completa — **código muerto inalcanzable** (ALTA prioridad)

- `R/biber_es.R:163` invoca **siempre** `parse_biber_features(..., language = "es")`.
- `NAMESPACE` exporta **solo** `biber_es` (no existe `biber_fr`).
- `R/parse_functions.R` contiene un bloque `if (language == "fr") { ... }`
  (aprox. líneas 234–367) que llama a **14 funciones `block_*_fr`
  distintas** que **no están definidas en `R/`**:
  `block_adj_prep_adv_fr, block_aux_tense_fr, block_clause_embedding_fr,
  block_contractions_fr, block_lexical_membership_fr, block_modals_fr,
  block_negation_fr, block_participial_clauses_fr, block_passive_voice_fr,
  block_personal_pronouns_fr, block_relatives_fr, block_specialized_verbs_fr,
  block_split_coordination_fr, block_stranded_split_fr`.
- Si esa rama llegara a ejecutarse → error `could not find function`.
  Es inalcanzable porque `language` está fijado a `"es"`.

**Propuesta:** eliminar la rama `fr`, simplificar la firma
`parse_biber_features(... language = c("fr","es"))` → sin parámetro
`language` (o fijar `"es"`), y retirar las constantes solo-fr del bloque
`else` (`weather_lemmas`/`raising_verbs`/`wh_question_lemmas`/
`subject_pron_lemmas` versión francesa). **Cambio invasivo** (~140 líneas);
requiere tu aprobación explícita.

### C2. Stubs de rasgos eliminados (BAJA prioridad)

| Función | Archivo | Rasgo | Estado |
|---|---|---|---|
| `block_contractions_es` | `features_coordination_negation.R:46` | f_59 (eliminado) | Stub: "devuelve doc_ids sin columnas adicionales" |
| `block_stranded_split_es` | `features_coordination_negation.R:70` | f_61/f_62 (eliminados) | Stub vestigial |

Son inocuos (no añaden columnas) pero ejecutan trabajo nulo y confunden.
**Propuesta:** eliminar definición + llamada en `parse_functions.R` tras
confirmar que no aportan columnas (test de regresión: 57 cols se mantiene).

---

## Sección D — Entradas de diccionario obsoletas

- `data-raw/dict.yaml`: **ninguna** entrada de rasgo eliminado. Limpio.
- `data-raw/word_lists.yaml`: solo **comentarios-cabecera con numeración
  antigua** (líneas 352 `# (f_60)`, 503–507 `# (f_61–f_64)`, 570
  `# (f_28)`). No son datos, son etiquetas de sección desactualizadas.
  **Propuesta:** renumerar comentarios a la nomenclatura vigente (cosmético,
  sin efecto funcional).

---

## Sección E — Tests obsoletos a eliminar

10 archivos `tests/testthat/test-french-*.R` prueban comportamiento
**francés** en un paquete que solo exporta `biber_es` (español):

```
test-french-aux-pronouns.R        test-french-modals.R
test-french-contractions.R        test-french-passives-clauses.R
test-french-coordination.R        test-french-regressions.R
test-french-edge-cases.R          test-french-relatives-participles.R
test-french-examples.R            test-french-udpipe.R
```

Más fixtures francesas: `tests/testthat/fixtures/french_edge_case_features.csv`,
`tests/testthat/helper-udpipe.R` (modelo `french-gsd`).

**A revisar (no eliminar aún):** `tests/testthat/test-spanish-basic.R`
referencia f_59/f_61/f_62/f_31_/f_32_ (rasgos eliminados/fusionados).
Requiere lectura para decidir si se reescribe o elimina.

**Propuesta:** eliminar los 10 `test-french-*.R` + fixture francesa;
reescribir `helper-udpipe.R` a `spanish-gsd`; auditar
`test-spanish-basic.R` en Fase 2.

---

## Sección F — Archivos sospechosos por nombre (revisar, no eliminar)

| Archivo | Observación |
|---|---|
| `R/pseudobibeR.fr-package.R` | Doc de paquete nombrado `.fr` dentro del paquete `.es` |
| `data-raw/build_french_dictionaries.R` | **Funcional** — construye `dict.rda`/`word_lists.rda` español pese al nombre. NO eliminar; renombrar opcional |
| `data-raw/build_french_register_corpus.R` | Verificar uso |
| `data-raw/build_french_samples.R` | Verificar uso |
| `data-raw/extract_french_lists.R` | Probable leftover del fork FR |
| `data-raw/french_udpipe_probe.R` | Probable leftover |
| `data-raw/analyze_apostrophe_patterns.R` | Apóstrofes — fenómeno francés, no español |

**No hay** archivos `_old/_v1/_legacy/_backup/_deprecated/.bak/~`. ✅

---

## Sección G — Estadística resumen

| Categoría | Cantidad | Acción propuesta |
|---|---|---|
| Rasgos espurios en salida | 0 (ya resuelto sesión) | — |
| Rama de código muerta (`fr`) | ~140 líneas, 14 fns inexistentes | Eliminar (C1, requiere aprobación) |
| Stubs vestigiales (`block_*_es`) | 2 | Eliminar (C2) |
| Entradas dict obsoletas | 0 datos / ~4 comentarios | Renumerar comentarios (D) |
| Tests obsoletos | 10 `test-french-*.R` + 1 fixture | Eliminar (E) |
| Tests a revisar | 1 (`test-spanish-basic.R`) | Auditar en Fase 2 |
| Archivos sospechosos por nombre | 7 | Revisar; mayoría conservar |
| Archivos `_old/_v1/...` | 0 | — |

**Diff de superficie estimado:** −10 archivos test, −1 fixture, −~140
líneas (rama fr), −2 funciones stub, ~4 comentarios renumerados.
La salida funcional de `biber_es()` **no cambia** (sigue 57 rasgos);
el cambio es eliminación de código inalcanzable y tests no aplicables.

---

## Próximo paso — REQUIERE TU APROBACIÓN (Q7=A, único gate)

Antes de borrar nada, confirma qué ejecuto:

1. **C1** — eliminar rama `language=="fr"` + 14 llamadas a `block_*_fr`
   inexistentes + constantes fr (invasivo, ~140 líneas).
2. **C2** — eliminar stubs `block_contractions_es` / `block_stranded_split_es`.
3. **E** — eliminar 10 `test-french-*.R` + fixture francesa + reapuntar
   `helper-udpipe.R` a spanish-gsd.
4. **D** — renumerar comentarios stale en `word_lists.yaml` (cosmético).

Responde p.ej. "ejecuta 2,3,4 / pospón 1" o "todo" o "solo 3".
Tras tu OK ejecuto la limpieza con commits granulares y verifico:
`devtools::check()` sin warnings de código no documentado, `biber_es()`
sigue 57 columnas, tests restantes pasan.
