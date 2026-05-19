# MULTIWORD_DIAGNOSIS.md — Auditoría Fase 3

**Paquete:** pseudobibeR.es · **Fecha:** 2026-05-19
**Objetivo:** verificar que el sistema de detección de locuciones
multi-token funciona en producción (no falla en silencio).

## Sección A — Arquitectura real (no la asumida por el plan)

El plan asumía `compose_spanish_multiwords()` + `multiword_expressions.yaml`
(estilo pseudobibeR.fr). **Esos archivos no existen.** La arquitectura
real, verificada en código, son **dos mecanismos**:

1. **Rama léxica (quanteda):** `parse_functions.R:184-208` reconstruye el
   texto, `quanteda::tokens()`, `tokens_compound()` con `multiword_patterns`
   (auto-derivado en `build_french_dictionaries.R:77` desde las entradas
   con `_` de `dict.yaml` + `word_lists.yaml`), luego `tokens_lookup()`.
   Corre **una vez al inicio**, antes de cualquier `block_*_es()`.
2. **Rama sintáctica:** `flag_mwe_tokens()` (`utils_extraction.R`) marca
   `in_mwe` sobre el dataframe parseado; filtros `next_token`/`prev_token`
   en los bloques (perífrasis `hay_que`/`tener_que`/`ir_a`, etc.).

`fr_analysis_notes.md` no fue necesario: el conocimiento de arquitectura
está en `biber_espanol_completo.md §2.3`.

## Sección B — Test diagnóstico

Texto:
> "O sea, no obstante las dificultades, hay que seguir. Sin embargo, a lo
> mejor mañana tendremos noticias. De verdad que, en fin, vamos a esperar.
> Es decir, mientras que él trabaja, ella descansa."

| Rasgo | Esperado | Observado | Estado |
|---|---|---|---|
| f_45_conjuncts | ≥2 | 4 (sin embargo, no obstante, o sea, es decir) | ✅ |
| f_47_hedges | ≥1 | 1 (a lo mejor) | ✅ |
| f_49_emphatics | ≥1 | 1 (de verdad) | ✅ |
| f_50_discourse_particles | ≥3 | 3 (o sea, es decir, en fin) | ✅ |
| f_53_modal_necessity | ≥1 | 1 (hay que) | ✅ |
| f_54_modal_predictive | ≥1 | 2 (vamos a + futuro) | ✅ |
| f_38_other_adv_sub | ≥1 | 0 → **1 tras fix** | ✅ |

**Regresiones críticas (deben pasar):**

| Chequeo | Esperado | Observado | Estado |
|---|---|---|---|
| f_19: "sea" en *o sea* NO cuenta como ser | 0 | 0 | ✅ |
| f_20: *hay que* NO cuenta como existencial | 0 | 0 | ✅ |

## Sección C — Hallazgo y fix

**f_38 (`mientras que`)** era el único fallo. Diagnóstico 5 pasos:

1. ¿Corre el compound step? Sí (`parse_functions.R:195`).
2. ¿`multiword_patterns` tiene la entrada? `mientras_que` está en
   `word_lists.yaml:604`.
3. ¿Formato dict correcto? No hay sección `f_38` en `dict.yaml` — f_38 se
   detecta **por código** (`block_clause_embedding_es`), no por lookup.
4. ¿El bloque lee el campo correcto? **Aquí estaba el bug**: el código
   exigía `pos ∈ {SCONJ,ADP,ADV}` + `dep_rel=mark`. En "mientras que"
   Spanish-GSD etiqueta `mientras`=**CCONJ**/mark y `que`=SCONJ/**fixed**.
   Ninguno casaba.
5. Orden de ejecución correcto.

**Fix aplicado** (commit `a284f81`): rama adicional `CCONJ + mark` con
allowlist `{mientras, conforme, según}` (evita capturar coordinantes
y/o/pero/ni). Verificado: `mientras que`=1, `mientras` simple=1, `y`=0.

## Sección D — Conclusión

El sistema multi-token **funciona correctamente**. 7/7 rasgos del
diagnóstico pasan tras el fix de f_38. Las dos regresiones críticas
(f_19 *o sea*, f_20 *hay que*) pasan. No se encontraron fallos
silenciosos en la infraestructura de compound — el único problema era
una rama de código de f_38 que no contemplaba el etiquetado CCONJ de
UDPipe para `mientras`.
