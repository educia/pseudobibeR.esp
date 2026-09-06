# Fase 8/9 (v2) — Verificación de regresión (antes / después)

Comparación del corpus **"200 textos académicos"** (199 documentos válidos,
ciencias agrícolas) procesado con la versión **antes** de esta segunda ronda
de revisión (commit `0d63895`, fix de f_21/f_22 — última confirmada antes de
Fase 8) y **después** (commit `30c21cc`, cierre de Fase 9 documentación).
Mismo modelo UDPipe (`spanish-gsd-ud-2.5-191206`), mismo texto,
`normalize = TRUE`, `safe = TRUE`. 0 documentos fallidos en ambas corridas.

Metodología idéntica a la Fase 7 (`FASE7_REGRESION.md`): correr antes/después
vía `git worktree`, tabular la variación de medias por rasgo, y verificar que
**toda variación caiga en un rasgo listado como cambiado** — cualquier cambio
en un rasgo no listado sería indicio de regresión y debía investigarse antes
de cerrar la fase.

---

## Cambios observados (todos los rasgos con |Δmedia| > 0)

| Rasgo | Media antes | Media después | Δ | Δ% | ¿Esperado? |
|---|---:|---:|---:|---:|---|
| `f_29_that_subj` | 3.7447 | 4.8473 | **+1.1026** | +29.4% | ✅ 8.2 — revert: vuelve a fusionar *quien/cual* sujeto |
| `f_31_wh_subj` | 1.1026 | 0.0000 | **−1.1026** | −100% | ✅ 8.2 — revert: vuelve a ser siempre-cero (reverso exacto de f_29) |
| `f_30_that_obj` | 0.7948 | 1.1862 | **+0.3914** | +49.2% | ✅ 8.2 — revert: vuelve a fusionar *quien/cual* CD |
| `f_41_adj_pred` | 3.4268 | 3.6631 | **+0.2363** | +6.9% | ✅ 8.1 — ampliado a complementos predicativos |
| `f_55_verb_public` | 6.3834 | 6.5828 | **+0.1994** | +3.1% | ✅ 8.5.B — `decir` agregado al diccionario |
| `f_64_phrasal_coordination` | 17.5442 | 17.7395 | **+0.1953** | +1.1% | ✅ 8.5.D — coordinación ADJ+*pero* cuando el 1º es `amod` |
| `f_43_type_token` | 0.6254 | 0.6549 | **+0.0294** | +4.7% | ✅ 8.3 — TTR/MATTR incluye puntuación |
| `f_32_wh_obj` | 0.0090 | 0.0000 | −0.0090 | −100% | ✅ 8.2 — revert: vuelve a ser siempre-cero |

## Resto de los 67 rasgos: sin cambio (Δ = 0 exacto, no solo por debajo de un umbral)

Los **59 rasgos restantes** —incluidos `f_06`/`f_07`/`f_08` (afectados en
teoría por el fix de encoding de `05f223e`, pero sin ningún caso en este
corpus que dependa del fallback morfológico) y `f_39_prepositions` (afectado
en teoría por el fix de deduplicación de locuciones con *al/del* de 8.5.A,
pero sin ninguna ocurrencia de esa combinación específica en este corpus)—
dieron **exactamente el mismo valor** antes y después, incluidas las 10
columnas siempre-cero y los rasgos sin cambio de código (`f_18`, `f_57`).

---

## Veredicto: ✅ sin indicios de regresión

**Cero cambios en rasgos no listados en el plan de la revisión v2.** Los 7
rasgos que cambiaron son exactamente los 7 anticipados por el plan (8.1 f_41,
8.2 f_29/f_30/f_31/f_32, 8.3 f_43, 8.5.B f_55, 8.5.D f_64); el octavo listado
como posible (`f_39`) y los tres de la nota sobre el fix de encoding
(`f_06`/`f_07`/`f_08`) no mostraron cambio en este corpus concreto porque no
contiene el patrón léxico que dispara esos fixes — lo cual es consistente con
que ambos fixes sean correcciones de casos de borde (contracciones *al/del*
en locuciones; formas acentuadas en un fallback que rara vez se activa), no
cambios de alcance general. No se investigó nada adicional: no hubo ninguna
variación fuera de lo previsto.

Ver `FASE7_REGRESION.md` para la comparación de la primera ronda de la
revisión (Fases 1–7), y `TABLA_COMPARATIVA_ES.md` para el detalle rasgo por
rasgo de ambas rondas combinadas.
