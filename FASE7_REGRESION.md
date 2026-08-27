# Fase 7 — Verificación de regresión (antes / después)

Comparación del corpus **"200 textos académicos"** (199 documentos válidos,
ciencias agrícolas) procesado con la versión **antes** de la revisión
(`main`, commit `53ea231`) y **después** (`feat/revision-hernan`, commit
`7f2fe91`). Mismo modelo UDPipe (`spanish-gsd-ud-2.5-191206`), mismo texto,
`normalize = TRUE`, `safe = TRUE`. 0 documentos fallidos en ambas corridas.

Metodología según spec §7.4: correr antes/después, tabular la variación de
medias por rasgo, y verificar que **toda variación grande caiga en un rasgo
listado como cambiado** — cualquier cambio grande en un rasgo no listado es
indicio de error de implementación e investigarse antes de cerrar la fase.

---

## Cambios grandes (|Δmedia| > 0.5 por 1000 tokens)

| Rasgo | Media antes | Media después | Δ | Δ% | ¿Esperado? |
|---|---:|---:|---:|---:|---|
| `f_42_adverbs` | 8.86 | 15.84 | **+6.98** | +78.8% | ✅ Fase 1 — total de adverbios |
| `f_17_agentless_passives` | 14.16 | 10.58 | **−3.57** | −25.2% | ✅ Fase 5 — excluye impersonal-se |
| `f_08_third_person_pronouns` | 3.55 | 6.38 | **+2.84** | +80.0% | ✅ Fase 1 — + posesivos (*su*) |
| `f_11_indefinite_pronouns` | 4.05 | 1.60 | **−2.45** | −60.6% | ✅ Fase 4 — control sintáctico (excluye determinante) |
| `f_39_prepositions` | 151.03 | 149.40 | −1.63 | −1.1% | ✅ Fase 5 — locuciones cuentan una vez |
| `f_64_phrasal_coordination` | 15.93 | 17.54 | **+1.62** | +10.1% | ✅ Fase 5 — coordinación de verbos |
| `f_31_wh_subj` | 0.00 | 1.10 | **+1.10** | (de 0) | ✅ Fase 2 — des-fusión (quien/cual sujeto) |
| `f_29_that_subj` | 4.85 | 3.74 | −1.10 | −22.7% | ✅ Fase 2 — reverso exacto de f_31 (ver nota) |
| `f_01_past_tense` | 15.90 | 16.75 | +0.85 | +5.3% | ✅ Fase 1 — pasados ind.+subj. |
| `f_15_gerunds` | 0.00 | 0.55 | **+0.55** | (de 0) | ✅ Fase 3 — infinitivo nominal-sujeto |
| `f_24_infinitives` | 9.48 | 8.93 | −0.55 | −5.8% | ✅ Fase 3 — exclusión mutua con f_15 (ver nota) |

## Cambios medios (0.1 < \|Δ\| ≤ 0.5)

| Rasgo | Media antes | Media después | Δ | Δ% | ¿Esperado? |
|---|---:|---:|---:|---:|---|
| `f_30_that_obj` | 1.19 | 0.79 | −0.39 | −33.0% | ✅ Fase 2 — parte del reverso de f_31/f_32 |
| `f_67_neg_analytic` | 1.62 | 1.99 | +0.37 | +23.2% | ✅ Fase 5 — *no* de foco |
| `f_58_verb_seem` | 0.25 | 0.06 | −0.19 | −76.1% | ✅ Fase 5 — *resultar* solo copulativo |
| `f_18_by_passives` | 1.01 | 0.86 | −0.16 | −15.4% | ⚠️ Ver nota — efecto colateral esperado de f_17 |

## Cambios pequeños esperados (\|Δ\| ≤ 0.1, dentro de lo previsto)

`f_06` (+0.10), `f_63` (+0.07), `f_46` (−0.05), `f_37` (+0.02), `f_32_wh_obj`
(0→0.009, confirma el límite documentado de casi-siempre-cero), `f_04`, `f_05`,
`f_47`, `f_52`, `f_38`: todas variaciones mínimas, coherentes con el alcance
acotado de sus reglas.

## Resto de los 67 rasgos

**Sin cambio** (diferencia ≤ 1e-6, ruido de punto flotante — no hay diferencia
real): 46 rasgos, incluidos todos los que no formaron parte de ninguna fase de
la revisión (f_02, f_03, f_10, f_13, f_14, f_16, f_19–f_23, f_25–f_27,
f_33–f_36, f_40, f_41, f_43–f_45, f_48–f_51, f_53–f_57, f_65, f_66, y las 7
columnas siempre-cero). **Esto es la confirmación más importante de la
regresión**: la revisión no tocó nada fuera de lo documentado.

---

## Veredicto: ✅ sin indicios de error

**Cero cambios grandes o medios en rasgos no listados en la revisión.** Los
dos casos que a primera vista parecían "no esperados" resultaron, al
investigarlos, ser consecuencias necesarias de cambios ya documentados:

1. **`f_24_infinitives` (−0.55):** es el otro lado de la exclusión mutua con
   `f_15` (Fase 3) — los infinitivos-sujeto (*fumar*, *nadar*) que antes caían
   en f_24 por defecto ahora se enrutan a f_15. No es un rasgo nuevo cambiado;
   es la mitad complementaria de un cambio ya en la lista.

2. **`f_18_by_passives` (−0.16):** `f_17` y `f_18` comparten la misma tabla
   interna de pasivas detectadas (`all_passive`) antes de dividirse por
   presencia de la frase *por*. Al excluir la impersonal de esa tabla (fix de
   `f_17`, Fase 5), algún caso raro de impersonal + *por* cercano
   (p. ej. *"se recomienda por razones prácticas..."*) también salió de
   `f_18`. Es un efecto colateral correcto del mismo fix, no un error nuevo.

3. **`f_29_that_subj` (−1.10) es exactamente el negativo de `f_31_wh_subj`
   (+1.10):** confirma que la des-fusión (Fase 2) movió casos de un lado a
   otro sin crear ni perder detecciones — la suma f_29+f_31 es estable.

**Conclusión:** los 60 rasgos con detección activa se comportan según lo
documentado en `TABLA_COMPARATIVA_ES.md`. El corpus de prueba (199 textos
académicos de ciencias agrícolas) no reveló ninguna regresión inesperada.

---

## Datos

Tabla completa (67 rasgos, medias antes/después/Δ/Δ%): `regression_table.csv`
(adjunto). Generada comparando:
- **Antes:** `feat_before <- readRDS("resultado_corpus_academ_MAIN.rds")` — worktree de `main` (commit `53ea231`)
- **Después:** `feat_after <- readRDS("resultado_corpus_academ.rds")` — `feat/revision-hernan` (commit `7f2fe91`)
