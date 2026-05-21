# Fase 2 — Auditoría rasgo por rasgo (positivo + negativo)

**Fecha:** 2026-05-21
**Commits referenciados:** `598d9e3` (baseline) · `7479f0e` (fix f_44) · `2766bec` (CONTRACT_CHECK) · `c7dd502` (test-feature-coverage extendido)
**Cobertura:** 67 rasgos (57 con detección real + 10 zero-output)
**Suite:** `tests/testthat/test-feature-coverage.R` + `tests/testthat/helper-feature-coverage.R`
**Estado de suite:** 0 FAIL · 362 PASS · 6 SKIP (4 BUG documentados + 2 PARCIAL spec)

## Leyenda

| Símbolo | Significado |
|---|---|
| ✅ OK | Caso positivo dispara (≥ pos_min), caso negativo no dispara (≤ neg_max) |
| ⚠️ PARCIAL | Detecta con ruido aceptable según `biber_espanol_completo.md` (spec autoriza el ruido o documenta limitación UDPipe) |
| ❌ FAIL | Bug confirmado contra spec. Skipeado con marcador `BUG f_XX` en la suite. Pendiente Fase 3 |
| 🚫 OVER | Falso positivo — la columna dispara cuando no debería |
| 0️⃣ ZERO-OK | Rasgo zero-output devuelve 0 con tres oraciones de control (incluyendo una en inglés) |

## Resumen ejecutivo (snapshot al cierre de Fase 2)

| Categoría | N | % |
|---|---|---|
| ✅ OK | **51** | 76.1 % |
| ⚠️ PARCIAL | 2 | 3.0 % |
| ❌ FAIL | 4 | 6.0 % |
| 🚫 OVER | 0 | 0 % |
| 0️⃣ ZERO-OK | 10 | 14.9 % |
| **Total** | **67** | 100 % |

**Bugs nuevos descubiertos en Fase 2 (no listados en Fase 1):** `f_06`, `f_23`.
**Bugs heredados de estabilización (Fase 0):** `f_24`, `f_47`.
**Parciales documentados por spec:** `f_22`, `f_50`.

> **Estado actualizado al cierre de Fase 3 (commit `8d285a5`):** los 4 ❌ pasaron a ✅. Estado final: **55 ✅ + 2 ⚠️ + 10 0️⃣ = 67**. Ver §"Estado post-Fase 3" al final del documento.

---

## Tabla 67-rasgos

| Rasgo | Tipo | Caso positivo | Caso negativo | Estado | Notas |
|---|---|---|---|---|---|
| f_01_past_tense | detección | "Juan llegó tarde y habló con ella." → ≥1 | "Juan hablaba mientras ella leía." → 0 | ✅ OK | |
| f_02_perfect_aspect | detección | "He terminado el informe esta mañana." → ≥1 | "Terminé el informe ayer." → 0 | ✅ OK | |
| f_03_present_tense | detección | "María trabaja en la oficina central." → ≥1 | "María trabajó ayer en casa." → 0 | ✅ OK | |
| f_04_place_adverbials | detección | "El perro está aquí y el gato allí afuera." → ≥1 | "Hoy comimos muy temprano." → 0 | ✅ OK | |
| f_05_time_adverbials | detección | "Llegó ayer y se marchará mañana temprano." → ≥1 | "El libro está aquí dentro." → 0 | ✅ OK | |
| f_06_first_person_pronouns | detección | "Yo creo que nosotros ganaremos." → ≥1 | "Comí pizza ayer." → debería 0, devuelve 1 | ❌ FAIL | Cuenta verbo Person=1 sin PRON explícito |
| f_07_second_person_pronouns | detección | "Tú sabes que usted tiene razón." → ≥1 | "Yo sé que él viene." → 0 | ✅ OK | |
| f_08_third_person_pronouns | detección | "Él la vio y ella le habló a ellos." → ≥1 | "Yo te di el libro a ti." → 0 | ✅ OK | |
| f_09_pronoun_it | zero-output | N/A | siempre 0 (3 probes incl. EN) | 0️⃣ ZERO-OK | |
| f_10_demonstrative_pronoun | detección | "Eso es importante. Esto me gusta mucho." → ≥1 | "Este libro es mío y esa casa es tuya." → 0 | ✅ OK | |
| f_11_indefinite_pronouns | detección | "Alguien llamó pero nadie respondió nada." → ≥1 | "Un perro y una casa grande." → 0 | ✅ OK | |
| f_12_proverb_do | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_13_wh_question | detección | "¿Qué quieres? ¿Dónde estás ahora?" → ≥1 | "La casa que compré es grande." → 0 | ✅ OK | |
| f_14_nominalizations | detección | "La producción y la evaluación del conocimiento." → ≥1 | "El lado del grado del partido." → 0 | ✅ OK | Regex sufijo NO matchea *lado/grado/partido* (verificado) |
| f_15_gerunds | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_16_other_nouns | métrico | "El perro corre en el parque con la pelota." → ≥2 | (no neg) | ✅ OK | Residual; sin neg test |
| f_17_agentless_passives | detección | "Se publicaron los resultados. La tarea fue realizada." → ≥1 | "Juan comió la manzana roja." → 0 | ✅ OK | Detecta perifrástica Y se-pasiva |
| f_18_by_passives | detección | "El libro fue escrito por María González." → ≥1 | "La tarea fue realizada rápido." → 0 | ✅ OK | |
| f_19_be_main_verb | detección | "El libro es interesante y la casa está limpia." → ≥1 | "El informe ha sido escrito hoy." → 0; "O sea, llegó tarde." → 0 | ✅ OK | Compound o_sea absorbe sea (test crítico verificado) |
| f_20_existential_there | detección | "Hay un problema grave en el sistema." → ≥1 | "Hay que esperar el resultado final." → 0 | ✅ OK | *Hay que* va a f_53, no f_20 |
| f_21_that_verb_comp | detección | "Dijo que vendría mañana sin falta." → ≥1 | "La casa que compré es vieja." → 0 | ✅ OK | |
| f_22_that_adj_comp | detección | "Es importante que vengas pronto." → (skip) | "Corrió porque tenía prisa." → 0 | ⚠️ PARCIAL | UDPipe: head de *que* no etiquetado ADJ en copulativa (§f_22) |
| f_23_wh_clause | detección | "No sé qué quieres ni dónde vives." → ≥1 | "El equipo que fue asignado finalizó..." → debería 0, devuelve 1 | ❌ FAIL | Cuenta *que* relativo sin tilde como wh-clause |
| f_24_infinitives | detección | "Quiero comer y necesito dormir ahora." → ≥1 | "Como pan todos los días." → 0; "Quiero terminar... para poder descansar." → debería 2, devuelve 3 | ❌ FAIL | Cuenta *poder* en perífrasis modal (es f_52, no f_24) |
| f_25_present_participle | detección | "Caminando por la calle, pensó en todo." → ≥1 | "Está hablando por teléfono ahora." → 0 | ✅ OK | Progresivo *estar+ger* excluido |
| f_26_past_participle | detección | "Terminada la reunión, todos se fueron." → ≥1 | "Ha terminado la reunión temprano." → 0 | ✅ OK | |
| f_27_past_participle_whiz | detección | "Los métodos utilizados fueron buenos." → ≥1 | "Terminada la sesión, salieron." → 0 | ✅ OK | |
| f_28_present_participle_whiz | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_29_that_subj | detección | "El hombre que vino ayer era alto." → ≥1 | "Dijo que vino tarde." → 0 | ✅ OK | Absorbe f_31 wh-subj |
| f_30_that_obj | detección | "El libro que María escribió es famoso." → ≥1 | "Es importante que vengas." → 0 | ✅ OK | Absorbe f_32 wh-obj |
| f_31_wh_subj | zero-output | N/A | siempre 0 (fusionado en f_29) | 0️⃣ ZERO-OK | |
| f_32_wh_obj | zero-output | N/A | siempre 0 (fusionado en f_30) | 0️⃣ ZERO-OK | |
| f_33_pied_piping | detección | "La casa en la que vivo. La persona con quien hablé. El motivo por el cual vino..." → ≥2 | "El libro que leí ayer." → 0 | ✅ OK | |
| f_34_sentence_relatives | detección | "Llegó tarde, lo cual molestó a todos." → ≥1 | "Lo que quieras está bien." → 0 | ✅ OK | |
| f_35_because | detección | "No vino porque estaba enfermo." → ≥1 | "Aunque llovía, salió igual." → 0 | ✅ OK | |
| f_36_though | detección | "Aunque llovía, salió a la calle." → ≥1 | "Porque llovía, no salió." → 0 | ✅ OK | |
| f_37_if | detección | "Si llueve mañana, no salgo." → ≥1 | "Preguntó si vendría a la fiesta." → 0 | ✅ OK | Distingue condicional vs indirecto |
| f_38_other_adv_sub | detección | "Mientras trabajaba, escuchaba música." → ≥1 | "Comió pan y bebió agua." → 0 | ✅ OK | |
| f_39_prepositions | métrico | "El libro de María está en la mesa con cuidado." → ≥3 | (no neg) | ✅ OK | |
| f_40_adj_attr | detección | "Un libro interesante y una casa grande." → ≥1 | "El libro es interesante." → 0 | ✅ OK | |
| f_41_adj_pred | detección | "El libro es interesante y útil." → ≥1 | "Un libro interesante llegó." → 0 | ✅ OK | |
| f_42_adverbs | métrico residual | "Habló rápidamente y actuó claramente." → ≥1 | (no neg) | ✅ OK | Residual verificado en CONTRACT_CHECK §3 |
| f_43_type_token | métrico | "El gato y el perro corren juntos felices." → 0–1 | (no neg) | ✅ OK | Ratio en [0, 1] |
| f_44_mean_word_length | métrico | worked example → 5.22 ± 0.01; "La producción de conocimiento riguroso." → 4–6 | (no neg) | ✅ OK | Fix `7479f0e`: todos no-PUNCT, sin filtro léxico |
| f_45_conjuncts | detección | "Llegó tarde. Sin embargo, lo intentó. Además ganó." → ≥1 | "Salió sin nada en las manos." → 0 | ✅ OK | Compound *sin_embargo* funciona |
| f_46_downtoners | detección | "Es casi imposible y apenas visible." → ≥1 | "Es muy claro y totalmente cierto." → 0 | ✅ OK | |
| f_47_hedges | detección | "Quizás venga. A lo mejor llega mañana." → debería ≥1, devuelve 0 | "Vendrá seguro y sin dudas." → 0 | ❌ FAIL | *quizás* no se detecta (spec §f_47 lo lista explícitamente) |
| f_48_amplifiers | detección | "Es muy bueno y totalmente claro." → ≥1 | "Es casi bueno y apenas claro." → 0 | ✅ OK | |
| f_49_emphatics | detección | "Realmente lo hizo. De verdad que sí." → ≥1 | "Lo hizo ayer por la tarde." → 0 | ✅ OK | Multi-token *de_verdad* funciona |
| f_50_discourse_particles | detección | "Bueno, o sea, es decir, llegó tarde." → ≥2 | "El alumno bueno estudió mucho." → (skip) | ⚠️ PARCIAL | Spec §f_50 autoriza ruido sin filtro posicional en baseline |
| f_51_demonstratives | detección | "Este libro y esa casa son nuevos." → ≥1 | "Eso es mío y esto es tuyo." → 0 | ✅ OK | |
| f_52_modal_possibility | detección | "Puedo ir y podríamos hablar luego." → ≥1 | "Tiene mucho poder político." → 0 | ✅ OK | |
| f_53_modal_necessity | detección | "Hay que esperar. Tengo que irme. Debe salir." → ≥2 | "Tengo un perro y un gato." → 0 | ✅ OK | Compounds *hay_que* y *tener_que* funcionan |
| f_54_modal_predictive | detección | "Hablará mañana. Vamos a comer pronto." → ≥1 | "Habló ayer y comió tarde." → 0 | ✅ OK | *ir_a + inf* funciona |
| f_55_verb_public | detección | "Dijo que sí y afirmó lo contrario." → ≥1 | "Comió pan y durmió bien." → 0 | ✅ OK | |
| f_56_verb_private | detección | "Creo que sé la respuesta y pienso mucho." → ≥1 | "Corrió rápido y saltó alto." → 0 | ✅ OK | |
| f_57_verb_suasive | detección | "Le pedí que viniera y recomendé esperar." → ≥1 | "Comió pan y bebió agua." → 0 | ✅ OK | |
| f_58_verb_seem | detección | "Parece que va a llover pronto." → ≥1 | "Apareció en la reunión tarde." → 0 | ✅ OK | Solo *parecer*; *aparecer* excluido (test crítico verificado) |
| f_59_contractions | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_60_that_deletion | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_61_stranded_preposition | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_62_split_infinitive | zero-output | N/A | siempre 0 | 0️⃣ ZERO-OK | |
| f_63_split_auxiliary | detección | "Ha probablemente sido analizado el caso." → ≥1 | "Fue mostrado el resultado." → 0 | ✅ OK | |
| f_64_phrasal_coordination | detección | "Juan y María vinieron temprano." → ≥1 | "Vino pero se fue rápido." → 0 | ✅ OK | |
| f_65_clausal_coordination | detección | "Llegó tarde. Y luego se fue." → ≥1 | "Juan y María vinieron." → 0 | ✅ OK | |
| f_66_neg_synthetic | detección | "Nadie vino y nada pasó. Nunca lo hizo." → ≥2 | "Vino alguien con algo." → 0 | ✅ OK | |
| f_67_neg_analytic | detección | "No vino y no lo hizo nunca." → ≥1 | "No obstante, finalmente llegó." → 0 | ✅ OK | Compound *no_obstante* absorbe el *no* |

---

## Diagnóstico por rasgo en estado distinto a ✅/0️⃣

### ❌ f_06_first_person_pronouns — bug nuevo (descubierto en Fase 2)

**Spec (`biber_espanol_completo.md` §f_06):**
- **Include**: lista léxica explícita de pronombres + filtro morfológico `Person=1`. La lista lista *yo, mí, me, conmigo, mi, mis, mío, ...*.
- **Exclude**: "Verbal first-person inflection without pronoun (e.g., *fui* alone — no detection; the verb conveys person but no pronoun exists)".

**Comportamiento actual:** la oración *"Comí pizza ayer."* (sin ningún PRON) devuelve `f_06 = 1`. El detector está disparando con la inflexión verbal `Person=1` aunque no haya pronombre.

**Hipótesis de causa:** en `R/features_tense_pronouns.R`, el filtro `Person=1` se aplica al universo de tokens sin restringir `upos == "PRON"`. Necesita un AND `upos %in% c("PRON")` (o lista equivalente, p.ej. también `DET` para *mi/nuestro/etc.*).

**Magnitud del sesgo:** sobreestimación sistemática en cualquier texto narrativo en primera persona — `f_06` quedará inflado tanto por los pronombres explícitos como por los verbos. Análogamente para `f_07` y `f_08` (pendiente verificar; los tests positivos del fc_cases pasan porque incluyen PRON explícitos junto a verbos conjugados).

**Prioridad Fase 3:** Prioridad 2 (rasgo crítico mencionado en el brief).

### ❌ f_23_wh_clause — bug nuevo (descubierto en Fase 2)

**Spec (`biber_espanol_completo.md` §f_23):**
- **Include**: "Accented interrogative words" (*qué, quién, quiénes, dónde, adónde, cuándo, cómo, cuál, cuáles, cuánto*) en **subordinada** y en **función argumental** (`ccomp` o `xcomp`), más la secuencia *lo que* argumental.
- **Exclude**: "Relative uses of the same words **without accents**".

**Comportamiento actual:** la oración *"El equipo que fue asignado finalizó el proyecto."* devuelve `f_23 = 1`. *que* sin tilde es relativo (acl:relcl en UDPipe), no wh.

**Hipótesis de causa:** el detector probablemente compara contra una lista que incluye *que* sin discriminar tilde, o usa `lemma` (donde UDPipe a veces normaliza la tilde) sin verificar el `token` superficial. La nota de la spec lo advierte: *"Known UDPipe issue: tilde diacritica is sometimes inconsistent in the parser's lemma output. May need to check both *quién* and *quien* and validate by parent verb being a cognition/communication verb."*

**Magnitud del sesgo:** sobreestimación severa en cualquier texto con cláusulas relativas (muy frecuentes). Se solapa funcionalmente con `f_29_that_subj`, que sí está bien.

**Prioridad Fase 3:** Prioridad 2 (rasgo crítico mencionado en el brief).

### ❌ f_24_infinitives — bug heredado de Fase 0

**Spec (`biber_espanol_completo.md` §f_24):**
- **Include**: `VerbForm=Inf` en función de complemento (`xcomp`, `ccomp`, o como objeto) + perífrasis verbales específicas (*ir a, empezar a, dejar de, volver a, acabar de*).
- **Exclude**: substantivized infinitives (a f_15) y usos imperativos.

**Comportamiento actual:** *"Quiero terminar el trabajo para poder descansar."* devuelve `f_24 = 3`. El detector cuenta *poder* (modal en perífrasis *para poder + inf*) cuando *poder* es modal de posibilidad → debe ir en `f_52`, no en `f_24`.

**Hipótesis de causa:** el filtro de "complemento" en el bloque de `f_24` admite *xcomp* de cualquier head incluyendo modales (*poder, querer, deber*) sin desambiguar con `f_52`/`f_53`.

**Prioridad Fase 3:** Prioridad 4 (no listado como crítico en el brief).

### ❌ f_47_hedges — bug heredado de Fase 0

**Spec (`biber_espanol_completo.md` §f_47):**
- **Include**: "Single tokens: *quizás, quizá, tal vez, acaso, aproximadamente, aparentemente*"; multi-token *a_lo_mejor, más_o_menos, algo_así*; cuantitativo *unos/unas + NUM*.

**Comportamiento actual:** *"Quizás el resultado depende de otros factores."* devuelve `f_47 = 0`. Falla con un lema listado explícitamente en la spec.

**Hipótesis de causa:** posibles candidatos a inspeccionar — (a) el lema viene como *quizá* sin tilde por normalización del parser y la lista solo trae la forma con tilde; (b) el bloque depende del POS y *quizás* viene tagged como ADV en posición que el filtro excluye; (c) la lista de hedges single-token quedó solo con la versión multi-token al integrar el compound.

**Prioridad Fase 3:** Prioridad 3 (rasgo multi-token; antes de fixear, ejecutar diagnóstico del compound recomendado en el brief).

### ⚠️ f_22_that_adj_comp — limitación UDPipe documentada por spec

**Comportamiento:** el caso positivo *"Es importante que vengas pronto."* no devuelve ≥ 1 — UDPipe spanish-gsd no etiqueta el head de *que* como ADJ en copulativas; lo etiqueta como adjetivo predicativo con dependencia distinta, lo que rompe el filtro estructural.

**Decisión spec:** documentado en `biber_espanol_completo.md` §f_22 como divergencia conocida del parser. No es bug; el test está skipeado con motivo explícito.

**Acción:** ninguna — se mantiene como ⚠️.

### ⚠️ f_50_discourse_particles — ruido aceptable por spec

**Comportamiento:** el caso negativo *"El alumno bueno estudió mucho."* devuelve `f_50 ≥ 1` (cuenta *bueno* adjetivo como partícula).

**Decisión spec:** `biber_espanol_completo.md` §f_50 dice explícitamente *"recommend starting with no positional filter"* — el ruido en `bueno` adjetivo es aceptable en el baseline. Tuning por corpus queda fuera del paquete.

**Acción:** ninguna — se mantiene como ⚠️.

---

## Tests críticos del brief explícitamente verificados

| Test crítico | Estado |
|---|---|
| f_06–f_08 pro-drop | ❌ FAIL en f_06 ("Comí pizza" → 1). f_07/f_08 pasan tests fc_cases pero no fueron probados con su análogo de pro-drop puro |
| f_14 regex no matchea *lado/grado/partido/sentido* | ✅ verificado en fc_cases neg |
| f_17 detecta se-pasiva | ✅ "Se publicaron los resultados" → 1 |
| f_19 vs f_50 — *sea* en *o sea* | ✅ "O sea, llegó tarde" → f_19 = 0 |
| f_20 vs f_53 — *hay que* | ✅ "Hay que esperar" → f_20 = 0 (va a f_53) |
| f_23 relativa *que* sin tilde | ❌ FAIL — devuelve 1 cuando debería 0 |
| f_29/f_30 absorben f_31/f_32 | ✅ f_31/f_32 siempre 0; f_29/f_30 disparan |
| f_44 worked example (= 5.22) | ✅ corregido en `7479f0e` |
| f_45/f_47/f_49/f_50/f_53/f_54 multi-token | ✅ excepto f_47 (BUG); compounds *sin_embargo*, *de_verdad*, *o_sea*, *hay_que*, *tener_que*, *ir_a*, *a_lo_mejor* funcionan |
| f_58 solo *parecer*, no *aparecer* | ✅ "Apareció en la reunión" → 0 |
| f_67 *no* dentro de *no obstante* | ✅ "No obstante, finalmente llegó" → 0 |

---

## Recomendación de orden para Fase 3

Conforme tu plan original de prioridades:

1. **Prioridad 1 — estructura:** ya cerrada en Fase 1 (commit `7479f0e`). Nada pendiente.

2. **Prioridad 2 — rasgos críticos del brief:**
   - **`f_06`**: filtro `Person=1` necesita AND `upos == "PRON"` (o lista de POS que incluya DET para posesivos). Verificar f_07/f_08 con tests análogos antes/después del fix.
   - **`f_23`**: filtro debe exigir tilde diacrítica explícita en el token superficial y excluir `dep_rel == "acl:relcl"`.

3. **Prioridad 3 — rasgo multi-token:**
   - **`f_47`**: diagnóstico inicial recomendado por el brief — ejecutar `udpipe_annotate("Quizás venga.")` e inspeccionar lema/upos de *quizás* antes de fixear el bloque.

4. **Prioridad 4 — restantes:**
   - **`f_24`**: desambiguar perífrasis modal: excluir infinitivos cuyo head sea modal (`f_52`/`f_53`) ya capturado.

---

**Estado al cierre de Fase 2:** documentación lista. No se aplicaron fixes (conforme al brief). Cuatro tests skipped con marcador `BUG f_XX` mantienen la suite verde y los bugs visibles. Espera tu aprobación + lista priorizada antes de empezar Fase 3.

---

## Estado post-Fase 3 (apéndice)

**Commit final:** `8d285a5`.

| Rasgo | Estado Fase 2 | Estado Fase 3 | Commit del fix |
|---|---|---|---|
| `f_06_first_person_pronouns` | ❌ | ✅ | `bcbf08c` |
| `f_23_wh_clause` | ❌ | ✅ | `76bc5c2` |
| `f_24_infinitives` | ❌ | ✅ | `04e3f4b` |
| `f_47_hedges` | ❌ | ✅ | `8d285a5` |

**Tabla post-Fase 3:**

| Categoría | N | % |
|---|---|---|
| ✅ OK | **55** | 82.1 % |
| ⚠️ PARCIAL (autorizado por spec) | 2 | 3.0 % |
| ❌ FAIL | 0 | 0 % |
| 🚫 OVER | 0 | 0 % |
| 0️⃣ ZERO-OK | 10 | 14.9 % |
| **Total** | **67** | 100 % |

**Diagnósticos sorpresa de Fase 3** (donde el bug real difería de la hipótesis de Fase 2):

- **`f_24`**: la hipótesis original era "cuenta *poder* en perífrasis modal". El bug real era que UDPipe mis-etiqueta el verbo raíz (*Quiero*) como `VerbForm=Inf` cuando es Fin. Fix quirúrgico: exigir AUX finito hijo para infinitivos en `root`.
- **`f_47`**: la hipótesis era "lema mal listado o filtro POS". El bug real era de encoding/locale: `quanteda::tokens` en locale "C" borra `Encoding="UTF-8"` y convierte *quizás* en bytes crudos `"quiz<c3><a1>s"` que no matchean el dict. Fix: forzar `LC_CTYPE=UTF-8` durante la llamada de `biber_es()`. Tiene efecto colateral positivo sobre todos los rasgos basados en dict con acentos.
- **`f_06`**: la hipótesis era "filtro Person=N sin restricción a PRON". Parcialmente correcto: la pasada léxica principal sí filtraba, pero un *supplement* morfológico paralelo no exigía que la forma/lema fuera realmente pronominal. Fix: el supplement ahora exige `tolower(token) %in% known_pronoun_forms | lemma %in% known_pronoun_forms`.

**Validación end-to-end:** ver `VALIDATION_REPORT.md`. Cero regresiones, pass rate global 93.9 %, pass rate por texto 100 %.
