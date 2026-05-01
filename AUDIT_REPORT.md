# AUDIT_REPORT.md — pseudobibeR.es
## Auditoría rasgo por rasgo (Fase 1)

**Referencia**: `biber_espanol_completo.md` (fuente de verdad)  
**Fecha**: 2026-04-30  
**Alcance**: 67 rasgos f_01–f_67 + rasgos extra en el código

---

## Resumen ejecutivo

| Estado | Cantidad |
|--------|----------|
| ✅ Correcto (coincide con documento) | 26 |
| ⚠️ Implementado con discrepancia | 17 |
| ❌ Intraducible implementado (debe eliminarse) | 8 |
| ➕ Extra no contemplado en documento | 4 |
| **Total columnas generadas actualmente** | **~71** |
| **Total correcto según documento** | **59** (67 − 8 intraducibles) |

---

## Tabla completa de los 67 rasgos

| ID | Nombre | Estado esperado | Estado en código | Acción requerida |
|----|--------|-----------------|-----------------|-----------------|
| f_01 | Past tense | ✅ Transferible (= pretérito indefinido, Tense=Past) | ⚠️ **Implementado como imperfecto (Tense=Imp)** | CORREGIR: intercambiar lógica de f_01 y f_71 |
| f_02 | Perfect aspect | ✅ Transferible (haber+participio) | ⚠️ Incluye `estar` como auxiliar además de `haber` | REVISAR: solo haber según documento |
| f_03 | Present tense | ✅ Transferible | ✅ Correcto (Tense=Pres+Mood=Ind+VerbForm=Fin) | — |
| f_04 | Place adverbials | ✅ Transferible | ✅ Lista léxica + filtro POS | — |
| f_05 | Time adverbials | ✅ Transferible | ✅ Lista léxica + filtro POS | — |
| f_06 | First person pronouns | ✅ Transferible | ✅ Lista léxica de lemmas explícitos | — |
| f_07 | Second person pronouns | ✅ Transferible | ✅ Incluye tú/vos/usted/ustedes/vosotros | — |
| f_08 | Third person pronouns | ✅ Transferible | ✅ él/ella/ellos/lo/la/le/les | — |
| f_09 | Pronoun *it* | ❌ Intraducible | ❌ **Implementado** (haber impersonal + ello + se expl) | ELIMINAR columna f_09 |
| f_10 | Demonstrative pronouns | ✅ Transferible | ✅ demonstrative_matchlist + POS=PRON | — |
| f_11 | Indefinite pronouns | ✅ Transferible | ✅ Lista léxica desde dict.yaml | — |
| f_12 | Pro-verb *do* | ❌ Intraducible | ❌ **Implementado** (hacer+clítico lo/la) | ELIMINAR columna f_12 |
| f_13 | Direct WH-questions | ✅ Transferible | ✅ wh_lemmas + filtro "?" en oración | — |
| f_14 | Nominalizations | ✅ Transferible | ✅ Sufijos -ción/-miento/-idad/-ez/-ura | — |
| f_15 | Gerunds | ❌ Intraducible | ❌ **Implementado** (VerbForm=Ger como NOUN) | ELIMINAR columna f_15; ajustar f_16 |
| f_16 | Other nouns | ✅ Transferible | ⚠️ Resta f_14 y f_15 (f_15 no debería restarse) | CORREGIR: f_16 = total NOUN − f_14 solo |
| f_17 | Agentless passives | ✅ Transferible (+se-pasiva) | ✅ Perifrástica + se-pasiva, sin agente "por" | — |
| f_18 | By-passives | ✅ Transferible | ✅ Pasiva + frase "por" | — |
| f_19 | Be as main verb | ✅ Transferible | ✅ ser+estar como verbo principal, excluye aux | — |
| f_20 | Existential *there* | ✅ Transferible | ✅ haber impersonal sin nsubj | — |
| f_21 | That verb complements | ✅ Transferible | ✅ que SCONJ+mark con head VERB/AUX | — |
| f_22 | That adj complements | ✅ Transferible | ✅ que SCONJ+mark con head ADJ | — |
| f_23 | WH-clauses | ✅ Transferible | ✅ wh_lemmas en función argumental | — |
| f_24 | Infinitives | ✅ Transferible | ✅ VerbForm=Inf con dep_rel clausal | — |
| f_25 | Present participial clauses | ✅ Transferible | ✅ VerbForm=Ger + dep_rel advcl/ccomp | — |
| f_26 | Past participial clauses | ✅ Transferible | ✅ VerbForm=Part + dep_rel advcl/ccomp/acl | — |
| f_27 | Past participial WHIZ | ✅ Transferible | ✅ VerbForm=Part + dep_rel acl + head NOUN | — |
| f_28 | Present participial WHIZ | ❌ Intraducible | ❌ **Implementado** (VerbForm=Ger+acl+NOUN) | ELIMINAR columna f_28 |
| f_29 | That relatives (sujeto) | ✅ Transferible | ⚠️ No fusionado con f_31 | FUSIONAR f_29+f_31 |
| f_30 | That relatives (objeto) | ✅ Transferible | ⚠️ No fusionado con f_32 | FUSIONAR f_30+f_32 |
| f_31 | WH-relatives (sujeto) | ⚠️ Problemático (fusionar con f_29) | ❌ Implementado por separado | FUSIONAR en f_29 |
| f_32 | WH-relatives (objeto) | ⚠️ Problemático (fusionar con f_30) | ❌ Implementado por separado | FUSIONAR en f_30 |
| f_33 | Pied-piping | ✅ Transferible | ✅ ADP inmediatamente antes de relativo | — |
| f_34 | Sentence relatives | ✅ Transferible | ✅ lo que/lo cual con antecedente neutro | — |
| f_35 | Because | ✅ Transferible (**solo** *porque*) | ❌ **Lista excesiva**: ya que, puesto que, dado que, pues, como | CORREGIR: solo "porque" |
| f_36 | Though | ✅ Transferible (**solo** *aunque*) | ❌ **Lista excesiva**: si bien, a pesar de que, aun cuando, etc. | CORREGIR: solo "aunque" |
| f_37 | If | ✅ Transferible (si + a menos que + salvo que) | ⚠️ Lista extendida (en caso de que, siempre que...) no en documento | REVISAR: reducir a si/a_menos_que/salvo_que |
| f_38 | Other adv subordinators | ✅ Transferible | ⚠️ Residual de todo SCONJ+mark; puede sobrecontar | REVISAR alcance |
| f_39 | Prepositions | ✅ Transferible | ✅ ADP + dep_rel case/fixed | — |
| f_40 | Attributive adjectives | ✅ Transferible | ✅ ADJ + dep_rel amod | — |
| f_41 | Predicative adjectives | ✅ Transferible | ⚠️ Falta **quedarse** y **permanecer**; tiene hacerse/tornarse (no en documento) | CORREGIR linking_verbs |
| f_42 | Adverbs | ✅ Transferible | ✅ Total ADV − stance/hedges/negación | — |
| f_43 | Type-token ratio | ✅ Transferible | ✅ MATTR / TTR vía quanteda | — |
| f_44 | Mean word length | ✅ Transferible | ✅ nchar sobre tokens léxicos | — |
| f_45 | Conjuncts | ✅ Transferible | ✅ lista léxica multi-token en dict.yaml | — |
| f_46 | Downtoners | ✅ Transferible | ✅ Lista léxica en dict.yaml | — |
| f_47 | Hedges | ✅ Transferible | ✅ Lista léxica en dict.yaml | — |
| f_48 | Amplifiers | ✅ Transferible | ✅ Lista léxica en dict.yaml | — |
| f_49 | Emphatics | ✅ Transferible | ✅ Lista léxica en dict.yaml | — |
| f_50 | Discourse particles | ✅ Transferible | ✅ Lista léxica en dict.yaml | — |
| f_51 | Demonstratives (det) | ✅ Transferible | ✅ demonstrative_matchlist + POS=DET+dep_rel=det | — |
| f_52 | Possibility modals | ⚠️ Problemático | ⚠️ Solo poder+caber; falta "es posible que" (doc) | AÑADIR es posible que |
| f_53 | Necessity modals | ⚠️ Problemático | ⚠️ Incluye "necesitar" (no en documento) | ELIMINAR necesitar de lista |
| f_54 | Predictive modals | ⚠️ Problemático | ❌ **Falta condicional** (Mood=Cnd); solo Tense=Fut + ir_a | CORREGIR: añadir Mood=Cnd |
| f_55 | Public verbs | ✅ Transferible | ⚠️ Lista muy amplia; varios verbos duplicados con f_56/f_57 | AUDITAR lista (exigir/sugerir/concluir/reconocer/opinar) |
| f_56 | Private verbs | ✅ Transferible | ⚠️ observar/notar/concluir/reconocer en f_55 también | AUDITAR duplicados con f_55 |
| f_57 | Suasive verbs | ✅ Transferible | ✅ Lista en dict.yaml | — |
| f_58 | Seem/appear | ✅ Transferible (**solo** *parecer*) | ❌ **Lista excesiva**: aparecer, aparentar, lucir, presentarse, sonar, tender, tornarse, verse, volverse | CORREGIR: solo parecer (+resultar, verse opcional) |
| f_59 | Contractions | ❌ Intraducible | ❌ **Implementado** (al/del) | ELIMINAR columna f_59 |
| f_60 | That-deletion | ❌ Intraducible | ❌ **Implementado** (ccomp/xcomp sin que) | ELIMINAR columna f_60 |
| f_61 | Stranded prepositions | ❌ Intraducible | ❌ **Implementado** (ADP + relativo siguiente) | ELIMINAR columna f_61 |
| f_62 | Split infinitives | ❌ Intraducible | ❌ **Implementado** (ADP + ADV + INF) | ELIMINAR columna f_62 |
| f_63 | Split auxiliaries | ✅ Transferible | ✅ ADV entre auxiliar y verbo principal | — |
| f_64 | Phrasal coordination | ✅ Transferible | ✅ y/e (CCONJ cc) entre mismos POS | — |
| f_65 | Clausal coordination | ✅ Transferible | ✅ y/e (CCONJ cc) con sujeto explícito | — |
| f_66 | Synthetic negation | ✅ Transferible | ✅ neg_synthetic_determiners + filtro anti-join con f_67 | — |
| f_67 | Analytic negation | ✅ Transferible | ✅ "no" + dep_rel advmod + head VERB/AUX | — |

---

## Rasgos extra/espurios en el código (no en los 67 de Biber)

| Columna en código | Descripción | Acción |
|---|---|---|
| `f_71_preterit` | Pretérito indefinido (extensión española — en NEWS.md y biber_es.R está documentada) | Mantener como rasgo extendido documentado, **no** incluir en el conteo de 59 rasgos Biber |
| `f_68_nominalization` | Duplicado de f_14 con nombre diferente, producido por block_lexical_complexity_es | Eliminar del output (ya existe f_14 desde block_lexical_membership_es) |
| `f_69_mente_adverbs` | Adverbios en -mente (extensión española) | Mantener como extra o eliminar según decisión del usuario |
| `f_70_long_words` | Palabras ≥ 6 caracteres (extensión española) | Mantener como extra o eliminar según decisión del usuario |

---

## Lista priorizada de cambios necesarios

### Prioridad CRÍTICA (afectan validez del análisis)

| # | Cambio | Archivo |
|---|--------|---------|
| 1 | **Corregir f_01**: debe capturar indefinido (Tense=Past, Mood=Ind) no imperfecto | `R/features_tense_pronouns.R` |
| 2 | **Corregir f_54**: añadir condicional (Mood=Cnd, VerbForm=Fin) además de Tense=Fut | `R/features_modals_verbs.R` |
| 3 | **Eliminar f_09** del output (intraducible implementado) | `R/features_tense_pronouns.R`, `R/parse_functions.R` |
| 4 | **Eliminar f_12** del output (intraducible implementado) | `R/features_tense_pronouns.R`, `R/parse_functions.R` |
| 5 | **Eliminar f_15** del output; ajustar f_16 para no restar gerundios | `R/features_coordination_negation.R` |
| 6 | **Eliminar f_28** del output (intraducible implementado) | `R/features_subordination.R` |
| 7 | **Eliminar f_59, f_60, f_61, f_62** del output (intraducibles implementados) | `R/features_subordination.R`, `R/features_coordination_negation.R` |
| 8 | **Corregir f_35**: solo "porque" (eliminar ya_que, puesto_que, dado_que, pues, como) | `R/features_subordination.R` |
| 9 | **Corregir f_36**: solo "aunque" (eliminar si_bien, a_pesar_de_que, etc.) | `R/features_subordination.R` |
| 10 | **Corregir f_58**: solo "parecer" como mínimo (eliminar aparecer, sonar, tender, etc.) | `data-raw/dict.yaml` |

### Prioridad ALTA (afectan comparabilidad con Biber)

| # | Cambio | Archivo |
|---|--------|---------|
| 11 | **Fusionar f_29+f_31**: que relativo sujeto (absorbe quien/cual sujeto) | `R/features_subordination.R` |
| 12 | **Fusionar f_30+f_32**: que relativo objeto (absorbe quien/cual objeto) | `R/features_subordination.R` |
| 13 | **Corregir f_41**: añadir quedarse y permanecer a linking_verbs; eliminar hacerse/tornarse | `R/features_modals_verbs.R` |
| 14 | **Corregir f_53**: eliminar "necesitar" de la lista de modales de necesidad | `data-raw/dict.yaml` |
| 15 | **Revisar f_37**: reducir a si/a_menos_que/salvo_que (eliminar variantes extensas no en documento) | `R/features_subordination.R` |

### Prioridad MEDIA (calidad de detección)

| # | Cambio | Archivo |
|---|--------|---------|
| 16 | **Revisar f_02**: verificar si estar como auxiliar de aspecto compuesto debe incluirse | `R/features_tense_pronouns.R` |
| 17 | **Auditar duplicados f_55/f_56**: resolver solapamiento de exigir/sugerir/concluir/reconocer/observar/notar | `data-raw/dict.yaml` |
| 18 | **Añadir "es posible que"** a f_52 (documento lo menciona) | `data-raw/dict.yaml` + `R/features_modals_verbs.R` |
| 19 | **Eliminar f_68_nominalization** del output (duplicada de f_14) | `R/features_lexical_complexity.R`, `R/parse_functions.R` |
| 20 | **Decidir** qué hacer con f_71, f_69, f_70 (extensiones españolas no-Biber) | Documentación |

---

## Mapa de archivos afectados

| Archivo | Cambios necesarios |
|---------|-------------------|
| `R/features_tense_pronouns.R` | f_01 (Tense→Past), eliminar f_09 y f_12 |
| `R/features_modals_verbs.R` | f_41 (linking_verbs), f_54 (añadir Mood=Cnd) |
| `R/features_subordination.R` | f_28 eliminar, f_29+f_31 fusionar, f_30+f_32 fusionar, f_35/f_36/f_37 restringir, f_60 eliminar |
| `R/features_coordination_negation.R` | f_15 eliminar, ajustar f_16, f_59/f_61/f_62 eliminar |
| `R/features_lexical_complexity.R` | f_68 eliminar del output |
| `R/parse_functions.R` | Eliminar columnas intraducibles del combine_features y output final |
| `data-raw/dict.yaml` | f_53 (eliminar necesitar), f_58 (solo parecer), f_55/f_56 (auditar duplicados) |

---

## Conteo final esperado tras correcciones

**59 rasgos Biber** (67 − 8 intraducibles):  
f_01–f_08, f_10–f_11, f_13–f_14, f_16–f_27, f_29–f_30, f_33–f_58 (con fusión f_31→f_29 y f_32→f_30), f_63–f_67

**Extensiones españolas** (columnas adicionales, fuera del núcleo Biber):  
f_71_preterit (si se mantiene), f_69_mente_adverbs, f_70_long_words (decisión pendiente)

---

*Fin del reporte de auditoría — FASE 1*
