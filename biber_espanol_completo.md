# Biber (1988) Features Adapted to Spanish

**Technical reference for pseudobibeR.es implementation**

This document specifies how each of the 67 lexico-grammatical features from Biber (1988) maps to Spanish in the `pseudobibeR.es` package. It is the authoritative source for implementation decisions and should be consulted whenever a feature's behavior is in question.

**Audience**: developers and AI agents implementing or auditing the package.

**Relationship to other documents**:
- `README.md` — user-facing documentation of the package
- `biber_espanol_completo.md` (this document) — implementation reference with feature-by-feature decisions
- `validation/test_corpus.yaml` — empirical test cases for each feature

---

## 1. Summary

### Output contract

`pseudobibeR.es` exposes **67 output columns**, matching the `pseudobibeR.fr` interface for side-by-side comparison and drop-in compatibility with scripts that expect Biber's 67-feature names. Of these 67 columns:

```
67 output columns total
- 55 features uniquely detected in Spanish
+  2 features detected via mergers (f_29 absorbs f_31, f_30 absorbs f_32)
= 57 features with real linguistic detection
+ 10 zero-output columns (surface-parity scars)
```

**Critical distinction**:

- **57 features with real linguistic detection** — validated against `tests/testthat/test-feature-coverage.R`, documented per-feature in §3, and constituting the linguistic 1:1 mapping to Biber (1988).
- **10 zero-output columns** — exist in the output for surface-level interface parity with `pseudobibeR.fr`. They always return **0**, regardless of input. They are **not bugs**; they are an explicit design contract. Pipelines that filter for `column > 0` will naturally ignore them.

### Zero-output columns

These 10 columns appear in the output but always return 0. They split into two groups:

**Group A — 8 untranslatable features** (no Spanish equivalent exists or is intelligible):

| Column | Reason |
|--------|--------|
| `f_09_pronoun_it` | Spanish is pro-drop; no expletive pronoun equivalent exists |
| `f_12_proverb_do` | Spanish resolves verbal anaphora via ellipsis; no grammaticalized pro-verb |
| `f_15_gerunds` | Spanish gerund (*-ando/-iendo*) has no nominal function; structurally non-transferable |
| `f_28_present_participle_whiz` | Spanish gerund cannot function as post-nominal modifier (function absorbed by `f_29`) |
| `f_59_contractions` | No grammaticalized contractions in standard Spanish (*al/del* are not informality markers) |
| `f_60_that_deletion` | The complementizer *que* is virtually obligatory in Spanish |
| `f_61_stranded_preposition` | Stranded prepositions are categorically ungrammatical in Spanish (function absorbed by `f_33`) |
| `f_62_split_infinitive` | No preverbal infinitive marker (*to*) exists to be split |

**Group B — 2 merged-source slots** (the feature is detected, but under another column's name):

| Column (zero) | Detection moved to | Reason |
|---------------|--------------------|--------|
| `f_31_wh_subj` | `f_29_that_subj` | Spanish *que* relativo covers both *that* and *who/which* in subject position — no lexical distinction |
| `f_32_wh_obj` | `f_30_that_obj` | Same formal collapse in object position |

### Feature categories (67-column layout)

| Category | Columns | Real-detection features | Zero-output columns |
|----------|---------|--------------------------|---------------------|
| A. Tense and aspect | f_01–f_03 | 3 | — |
| B. Place and time adverbials | f_04–f_05 | 2 | — |
| C. Pronouns | f_06–f_12 | 5 | f_09, f_12 |
| D. Questions | f_13 | 1 | — |
| E. Nominal forms | f_14–f_16 | 2 | f_15 |
| F. Passives | f_17–f_18 | 2 | — |
| G. Stative forms | f_19–f_20 | 2 | — |
| H. Subordination | f_21–f_38 | 15 | f_28, f_31, f_32 |
| I. Prep, adj, adv | f_39–f_42 | 4 | — |
| J. Lexical specificity | f_43–f_44 | 2 (metrics, not counts) | — |
| K. Lexical classes | f_45–f_50 | 6 | — |
| L. Demonstratives | f_51 | 1 | — |
| L. Modals | f_52–f_54 | 3 | — |
| M. Specialized verbs | f_55–f_58 | 4 | — |
| N. Reduced forms | f_59–f_63 | 1 | f_59, f_60, f_61, f_62 |
| O. Coordination | f_64–f_65 | 2 | — |
| P. Negation | f_66–f_67 | 2 | — |
| **Total** | **67 columns** | **57 with detection** | **10 zero-output** |

---

## 2. Cross-cutting decisions

These decisions apply to multiple features and should be implemented consistently.

### 2.1 Pro-drop and pronoun counting (f_06, f_07, f_08)

Spanish is a null-subject language. The features f_06–f_08 count **only explicit pronoun forms**. This systematically underestimates first/second/third person presence compared to English — this is a known limitation, not a bug.

**Implementation strategy**: **morphological filter as primary** (per user decision documented in `Decisiones_rasgos_biber.md`):

- **Primary mechanism**: UDPipe morphological tags. Filter tokens by `Person=1`, `Person=2`, `Person=3` features on tokens whose POS is `PRON` or `DET` (for possessives).
- **Secondary mechanism**: lexical list only to disambiguate edge cases not captured reliably by morphology — specifically *usted/ustedes* (morphologically `Person=3` but semantically 2nd person), *vos* in regional varieties, and ambiguous *se*.

**Why morphological-first**: relying on the parser's morphological analysis ensures coverage of all pronominal inflected forms (subject, object, possessive) without maintaining a brittle lexical list. The lexical list serves only as a corrective overlay.

**Document the limitation** in the function's roxygen2 docs: pro-drop guarantees that explicit-pronoun counts systematically underestimate person-marking in Spanish texts compared to English. Cross-linguistic comparison of f_06–f_08 raw counts is not meaningful; use within-language normalization.

### 2.2 Modals as periphrases, not grammaticalized verbs (f_52, f_53, f_54)

Spanish has no grammaticalized modal verbs equivalent to English. Modality is expressed through:
- Full verbs: *poder*, *deber* with full conjugation
- Periphrases: *tener que + inf*, *hay que + inf*, *ir a + inf*
- Morphological tenses: future, conditional

Maintain three separate features (mirroring Biber) but document that the lexical distinction (possibility/necessity/prediction) is less crisp in Spanish since the same lemma (*poder*, *deber*) covers multiple modal values depending on context.

### 2.3 Multi-token expressions

Spanish has many grammaticalized multi-token expressions that UDPipe tokenizes as separate tokens but function as single units (e.g., *sin embargo*, *o sea*, *hay que*, *a lo mejor*).

**Implementation strategy**: two-mechanism approach (replicating `pseudobibeR.fr`):

1. **`quanteda::tokens_compound()` + `tokens_lookup`** for closed lexical lists (hedges, conjuncts, amplifiers, discourse particles). Tokens are fused with `_` before lookup. This MUST run once at the start of `biber_es()`, before any `block_*_es()` function executes.

2. **Filters on `next_token`/`prev_token`** with grammatical conditions (POS, `dep_rel`) for periphrases that require syntactic verification (e.g., *hay que* + infinitive for f_53, *ir a* + infinitive for f_54).

**Critical**: features that depend on multi-token detection (f_45, f_47, f_49, f_50, f_53, f_54, f_38, f_37) will fail silently if the fusion step is not applied. Always verify with debug prints that `tokens_compound()` runs and that compound tokens appear in the modified token stream.

### 2.4 Detection strategy by feature type

| Strategy | Features |
|----------|----------|
| **Closed lexical list** | f_04, f_05, f_45, f_46, f_47, f_48, f_49, f_50, f_55, f_56, f_57, f_58 |
| **Morphological filter** | f_01, f_02, f_03, f_06–f_08, f_24, f_25, f_26, f_27 |
| **Lexical list + syntactic filter** | f_10, f_11, f_51, f_29, f_30, f_33, f_34 |
| **Multi-token compound + lexical filter** | f_45 (locutions), f_47 (locutions), f_50 (locutions), f_53 (periphrases), f_54 (periphrases) |
| **POS count** | f_14 (suffix patterns), f_16, f_39, f_40, f_41, f_42 |
| **Metric calculation** | f_43 (MATTR), f_44 (mean length) |
| **NOT normalized by `normalize = TRUE`** | f_43, f_44 (these are ratios/averages, not counts) |

### 2.5 Interlingual comparability

The following features produce values that are **NOT directly comparable** between English (Biber's reference) and Spanish:

- **f_43 TTR**: systematically lower in Spanish due to richer inflectional morphology
- **f_44 word length**: systematically higher in Spanish (*-mente* adverbs, *-ción/-miento* nominalizations)
- **f_29, f_30, f_33**: higher in Spanish due to absorption of intranslatable features (f_28, f_61)
- **f_67 analytic negation**: distributed differently due to mandatory negative concord

Use within-language Z-scores or normalized values when comparing across languages.

### 2.6 Features absorbed by Spanish equivalents

Functions covered by untranslatable English features are absorbed elsewhere in Spanish:

| English feature | Spanish equivalent absorbs into |
|----------------|--------------------------------|
| f_09 *it* | Null subject (not detectable as feature) and impersonal constructions |
| f_12 pro-verb *do* | Verbal ellipsis (not detectable) |
| f_15 nominal gerunds | f_14 (nominalizations) and f_24 (substantivized infinitives) |
| f_28 present participial WHIZ | f_29 active relative clauses (raises f_29 frequency) |
| f_61 stranded prepositions | f_33 pied-piping (raises f_33 frequency) |

**On the surface-parity contract**: although these features have no real Spanish detection, their column slots (`f_09_pronoun_it`, `f_12_proverb_do`, `f_15_gerunds`, `f_28_present_participle_whiz`, `f_61_stranded_preposition`) are preserved in the output as constant-zero columns. This is intentional and documented in §1. Tools that consume both `pseudobibeR.es` and `pseudobibeR.fr` output can rely on identical column layouts. The same applies to the merged-source slots `f_31_wh_subj` and `f_32_wh_obj`, whose detected counts are emitted under `f_29_that_subj` and `f_30_that_obj` respectively.

---

## 3. Feature-by-feature specification

For each feature, the structure is:
- **Biber (1988)**: original definition and function
- **Spanish equivalent**: target construction
- **Detection method**: how to implement
- **Include**: what counts
- **Exclude**: what does NOT count (false positives to avoid)
- **Notes**: implementation considerations

---

### A. Tense and aspect

#### f_01_past_tense

**Biber (1988)**: Verbs in simple past tense.

**Spanish equivalent**: Preterite (*pretérito indefinido / perfecto simple*).

**Detection method**: morphological filter.

**Include**: Verb forms with `Tense=Past` + `Mood=Ind` + `VerbForm=Fin` (e.g., *habló, fue, dijo, llegó*).

**Exclude**:
- Imperfect (*hablaba, era, decía*) — decision: NOT counted in this version
- Past participles in perfect periphrasis (those go to f_02)
- Pluscuamperfect auxiliary forms

**Notes**: The exclusion of the imperfect is a deliberate decision to maintain the closest possible mapping to English simple past. The imperfect's narrative function is acknowledged but treated separately. Document this in the function's docs.

---

#### f_02_perfect_aspect

**Biber (1988)**: Perfect aspect indicated by *have* as auxiliary + past participle.

**Spanish equivalent**: Compound past tense (*pretérito perfecto compuesto*): *haber* (auxiliary) + past participle.

**Detection method**: dependency-based.

**Include**: Token with lemma `haber` in `aux` dependency, with present-tense forms (*ha, han, he, hemos, has, habéis*) + following past participle.

**Exclude**:
- Pluscuamperfect (*había hablado*) — anteriority, not perfect aspect
- Future perfect (*habrá hablado*)
- Conditional perfect (*habría hablado*)
- Subjunctive perfect (*haya hablado*) — implementation choice: also exclude to stay close to English

**Notes**: Spanish peninsular variety uses the compound perfect more frequently than Latin American variety (which prefers the preterite). Frequencies will vary by corpus origin — this is expected, not a bug.

---

#### f_03_present_tense

**Biber (1988)**: Verbs in present tense.

**Spanish equivalent**: Present indicative.

**Detection method**: morphological filter.

**Include**: Verb forms with `Tense=Pres` + `Mood=Ind` + `VerbForm=Fin`.

**Exclude**:
- Present subjunctive (*hable, diga, sea*) — has modal value
- Imperative (*habla, diga*) — different function
- Future morphological (*hablará*) — goes to f_54

**Notes**: Historical present (*Colón llega en 1492*) is correctly captured as present indicative.

---

### B. Place and time adverbials

#### f_04_place_adverbials

**Biber (1988)**: Place adverbs (e.g., *above, beside, outdoors*).

**Spanish equivalent**: Closed lexical list of place adverbs.

**Detection method**: lexical list.

**Include** (lemma must be `ADV`):
*aquí, ahí, allí, allá, acá, arriba, abajo, dentro, fuera, afuera, adentro, cerca, lejos, enfrente, alrededor, encima, debajo, detrás, delante, junto, atrás, adelante*

**Exclude**:
- Same words functioning as prepositions or part of prepositional locutions (*dentro de la casa* — *dentro* here is preposition-like)
- Multi-token locutions in the first version (decision: keep list lexically simple)

**Notes**: If detection returns positive matches in texts without obvious place references, the lexical list is likely too broad. Audit by checking what specific tokens triggered detection.

---

#### f_05_time_adverbials

**Biber (1988)**: Time adverbs (e.g., *early, instantly, soon*).

**Spanish equivalent**: Closed lexical list of time adverbs.

**Detection method**: lexical list.

**Include** (lemma must be `ADV`):
*ayer, hoy, mañana, ahora, antes, después, luego, temprano, tarde, pronto, enseguida, inmediatamente, recientemente, actualmente, siempre, nunca, jamás, todavía, aún, ya, mientras*

**Exclude**:
- *Ya* in modal/discursive uses (*ya verás* — modal; *ya, ya* — discursive). This requires contextual filtering. As a fallback, accept some noise.

**Notes**: *Ya* is the most polysemous item. Conservative approach: include it but flag in code that this will inflate counts in dialogues. Spanish-specific: *temprano* and *tarde* are exclusively temporal in this list (not the noun *tarde* "afternoon" or the adjective). Rely on POS=ADV to discriminate.

---

### C. Pronouns

#### f_06_first_person_pronouns

**Biber (1988)**: First-person pronouns (*I, me, my, myself, we, us, our, ourselves*).

**Spanish equivalent**: Explicit first-person pronominal forms (subject, object, possessive).

**Detection method**: lexical list + morphological filter (`Person=1`).

**Include**:
*yo, mí, me, conmigo, mi, mis, mío, mía, míos, mías, nosotros, nosotras, nos, nuestro, nuestra, nuestros, nuestras*

**Exclude**:
- Verbal first-person inflection without pronoun (e.g., *fui* alone — no detection; the verb conveys person but no pronoun exists)

**Notes**: This systematically underestimates first-person presence in Spanish due to pro-drop. Document in roxygen2.

---

#### f_07_second_person_pronouns

**Biber (1988)**: Second-person pronouns.

**Spanish equivalent**: Explicit second-person pronominal forms across address systems.

**Detection method**: lexical list + morphological filter (`Person=2`).

**Include**:
*tú, te, ti, contigo, tu, tus, tuyo, tuya, tuyos, tuyas, vos, vosotros, vosotras, os, vuestro, vuestra, vuestros, vuestras, usted, ustedes*

**Exclude**:
- *Se* (ambiguous: reflexive, passive, dative)

**Notes**: Includes *usted/ustedes* despite morphological 3rd person — they are semantically 2nd person. Document this decision. Includes *vos* for Rioplatense, Central American, and Andean varieties. Excludes 2nd-person inflection without pronoun.

---

#### f_08_third_person_pronouns

**Biber (1988)**: Third-person personal pronouns excluding *it*.

**Spanish equivalent**: Third-person pronominal forms.

**Detection method**: lexical list + morphological filter.

**Include**:
*él, ella, ellos, ellas, le, les, lo, la, los, las, sí, consigo*

**Exclude**:
- *Su, sus* — ambiguous with f_07 (also covers *usted's*)
- *Se* in reflexive/passive uses — count only when clearly anaphoric (currently too noisy; document limitation)

**Notes**: Atonic forms (*le, les, lo, la, los, las*) are highly frequent but generate noise from dative, accusative, and pseudo-reflexive uses. Accept the noise; document the limitation.

---

#### f_09_pronoun_it ⚠️ ZERO-OUTPUT

**Biber (1988)**: The pronoun *it* in all its functions — referential, extraposition (*it is clear that*), and expletive (*it rains*).

**Spanish equivalent**: **None — structurally non-transferable.**

**Detection method**: column always emits `0`.

**Why zero-output**: Spanish is a pro-drop language. The three functions of English *it* dissolve into:
- Referential: null subject (*se rompió*) or atonic object pronoun (*lo rompió*, captured in f_08)
- Extraposition: direct construction without dummy subject (*es claro que…*)
- Impersonal weather/atmospheric verbs: zero-subject verb (*llueve, nieva*)

The category *it* exists in English precisely to satisfy obligatory-subject requirements that Spanish does not impose. There is nothing to count.

**Output contract**: this column appears in `biber_es()` output and always returns `0`. Pipelines should ignore it.

---

#### f_10_demonstrative_pronoun

**Biber (1988)**: *That, this, these, those* as pronouns (not determiners).

**Spanish equivalent**: Demonstrative pronouns in three deixis degrees.

**Detection method**: lexical list + syntactic filter (function is NOT determiner).

**Include**:
- Neuter forms (always pronominal): *esto, eso, aquello*
- Non-neuter forms (require syntactic check): *este, esta, estos, estas, ese, esa, esos, esas, aquel, aquella, aquellos, aquellas*

**Exclude**:
- Non-neuter forms when functioning as `det` (those go to f_51)

**Notes**: Neuter forms (*esto, eso, aquello*) are unambiguously pronominal — detect by lexical match alone. Non-neuter forms require checking that the dependency relation is NOT `det`. Pre-2010 corpora may contain accented forms (*éste, ése, aquél*); normalize before matching or include both variants.

---

#### f_11_indefinite_pronouns

**Biber (1988)**: Indefinite pronouns (*anybody, nothing, someone*, etc.).

**Spanish equivalent**: Indefinite pronouns.

**Detection method**: lexical list with syntactic filter for ambiguous cases.

**Include**:
*alguien, nadie, algo, nada, todo, todos, todas, ninguno, ninguna, alguno, alguna, algunos, algunas, cualquiera, quienquiera*

**Exclude**:
- *Un, una, unos, unas* — these are **articles** (`DET`), not pronouns. Critical to exclude.
- *Todo/nada* when functioning as `det` (e.g., *todo el día* — `det` of `día`)

**Notes**: For *todo/nada*, require dependency NOT to be `det`. Multi-token locutions like *todo el mundo* are explicitly considered per the user's documented decision (`Decisiones_rasgos_biber.md`): the syntactic filter for *todo/nada* as `det` is the primary mechanism; locutions like *todo el mundo, cada quien* can be added incrementally as the multi-token compound infrastructure stabilizes (see §2.3). Treat them as TODO for the next iteration, not as a permanent omission.

---

#### f_12_proverb_do ⚠️ ZERO-OUTPUT

**Biber (1988)**: *Do* functioning as pro-verb, substituting a previously mentioned verb or predicate (*She runs faster than he does*, *I think so too, and so does Mary*). Distinct from auxiliary *do* and emphatic *do*. Functionally marks anaphoric verbal cohesion.

**Spanish equivalent**: **None — structurally non-transferable.**

**Detection method**: column always emits `0`.

**Why zero-output**: Spanish has no grammaticalized pro-verb. Verbal anaphora is resolved through:
- Verbal ellipsis (*Ella corre más rápido que él* — verb is simply omitted)
- Total clausal ellipsis (*También María*)
- Occasionally a comodín verb *hacer* in marked stylistic contexts (*Lo hice porque él también lo hizo*)

None of these constructions is gramaticalized enough to count as the equivalent of pro-verb *do*. The phenomenon dissolves into elision that the parser cannot reliably detect as a discrete feature.

**Output contract**: this column appears in `biber_es()` output and always returns `0`.

---

### D. Questions

#### f_13_wh_question

**Biber (1988)**: Direct WH-questions.

**Spanish equivalent**: Direct questions introduced by accented interrogative words.

**Detection method**: lexical list + punctuation + syntactic filter.

**Include**:
- Accented interrogative words (`PRON` or `ADV` with feature `PronType=Int`): *qué, quién, quiénes, dónde, adónde, cuándo, cómo, cuál, cuáles, cuánto, cuánta, cuántos, cuántas, por qué*
- Plus the presence of `?` ending the clause (and ideally `¿` opening it).

**Exclude**:
- Same words without accent (*que, quien, donde, cuando, como, cual, cuanto*) — those are relatives or non-interrogative subordinators
- Indirect questions (those go to f_23 — wh-clause)

**Notes**: The diacritic accent is the primary disambiguator. UDPipe should preserve it. In poorly-edited corpora where accents are missing, fall back to detecting these words in the immediate scope of `?` punctuation.

---

### E. Nominal forms

#### f_14_nominalizations

**Biber (1988)**: Nouns derived from verbs or adjectives via nominalizing suffixes (*-tion, -ment, -ness, -ity*).

**Spanish equivalent**: Nouns with Spanish nominalizing suffixes.

**Detection method**: morphological suffix pattern on `NOUN` tokens.

**Include patterns** (case-insensitive, on lemma):
- `*ción` (production, evaluation): *producción, evaluación, investigación*
- `*sión` (discussion, vision): *discusión, decisión*
- `*miento` (movement, knowledge): *conocimiento, movimiento, sentimiento*
- `*mento` (less productive variant): *medicamento, monumento*
- `*idad` (productivity, complexity): *complejidad, productividad*
- `*ez` (smallness, depth): *pequeñez, rapidez*
- `*ura` (sweetness, height): *dulzura, altura*

**Exclude**:
- Non-noun tokens (e.g., verbs ending in the same letters)
- False positives are acceptable (e.g., *canción, nación* don't derive from verbs but match the pattern) — mirror Biber's approach in English (*fashion, notion* also produce false positives but are accepted noise)

**Notes**: This is the suffix-pattern strategy, NOT a closed list. Apply regex on lemma with `NOUN` POS filter. The token *lado* (side) does NOT match any of these patterns — if it appears as a nominalization in output, the regex is faulty.

---

#### f_15_gerunds ⚠️ ZERO-OUTPUT

**Biber (1988)**: *-ing* forms functioning as nouns — participial forms in nominal position (*Swimming is good exercise*, *I enjoy reading*).

**Spanish equivalent**: **None — structurally non-transferable.**

**Detection method**: column always emits `0`.

**Why zero-output**: the Spanish gerund (*-ando/-iendo*) is functionally restricted to adverbial use (captured by `f_25_present_participle`). The English *-ing* nominal function maps in Spanish to:
- Substantivized infinitives (*el correr es sano, nadar es bueno*) — argumentally captured by f_24
- Nominalizations with derivational suffixes (*la enseñanza, el estudio*) — captured by f_14

Per the user decision (`Decisiones_rasgos_biber.md`), this feature is "intermediate" — not as categorically non-transferable as f_09 or f_12, but its function is already absorbed into existing features. A future iteration could explore substantivized infinitives as a partial proxy via dependency analysis (infinitive in argumental position, `nsubj` or `obj`), but this would significantly overlap with f_24. For now, treat as zero-output and document the absorption.

**Output contract**: this column appears in `biber_es()` output and always returns `0`. The function it would track is already counted under `f_14` and `f_24`.

---

#### f_16_other_nouns

**Biber (1988)**: Total nouns minus nominalizations and nominal gerunds.

**Spanish equivalent**: Total nouns minus nominalizations. Per the user decision, the formula explicitly accounts for the fact that f_15 is zero-output in Spanish.

**Detection method**: count residual.

**Formula** (current, with f_15 zero-output):
```
f_16 = (total tokens with POS=NOUN) − f_14_nominalizations
```

**Formula** (English original, for reference):
```
f_16 = (total tokens with POS=NOUN) − f_14 − f_15
```

In Spanish, the f_15 subtrahend collapses to zero. The Spanish f_16 therefore absorbs slightly more material than its English counterpart — specifically, the nouns that in English would have been routed to f_15 nominal gerunds. Document this absorption in the function's comments.

**Notes**: Since f_15 is zero-output in Spanish, f_16 absorbs slightly more than its English equivalent. This is a deliberate consequence of f_15 being structurally untranslatable, not a bug.

---

### F. Passives

#### f_17_agentless_passives

**Biber (1988)**: Passive constructions without explicit agent (e.g., *The task was done*).

**Spanish equivalent**: Two constructions:
1. Periphrastic passive without agent (*La tarea fue realizada*)
2. Reflexive passive (*se*-passive): *Se publicaron los resultados*

**Detection method**: dependency-based, two patterns.

**Include — periphrastic passive**:
- Lemma `ser` as `aux:pass` + past participle (or root + `cop` if UDPipe handles it that way)
- AND no `obl:agent` or similar agent dependent
- AND no preposition *por* introducing an agent phrase

**Include — se-passive**:
- Lemma `se` in `expl:pass` or `expl:pv` relation
- Verb in 3rd person (singular or plural) with passive interpretation
- Distinguished from reflexive/recipocal *se* by: typically inanimate subject, verb is transitive in non-reflexive use

**Exclude**:
- Periphrastic passive WITH agent (those go to f_18)
- Reflexive uses of *se* (action falls on the subject)
- Pseudo-impersonal *se* with animate-only verbs

**Notes**: This is one of the trickiest features. The se-passive distinction from reflexive is heuristic and will produce some noise. Acceptable tolerance: ±2 from expected. Critical: f_17 includes BOTH constructions — counting only the periphrastic would severely underestimate the functional equivalent.

---

#### f_18_by_passives

**Biber (1988)**: Passive with explicit agent (*by-passive*).

**Spanish equivalent**: Periphrastic passive with agent introduced by *por*.

**Detection method**: dependency-based.

**Include**:
- Lemma `ser` as `aux:pass` + past participle
- AND presence of `obl:agent` (or `obl` with case marker *por*)

**Exclude**:
- Se-passives (cannot take explicit agent, ungrammatical: *se hizo por Steve)
- Historical *de + agent* (very rare; not worth handling)

**Notes**: Cleaner detection than f_17 because the agent phrase is the discriminator. This feature should have lower frequencies than f_17 in most corpora.

---

### G. Stative forms

#### f_19_be_main_verb

**Biber (1988)**: *Be* as main verb (copula or existential).

**Spanish equivalent**: *Ser* and *estar* as main verbs (NOT auxiliary).

**Detection method**: dependency-based.

**Include**:
- Lemma `ser` or `estar` with dependency relation `root` or `cop` (copula)
- AND token is NOT functioning as auxiliary

**Exclude**:
- *Ser* as `aux:pass` in periphrastic passive (those are f_17/f_18)
- *Estar* as `aux` in progressive periphrasis (*está hablando*)
- *Haber* as existential (that's f_20)
- Pseudo-copulative verbs (*parecer, volverse, quedarse, ponerse, resultar, permanecer*) — those go to f_41 only

**Critical false positive to avoid**: *sea* when it's part of the multi-token discourse marker *o sea*. If the multi-token compound step ran correctly, *o_sea* is now a single token and won't be misidentified. If detection still picks it up, the multi-token compound step failed for this expression.

**Notes**: Spanish requires inclusion of both *ser* AND *estar* — this is a 2-verb expansion compared to English's single *be*. The pseudo-copulative exclusion is critical to avoid double-counting with f_41.

---

#### f_20_existential_there

**Biber (1988)**: Existential *there + be* construction.

**Spanish equivalent**: Impersonal *haber* (*hay, había, hubo, habrá, habría, haya, hubiera, hubiese*).

**Detection method**: lexical list with critical exclusion filter.

**Include**:
Impersonal forms of *haber*: *hay, había, hubo, habrá, habría, haya, hubiera, hubiese*

**Exclude (critical)**:
- *Hay que + infinitive* — this is a periphrasis of obligation (goes to f_53), NOT existential
- *Haber* as auxiliary in perfect aspect (those go to f_02)

**Implementation**: after the multi-token compound step, *hay_que* should be a single token tagged as a necessity modal. If *hay* appears alone (not as *hay_que*), it's existential. Verify by checking `next_token` for *que + Verb[VerbForm=Inf]*.

**Notes**: Cleaner than the English equivalent because there's no ambiguity between existential and locative (English *there* can be both; Spanish *haber* is exclusively existential in these forms).

---

### H. Subordination

#### f_21_that_verb_comp

**Biber (1988)**: *That* complement clauses depending on a verb.

**Spanish equivalent**: *Que* complement clauses depending on a verb.

**Detection method**: dependency-based.

**Include**:
- Token *que* with dependency relation `mark`
- Whose head is a `VERB`
- And the construction is a complement clause (`ccomp`, `xcomp`, or similar)

**Exclude**:
- *Que* as relative pronoun in `acl:relcl` (those go to f_29/f_30)
- *Que* in adjective complement (those go to f_22)
- *Que* in comparatives (*más alto que*) — not a complement clause
- *Lo que* — relative construction, not a complement (goes to f_30 or f_34)

**Notes**: Spanish *que* is highly polysemous. Strict dependency-based filtering is required. The mode (indicative/subjunctive) of the subordinate verb does NOT affect counting.

---

#### f_22_that_adj_comp

**Biber (1988)**: *That* complement clauses depending on an adjective.

**Spanish equivalent**: *Que* (or *preposition + que*) complement clauses depending on an adjective.

**Detection method**: dependency-based.

**Include**:
- Token *que* with `mark`
- Whose head is an `ADJ` (or a copula construction whose root is `ADJ`)
- Examples: *es importante que vengas, está seguro de que vendrá, contento de que ganaras*

**Exclude**:
- Same false positives as f_21

**Notes**: The subjunctive is virtually obligatory in this construction. Some adjectives require *de que* (or other prepositions) — include these cases by checking for the preposition + *que* sequence on `mark`. Known UDPipe issue: the head of *que* is not always tagged `ADJ` in copular constructions — this feature has known noise.

---

#### f_23_wh_clause

**Biber (1988)**: WH-clauses functioning as verbal arguments (indirect questions).

**Spanish equivalent**: Subordinate clauses introduced by accented interrogative words in argumental position.

**Detection method**: lexical list + syntactic filter.

**Include**:
- Accented interrogative words (same list as f_13): *qué, quién, quiénes, dónde, adónde, cuándo, cómo, cuál, cuáles, cuánto*
- In subordinate (not main) clause position
- AND in argumental function (`ccomp` or `xcomp`)
- Plus the sequence *lo que* in argumental position

**Exclude**:
- Direct questions (those go to f_13 — distinguished by `?` and main clause status)
- Relative uses of the same words without accents

**Notes**: Known UDPipe issue: tilde diacritica is sometimes inconsistent in the parser's lemma output. May need to check both *quién* and *quien* and validate by parent verb being a cognition/communication verb.

---

#### f_24_infinitives

**Biber (1988)**: Infinitive constructions.

**Spanish equivalent**: Verbs in infinitive form functioning as verbal complements.

**Detection method**: morphological filter.

**Include**:
- Verbs with `VerbForm=Inf`
- In complement function (`xcomp`, `ccomp`, or as object)
- Plus verbal periphrases: *ir a + inf, empezar a + inf, dejar de + inf, volver a + inf, acabar de + inf*

**Exclude**:
- Substantivized infinitives (*el correr es sano*) — these would conceptually go to f_15 which is intranslatable
- Infinitives in imperative use

**Notes**: Spanish infinitives are more functionally diverse than English *to-infinitives*. The decision to include verbal periphrases inflates this feature relative to English. Document this.

---

#### f_25_present_participle

**Biber (1988)**: Present participial adverbial clauses (*-ing* adverbial).

**Spanish equivalent**: Gerund clauses with adverbial function (*-ando/-iendo*).

**Detection method**: morphological + dependency.

**Include**:
- Gerund (`VerbForm=Ger`) in adverbial function (`advcl`)
- Compound gerund: *habiendo + participio* (same dependency conditions)

**Exclude**:
- Gerund in progressive periphrasis (*está hablando*) — auxiliary `estar` + gerund
- Gerunds with `aux` relation

**Notes**: Cleaner than English because the Spanish gerund is almost exclusively adverbial — no nominal or attributive functions to disambiguate. The progressive periphrasis exclusion is critical to avoid noise.

---

#### f_26_past_participle

**Biber (1988)**: Past participial adverbial clauses.

**Spanish equivalent**: Absolute participle constructions.

**Detection method**: dependency-based.

**Include**:
- Past participle (`VerbForm=Part`) with `advcl` dependency
- WITHOUT auxiliary present (no *ser/estar/haber* + participio in this clause)
- Include *una vez + participio* construction

**Exclude**:
- Participles in periphrastic passive (those are f_17/f_18) — auxiliary present
- Participles in perfect aspect (those are f_02) — auxiliary *haber*
- Post-nominal participles (those are f_27)

**Notes**: Gender/number agreement of the participle with its noun does not affect detection. Distinguishing from f_27 requires the dependency function (`advcl` vs `acl`).

---

#### f_27_past_participle_whiz

**Biber (1988)**: Reduced relative clauses with past participle (post-nominal).

**Spanish equivalent**: Post-nominal participles as nominal modifiers.

**Detection method**: dependency-based.

**Include**:
- Past participle (`VerbForm=Part`) with `acl` dependency
- Post-nominal position (head is a noun, position is after the noun)
- WITHOUT auxiliary

**Exclude**:
- Adverbial participles (those are f_26 — `advcl`)
- Participles with auxiliary (f_17/f_18/f_02)
- Participles functioning as adjectives in `amod` (those should go to f_40, but the decision is to EXCLUDE them from f_40 to avoid double-counting with f_27)

**Notes**: Post-nominal position is virtually categorical in Spanish, which simplifies detection. This feature is likely more frequent in Spanish academic text than in English equivalents because Spanish lacks the alternative of present-participle WHIZ (f_28).

---

#### f_28_present_participle_whiz ⚠️ ZERO-OUTPUT

**Biber (1988)**: Reduced relative clauses with present participle, post-nominal (*the event causing this decline*, *the man swimming in the pool*).

**Spanish equivalent**: **None — structurally non-transferable.**

**Detection method**: column always emits `0`.

**Why zero-output**: the Spanish gerund (*-ando/-iendo*) cannot grammatically function as a post-nominal modifier. The construction *\*el evento causando este descenso* is ungrammatical in standard Spanish. The equivalent meaning is expressed via:
- Active relative clause: *el evento que causa este descenso*

This means the function distributed in English between f_28 and the full relatives is, in Spanish, **entirely** routed into the relatives (f_29). Attempting to extract a separate count for f_28 in Spanish would necessarily contaminate f_29 by re-counting the same material under two columns. The clean solution is to leave f_28 as zero-output and let f_29 reflect the inflated frequency.

**Output contract**: this column appears in `biber_es()` output and always returns `0`. The function it would capture is absorbed by `f_29_that_subj`, which will show systematically higher counts in Spanish than in English (a documented and expected interlingual difference, see §2.5).

---

#### f_29_that_subj (MERGED: f_29 + f_31)

**Biber (1988)** (originals):
- f_29: *That* relative clauses on subject position
- f_31: WH-relative clauses on subject position

**Spanish equivalent (merged)**: *Que* relative clauses where the relative pronoun is the subject of the relative clause. Spanish *que* lexically collapses both English *that* and *who/which*.

**Detection method**: dependency-based.

**Include**:
- Token *que* with relation `mark` or `nsubj` inside an `acl:relcl` clause
- AND the relative functions as the subject of the relative clause's verb

**Special handling**: UDPipe Spanish-GSD tags relative *que* as `SCONJ/mark` (not `PRON`). Detection logic must account for this. The relative is the subject when, within the `acl:relcl` clause, no other `nsubj` is present.

**Optional formal variants** (lower frequency, more formal): *quien, quienes, el cual, la cual, los cuales, las cuales* in subject position of relatives.

**Notes**: Higher frequencies expected than English equivalents because Spanish absorbs:
- Both English subject relatives (f_29 + f_31)
- Functions covered by present participial WHIZ (f_28, intranslatable in Spanish)

---

#### f_30_that_obj (MERGED: f_30 + f_32)

**Biber (1988)** (originals):
- f_30: *That* relative clauses on object position
- f_32: WH-relative clauses on object position

**Spanish equivalent (merged)**: *Que* relative clauses where the relative pronoun is the object of the relative clause.

**Detection method**: dependency-based.

**Include**:
- Token *que* with relation `mark` inside `acl:relcl`
- AND the relative functions as the direct object of the relative clause's verb
- Including *lo que* constructions in object position

**Special handling**: Object relatives in Spanish are obligatory (cannot be omitted as in English). This produces systematically higher frequencies.

**Optional formal variants**: *a quien, a quienes, al cual, a la cual* with explicit accusative *a*.

**Exclude**:
- Prepositional relatives (those go to f_33 pied-piping)

**Notes**: Higher frequencies expected for the same reasons as f_29.

---

#### f_31_wh_subj ⚠️ ZERO-OUTPUT (MERGED INTO f_29)

**Biber (1988)**: WH-relative clauses on subject position (*the man who likes popcorn*).

**Spanish equivalent**: **Detected, but emitted under `f_29_that_subj`.**

**Detection method**: column always emits `0`. The actual detection happens in `f_29_that_subj`.

**Why zero-output**: in Spanish there is no formal lexical distinction between *that*-relatives and *who/which*-relatives in subject position — both surface as *que*. Per the user decision (`Decisiones_rasgos_biber.md`), f_29 and f_31 are fused into a single feature column `f_29_that_subj`. The optional formal variants *quien/quienes* and *el cual/la cual* are absorbed into f_29 as well.

**Output contract**: this column appears in `biber_es()` output and always returns `0`. Pipelines comparing English f_31 to Spanish should consult `f_29_that_subj` instead, with the understanding that f_29 in Spanish includes what English would split across f_29 and f_31.

---

#### f_32_wh_obj ⚠️ ZERO-OUTPUT (MERGED INTO f_30)

**Biber (1988)**: WH-relative clauses on object position (*the man who Sally likes*).

**Spanish equivalent**: **Detected, but emitted under `f_30_that_obj`.**

**Detection method**: column always emits `0`. The actual detection happens in `f_30_that_obj`.

**Why zero-output**: same formal collapse as f_31. Spanish *que* covers both *that* and *who/which* in object position. Per the user decision, f_30 and f_32 are fused into `f_30_that_obj`. The optional formal variants *a quien/a quienes/al cual/a la cual* are absorbed into f_30.

**Output contract**: this column appears in `biber_es()` output and always returns `0`. Consult `f_30_that_obj` for the merged count.

---

#### f_33_pied_piping

**Biber (1988)**: Pied-piping relative clauses (preposition + relative).

**Spanish equivalent**: Preposition + relative pronoun constructions. Spanish does NOT permit stranded prepositions, so all prepositional relatives are pied-piping.

**Detection method**: dependency-based.

**Include all variants**:
- *prep + que*: *en que, por que, con que*
- *prep + el/la/los/las + que*: *en el que, por la que, con los que*
- *prep + quien/quienes*: *con quien, para quienes, a quien*
- *prep + el cual / la cual / los cuales / las cuales*: *por el cual, en la cual*

**Detection logic**:
- Token *que, quien, quienes, cual, cuales* (or *el/la/los/las cual/es*) in `acl:relcl`
- Preceded by a preposition (`ADP` token) that is part of the relative clause

**Notes**: Spanish has more pied-piping than English because f_61 (stranded prepositions) is impossible. Document this. Critical: this feature absorbs the function that would otherwise distribute between f_33 and f_61 in English.

---

#### f_34_sentence_relatives

**Biber (1988)**: Sentence relatives — relative clauses with clausal antecedent.

**Spanish equivalent**: *Lo que* and *lo cual* with clausal antecedent.

**Detection method**: heuristic-based.

**Include**:
- Sequence *lo que* or *lo cual* at the start of a clause
- Preceded by comma (or other clause-boundary punctuation)
- Antecedent is clausal (not nominal)

**Exclude**:
- *Lo que* in free relatives (*lo que quieras* — what you want) — these are argumental, not commenting on previous clause
- *Lo que* as object of cognition verb (*sabe lo que dijiste* — knows what you said) — goes to f_23 or f_30

**Heuristic shortcut**: comma + *lo que* or *lo cual* at clause initial position is a strong signal. Combine with dependency check that the antecedent is not a noun in the immediate preceding context.

---

#### f_35_because

**Biber (1988)**: *Because* as causative subordinator.

**Spanish equivalent**: *Porque* as primary causative subordinator.

**Detection method**: lexical match with syntactic verification.

**Include**:
Only *porque* (single token after potential normalization).

**Exclude (deliberately)**:
- *Dado que, puesto que, ya que, como* (causal) — these are excluded to maintain the narrow lexical restriction Biber applies in English (which also excludes *since* and *as* in causal uses from f_35).
- *Por que* (two tokens) — different construction (purpose with subjunctive)
- *Por qué* — interrogative

**Notes**: Strict 1-to-1 mapping. The expansion of the list to include other causative subordinators is a deliberate non-choice — it would deviate from Biber's narrow scope.

---

#### f_36_though

**Biber (1988)**: Concessive subordinators *although, though*.

**Spanish equivalent**: *Aunque* as the unique concessive subordinator.

**Detection method**: lexical match.

**Include**:
Only *aunque*.

**Exclude (deliberately)**:
- *Si bien, aun cuando, a pesar de que, por más que* — formal alternatives excluded to mirror Biber's narrow English scope

**Notes**: *Aunque* covers both English *although* and *though* in a single lemma. The mood of the subordinate verb (indicative for real concession, subjunctive for hypothetical) does not affect counting.

---

#### f_37_if

**Biber (1988)**: Conditional subordinators *if, unless*.

**Spanish equivalent**: *Si* (conditional) + locutions for *unless*.

**Detection method**: lexical + syntactic filter.

**Include**:
- *Si* in conditional use (subordinator)
- Multi-token locutions: *a menos que, salvo que* (after the compound step)

**Exclude (critical)**:
- *Si* as indirect interrogative complementizer (*preguntó si venía* — asked if/whether he was coming). This requires checking that the head verb is NOT a verb of communication or cognition.

**Known UDPipe issue**: *Si* at the start of a sentence may be mis-tagged as `CCONJ` rather than `SCONJ`. The detector should be more robust when *si* is in mid-sentence position.

**Notes**: The exclusion of indirect interrogative *si* is critical. If detection over-fires on *si*, the indirect-question filter is missing or weak.

---

#### f_38_other_adv_sub

**Biber (1988)**: Residual category of adverbial subordinators (e.g., *since, while, whereas, whereby*).

**Spanish equivalent**: Residual list of adverbial subordinators not covered by f_35–f_37.

**Detection method**: lexical list + multi-token compound (for locutions).

**Include**:
- Single tokens: *mientras, conforme, según* (when used as subordinator), *cuando* (when used as subordinator)
- Multi-token (after compound step): *mientras_que, desde_que, de_modo_que, de_manera_que, de_tal_forma_que, para_que, siempre_que, con_tal_de_que*

**Notes**: This is an open category. The list above is the baseline; expansion is acceptable for specific corpus needs. *Para que* (purpose subordinator) is included here despite Biber not explicitly covering purpose in his English f_38.

---

### I. Prepositional phrases, adjectives, and adverbs

#### f_39_prepositions

**Biber (1988)**: Total prepositional phrases.

**Spanish equivalent**: Total preposition count.

**Detection method**: POS count.

**Include**:
- All tokens with POS `ADP`

**Notes**:
- UDPipe decomposes contractions *al* (a + el) and *del* (de + el) automatically — the preposition is counted separately.
- Prepositional locutions (*a través de, en cuanto a, a pesar de*) are tagged variably by UDPipe. Accept the noise — Biber does not distinguish simple prepositions from locutions in English either.
- Known UDPipe issue: counts vary slightly depending on noun phrase structure.

---

#### f_40_adj_attr

**Biber (1988)**: Attributive adjectives (modifying a noun within the NP).

**Spanish equivalent**: Adjectives in modifier function.

**Detection method**: POS + dependency.

**Include**:
- POS `ADJ`
- Dependency `amod`
- Position can be pre- or post-nominal (Spanish defaults to post-nominal)

**Exclude (critical)**:
- Past participles functioning as `amod` — those go to f_27 to avoid double-counting
- Present participles (don't exist as adjectives in Spanish; would have gone to f_28 which is intranslatable)

**Notes**: The post-nominal default position in Spanish differs from English's pre-nominal default but does not affect counting — the dependency relation is the discriminator.

---

#### f_41_adj_pred

**Biber (1988)**: Predicative adjectives (post-copula).

**Spanish equivalent**: Adjectives in predicative function depending on copular or pseudo-copular verb.

**Detection method**: dependency-based.

**Include**:
- POS `ADJ`
- In predicative position after one of: *ser, estar, parecer, volverse, quedarse, ponerse, resultar, permanecer*
- In UD: the adjective is the `root` of the copular construction with `cop` dependent (typical UD Spanish encoding)

**Exclude**:
- Past participles in `cop` constructions — those go to f_17 (passive)
- Attributive adjectives (those are f_40)

**Notes**: Pseudo-copulative verbs are a Spanish-specific expansion (English only has *be, seem, become*). All must be included. Spanish grammatical agreement (gender/number) does not affect detection.

---

#### f_42_adverbs

**Biber (1988)**: Total adverbs minus those in specific categories.

**Spanish equivalent**: Residual adverb count.

**Detection method**: count residual.

**Formula**:
```
f_42 = total ADV tokens
     − f_04 (place adverbials)
     − f_05 (time adverbials)
     − f_45 (conjuncts, including their adverb-tagged tokens)
     − f_46 (downtoners)
     − f_47 (hedges, only the single-token ones)
     − f_48 (amplifiers)
     − f_49 (emphatics, only the single-token ones)
```

**Include**:
- All POS `ADV` tokens not subtracted above
- This includes *no* (paralleling English's *not* inclusion)
- High productivity of *-mente* adverbs in Spanish

**Notes**: This is a residual feature. Its accuracy depends on f_04, f_05, and f_45–f_49 being correctly computed first. Errors propagate. Higher mean values in Spanish than English are expected due to *-mente* productivity.

---

### J. Lexical specificity

#### f_43_type_token

**Biber (1988)**: Type-token ratio.

**Spanish equivalent**: Same metric — MATTR over surface forms.

**Detection method**: metric calculation via `quanteda.textstats`.

**Include**:
- All tokens (including punctuation per Biber's English approach)
- Surface forms (not lemmas) — paralleling English

**Critical**: this feature is a **ratio**, NOT a count. It must NOT be normalized per 1000 tokens when `normalize=TRUE`. Hard-code this exclusion in the normalization step.

**Notes**: Spanish TTRs will be systematically lower than English due to richer inflectional morphology (more forms per lemma). This is expected. Values are not interlinguistically comparable. Default measure: MATTR with window size 50.

---

#### f_44_mean_word_length

**Biber (1988)**: Mean word length in characters (excluding punctuation).

**Spanish equivalent**: Same metric — identical implementation to English.

**Detection method**: arithmetic average over **all non-punctuation tokens**.

**Formula** (authoritative):
```
f_44 = sum(nchar(token, type = "chars"))  /  count(token)
       over ALL tokens where upos != "PUNCT"
```

**⚠️ Critical implementation requirements** (these were violated in earlier versions — explicit reinforcement per user decision):

1. **Include ALL non-punctuation tokens** in both numerator and denominator. This means:
   - ✅ Nouns, verbs, adjectives, adverbs, proper nouns
   - ✅ **Determiners, prepositions, pronouns, auxiliaries, conjunctions** — these MUST be counted
   - ✅ Numerals, symbols, particles
   - ❌ Punctuation only (`upos == "PUNCT"`)

2. **Do NOT restrict to lexical content categories.** Filtering by `upos %in% c("NOUN", "VERB", "ADJ", "ADV", "PROPN")` or similar is **incorrect** — that produces a different metric ("mean lexical word length") that does not match Biber's specification and inflates values out of the expected 4–6 range.

3. **Do NOT apply a minimum-length threshold.** Excluding tokens of 1 or 2 characters is **incorrect** — short function words (*y, a, en, el, la, de*) are part of the average. Their exclusion biases the metric upward.

4. **Use `nchar(token, type = "chars")`**, not `nchar(token, type = "bytes")`. Accented characters (*á, é, í, ó, ú, ñ, ü*) count as **one character each**. Byte-mode counting double-counts UTF-8 multi-byte characters.

5. **Critical**: this feature is an **average**, NOT a count. Like f_43, it must NOT be normalized per 1000 tokens when `normalize=TRUE`. Hard-code this exclusion in the normalization step.

**Worked example** (regression sanity check):

For *"El informe fue redactado por el equipo de investigación."* the expected calculation is:

| Token | upos | chars |
|-------|------|-------|
| El | DET | 2 |
| informe | NOUN | 7 |
| fue | AUX | 3 |
| redactado | VERB | 9 |
| por | ADP | 3 |
| el | DET | 2 |
| equipo | NOUN | 6 |
| de | ADP | 2 |
| investigación | NOUN | 13 |
| . | PUNCT | — (excluded) |

Sum = 47, count = 9, **expected f_44 ≈ 5.22**

If `biber_es()` returns 7.6 or 8.75 for this sentence, the implementation is filtering tokens (either by lexical category or by minimum length) — this is a bug per the spec.

**Notes**:
- Spanish averages slightly higher than English due to *-mente* adverbs (e.g., *rápidamente* = 11 chars) and *-ción/-miento* nominalizations, but stay within the **4–6 range** for general text. Values consistently outside this range signal a filter bug.
- Clitics attached to verbs (*dímelo, haciéndolo*): if UDPipe segments them, they count as separate tokens; if not, they inflate single-token lengths. Document the parser's behavior for the chosen UDPipe model.
- Cross-linguistic comparison of raw f_44 values is not meaningful — Spanish is structurally longer-worded due to derivational morphology. Use within-language Z-scores for register comparison.

---

### K. Lexical classes

#### f_45_conjuncts

**Biber (1988)**: Conjunctive adverbs (*consequently, however, therefore*).

**Spanish equivalent**: Conjuncts including frequent multi-token locutions.

**Detection method**: lexical list with multi-token compound preprocessing.

**Include**:
- Single tokens: *además, asimismo, así, también, tampoco, consecuentemente*
- Multi-token (require compound step): *sin_embargo, no_obstante, por_lo_tanto, por_consiguiente, en_consecuencia, por_otra_parte, por_otro_lado, así_pues, con_todo, aun_así, mientras_tanto, en_cambio*

**Critical implementation**: the multi-token locutions MUST be fused via `tokens_compound()` before lookup. Detection on individual *sin* or *embargo* will fail. If f_45 returns 0 for a text containing *sin embargo*, the compound step is not running.

**Notes**: Spanish has more multi-token conjuncts than English. Single tokens like *así, también* may require positional filtering (clause-initial) to avoid false positives — but starting simple (lexical match alone) is acceptable noise.

---

#### f_46_downtoners

**Biber (1988)**: Adverbs that reduce force (*barely, almost, nearly, somewhat*).

**Spanish equivalent**: Downtoners as a list.

**Detection method**: lexical list + multi-token compound for locutions.

**Include**:
- Single tokens: *casi, apenas, ligeramente, levemente, algo, bastante, parcialmente, escasamente, medianamente*
- Multi-token: *un_poco, en_cierta_medida, hasta_cierto_punto*

**Known ambiguities**:
- *Apenas* can be temporal (*apenas llegó, salió* — as soon as he arrived) — accept noise
- *Bastante* can be downtoner or amplifier depending on context — accept ambiguity

**Notes**: *Más o menos* is in f_47 (hedges), not here.

---

#### f_47_hedges

**Biber (1988)**: Hedging expressions (*maybe, perhaps, sort of, kind of*).

**Spanish equivalent**: Epistemic hedges with substantial multi-token component.

**Detection method**: lexical list + multi-token compound + special handling for *unos/unas* + numeral.

**Include**:
- Single tokens: *quizás, quizá, tal vez, acaso, aproximadamente, aparentemente*
- Multi-token: *a_lo_mejor, más_o_menos, algo_así, una_especie_de, alrededor_de, cerca_de*
- Quantitative hedge: *unos/unas* immediately before a `NUM` (e.g., *unos veinte estudiantes*)

**Optional (documented per user decision)**:
- *Como* in approximative use (*había como veinte personas* — "there were like twenty people"). This usage is highly relevant for oral and informal corpora but generates noise in formal/written texts where *como* primarily functions as a comparator or causal subordinator. **Decision** (per `Decisiones_rasgos_biber.md`): include in the lexical list with a documented note that it inflates counts in oral corpora; corpus-specific tuning may be needed.

**Notes**: The quantitative *unos/unas* + numeral case is Spanish-specific and important for oral corpora. Implement as a syntactic check, not a simple lexical match (since *unos* alone is an article).

---

#### f_48_amplifiers

**Biber (1988)**: Adverbs that intensify (*absolutely, completely, extremely, very*).

**Spanish equivalent**: Amplifying adverbs.

**Detection method**: lexical list.

**Include**:
*muy, absolutamente, totalmente, completamente, extremadamente, sumamente, perfectamente, altamente, enormemente, extraordinariamente, notablemente, tremendamente, plenamente, profundamente*

**Optional (corpus-dependent)**:
- Colloquial amplifiers for spoken/informal corpora: *súper, mega, re, mazo, bien* (as in *bien caro*)

**NOT included in baseline**:
- The *-ísimo* superlative morphology (*rapidísimo, importantísimo*) — this is a morphological process, not a lexical item. Future work could add it as a separate feature.

**Notes**: *Muy* will dominate counts (highest frequency amplifier in Spanish).

---

#### f_49_emphatics

**Biber (1988)**: Emphatic expressions (*a lot, really, for sure*).

**Spanish equivalent**: Emphatics with substantial multi-token component.

**Detection method**: lexical list + multi-token compound.

**Include**:
- Single tokens: *mucho, realmente, precisamente, exactamente, justamente*
- Multi-token: *sin_duda, de_verdad, en_serio, claro_que_sí, en_absoluto, para_nada, sí_que, de_hecho*

**Special: *sí que***: this construction (*sí que lo hice*) partially recovers the function of English emphatic *do* (intranslatable as f_12). Include it deliberately.

**Notes**: Colloquial *vaya* and *que* (as in *¡que sí!*) are highly relevant for oral corpora but not in the baseline list — add for spoken-language work.

---

#### f_50_discourse_particles

**Biber (1988)**: Clause-initial discourse markers (*well, now, anyway*).

**Spanish equivalent**: Clause-initial discourse markers + reformulators, with substantial multi-token component.

**Detection method**: lexical list + multi-token compound + positional filter (initial position for some items).

**Include**:
- Single tokens at clause-initial position: *bueno, pues, mira, mire, oye, oiga, claro*
- Multi-token (no positional restriction): *o_sea, es_decir, pues_bueno, ahora_bien, pues_bien, de_todas_formas, de_todas_maneras, en_fin*

**Critical**: *o sea* is the most prototypical Spanish reformulator and MUST be detected. If detection returns 0 for *o sea*, the multi-token compound step is broken.

**Side effect of correct compound**: when *o sea* fuses into *o_sea*, the false positive of *sea* being counted as *ser* main verb (f_19) automatically disappears. Use this as a regression test.

**Optional (variety-dependent)**: *dale* (Rioplatense), *ándale* (Mexican), *vale* (Peninsular).

**Notes**: Spanish discourse particle system is richer than English. Strict positional filter (clause-initial) may reduce false positives at the cost of recall — recommend starting with no positional filter and tightening if precision is too low.

---

### L. Demonstratives

#### f_51_demonstratives

**Biber (1988)**: Demonstratives functioning as determiners (*that book, this idea*).

**Spanish equivalent**: Demonstratives in determiner function across three deixis degrees.

**Detection method**: lexical list + syntactic filter (`det` dependency).

**Include**:
- *este, esta, estos, estas, ese, esa, esos, esas, aquel, aquella, aquellos, aquellas*
- Function must be `det`

**Exclude**:
- Same forms when functioning as pronouns (those go to f_10)
- Neuter forms (*esto, eso, aquello*) — these are exclusively pronominal, never determiners

**Optional consideration**: *dicho, dicha, dichos, dichas* — formal academic demonstrative ("said X"). Frequent in Spanish academic writing but not in Biber's English. Decision: NOT in baseline; document as known gap.

---

### L. Modals

#### f_52_modal_possibility

**Biber (1988)**: Possibility modals (*can, may, might, could*).

**Spanish equivalent**: *Poder* in present, imperfect, and conditional + epistemic periphrasis.

**Detection method**: lemma-based + multi-token compound for *es posible que*.

**Include**:
- Lemma `poder` in finite forms: present indicative (*puedo, puedes, puede, podemos, podéis, pueden*), imperfect (*podía*, etc.), simple conditional (*podría*, etc.)
- Multi-token: *es_posible_que* (epistemic periphrasis, lower frequency)

**Exclude**:
- *Poder* in nominal use (*tiene poder* — has power) — different POS
- Past forms of *poder* (*pude, pudo*) — those are completed past actions, not modal possibility in Biber's sense

**Notes**: Ambiguity between possibility/capacity/permission senses of *poder* is real but irresolvable without context — accept it. Higher frequencies than English equivalents because *poder* is a fully conjugated verb appearing in many tenses, not a defective modal.

---

#### f_53_modal_necessity

**Biber (1988)**: Necessity modals (*ought, should, must*).

**Spanish equivalent**: *Deber* + necessity periphrases *tener que* and *hay que*.

**Detection method**: lemma + multi-token compound.

**Include**:
- Lemma `deber` in present, imperfect, conditional + infinitive
- Multi-token periphrasis: *tener_que* + infinitive
- Multi-token periphrasis: *hay_que* + infinitive (impersonal)

**Critical implementation**:
- *Hay que* MUST be detected here (NOT in f_20 existential)
- *Tener que* MUST be fused via `tokens_compound()` and recognized as a periphrasis
- Both periphrases require checking that `next_token` is `VerbForm=Inf`

**Disambiguation**:
- *Deber* + infinitive (deontic): *debe salir* — counts in f_53
- *Deber de* + infinitive (epistemic): *debe de haber salido* — Spanish norm distinguishes but spoken usage neutralizes. Accept noise; both count.

**Notes**: *Tener que* in spoken Spanish is more frequent than *deber*. Don't underweight it.

---

#### f_54_modal_predictive

**Biber (1988)**: Predictive modals (*will, would, shall*).

**Spanish equivalent**: Synthetic future + conditional + *ir a + infinitive* periphrasis.

**Detection method**: morphological + multi-token compound.

**Include**:
- Synthetic future of indicative (`Tense=Fut`, `Mood=Ind`, `VerbForm=Fin`): *hablará, irá, será*
- Simple conditional (`Mood=Cnd`): *hablaría, iría, sería*
- Compound conditional: *habría hablado*
- Multi-token: *ir_a* (in any tense) + infinitive: *vamos a hablar, va a llover*

**Critical implementation**:
- *Ir a + infinitive* is the most frequent future expression in spoken Spanish and MUST be included
- Requires multi-token compound on *ir a* (or detection of *ir* + *a* + Verb[Inf])
- Be aware: *ir a + place* (*va a Madrid*) is NOT a predictive modal — exclude when the complement is a noun, not an infinitive

**Notes**: The synthetic future of indicative also expresses epistemic probability in present (*tendrá unos 40 años* — he must be about 40). This usage is morphologically identical and will be counted — accept the noise.

---

### M. Specialized verbs

#### f_55_verb_public

**Biber (1988)**: Public verbs of communication (*assert, declare, mention, say, state, suggest*).

**Spanish equivalent**: Lemma list of public communication verbs.

**Detection method**: lemma-based.

**Include** (lemmas):
*decir, afirmar, declarar, mencionar, explicar, señalar, indicar, anunciar, reportar, informar, sostener, alegar, sugerir, argumentar, comunicar, expresar, manifestar, observar, notar, precisar, aclarar, especificar, agregar, añadir*

**Special**:
- *Decir* will dominate frequencies (super-coordinator)
- *Sugerir* is also in f_57 (suasive) — Biber double-lists in English too, mirror this

---

#### f_56_verb_private

**Biber (1988)**: Private verbs of mental process (*assume, believe, doubt, know, think*).

**Spanish equivalent**: Lemma list of mental-process verbs.

**Detection method**: lemma-based.

**Include** (lemmas):
*creer, pensar, saber, conocer, sentir, considerar, suponer, asumir, dudar, entender, comprender, esperar, imaginar, recordar, reconocer, percibir, parecer*

Plus multi-token: *darse_cuenta, tener_en_cuenta*

**Special considerations**:
- *Saber* vs *conocer*: both are included (they distribute English *know*)
- *Esperar* covers both *expect* and *hope* — include
- *Parecer* — INCLUDED here, but ALSO in f_58. Decision: count in both (double-count is intentional, mirrors Biber English where *seem* appears only in f_58 but functionally overlaps with private verbs).

**Notes**: Reflexive forms (*sentirse, encontrarse, imaginarse*) should be detected if `lemma == "sentir"` etc.

---

#### f_57_verb_suasive

**Biber (1988)**: Suasive verbs (commanding, recommending, requesting).

**Spanish equivalent**: Lemma list of suasive verbs.

**Detection method**: lemma-based.

**Include** (lemmas):
*ordenar, mandar, insistir, proponer, recomendar, solicitar, pedir, sugerir, instar, rogar, suplicar, exigir, requerir, aconsejar, prohibir, permitir, demandar, decretar*

**Special**:
- *Pedir* (request, not *preguntar* "ask a question") — both have English *ask*, but only *pedir* is suasive
- *Sugerir* also in f_55 — count in both
- Subjunctive is virtually obligatory after these verbs in Spanish, generating a systematic correlation with f_21

---

#### f_58_verb_seem

**Biber (1988)**: Evidentiality verbs *seem* and *appear*.

**Spanish equivalent**: *Parecer* (and only *parecer*).

**Detection method**: lemma-based.

**Include** (lemma):
- *parecer* in finite forms: *parece, parecen, parecía, pareció, parecería, parezca*
- Plus *parecer ser* (multi-token if compound applies)

**Critical exclude**:
- *Aparecer* is NOT included. Despite being the cognate of English *appear*, in Spanish it means "to appear physically" (*apareció en la reunión*), not epistemic evidence. Including it would be a categorial error.

**Optional**:
- *Resultar* (in evidentiality use: *resulta que*) — include for academic registers
- *Verse* (*se ve que*) — include for informal/oral registers

**Notes**: *Parecer* is ALSO in f_56. Both features will count it. This intentional double-counting mirrors how *seem* relates to mental-process verbs in English.

---

### N. Reduced forms

#### f_59_contractions ⚠️ ZERO-OUTPUT

**Biber (1988)**: Contracted forms of auxiliaries and negations (*I'm, she's, they've, don't, won't, can't*). Markers of informal register.

**Spanish equivalent**: **None — structurally non-transferable.**

**Detection method**: column always emits `0`.

**Why zero-output**: standard Spanish has no grammaticalized contractions equivalent to English. The two obligatory contractions *al* (a + el) and *del* (de + el) are **mandatory in all registers** — they are not informality markers. The English f_59 measures register variation; the Spanish *al/del* would measure nothing. Colloquial phonological reductions written in transcription (*pa* for *para*, *to* for *todo*, *na* for *nada*) exist only in highly informal written corpora and would require corpus-specific configuration far from Biber's original definition.

**Output contract**: this column appears in `biber_es()` output and always returns `0`.

---

#### f_60_that_deletion ⚠️ ZERO-OUTPUT

**Biber (1988)**: Omission of subordinator *that* in complement clauses (*I think he went*).

**Spanish equivalent**: **None — structurally non-transferable.**

**Detection method**: column always emits `0`.

**Why zero-output**: the Spanish complementizer *que* is virtually obligatory in complement clauses (*\*creo fue* is ungrammatical). There is essentially nothing to count — the deletion phenomenon does not exist in standard Spanish. The systematic zero value is itself a typologically interesting datum: Spanish marks complementation overtly where English allows reduction.

**Output contract**: this column appears in `biber_es()` output and always returns `0`.

---

#### f_61_stranded_preposition ⚠️ ZERO-OUTPUT

**Biber (1988)**: Prepositions stranded at the end of a clause (*the candidate I was thinking of*).

**Spanish equivalent**: **None — categorically ungrammatical in Spanish.**

**Detection method**: column always emits `0`.

**Why zero-output**: stranded prepositions are not stylistically marked in Spanish — they are simply ungrammatical. Spanish requires pied-piping (*el candidato en el que pensaba*). This is the structural mirror of f_33's higher frequency in Spanish: the function that English distributes between f_33 (formal) and f_61 (informal) collapses entirely into f_33 in Spanish. The non-existence of f_61 and the elevated frequency of f_33 are two sides of the same typological coin (see §2.5).

**Output contract**: this column appears in `biber_es()` output and always returns `0`. The function it would capture is absorbed by `f_33_pied_piping`.

---

#### f_62_split_infinitive ⚠️ ZERO-OUTPUT

**Biber (1988)**: Adverb inserted between *to* and the infinitive (*to boldly go*).

**Spanish equivalent**: **None — the construction requires a preverbal infinitive marker (*to*) that Spanish lacks.**

**Detection method**: column always emits `0`.

**Why zero-output**: Spanish infinitives are single morphological words (*ir, comer, vivir*) with no separable marker that could host an inserted adverb. The construction is structurally impossible. A loose Spanish analog might track adverb position relative to the infinitive (*adverbio + infinitivo* vs. *infinitivo + adverbio*) as a stylistic variable, but this would be a different feature, not f_62.

**Output contract**: this column appears in `biber_es()` output and always returns `0`.

---

#### f_63_split_auxiliary

**Biber (1988)**: Adverb inserted between auxiliary and main verb.

**Spanish equivalent**: Same construction with Spanish auxiliaries.

**Detection method**: dependency-based positional check.

**Include**:
- Auxiliary verb (lemma `haber, ser, estar, poder, deber, ir`) + adverb + main verb (participle or infinitive)
- The adverb is in linear position between the two verbs

**Exclude**:
- Clitic pronouns occupying the intermediate position (those are pronouns, not adverbs)
- Adverbs preceding the entire periphrasis (*aparentemente fue mostrado* — adverb is pre-auxiliary, not split)
- Adverbs following the entire periphrasis (*fue mostrado aparentemente* — adverb is post-V, not split)

**Notes**: Spanish allows more flexible adverb positioning than English, making this construction less marked. Frequencies will be lower than English's split auxiliary. Clitics are the main false-positive risk — make sure to filter by POS=ADV.

---

### O. Coordination

#### f_64_phrasal_coordination

**Biber (1988)**: Phrasal coordination via *and* (NP and NP, AdjP and AdjP, etc.).

**Spanish equivalent**: Phrasal coordination via *y/e*.

**Detection method**: dependency-based.

**Include**:
- Token *y* or *e* with `cc` dependency
- Connecting two elements of the same POS (NOUN+NOUN, ADJ+ADJ, VERB+VERB, ADV+ADV)

**Exclude (deliberately)**:
- *O, u* (disjunctive) — exclude to mirror Biber English (which only includes *and*)
- *Ni* (negative coordinator) — exclude same reason
- *Pero* — that's adversative, not in f_64

**Notes**: *Y* vs *e* are allomorphs of the same coordinator (*e* before words starting with i-/hi-). Both must be included. In multi-element coordinations (*A, B y C*), *y* appears only before the last element — UDPipe usually marks it correctly.

---

#### f_65_clausal_coordination

**Biber (1988)**: *And* in clause-initial position coordinating independent clauses.

**Spanish equivalent**: *Y/e* in clause-initial position.

**Detection method**: positional + POS check.

**Include**:
- *Y* or *e* in clause-initial position
- "Clause-initial" defined as: preceded by sentence-final punctuation (period, semicolon, colon) OR at the absolute start of the document

**Exclude**:
- *Y/e* in mid-sentence coordination (those are f_64)
- *Pero, mas* in clause-initial position (excluded to mirror Biber's English exclusion of *but*)

**Notes**: Detection quality depends on punctuation quality of the corpus. Poorly punctuated texts will under-detect this feature.

---

### P. Negation

#### f_66_neg_synthetic

**Biber (1988)**: Synthetic negation — negative determiners and pronouns (*no, neither, nobody, nothing, nowhere, never*).

**Spanish equivalent**: Negative concord items.

**Detection method**: lexical list.

**Include**:
*nadie, nada, ningún, ninguno, ninguna, ningunos, ningunas, nunca, jamás, tampoco*

**Notes**:
- These can appear in any clause position (pre- or post-verbal)
- They coexist with *no* (analytic negation, f_67) in post-verbal position due to mandatory negative concord (*no vino nadie* = both f_66 and f_67 count)
- Count the negative item regardless of *no*'s presence — this is correct, not double-counting

---

#### f_67_neg_analytic

**Biber (1988)**: Analytic negation via *not* and contracted forms.

**Spanish equivalent**: Pre-verbal *no*.

**Detection method**: lexical + positional.

**Include**:
- Token *no* in pre-verbal position (its function is to negate the following verb)

**Exclude**:
- *No* as one-word response (turn-final or sentence-final, no verb to negate)
- *No* as prefix in compound words (*no-violento, no-gubernamental*) — typically hyphenated
- *No* in fixed locutions (*no obstante, no bien*) — these are conjuncts, going to f_45

**Notes**:
- Simpler than English: no contractions to disambiguate (Spanish *no* never contracts)
- The coexistence with f_66 (negative concord) is grammatically correct, not a bug
- Critical exclusion: when *no obstante* gets fused via `tokens_compound()`, the *no* inside should NOT also count as analytic negation. Verify after compound step.

---

## 4. Implementation priorities

When auditing or implementing the package, address features in this priority order:

### Priority 1: Foundation (must work first)
- f_39 prepositions (simplest, used in many other detection paths)
- f_01, f_02, f_03 (tense features — basic morphological filters)
- f_43, f_44 (metrics — verify they're NOT being normalized)

### Priority 2: Multi-token infrastructure
- The `tokens_compound()` step in `biber_es()` must run before any block
- Verify by checking that *o sea, sin embargo, hay que, ir a* appear as compound tokens after the step
- Without this, f_45, f_47, f_49, f_50, f_53, f_54 all silently underperform

### Priority 3: Dependency-based features
- f_17, f_18 (passives — including se-passive!)
- f_29, f_30 (relatives — verify the merger logic)
- f_33 (pied-piping — verify all variants detected)
- f_19, f_20 (ser/estar + hay/hay que disambiguation)

### Priority 4: Specialized lists
- f_55, f_56, f_57, f_58 (verb lemma lists)
- f_45, f_47, f_49, f_50 (multi-token discourse markers)

### Priority 5: Residuals (depend on others)
- f_16 (depends on f_14)
- f_42 (depends on f_04, f_05, f_45–f_49)
- f_14 (verify suffix regex doesn't over-match on *lado* etc.)

---

## 5. Validation checklist

After implementation, verify with the validation corpus that:

**Output contract (67-column layout)**:

- [ ] `biber_es()` returns exactly **67 columns** with `f_` prefix
- [ ] No feature numbered above f_67 exists in output (no f_68, f_69, f_70, etc.)
- [ ] The 10 zero-output columns exist and **always return 0**: `f_09_pronoun_it`, `f_12_proverb_do`, `f_15_gerunds`, `f_28_present_participle_whiz`, `f_31_wh_subj`, `f_32_wh_obj`, `f_59_contractions`, `f_60_that_deletion`, `f_61_stranded_preposition`, `f_62_split_infinitive`
- [ ] No duplicate columns with `_rate`, `_count`, or `_raw` suffix
- [ ] Column order matches `pseudobibeR.fr` output for cross-language compatibility

**Metric columns (must NOT be normalized)**:

- [ ] `f_43_type_token` is NOT divided by 1000 tokens when `normalize=TRUE`
- [ ] `f_44_mean_word_length` is NOT divided by 1000 tokens when `normalize=TRUE`
- [ ] `f_44_mean_word_length` returns values in the **4–6 range** for general Spanish text
- [ ] `f_44_mean_word_length` uses **all non-punctuation tokens** (not filtered to lexical categories, no minimum-length threshold)

**Multi-token detection**:

- [ ] Multi-token expressions (*o sea, sin embargo, hay que, ir a, a lo mejor, de verdad*) appear as compound tokens after `tokens_compound()` preprocessing
- [ ] `f_50` detects *o sea* (returns ≥1 for *"O sea, llegó tarde"*)
- [ ] `f_45` detects *sin embargo* (returns ≥1 for *"Sin embargo, llegó"*)
- [ ] `f_53` detects *hay que* (returns ≥1 for *"Hay que esperar"*)
- [ ] `f_20` does NOT detect *hay que* (returns 0 for the same text — captured by f_53)
- [ ] `f_19` does NOT count *sea* when part of *o sea*
- [ ] `f_17` detects se-passive (returns ≥1 for *"Se publicaron los resultados"*)

**Other invariants**:

- [ ] `demonstrative_matchlist` exists and is loaded without warnings
- [ ] `f_14_nominalizations` does NOT match *lado, grado, partido* (they are not nominalizations)
- [ ] `f_23_wh_clause` does NOT count relative *que* (only accented interrogatives + *lo que*)
- [ ] `f_29_that_subj` absorbs both *that*-relative and *who/which*-relative subject positions
- [ ] `f_30_that_obj` absorbs both *that*-relative and *who/which*-relative object positions

---

## 6. Known limitations and acceptable noise

The following are documented limitations, not bugs:

1. **Zero-output columns are by design, not bugs** (see §1 and §2.6): the 10 columns `f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, f_62` always return 0 because they represent either untranslatable features or merged-source slots. They exist for surface-parity with `pseudobibeR.fr`. Their consistent zero values are the correct output.

2. **Subject-null underestimates pronouns** (f_06–f_08): Spanish allows omitting subject pronouns; count is structurally low. Cross-linguistic comparison of raw pronoun counts is not meaningful.

3. **Ambiguous lexical items**: *ya, apenas, bastante, mucho, parece, como* have multiple senses that detection cannot disambiguate without semantic resources. Accept noise; document the ambiguity in feature-level notes.

4. **Se-passive vs reflexive**: distinguishing these requires semantic knowledge (animacy of subject, transitivity of verb). The package uses heuristics; some noise is unavoidable.

5. **UDPipe known issues**:
   - Relative *que* tagged as `SCONJ/mark`, not `PRON`
   - *Si* clause-initial sometimes tagged `CCONJ`
   - Accented interrogatives (*quién, dónde*) lemmatized inconsistently
   - Adjective predicative is the root of copular construction
   - These are handled in the relevant blocks; alternative parsers (spaCy AnCora) may need separate handling.

6. **Imperfect not counted in f_01**: deliberate decision to mirror English simple past. Imperfect's narrative function is acknowledged but separated.

7. **Frequencies not interlingually comparable**: f_43, f_44, f_29, f_30, f_33, f_67 systematically differ from English values for structural reasons. Use within-language Z-scores for cross-linguistic comparison.

8. **f_22 in copular constructions**: UDPipe does not always tag the head of *que* as `ADJ` in copular constructions (*es importante que vengas*), which causes occasional under-detection. Documented limitation; would require parser-specific patches to resolve.

9. **f_50 noise without positional filter**: discourse particles (*bueno, pues*) appearing in non-initial positions are counted, inflating values slightly. The user decision authorizes this tradeoff for higher recall (see §f_50).
