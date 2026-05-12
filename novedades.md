# Novedades de pseudobibeR.es

## Versión 0.022

### App Shiny — columna "Palabras" ahora muestra locuciones multi-token

La función `extract_evidence()` en `app.R` solo mostraba tokens con lema
individual (filtraba entradas con `_`). Locuciones como *sin embargo*,
*a lo mejor*, *o sea*, *de verdad*, *hay que*, etc. nunca aparecían en
la columna Palabras aunque el rasgo tuviera Valor > 0.

**Nuevo helper `detect_mwe_evidence()`**: escanea la secuencia de tokens
buscando coincidencias con todas las entradas multi-token del diccionario
(`dict[["f_45"]]$sin_embargo`, etc.). Por cada coincidencia reconstituye
la frase original ("Sin embargo", "A lo mejor") y la agrupa por rasgo.

**Nuevo helper `combine_ev()`**: fusiona la evidencia de lema individual
con la evidencia MWE por feature, eliminando duplicados y respetando el
límite de 8 resultados visibles.

**Rasgos actualizados**:
- f_04/f_05 adverbiales, f_35/f_36/f_38 subordinadores: añaden evidencia MWE
- f_45/f_46/f_47/f_48/f_49/f_50: ahora muestran locuciones completas
- f_53 modal necesidad: colecta por lema cabeza (deber/haber/tener) para
  capturar formas flexionadas "hay", "debemos", "tiene que"; excluye
  condicional y futuro que van a f_54
- f_19 ser/estar principal: aplica `!in_mwe` (excluye "sea" en "o sea",
  "es" en "es decir"); excluye dep_rel=aux y dep_rel=aux:pass
- f_20 haber existencial: filtra `pos == "VERB"` (UDPipe marca el haber
  existencial como VERB root; el auxiliar en tiempos compuestos como AUX)
- f_17 pasiva agentless: añade "se" pronominales además de aux:pass
- f_39 preposiciones: muestra todas las ADP (alineado con el contador)
- f_42 adverbios: excluye lemmas de f_04/f_05/f_45/negación/wh para
  evitar duplicados con las categorías especializadas
- f_56 verbos privados: excluye lemas de f_58 (parecer/resultar)
- f_54 modal predictivo: añade Mood=Cnd (condicional) al criterio futuro

---

## Versión 0.021

### Marcador de locuciones multi-palabra para la rama sintáctica

El mecanismo `quanteda::tokens_compound()` ya fusionaba locuciones antes del
`tokens_lookup` para la rama léxica, pero los bloques de rasgos sintácticos
(`block_*_es()`) operan sobre el dataframe parseado por UDPipe — donde "o
sea", "es decir", "sin embargo", etc. siguen siendo tokens individuales con
su lemma original. Esto causaba falsos positivos: "sea" en *o sea* y "es" en
*es decir* se contaban como `ser` principal en f_19, "hay" en *hay que* en
algunos contextos como existencial f_20, etc.

**`R/utils_extraction.R` — nuevo `flag_mwe_tokens()`**: marca con
`in_mwe = TRUE` cada token cuya posición pertenece a una secuencia contigua
que coincide (case-insensitive, mismo `doc_id` + `sentence_id`) con alguno de
los patrones de `multiword_patterns`. Algoritmo paralelo al de
`tokens_compound()`: barrido de ventanas de tamaño n por cada patrón.

**`R/parse_functions.R`**: se invoca una sola vez después de extraer
morfología y antes de ejecutar cualquier bloque de rasgos, garantizando que
todos los bloques compartan el mismo flag.

**`R/features_passive.R` — f_19 (ser/estar principal)**: filtra
`!in_mwe`. Elimina los FP de "sea" en *o sea*, "es" en *es decir*, "sea" en
*es decir que sea*, etc. Sin afectar usos legítimos como *"la situación es
compleja"*.

### Validación

- Se añade **`text_08_casos_limite`** al corpus de validación: texto con
  casos negativos específicos para perifrasis modales (*hay que*, *tener que*,
  *vamos a*), discourse particles que contienen formas de *ser* (*o sea*,
  *es decir*), existencial real coexistiendo con *hay_que*, y *si*
  condicional vs interrogativo indirecto.
- Resultado: **0 FAIL, 166 OK, 15 TOL** en 181 comparaciones sobre 8 textos
  (100% pass rate por texto). Los 9 rasgos críticos de text_08 pasan con
  delta = 0 exacto.

---

## Versión 0.020

### Sistema de validación

Se crea el sistema de validación bajo `validation/`:

- **`validation/test_corpus.yaml`**: 7 textos representativos de distintos registros (narrativa en 3.ª persona, académico/expositivo, conversacional/interactivo, instruccional/prescriptivo, futuro y modales predictivos, demostrativos/relativos/negación, verbos especializados/hedges/downtoners) con 172 conteos esperados rasgo por rasgo, tolerancias por rasgo y notas de decisión.
- **`validation/run_validation.R`**: script que parsea cada texto con UDPipe (modelo `spanish-gsd`), ejecuta `biber_es()`, compara con los conteos esperados y genera:
  - `validation/output/validation_report.md` — reporte legible con tabla de resultados por texto y rasgo.
  - `validation/output/validation_report.csv` — versión tabular para análisis posterior.
- Resultado final: **0 FAIL, 157 OK, 15 TOL** en los 7 textos (100 % de tasa de aprobación).

---

### Correcciones de rasgos

#### `R/utils_extraction.R` — `dictionary_to_lemmas()`

- Nuevo parámetro `head_word = FALSE`.
  - **Modo por defecto (`head_word = FALSE`)**: solo pasan entradas de una sola palabra. Las locuciones multi-token (`a_menudo`, `de_vez_en_cuando`, `sin_embargo`, `tal_vez`) se manejan exclusivamente en la rama quanteda (`tokens_lookup`), que las captura como *compounds*. Extraer el primer token de esas locuciones ("a", "de", "sin", "tal") introducía preposiciones y palabras comunes, causando sobreconteo masivo en f_04, f_05, f_11, f_46 y f_47.
  - **Modo `head_word = TRUE`**: extrae el primer token de cada locución. Usado en perifrasis modales (`hay_que` → `hay`, `tener_que` → `tener`, `haber_de` → `haber`) para buscar el verbo cabeza en el árbol sintáctico.

#### `R/features_passive.R` — f_17 pasiva sin agente (se-pasiva)

- UDPipe Spanish-GSD lematiza el "se" pasivo-reflejo como "él" con `dep_rel = iobj` y **no** marca `Voice = Pass` ni `dep_rel = expl:pass`. La detección anterior nunca encontraba nada.
- Nuevo criterio: `tolower(token) == "se"` + `pos = PRON` + verbo finito (`VerbForm = Fin`) de 3.ª persona como *head*. Detecta correctamente pasivas reflejas (*se publicaron los hallazgos*), impersonales (*se debe seguir el procedimiento*) y pasivas de proceso (*se recomienda utilizar*).

#### `R/features_tense_pronouns.R` y `R/features_stance.R` — f_08 pronombres de 3.ª persona

- **`data-raw/dict.yaml`**: se eliminan "el", "la", "los", "las" (sin tilde) de `f_08_third_person_pronouns`. La rama quanteda no verifica POS y coincidía con artículos DET, inflando el conteo en más de 10 ocurrencias por texto.
- Se añade exclusión de `Reflex = Yes` en el suplemento morfológico (`supplement_pronouns_morphological_fr`) y en `count_person_pronouns()`, evitando que el "se" pasivo/reflexivo/dativo se cuente como pronombre referencial de 3.ª persona.

#### `R/features_subordination.R` — f_24, f_33, f_23

- **f_24 infinitivos**: el filtro de `dep_rel` excluía infinitivos con `dep_rel = root` (perifrasis modales: *se debe seguir* → `seguir` como raíz con `debe` como AUX) y `dep_rel = csubj`. Se simplifica a contar todos los tokens con `VerbForm = Inf` y `pos = VERB`.
- **f_33 pied-piping**: el patrón en Spanish-GSD es ADP + DET + PRON (*con los que*, *en el que*, *por los cuales*). El filtro anterior exigía ADP inmediatamente antes del relativo. Se añade detección con `lag(n = 2)`: ADP dos posiciones antes, DET en posición intermedia. También se acepta SCONJ (UDPipe a veces etiqueta *que* como SCONJ en estas construcciones).
- **f_23 cláusulas wh**: "que" sin tilde estaba en `wh_lemmas`, atrayendo todos los relativos como interrogativas indirectas. Se aplica `PronType = Int` en feats para PRON; solo lemmas con tilde para ADV (`dónde`, `cuándo`, `cómo`, `cuánto/a/os/as`); SCONJ completamente excluido.

#### `R/features_coordination_negation.R` — f_66 negación sintética

- *nunca*, *jamás*, *tampoco* estaban en `neg_analytic_adverbs` y nunca se contaban en ningún rasgo. Se agregan explícitamente a `neg_synthetic_terms` dentro de `block_negation_es()`.
- Se acepta `pos = CCONJ` para detectar "ni" (UDPipe lo etiqueta como conjunción coordinante).
- Se elimina el `anti_join` que excluía palabras negativas cuando el verbo cabeza ya tenía "no" preverbal. La concordancia negativa española (*no vino nadie*, *nunca lo hizo nadie*) es obligatoria: Biber cuenta ambos elementos de forma independiente (f_66 + f_67).

#### `R/features_modals_verbs.R` — f_39, f_42, f_56

- **f_39 preposiciones**: se elimina el filtro `dep_rel %in% c("case", "fixed")`. UDPipe etiqueta algunas preposiciones como `mark` cuando introducen cláusulas no finitas (*Para obtener*, *antes de comenzar*). Biber cuenta todas las ADP como medida de densidad nominal/frasal.
- **f_42 adverbios generales**: se amplía la lista de exclusión para incluir lemmas de f_04 (lugar), f_05 (tiempo), f_45 (conjunciones adverbiales) y wh-adverbs con tilde (*dónde*, *cuándo*, *cómo*, *cuánto/a/os/as*). Evita el doble conteo con las categorías especializadas correspondientes.
- **f_56 verbos privados**: se excluyen los lemmas de f_58 (*parecer*, *resultar*) para evitar doble conteo. *Parecer* y *resultar* son verbos evidenciales epistémicos (f_58), no verbos de proceso mental (f_56).
- **f_52/f_53 perifrasis modales**: se usa `head_word = TRUE` en las llamadas a `dictionary_to_lemmas()` para recuperar correctamente el verbo cabeza de las locuciones (`hay_que` → `hay`, `tener_que` → `tener`).

---

### Aplicación Shiny (`app.R`)

- Se elimina la columna "Tipo" de la tabla de resultados (ya no hay extensiones españolas que distinguir de los rasgos originales de Biber).
- Se retira el `selectInput("fil_tipo", ...)` de la barra de filtros.
- `feat_cols` excluye `f_69`, `f_70` y `f_71` (`!grepl("^f_(69|70|71)_", feat_cols)`) — se calculan internamente pero no se muestran en la tabla.
- Se actualizan `columnDefs`, se elimina el `rowCallback` JS y el `formatStyle("Tipo")`.
- Footer actualizado: "57 rasgos de Biber adaptados al español".
- CSS: se elimina `.row-ext` (ya no hay filas de extensión que destacar visualmente).

---

### README

- README reescrito siguiendo la estructura de `pseudobibeR.fr`: badges, descripción general, instalación, inicio rápido, notas de parseo específicas para UDPipe Spanish-GSD, tests, app Shiny, diccionarios, tabla de 57 rasgos con ejemplos, tabla de rasgos eliminados y tabla de rasgos fusionados.
- Corregida la aritmética: 8 rasgos eliminados + 2 fusionados = 10 ausentes → 67 − 10 = **57 rasgos**.
- `f_71` completamente eliminado del README (no es un rasgo de Biber).
- Notas de parseo para UDPipe Spanish-GSD: se-pasivas, pro-drop, copulativas *ser/estar*, condicional *si*, relativas con *que* como SCONJ.
