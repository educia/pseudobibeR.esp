# pseudobibeR.es

[![R-CMD-check](https://github.com/browndw/pseudobibeR.es/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Tests](https://github.com/browndw/pseudobibeR.es/workflows/Tests/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`pseudobibeR.es` implementa, para textos en lengua española, el catálogo de rasgos lexicogramaticales y funcionales descrito por Biber (1988). Constituye la contraparte hispánica de los paquetes `pseudobibeR` (inglés) y `pseudobibeR.fr` (francés), con los que comparte el inventario de referencia. Su propósito es facilitar el análisis multidimensional de variación de registro y género textual mediante la agregación, por documento, de las 67 categorías originales del marco de Biber, adaptadas a las propiedades morfosintácticas del español.

## Descripción general

El paquete no realiza por sí mismo el etiquetado gramatical ni el análisis sintáctico. Se apoya en herramientas externas de análisis morfosintáctico (principalmente [UDPipe](https://bnosac.github.io/udpipe/en/) con el modelo `spanish-gsd`) para obtener una representación en Universal Dependencies, sobre la cual se aplican las heurísticas de extracción específicas del español implementadas en los bloques `block_*_es()`. La precisión de los rasgos extraídos depende, por tanto, de la calidad del análisis morfológico y de dependencias subyacente.

El español presenta diferencias estructurales relevantes respecto del inglés y del francés que han condicionado el diseño del extractor:

- El **sujeto nulo** o *pro-drop* hace innecesario un pronombre expletivo equivalente al inglés *it*.
- La **negación** opera mediante palabras negativas preverbales o posverbales con concordancia negativa obligatoria.
- Las **cláusulas relativas** introducidas por *que* funcionan tanto en posición de sujeto como de objeto.
- El **gerundio** español no admite la función nominal del *-ing* inglés.

### Contrato de salida

La salida principal contiene **67 columnas de rasgo** organizadas en las 16 categorías de Biber (A–P). De ellas:

- **57 columnas** corresponden a rasgos con detección lingüística efectiva (55 rasgos únicos del español más 2 fusiones: f_29 absorbe f_31, y f_30 absorbe f_32).
- **10 columnas** son cicatrices de paridad superficial con valor constante igual a cero, conservadas para mantener la compatibilidad columna a columna con `pseudobibeR.fr` y permitir la sustitución directa en flujos de análisis preexistentes. Estas columnas corresponden a rasgos intraducibles al español: f_09 (`pronoun_it`), f_12 (`proverb_do`), f_15 (`gerunds`), f_28 (`present_participle_whiz`), f_31 (`wh_subj`), f_32 (`wh_obj`), f_59 (`contractions`), f_60 (`that_deletion`), f_61 (`stranded_preposition`) y f_62 (`split_infinitive`).

El recuento real, por tanto, asciende a 55 rasgos detectados más 2 fusiones, lo que mantiene la equivalencia funcional con las 57 categorías efectivas de Biber (1988) tras descontar las inaplicables al español.

## Instalación

### Versión de desarrollo desde GitHub

```r
# install.packages("devtools")
devtools::install_github("browndw/pseudobibeR.es")
```

### Instalación desde un archivo `tar.gz`

1. Se descarga el archivo `pseudobibeR.es_<versión>.tar.gz` desde la [página de versiones del repositorio](https://github.com/browndw/pseudobibeR.es/releases).
2. En una sesión de R se ejecuta:

   ```r
   install.packages("/ruta/al/archivo/pseudobibeR.es_<versión>.tar.gz",
                    repos = NULL, type = "source")
   ```

3. Se reinicia la sesión de R para que el paquete recién instalado quede disponible.

## Inicio rápido

La función principal es `biber_es()`. Recibe la salida de `udpipe::udpipe_annotate()` obtenida con un modelo Universal Dependencies para español y devuelve un `data.frame` con una fila por documento.

```r
library(udpipe)
library(pseudobibeR.es)

# Descarga del modelo (solo la primera vez)
model    <- udpipe_download_model(language = "spanish-gsd")
ud_model <- udpipe_load_model(model$file_model)

# Corpus de prueba
text_data <- data.frame(
  doc_id = c("doc_1", "doc_2"),
  text = c(
    "El informe fue redactado por el equipo de investigación.",
    "La autora explica que el método permite comparar dos modelos."
  )
)

# Anotación morfosintáctica
parsed_data <- udpipe_annotate(
  ud_model,
  x      = text_data$text,
  doc_id = text_data$doc_id,
  tagger = "default",
  parser = "default"
)

# Extracción de rasgos
features <- biber_es(parsed_data, measure = "MATTR", normalize = TRUE)
print(features)
```

## Notas de parseo específicas del español

- **Modelo UDPipe recomendado**: `spanish-gsd-ud-2.5-191206.udpipe`. Las heurísticas del paquete han sido validadas contra este modelo. Existen modelos más recientes (AnCora, entre otros) cuyo uso no se garantiza al mismo nivel.
- **Campo `feats` obligatorio**: el extractor depende de las características morfológicas codificadas en la columna `feats` (por ejemplo, `Tense=Past`, `Mood=Ind`, `VerbForm=Fin`). Se debe invocar `udpipe_annotate()` con `parser = "default"` y `tagger = "default"`.
- **Sujeto nulo (pro-drop)**: en los rasgos f_06 a f_08 se cuentan exclusivamente los pronombres personales explícitos. El rasgo f_09, propio del inglés (`it` expletivo), no se aplica al español y se preserva como columna constante igual a cero.
- **Relativas con *que***: el modelo `spanish-gsd` etiqueta el pronombre relativo *que* como `SCONJ/mark`, no como `PRON`. El extractor contempla este comportamiento: las relativas introducidas por *que* se contabilizan en `f_29_that_subj`, mientras que `f_30_that_obj` captura las relativas con *quien* o *cual* en posición oblicua.
- **Condicional *si***: cuando *si* aparece en posición inicial de cláusula puede ser etiquetado como `CCONJ`. La heurística es más estable cuando *si* ocupa una posición intermedia.
- **Construcciones copulativas**: en Universal Dependencies para el español, el adjetivo predicativo es la raíz de la construcción copulativa, mientras que *ser* o *estar* dependen como `cop`. El extractor detecta correctamente ambos patrones para `f_41_adj_pred`.

## Características principales

- **67 columnas de rasgo** organizadas en 16 categorías (A–P), con 57 detectores activos y 10 columnas constantes igual a cero por paridad superficial con `pseudobibeR.fr`.
- **Normalización opcional** a frecuencias por 1 000 tokens léxicos.
- **Cuatro medidas de diversidad léxica** disponibles para f_43 (`MATTR`, `TTR`, `CTTR`, `MSTTR`).
- **Integración con UDPipe** mediante la interfaz nativa de R.
- **Funciones complementarias**: `biber_es_batch()` para procesamiento por lotes y `biber_es_traced()` para análisis con trazabilidad de evidencia token a token.
- **Exportación a XLSX** mediante `write_biber_xlsx()`, con orientación tidy compatible con `readxl`, `pandas` y SPSS.
- **Diccionarios y listas léxicas** específicas del español incluidos como datos del paquete.
- **Suite de pruebas exhaustiva** (más de 640 pruebas) con cobertura de regresión para los patrones complejos detectados en auditoría sistemática.

## Dependencias y requisitos

### Dependencias del núcleo

- R (≥ 3.5.0)
- `dplyr`, `purrr`, `quanteda`, `quanteda.textstats`, `rlang`, `stringi`, `stringr`, `tibble`, `magrittr`.

### Componentes para el análisis morfosintáctico

- Paquete [`udpipe`](https://bnosac.github.io/udpipe/en/).
- Modelo UDPipe `spanish-gsd-ud-2.5`, que puede descargarse con:

  ```r
  udpipe::udpipe_download_model("spanish-gsd")
  ```

### Dependencias opcionales

- `writexl` y `readxl`: requeridos para `write_biber_xlsx()` y para la lectura de corpus en formato Excel.
- `cli`: utilizado para barras de progreso en modo robusto de `biber_es_batch()`.

## Argumentos de la función `biber_es()`

La función `biber_es()` admite tres argumentos:

| Argumento | Descripción |
|-----------|-------------|
| `tokens` | Objeto devuelto por `udpipe::udpipe_annotate()` o un `data.frame` con las columnas `doc_id`, `token`, `lemma`, `upos`, `xpos`, `feats`, `head_token_id`, `dep_rel`. |
| `measure` | Medida de diversidad léxica empleada para f_43. Valores admitidos: `"MATTR"` (recomendada para corpus de longitud variable), `"TTR"`, `"CTTR"`, `"MSTTR"` o `"none"`. Valor por defecto: `"MATTR"`. |
| `normalize` | Lógico. Si `TRUE` (valor por defecto), los conteos se normalizan a frecuencia por 1 000 tokens léxicos. Si `FALSE`, se devuelven los recuentos absolutos. |

### Ejemplo con parámetros personalizados

```r
features <- biber_es(parsed_data,
                     measure   = "MATTR",
                     normalize = TRUE)
```

### Valor de retorno

Un `data.frame` con una fila por documento y las siguientes columnas:

- `doc_id`: identificador del documento, heredado de la anotación.
- `f_01_past_tense` a `f_67_neg_analytic`: 67 columnas con los conteos (o frecuencias normalizadas) de los rasgos de Biber. Las 10 columnas zero-output presentan siempre el valor cero.
- `n_tokens`: número total de tokens del documento, excluyendo puntuación.
- `n_lex_tokens`: número de tokens léxicos (NOUN, VERB, ADJ, ADV, PROPN).

## Rasgos lingüísticos extraídos

A continuación se enumeran los 67 rasgos organizados según las categorías originales de Biber (1988). Las columnas constantes igual a cero, conservadas por paridad con `pseudobibeR.fr`, se identifican mediante el símbolo &nbsp;0️⃣.

### A. Tiempo y aspecto

| Código | Descripción |
|--------|-------------|
| `f_01_past_tense` | Pretérito indefinido (`Tense=Past, Mood=Ind, VerbForm=Fin`). |
| `f_02_perfect_aspect` | Aspecto perfecto: *haber* + participio. |
| `f_03_present_tense` | Presente de indicativo. |

### B. Adverbiales de lugar y tiempo

| Código | Descripción |
|--------|-------------|
| `f_04_place_adverbials` | Adverbiales de lugar (*aquí*, *allí*, *encima*…). |
| `f_05_time_adverbials` | Adverbiales de tiempo (*ayer*, *hoy*, *siempre*…). |

### C. Pronombres y proverbos

| Código | Descripción |
|--------|-------------|
| `f_06_first_person_pronouns` | Pronombres de primera persona explícitos. |
| `f_07_second_person_pronouns` | Pronombres de segunda persona explícitos. |
| `f_08_third_person_pronouns` | Pronombres de tercera persona explícitos. |
| `f_09_pronoun_it` &nbsp;0️⃣ | Pronombre expletivo *it*. Inexistente en español por sujeto nulo. |
| `f_10_demonstrative_pronoun` | Pronombres demostrativos (*esto*, *eso*, *aquello*…). |
| `f_11_indefinite_pronouns` | Pronombres indefinidos (*alguien*, *nadie*, *algo*…). |
| `f_12_proverb_do` &nbsp;0️⃣ | Proverbo *do*. Sin equivalente productivo en español. |

### D. Interrogativas

| Código | Descripción |
|--------|-------------|
| `f_13_wh_question` | Preguntas directas con palabra interrogativa. |

### E. Formas nominales

| Código | Descripción |
|--------|-------------|
| `f_14_nominalizations` | Nominalizaciones derivadas (sufijos productivos: *-ción*, *-idad*, *-miento*, etc.). |
| `f_15_gerunds` &nbsp;0️⃣ | Gerundios nominales. El gerundio español no admite función nominal. |
| `f_16_other_nouns` | Sustantivos restantes (NOUN/PROPN, excluyendo las nominalizaciones contadas en f_14). |

### F. Pasivas

| Código | Descripción |
|--------|-------------|
| `f_17_agentless_passives` | Pasivas sin agente expreso (perifrásticas y se-pasivas). |
| `f_18_by_passives` | Pasivas con agente introducido por *por*. |

### G. Formas estativas

| Código | Descripción |
|--------|-------------|
| `f_19_be_main_verb` | *Ser* o *estar* como verbo principal (cópula nominal o adjetival). |
| `f_20_existential_there` | *Haber* impersonal existencial (*hay*, *había*, *hubo*…). |

### H. Subordinación

| Código | Descripción |
|--------|-------------|
| `f_21_that_verb_comp` | *Que* complementante tras verbo. |
| `f_22_that_adj_comp` | *Que* complementante tras adjetivo. |
| `f_23_wh_clause` | Cláusula con palabra *wh-* (interrogativa indirecta). |
| `f_24_infinitives` | Infinitivos en función de complemento o como núcleo de perífrasis modal. |
| `f_25_present_participle` | Gerundio adverbial o de complemento. |
| `f_26_past_participle` | Participio adverbial o absoluto. |
| `f_27_past_participle_whiz` | Participio postnominal (reducción de relativa de objeto). |
| `f_28_present_participle_whiz` &nbsp;0️⃣ | Gerundio postnominal. Construcción agramatical en español normativo. |
| `f_29_that_subj` | Cláusulas relativas con *que* (fusión de f_29 y f_31; absorbe el caso de relativa de sujeto). |
| `f_30_that_obj` | Cláusulas relativas con *quien* o *cual* en posición oblicua (fusión de f_30 y f_32). |
| `f_31_wh_subj` &nbsp;0️⃣ | Relativa de sujeto con *wh-*. Absorbida en f_29. |
| `f_32_wh_obj` &nbsp;0️⃣ | Relativa de objeto con *wh-*. Absorbida en f_30. |
| `f_33_pied_piping` | Preposición + relativo (*del que*, *con el cual*…). |
| `f_34_sentence_relatives` | Relativas oracionales (*lo que*, *lo cual*…). |
| `f_35_because` | Subordinada causal con *porque*. |
| `f_36_though` | Subordinada concesiva con *aunque*. |
| `f_37_if` | Subordinada condicional con *si*. |
| `f_38_other_adv_sub` | Otros subordinantes adverbiales (*cuando*, *mientras*, *según*…). |

### I. Sintagmas preposicionales, adjetivos y adverbios

| Código | Descripción |
|--------|-------------|
| `f_39_prepositions` | Preposiciones (POS = ADP). |
| `f_40_adj_attr` | Adjetivos atributivos (relación de dependencia `amod`). |
| `f_41_adj_pred` | Adjetivos predicativos. |
| `f_42_adverbs` | Adverbios generales (excluyendo los ya capturados en otros rasgos). |

### J. Especificidad léxica

| Código | Descripción |
|--------|-------------|
| `f_43_type_token` | Diversidad léxica (MATTR u otra medida seleccionada). |
| `f_44_mean_word_length` | Longitud media en caracteres de los tokens no puntuados. |

### K. Clases léxicas

| Código | Descripción |
|--------|-------------|
| `f_45_conjuncts` | Conjuntos textuales (*sin embargo*, *por tanto*, *además*…). |
| `f_46_downtoners` | Atenuadores (*casi*, *apenas*, *ligeramente*…). |
| `f_47_hedges` | Modalizadores epistémicos (*quizás*, *tal vez*, *probablemente*…). |
| `f_48_amplifiers` | Amplificadores (*muy*, *totalmente*, *enormemente*…). |
| `f_49_emphatics` | Enfáticos (*de hecho*, *sin duda*, *realmente*…). |
| `f_50_discourse_particles` | Partículas discursivas (*bueno*, *pues*, *claro*…). |
| `f_51_demonstratives` | Determinantes demostrativos (*este*, *ese*, *aquel*…). |

### L. Modales

| Código | Descripción |
|--------|-------------|
| `f_52_modal_possibility` | Modal de posibilidad (*poder + infinitivo*). |
| `f_53_modal_necessity` | Modal de necesidad (*deber + infinitivo*, *tener que + infinitivo*, *haber que + infinitivo*). |
| `f_54_modal_predictive` | Modal predictivo: futuro sintético, condicional y *ir a + infinitivo*. |

### M. Verbos especializados

| Código | Descripción |
|--------|-------------|
| `f_55_verb_public` | Verbos públicos (*afirmar*, *declarar*, *anunciar*…). |
| `f_56_verb_private` | Verbos privados o cognitivos (*creer*, *pensar*, *saber*…). |
| `f_57_verb_suasive` | Verbos suasivos (*recomendar*, *pedir*, *sugerir*…). |
| `f_58_verb_seem` | Verbos de apariencia (*parecer*, *resultar*). |

### N. Formas reducidas

| Código | Descripción |
|--------|-------------|
| `f_59_contractions` &nbsp;0️⃣ | Contracciones ortográficas. Inexistentes en español escrito normativo. |
| `f_60_that_deletion` &nbsp;0️⃣ | Omisión de *que* complementante. Construcción marginal en español escrito. |
| `f_61_stranded_preposition` &nbsp;0️⃣ | Preposición varada. Imposible en español: la preposición siempre precede al relativo. |
| `f_62_split_infinitive` &nbsp;0️⃣ | Infinitivo escindido. Imposible en español. |
| `f_63_split_auxiliary` | Auxiliar separado del verbo principal por un elemento interpuesto. |

### O. Coordinación

| Código | Descripción |
|--------|-------------|
| `f_64_phrasal_coordination` | Coordinación de sintagmas (N y N, ADJ y ADJ…). |
| `f_65_clausal_coordination` | Coordinación de cláusulas independientes. |

### P. Negación

| Código | Descripción |
|--------|-------------|
| `f_66_neg_synthetic` | Negación sintética (*nadie*, *nunca*, *jamás*, *tampoco*…). |
| `f_67_neg_analytic` | Negación analítica (*no* + verbo). |

## Limitaciones conocidas

Las siguientes limitaciones obedecen a decisiones de diseño documentadas o a comportamientos del modelo `spanish-gsd`. No constituyen errores: el comportamiento observado es el esperado tras la auditoría sistemática del paquete.

- **f_22_that_adj_comp**: el modelo UDPipe `spanish-gsd` no etiqueta consistentemente el núcleo de *que* como `ADJ` en las construcciones copulativas (por ejemplo, *es importante que…*). El rasgo presenta subdetección por limitación del analizador sintáctico.
- **f_26_past_participle** y **f_27_past_participle_whiz**: el participio absoluto y el participio postnominal se etiquetan con una relación de dependencia variable según el contexto. La detección es correcta cuando el análisis sintáctico acierta, pero presenta inconsistencias en construcciones límite.
- **f_50_discourse_particles**: la implementación actual no aplica filtro posicional, por lo que ciertos monotokens (*bueno*, *claro*) pueden contabilizarse fuera de la posición inicial de cláusula. Este ruido está autorizado por la especificación.
- **f_29 y f_30 en cláusulas con sujeto elidido**: en construcciones con sujeto nulo y sin `nsubj` explícito, la distinción entre relativa de sujeto y de objeto no es recuperable; la relativa se imputa a `f_29_that_subj`.
- **Rasgos intraducibles**: las 10 columnas de paridad superficial (f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, f_62) devuelven siempre cero. Su justificación lingüística se detalla en la tabla anterior.

## Funciones complementarias

El paquete proporciona dos funciones adicionales que **no sustituyen** a `biber_es()`, sino que la complementan en escenarios específicos. Ambas comparten el catálogo de detectores con `biber_es()` y producen conteos equivalentes; sus diferencias residen en la interfaz de entrada y en el formato de salida.

### `biber_es_batch()`: procesamiento por lotes

Función orientada al procesamiento de corpus extensos almacenados en disco o en memoria. Combina la ingestión de datos, el análisis morfosintáctico con UDPipe y la extracción de rasgos en una sola invocación.

#### Firma

```r
biber_es_batch(input,
               model,
               text_column = "text",
               id_column   = NULL,
               trace       = FALSE,
               safe        = FALSE,
               measure     = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
               normalize   = TRUE,
               progress    = TRUE)
```

#### Parámetros

| Parámetro | Descripción |
|-----------|-------------|
| `input` | Ruta a un archivo CSV (cadena de caracteres) o `data.frame` en memoria. La interfaz polimórfica admite ambos sin configuración adicional. Los archivos CSV se leen con codificación UTF-8 forzada. |
| `model` | Modelo UDPipe previamente cargado mediante `udpipe::udpipe_load_model()`. Parámetro obligatorio: no admite valor por defecto para evitar efectos de red implícitos. |
| `text_column` | Nombre de la columna que contiene el texto de cada documento. Valor por defecto: `"text"`. |
| `id_column` | Nombre de la columna de identificadores. Si es `NULL` (valor por defecto), se generan identificadores automáticos con el patrón `doc_0001`, `doc_0002`, etc. |
| `trace` | Lógico. Si `TRUE`, el resultado incluye un elemento `evidence` con la trazabilidad token a token. Valor por defecto: `FALSE`. |
| `safe` | Lógico. Si `FALSE` (modo rápido, valor por defecto), se realiza una única invocación de UDPipe sobre el vector completo de textos; un fallo individual interrumpe el procesamiento. Si `TRUE` (modo robusto), se procesa documento por documento con `tryCatch`; los fallos individuales se acumulan en el elemento `failed_docs` del resultado. |
| `measure` | Medida de diversidad léxica para f_43. Equivalente al parámetro homónimo de `biber_es()`. |
| `normalize` | Lógico. Normalización de los conteos a frecuencia por 1 000 tokens léxicos. Valor por defecto: `TRUE`. |
| `progress` | Lógico. Muestra una barra de progreso durante el procesamiento en modo robusto cuando el paquete `cli` está disponible. Valor por defecto: `TRUE`. Sin efecto en modo rápido. |

#### Estructura de retorno

Una lista con los siguientes elementos:

- `counts`: `data.frame` con orientación *tidy* y una fila por documento. Contiene `doc_id`, las columnas de metadatos del corpus de entrada (si las hay), los 67 rasgos `f_NN_*`, y las columnas auxiliares `n_tokens` y `n_lex_tokens`. El total habitual es de 70 columnas más las columnas de metadatos heredadas.
- `evidence` (solo si `trace = TRUE`): tibble en formato largo con el esquema descrito en `biber_es_traced()`.
- `failed_docs` (solo si `safe = TRUE`): `data.frame` con tres columnas (`doc_id`, `error_message`, `stage`). Está vacío si todos los documentos se procesaron correctamente.

#### Modos de operación

- **Modo rápido** (`safe = FALSE`, valor por defecto): apropiado para corpus curados y homogéneos. Maximiza el rendimiento al procesar todos los documentos en una sola pasada de UDPipe. Un fallo en cualquier documento interrumpe la ejecución.
- **Modo robusto** (`safe = TRUE`): apropiado para corpus heterogéneos o de procedencia desconocida. Procesa los documentos individualmente; los fallos se aíslan y se reportan en `result$failed_docs`. El sobrecoste de rendimiento es modesto.

#### Exportación a XLSX

La escritura del resultado a un archivo Excel se realiza mediante la función auxiliar `write_biber_xlsx()`:

```r
write_biber_xlsx(resultado, path = "salida.xlsx", include_per_1k = TRUE)
```

El archivo generado contiene las hojas `raw` (conteos brutos o normalizados según corresponda), `metadata` (fecha de generación, versión del paquete, dimensiones del corpus) y, opcionalmente, `per_1k` (frecuencias normalizadas adicionales), `evidence` (si se invocó con `trace = TRUE`) y `failed_docs` (si se invocó con `safe = TRUE` y existen fallos). La orientación es *tidy* en todas las hojas (un documento por fila), lo que permite la reimportación directa con `readxl::read_xlsx()` en R, `pandas.read_excel()` en Python o el importador estándar de SPSS sin necesidad de transposiciones manuales.

#### Ejemplo reproducible

```r
library(udpipe)
library(pseudobibeR.es)

ud_model <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")

# Corpus con metadatos
corpus <- data.frame(
  doc_id = c("d1", "d2", "d3"),
  genero = c("narrativo", "academico", "conversacional"),
  ano    = c(2020, 2021, 2022),
  text   = c(
    "María llegó tarde a la reunión.",
    "El método permite comparar dos modelos estadísticos.",
    "Sí, claro, lo que se quiera."
  )
)

resultado <- biber_es_batch(
  input       = corpus,
  model       = ud_model,
  text_column = "text",
  id_column   = "doc_id",
  safe        = TRUE,
  normalize   = FALSE
)

# Inspección del resultado
str(resultado$counts[, 1:8])
nrow(resultado$failed_docs)  # 0 si todos los documentos se procesaron correctamente

# Exportación a Excel
write_biber_xlsx(resultado, path = "corpus_biber.xlsx", include_per_1k = TRUE)
```

### `biber_es_traced()`: análisis con trazabilidad de evidencia

Función orientada a la auditoría manual, la depuración del análisis y los usos didácticos. Devuelve, además de los conteos por documento, un *tibble* en formato largo que enumera los tokens individuales que dispararon cada detección.

#### Firma

```r
biber_es_traced(tokens,
                measure   = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                normalize = TRUE)
```

Los parámetros tienen el mismo significado que en `biber_es()`.

#### Estructura de retorno

Una lista con dos elementos:

- `counts`: `data.frame` idéntico al producido por `biber_es()`, con una fila por documento y 70 columnas (`doc_id`, 67 rasgos, `n_tokens`, `n_lex_tokens`). La compatibilidad con `biber_es()` es bit a bit: ambas funciones devuelven los mismos valores numéricos para los mismos parámetros.
- `evidence`: tibble en formato largo con nueve columnas:

| Columna | Tipo | Contenido |
|---------|------|-----------|
| `doc_id` | character | Identificador del documento. |
| `feature` | character | Código del rasgo, por ejemplo `f_01_past_tense`. |
| `sentence_id` | integer | Posición de la oración dentro del documento. |
| `token_id` | integer | Posición del token dentro de la oración (identificador UD). |
| `token` | character | Forma superficial del token. |
| `lemma` | character | Lema asignado por UDPipe. |
| `upos` | character | Categoría gramatical universal (NOUN, VERB, ADJ, …). |
| `feats` | character | Atributos morfológicos en formato UD (`Tense=Past\|Mood=Ind\|VerbForm=Fin`). |
| `head_token_id` | integer | Identificador del núcleo sintáctico para reconstruir la dependencia. |

#### Interpretación de la columna `evidence`

No todos los rasgos generan filas de evidencia. La siguiente tabla resume el comportamiento esperado según el tipo de detector:

| Tipo de rasgo | Comportamiento de la evidencia | Ejemplos |
|---------------|--------------------------------|----------|
| Detección estructural directa | Una fila por token detectado. Se cumple la igualdad `count == nrow(evidence)`. | f_01–f_03, f_13, f_14, f_16, f_17–f_20, f_21, f_23–f_27, f_29, f_30, f_33–f_42, f_52, f_54, f_63–f_67. |
| Detección con apoyo de suplementos | Número variable de filas, acotado superiormente por el conteo (`1 ≤ nrow(evidence) ≤ count`). | f_06, f_07, f_08, f_10, f_11, f_51, f_53, f_55–f_58. |
| Métricas continuas | No producen filas de evidencia. El valor se encuentra exclusivamente en `counts`. | f_43 (TTR), f_44 (longitud media). |
| Rasgos sin equivalente en español | No producen filas de evidencia. La columna correspondiente en `counts` presenta siempre el valor cero. | f_09, f_12, f_15, f_28, f_31, f_32, f_59–f_62. |

Para los rasgos de la categoría K (f_45–f_50), cuya detección se realiza por búsqueda en diccionario, la evidencia token a token no se produce en la versión actual. Si se requiere recuperarla manualmente, se puede emplear:

```r
parsed_data %>% dplyr::filter(tolower(lemma) %in% dict$f_47_hedges)
```

#### Ejemplo reproducible

```r
library(udpipe)
library(pseudobibeR.es)

ud_model <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")
parsed   <- udpipe_annotate(ud_model,
                            x      = "Quizás el resultado depende de otros factores.",
                            doc_id = "doc_1")

# Llamada equivalente a biber_es() pero con trazabilidad
resultado <- biber_es_traced(parsed, measure = "none", normalize = FALSE)

# Los conteos coinciden con biber_es()
identical(resultado$counts,
          biber_es(parsed, measure = "none", normalize = FALSE))
#> [1] TRUE

# Tokens que dispararon detecciones en este documento
resultado$evidence
```

## Desarrollo y pruebas

El paquete utiliza [`testthat`](https://testthat.r-lib.org/) para las pruebas unitarias y de integración. Los recursos fuente (oraciones de ejemplo, diccionarios, listas léxicas) se almacenan en `data-raw/`. El paquete distribuye únicamente los objetos `.rda` compilados en `data/` y los fixtures sintéticos declarados directamente en los archivos de prueba.

### Estructura de pruebas

- `tests/testthat/test-spanish-basic.R`: fixtures sintéticos para rasgos principales sin necesidad de UDPipe.
- `tests/testthat/test-spanish-examples.R`: pruebas de integración con UDPipe, alimentadas por `data-raw/spanish_examples.yaml`.
- `tests/testthat/test-spanish-modals.R`: cobertura de perífrasis modales (*poder + infinitivo*, *deber + infinitivo*, *tener que + infinitivo*, *ir a + infinitivo*, futuro sintético).
- `tests/testthat/test-feature-coverage-evidence.R`: invariante de cobertura de evidencia para los 55 rasgos con detección activa.
- `tests/testthat/test-evidence-audit-regression.R`: pruebas de regresión específicas para los detectores corregidos durante la auditoría sistemática (f_06, f_23, f_24, f_47).

### Ejecución de las pruebas

```r
# Suite completa
testthat::test_package("pseudobibeR.es")

# Archivos específicos
testthat::test_file("tests/testthat/test-spanish-basic.R")
testthat::test_file("tests/testthat/test-feature-coverage-evidence.R")
```

## Datos del paquete

El paquete incluye los siguientes conjuntos de datos como objetos exportados:

- `dict`: patrones de diccionario para la detección léxica de rasgos (conjunciones, adverbios, pronombres, modales, verbos especializados).
- `word_lists`: listas léxicas auxiliares (sufijos de nominalización, listas de exclusión, formas pronominales, marcadores demostrativos).

Pueden consultarse mediante:

```r
library(pseudobibeR.es)

names(dict)                                # diccionarios disponibles
word_lists$nominalization_suffixes         # sufijos de nominalización
word_lists$f_06_first_person_pronouns      # pronombres de primera persona
```

## Actualización de diccionarios y ejemplos

Los recursos léxicos se editan en los archivos YAML de `data-raw/` y se compilan al formato `.rda` mediante un script de construcción. Las contribuciones deben preservar tanto el archivo YAML editado como el `.rda` regenerado para mantener la coherencia entre el desarrollo y la distribución.

1. Edición del YAML correspondiente (`data-raw/dict.yaml`, `data-raw/word_lists.yaml` o `data-raw/spanish_examples.yaml`).
2. Regeneración de los objetos del paquete:

   ```r
   source("data-raw/build_french_dictionaries.R")
   ```

3. Validación con `devtools::test()` y `devtools::document()`.

## Cita

Al utilizar `pseudobibeR.es` en publicaciones académicas se recomienda citar tanto la obra fundacional como el paquete:

**Obra fundacional:**

> Biber, D. (1988). *Variation across speech and writing*. Cambridge University Press. <https://doi.org/10.1017/CBO9780511621024>

**Paquete:**

> Cordovez, M. (2024). *pseudobibeR.es: Extracción de rasgos morfológicos de Biber para español*. R package version 0.1.0. <https://github.com/browndw/pseudobibeR.es>

## Aplicación Shiny

El repositorio incluye una aplicación Shiny (`app.R`) para la exploración interactiva del catálogo de rasgos sobre texto libre. Su ejecución requiere que el modelo `spanish-gsd-ud-2.5-191206.udpipe` esté presente en el directorio de trabajo.

```r
shiny::runApp()
```

## Contribuciones

Las contribuciones al paquete pueden remitirse mediante *issues* o *pull requests* en [GitHub](https://github.com/browndw/pseudobibeR.es). Al reportar problemas se recomienda incluir:

- Un ejemplo mínimo reproducible.
- La versión de R y de los paquetes implicados.
- El modelo UDPipe utilizado y su versión.

## Licencia

Distribuido bajo licencia MIT. Véase el archivo [LICENSE](LICENSE) para los términos completos.

## Referencias y recursos

- Biber, D. (1985). Investigating macroscopic textual variation through multifeature/multidimensional analyses. *Linguistics*, 23(2), 337–360. <https://doi.org/10.1515/ling.1985.23.2.337>
- Biber, D. (1988). *Variation across speech and writing*. Cambridge University Press.
- [`pseudobibeR`](https://github.com/browndw/pseudobibeR): versión original para textos en inglés.
- [`pseudobibeR.fr`](https://github.com/browndw/pseudobibeR.fr): versión para textos en francés.
- [`udpipe`](https://bnosac.github.io/udpipe/en/): herramienta de análisis morfosintáctico para R.
- [`quanteda`](https://quanteda.io/): marco de análisis cuantitativo de texto empleado internamente.
- [Universal Dependencies](https://universaldependencies.org/): especificación del formato de anotación sintáctica empleado.
