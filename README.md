# pseudobibeR.es

[![R-CMD-check](https://github.com/browndw/pseudobibeR.es/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Tests](https://github.com/browndw/pseudobibeR.es/workflows/Tests/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`pseudobibeR.es` adapta al español el catálogo de 67 rasgos lexicogramaticales propuesto por Biber (1988) para el análisis multidimensional de variación de registro y género textual. El paquete mantiene el mismo inventario de referencia que `pseudobibeR` (inglés) y `pseudobibeR.fr` (francés), lo que hace posible la comparación directa entre lenguas en estudios de corpus multilingüe.

## Descripción general

`pseudobibeR.es` no realiza etiquetado gramatical ni análisis sintáctico propio: trabaja sobre las anotaciones que produce [UDPipe](https://bnosac.github.io/udpipe/en/) con el modelo `spanish-gsd`, que sigue el estándar de Universal Dependencies. A partir de esa representación morfosintáctica, el paquete cuantifica cada rasgo mediante heurísticas de extracción diseñadas específicamente para el español. La calidad de los recuentos depende directamente de la precisión del análisis morfosintáctico subyacente.

El español presenta propiedades gramaticales que han condicionado el diseño de varios detectores:

- El **sujeto nulo** (*pro-drop*) implica que los pronombres personales de sujeto se omiten con frecuencia. Solo se cuentan los pronombres explícitos, y no existe en español un pronombre expletivo equivalente al inglés *it*.
- La **negación** puede realizarse mediante palabras negativas preverbales o posverbales con concordancia negativa obligatoria, lo que requiere heurísticas distintas a las del inglés.
- Las **cláusulas relativas** introducidas por *que* funcionan tanto en posición de sujeto como de objeto de la relativa, lo que dificulta su distinción sistemática.
- El **gerundio** español no admite la función nominal del gerundio inglés (*-ing*): la forma *nadando* no equivale a *swimming* cuando este actúa como sustantivo.

### Contrato de salida

La función principal devuelve siempre **67 columnas de rasgo**, organizadas en las 16 categorías de Biber (A–P). La cifra corresponde al marco original; en español no todas son aplicables:

- **57 columnas** registran rasgos con detección activa. Las relativas con *quien*/*el cual* se cuentan integradas en `f_29`/`f_30` (relativas con *que*, en función de sujeto y de complemento directo respectivamente): `f_31` y `f_32` se mantienen como columnas siempre-cero, reservadas por compatibilidad de esquema. `f_15` se evaluó como candidato a detección (infinitivo nominal-sujeto) pero se revirtió a su comportamiento original por decisión del usuario.
- **10 columnas** valen siempre cero porque los rasgos que representan no existen en español o son ajenos al estándar escrito normativo: `f_09` (*it* expletivo), `f_12` (proverbo *do*), `f_15` (gerundio nominal), `f_28` (gerundio postnominal), `f_31` (relativas con *quien*/*el cual* en función de sujeto, integradas en `f_29`), `f_32` (íd. en función de complemento directo, integradas en `f_30`), `f_59` (contracciones ortográficas), `f_60` (omisión del complementante *que*), `f_61` (preposición varada) y `f_62` (infinitivo escindido). Estas columnas se conservan para garantizar la compatibilidad con `pseudobibeR` y `pseudobibeR.fr` y permitir su sustitución directa en flujos de análisis preexistentes.

> **Nota:** el comportamiento real de cada rasgo sobre el
> modelo `spanish-gsd`, con sus etiquetas nuevas y limitaciones documentadas,
> está en [`TABLA_RASGOS_ES.md`](TABLA_RASGOS_ES.md); la comparación
> antes/después, en [`TABLA_COMPARATIVA_ES.md`](TABLA_COMPARATIVA_ES.md).

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

- **Modelo UDPipe recomendado**: `spanish-gsd-ud-2.5-191206.udpipe`. Las heurísticas del paquete han sido validadas con este modelo. Existen modelos más recientes (AnCora, entre otros) cuya compatibilidad no se garantiza en el mismo grado.
- **Campo `feats` obligatorio**: los detectores dependen de las características morfológicas en la columna `feats` (por ejemplo, `Tense=Past`, `Mood=Ind`, `VerbForm=Fin`). Es necesario invocar `udpipe_annotate()` con `parser = "default"` y `tagger = "default"`.
- **Sujeto nulo (pro-drop)**: en los rasgos `f_06` a `f_08` se cuentan los pronombres personales explícitos **y los posesivos** (ruteados por persona: *mi*/*nuestro* → f_06, *tu*/*vuestro* → f_07, *su* → f_08). El rasgo `f_09`, propio del inglés (*it* expletivo), no tiene equivalente en español y se conserva como columna constante igual a cero.
- **Relativas con *que***: el modelo `spanish-gsd` etiqueta el pronombre relativo *que* como `SCONJ/mark`, no como `PRON`. El paquete contempla este comportamiento: las relativas introducidas por *que* se contabilizan en `f_29_that_subj` (sujeto) y `f_30_that_obj` (objeto, cuando hay sujeto explícito en la relativa); las relativas con *quien*/*el cual* se fusionan en esos mismos conteos (`f_31`/`f_32` se mantienen siempre en cero, por decisión del usuario), y las de preposición antepuesta van a `f_33`.
- **Condicional *si***: cuando *si* aparece en posición inicial de cláusula, puede ser etiquetado como `CCONJ`. La heurística es más estable cuando *si* ocupa una posición intermedia.
- **Construcciones copulativas**: en Universal Dependencies para el español, el adjetivo predicativo es la raíz de la construcción copulativa, mientras que *ser* o *estar* dependen como `cop`. El extractor detecta ambos patrones para `f_41_adj_pred`, incluidos los casos de cópula invertida (sujeto tácito o pospuesto) sin restricción de lema en el verbo — por eso también cuentan los complementos predicativos (*llegó cansada*, *encontraron abierta la puerta*, *se considera relevante*), no solo los atributos de *ser*/*estar*.

## Características principales

- **67 columnas de rasgo** en 16 categorías (A–P): 57 con detección activa y 10 siempre en cero, conservadas para mantener la compatibilidad con `pseudobibeR` y `pseudobibeR.fr`.
- **Normalización opcional** a frecuencias por 1 000 tokens léxicos.
- **Cuatro medidas de diversidad léxica** para `f_43`: `MATTR`, `TTR`, `CTTR` y `MSTTR`.
- **Integración con UDPipe** mediante la interfaz nativa de R.
- **Funciones complementarias**: `biber_es_batch()` para procesar corpus extensos en lote y `biber_es_traced()` para auditar las detecciones token a token.
- **Exportación a XLSX** mediante `write_biber_xlsx()`, en formato de una fila por documento, directamente importable con `readxl`, `pandas` y SPSS.
- **Diccionarios y listas léxicas** específicos del español incluidos como datos del paquete.
- **Suite de pruebas** (más de 640 pruebas unitarias e integración) con cobertura de regresión para los detectores de mayor complejidad.

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
| `measure` | Medida de diversidad léxica para `f_43`. Valores admitidos: `"MATTR"` (recomendada para corpus de longitud variable), `"TTR"`, `"CTTR"`, `"MSTTR"` o `"none"`. Valor por defecto: `"MATTR"`. |
| `normalize` | Lógico. Si `TRUE` (valor por defecto), los recuentos se normalizan a frecuencia por 1 000 tokens léxicos. Si `FALSE`, se devuelven los recuentos absolutos. |

### Ejemplo con parámetros personalizados

```r
features <- biber_es(parsed_data,
                     measure   = "MATTR",
                     normalize = TRUE)
```

### Valor de retorno

Un `data.frame` con una fila por documento y las siguientes columnas:

- `doc_id`: identificador del documento, heredado de la anotación.
- `f_01_past_tense` a `f_67_neg_analytic`: 67 columnas con los recuentos (o frecuencias normalizadas) de los rasgos. Las 10 columnas sin equivalente en español presentan siempre el valor cero.
- `n_tokens`: número total de tokens, excluida la puntuación.
- `n_lex_tokens`: número de tokens léxicos (NOUN, VERB, ADJ, ADV, PROPN).

## Rasgos lingüísticos extraídos

El paquete extrae 67 rasgos lexicogramaticales organizados según las categorías propuestas por Biber (1988). Cada rasgo representa un fenómeno lingüístico asociado con la variación entre registros y géneros textuales. Aunque la nomenclatura conserva los nombres originales para mantener la compatibilidad con `pseudobibeR` y `pseudobibeR.fr`, las definiciones y heurísticas han sido adaptadas al español. Las columnas que siempre valen cero se identifican con el símbolo &nbsp;0️⃣.

### A. Tiempo y aspecto

Estos rasgos describen cómo el texto sitúa los eventos en el tiempo y expresa las relaciones aspectuales entre ellos. En conjunto, permiten caracterizar la orientación temporal predominante de cada género textual.

| Código | Descripción |
|--------|-------------|
| `f_01_past_tense` | Verbos en tiempos de pasado de indicativo y subjuntivo (*cantó*, *cantaba*, *cantara*, *había cantado*). Se identifica mediante `Tense ∈ {Past, Imp, Pqp}` sobre verbo finito, sin filtrar el modo. |
| `f_02_perfect_aspect` | Aspecto perfecto, expresado mediante la perífrasis *haber* + participio (*ha llegado*, *habían terminado*). Indica que la acción tiene relevancia en el momento de referencia. |
| `f_03_present_tense` | Verbos en presente de indicativo (*habla*, *se observa*, *representa*). Característico de textos expositivos, normativos y de carácter general. |

### B. Adverbiales de lugar y tiempo

Los adverbiales de lugar y tiempo anclan los eventos en coordenadas espaciales y temporales concretas. Su frecuencia es especialmente alta en textos narrativos e informativos.

| Código | Descripción |
|--------|-------------|
| `f_04_place_adverbials` | Adverbios y locuciones de lugar (*aquí*, *allí*, *arriba*, *encima*, *lejos*...). |
| `f_05_time_adverbials` | Adverbios y locuciones de tiempo (*ayer*, *hoy*, *siempre*, *entonces*, *después*...). |

### C. Pronombres y proverbos

Estos rasgos capturan cómo el texto hace referencia a los participantes del discurso mediante pronombres personales, demostrativos e indefinidos. Dado el carácter pro-drop del español, solo se cuentan los pronombres explícitos; su frecuencia es, por tanto, sistemáticamente menor que en inglés o francés.

| Código | Descripción |
|--------|-------------|
| `f_06_first_person_pronouns` | Pronombres personales de primera persona explícitos, tanto singulares (*yo*, *me*, *mí*, *conmigo*) como plurales (*nosotros*, *nos*, *nuestro*...). |
| `f_07_second_person_pronouns` | Pronombres personales de segunda persona explícitos (*tú*, *te*, *ti*, *vosotros*, *usted*, *ustedes*...). |
| `f_08_third_person_pronouns` | Pronombres personales de tercera persona explícitos (*él*, *ella*, *ellos*, *le*, *lo*, *la*...). |
| `f_09_pronoun_it` &nbsp;0️⃣ | En inglés, *it* puede funcionar como sujeto expletivo sin referente real (*it rains*, *it seems that...*). El español no dispone de este pronombre: el verbo puede aparecer sin sujeto explícito. Esta columna se conserva por compatibilidad y siempre devuelve cero. |
| `f_10_demonstrative_pronoun` | Pronombres demostrativos en función pronominal (*esto*, *eso*, *aquello*, *este*, *ese* sin sustantivo). |
| `f_11_indefinite_pronouns` | Pronombres indefinidos (*alguien*, *nadie*, *algo*, *nada*, *cualquiera*, *todo*...). |
| `f_12_proverb_do` &nbsp;0️⃣ | En inglés, *do* sustituye a un sintagma verbal previamente mencionado (*She sings well, and so does he*). El español no dispone de un mecanismo equivalente con esta distribución gramatical. Esta columna siempre devuelve cero. |

### D. Interrogativas

Las preguntas directas son características de géneros orales e interactivos. Este rasgo identifica únicamente las interrogativas directas introducidas por una palabra interrogativa.

| Código | Descripción |
|--------|-------------|
| `f_13_wh_question` | Preguntas directas que comienzan con una palabra interrogativa (*qué*, *quién*, *cómo*, *dónde*, *cuándo*, *por qué*...). |

### E. Formas nominales

Este grupo reúne construcciones nominales que compactan información en el texto. Una alta densidad de nominalizaciones y sustantivos es característica del estilo elaborado propio de la escritura académica y técnica.

| Código | Descripción |
|--------|-------------|
| `f_14_nominalizations` | Nominalizaciones derivadas: sustantivos formados a partir de verbos o adjetivos mediante sufijos productivos (*-ción*, *-sión*, *-idad*, *-miento*, *-eza*, *-ura*...). Son el principal indicador de densidad nominal en el análisis multidimensional. |
| `f_15_gerunds` &nbsp;0️⃣ | En inglés, el gerundio (*-ing*) puede funcionar como sustantivo (*Swimming is healthy*). El gerundio español (*nadando*) no admite esta función: el equivalente sería el infinitivo (*nadar*) o una nominalización (*la natación*). Esta columna siempre devuelve cero. |
| `f_16_other_nouns` | Sustantivos comunes y propios (NOUN y PROPN) que no constituyen nominalizaciones derivadas; es decir, los no contabilizados en `f_14`. |

### F. Pasivas

Las construcciones pasivas permiten omitir o desplazar el agente de la acción, recurso frecuente en textos académicos, periodísticos e institucionales. El español dispone de dos mecanismos principales de pasivización.

| Código | Descripción |
|--------|-------------|
| `f_17_agentless_passives` | Pasivas sin complemento agente: perifrásticas (*fue redactado*, *fueron aprobadas*) y pasivas reflejas (*se publicaron los informes*). Se excluye la impersonal con *se* (*se entrevistó a los candidatos*, *se recomienda leer*), que no tiene sujeto paciente. |
| `f_18_by_passives` | Pasivas con agente explícito introducido por la preposición *por* (*fue redactado por el equipo*, *fue aprobado por el comité*). |

### G. Formas estativas

Las formas estativas son características de textos que describen estados o atribuyen propiedades, en contraste con los textos que narran acciones dinámicas.

| Código | Descripción |
|--------|-------------|
| `f_19_be_main_verb` | *Ser* o *estar* en función copulativa: tanto en construcciones nominales (*es profesor*, *es la mejor opción*) como adjetivales (*está cansado*, *es importante*). |
| `f_20_existential_there` | *Haber* impersonal en construcciones existenciales: *hay*, *había*, *habrá*, *hubo*... Equivale al *there is/are* del inglés. |

### H. Subordinación

Estos rasgos identifican los distintos mecanismos de subordinación del español, desde las completivas hasta las adverbiales. Una alta frecuencia de subordinación caracteriza los registros elaborados.

| Código | Descripción |
|--------|-------------|
| `f_21_that_verb_comp` | Cláusulas subordinadas sustantivas introducidas por *que* que funcionan como complemento directo de un verbo (*cree que...*, *afirma que...*, *sostiene que...*). Se distingue de `f_22` comprobando que el predicado real (el head del verbo subordinado) sea un verbo, no un adjetivo. |
| `f_22_that_adj_comp` | Cláusulas subordinadas sustantivas introducidas por *que* que complementan a un adjetivo (*es probable que...*, *está claro que...*, *es necesario que...*). El adjetivo se busca en el head del verbo subordinado, no en el head inmediato de *que* (que siempre es un verbo). |
| `f_23_wh_clause` | Cláusulas interrogativas indirectas introducidas por una palabra interrogativa (*no sé quién vino*, *preguntó cómo se hacía*). |
| `f_24_infinitives` | Infinitivos en función de complemento verbal o como núcleo de una perífrasis (*quiere estudiar*, *empieza a llover*, *puede hacerlo*). |
| `f_25_present_participle` | Gerundio en función adverbial o de complemento predicativo (*llegando tarde*, *salió corriendo*, *lo vi entrando*). |
| `f_26_past_participle` | Participio en construcción absoluta o adverbial (*terminado el examen*, *publicados los resultados*, *aprobada la ley*). |
| `f_27_past_participle_whiz` | Participio postnominal que reduce una cláusula relativa de objeto (*el artículo publicado ayer*, *los datos analizados previamente*). |
| `f_28_present_participle_whiz` &nbsp;0️⃣ | En inglés, el gerundio puede ocupar posición postnominal como modificador (*the man running in the park*). Esta construcción es agramatical en el español normativo escrito. La columna siempre devuelve cero. |
| `f_29_that_subj` | Cláusulas relativas en función de sujeto, introducidas por *que* (*el libro que está en la mesa*) **o por *quien*/*el cual*** (*la autora, quien presentó el proyecto*): ambos pronombres relativos se fusionan en este mismo conteo. En pro-drop puro, cuando la relativa no tiene sujeto explícito, el objeto-relativa también se imputa aquí (limitación documentada). |
| `f_30_that_obj` | Cláusulas relativas en función de complemento directo, introducidas por *que* (*el libro que María escribió*) **o por *quien*/*el cual*** sin preposición: solo dispara cuando la relativa tiene un sujeto explícito que revela el hueco de objeto. |
| `f_31_wh_subj` &nbsp;0️⃣ | Columna siempre cero. El conteo de relativas con *quien*/*el cual* en función de sujeto vive en `f_29` (ver arriba); se mantiene solo por compatibilidad de esquema (67 columnas fijas). |
| `f_32_wh_obj` &nbsp;0️⃣ | Columna siempre cero. El conteo de relativas con *quien*/*el cual* en función de complemento directo vive en `f_30` (ver arriba); se mantiene solo por compatibilidad de esquema. |
| `f_33_pied_piping` | Relativas en que la preposición precede al pronombre relativo: *del que*, *con el cual*, *para quien*, *a la que*... |
| `f_34_sentence_relatives` | Relativas oracionales cuyo antecedente es toda una proposición: *lo que*, *lo cual*. |
| `f_35_because` | Cláusulas causales introducidas por *porque*. |
| `f_36_though` | Cláusulas concesivas introducidas por *aunque*. |
| `f_37_if` | Cláusulas condicionales introducidas por *si*. |
| `f_38_other_adv_sub` | Cláusulas adverbiales con otros subordinantes (*cuando*, *mientras*, *según*, *para que*, *a menos que*...). |

### I. Sintagmas preposicionales, adjetivos y adverbios

Estos rasgos miden la densidad de modificadores y complementos circunstanciales. En el análisis multidimensional, diferencian los registros informativos de los conversacionales.

| Código | Descripción |
|--------|-------------|
| `f_39_prepositions` | Preposiciones (categoría ADP en Universal Dependencies), incluidas las locuciones preposicionales (*a causa de*, *en relación con*...), que cuentan una sola vez. Una alta frecuencia de preposiciones es característica de la complejidad nominal del discurso escrito formal. |
| `f_40_adj_attr` | Adjetivos que modifican directamente a un sustantivo (*libro interesante*, *casa grande*, *resultado positivo*). En Universal Dependencies se identifican mediante la relación de dependencia `amod`. |
| `f_41_adj_pred` | Adjetivos predicativos: adjetivos que aparecen como atributo en una construcción copulativa (*el resultado es positivo*, *está cansada*, *se considera relevante*). |
| `f_42_adverbs` | Total de adverbios (todos los tokens `ADV`). Los solapamientos con otras categorías (f_04, f_05, f_46–f_50, f_67) son deliberados, siguiendo el rasgo *Total adverbs* del inglés. |

### J. Especificidad léxica

La especificidad léxica mide el grado de variación y complejidad del vocabulario empleado. Diferencia los textos con un repertorio léxico amplio de aquellos con vocabulario repetitivo.

| Código | Descripción |
|--------|-------------|
| `f_43_type_token` | Diversidad léxica, calculada mediante la medida indicada en el argumento `measure`, **incluida la puntuación** (fiel a Biber 1988). La opción por defecto, MATTR (*Moving-Average Type-Token Ratio*), compensa el efecto de la longitud del texto y resulta adecuada para corpus de longitud variable. |
| `f_44_mean_word_length` | Longitud media de los tokens en caracteres, excluida la puntuación. Los textos técnicos y académicos suelen presentar valores más altos que los conversacionales. |

### K. Clases léxicas

Estos rasgos recogen elementos léxicos con funciones discursivas específicas: conectores, modalizadores, intensificadores y partículas conversacionales. Capturan la actitud del hablante y la organización del discurso.

| Código | Descripción |
|--------|-------------|
| `f_45_conjuncts` | Conectores textuales que articulan el discurso (*sin embargo*, *por tanto*, *además*, *no obstante*, *en consecuencia*...). |
| `f_46_downtoners` | Atenuadores que reducen la fuerza de la afirmación (*apenas*, *ligeramente*, *en cierta medida*, *un poco*...). *casi* pasó a `f_47`. |
| `f_47_hedges` | Modalizadores epistémicos y aproximativos que expresan incertidumbre o reserva del hablante (*quizás*, *tal vez*, *probablemente*, *más o menos*, *una especie de*, *casi*...). |
| `f_48_amplifiers` | Amplificadores que intensifican la propiedad expresada (*muy*, *totalmente*, *enormemente*, *absolutamente*, *completamente*...). |
| `f_49_emphatics` | Expresiones enfáticas que refuerzan la asertividad del enunciado (*de hecho*, *sin duda*, *realmente*, *por supuesto*, *ciertamente*...). |
| `f_50_discourse_particles` | Partículas discursivas características de la oralidad (*bueno*, *pues*, *claro*, *vamos*, *mira*...). |
| `f_51_demonstratives` | Determinantes demostrativos antepuestos a un sustantivo (*este libro*, *esa propuesta*, *aquellos resultados*). No confundir con los pronombres demostrativos, recogidos en `f_10`. |

### L. Modales

El español no dispone de verbos modales auxiliares equivalentes a los del inglés (*can*, *must*, *will*). Las modalidades deóntica y epistémica se expresan mediante perífrasis verbales, cuyo inventario recogen estos tres rasgos.

| Código | Descripción |
|--------|-------------|
| `f_52_modal_possibility` | Perífrasis de posibilidad: *poder* + infinitivo (*puede llover*, *podría ocurrir*). |
| `f_53_modal_necessity` | Perífrasis de necesidad u obligación: *deber* + infinitivo, *tener que* + infinitivo, *haber que* + infinitivo (*debe terminar*, *tiene que revisarlo*, *hay que actuar*). |
| `f_54_modal_predictive` | Expresiones de predicción o probabilidad futura: futuro sintético (*llegará*, *se publicará*), condicional (*llegaría*) y la perífrasis *ir a* + infinitivo (*va a llover*, *iban a publicar*). |

### M. Verbos especializados

Estos rasgos agrupan verbos según la función discursiva que desempeñan. Son indicadores sensibles de la actitud epistémica y pragmática del escritor, y permiten distinguir géneros académicos, periodísticos e interactivos.

| Código | Descripción |
|--------|-------------|
| `f_55_verb_public` | Verbos de comunicación pública que introducen actos de habla o declaraciones (*afirmar*, *declarar*, *anunciar*, *sostener*, *señalar*, *decir*...). |
| `f_56_verb_private` | Verbos cognitivos que expresan procesos mentales o estados internos del sujeto (*creer*, *pensar*, *saber*, *suponer*, *esperar*...). |
| `f_57_verb_suasive` | Verbos suasivos que expresan influencia sobre la conducta de otro (*recomendar*, *pedir*, *sugerir*, *proponer*, *ordenar*...). |
| `f_58_verb_seem` | Verbos de apariencia que presentan el contenido como una impresión o evaluación del sujeto (*parecer*, *resultar*). |

### N. Formas reducidas

Los rasgos de esta categoría identifican fenómenos de reducción o elisión característicos de la oralidad en inglés. La mayoría carece de equivalente en el español escrito normativo, por lo que las columnas correspondientes se mantienen exclusivamente para garantizar la compatibilidad con las demás implementaciones del paquete.

| Código | Descripción |
|--------|-------------|
| `f_59_contractions` &nbsp;0️⃣ | Contracciones ortográficas del tipo *it's*, *don't*, *I've*, propias del inglés informal. El español no dispone de contracciones ortográficas en la escritura normativa (*del* y *al* no se consideran contracciones en el sentido de Biber). La columna siempre devuelve cero. |
| `f_60_that_deletion` &nbsp;0️⃣ | En inglés es habitual omitir el complementante *that* (*I think she's right* por *I think that she's right*). En español la omisión de *que* complementante es muy marginal y ajena al estándar escrito. La columna siempre devuelve cero. |
| `f_61_stranded_preposition` &nbsp;0️⃣ | En inglés la preposición puede quedar al final de una relativa (*the person I spoke to*). El español no admite esta construcción: la preposición siempre precede al pronombre relativo. La columna siempre devuelve cero. |
| `f_62_split_infinitive` &nbsp;0️⃣ | En inglés, un adverbio puede intercalarse entre *to* y el infinitivo (*to quickly finish*). El español carece de marcador de infinitivo equivalente a *to*, por lo que esta construcción es imposible. La columna siempre devuelve cero. |
| `f_63_split_auxiliary` | Construcción en que un elemento (adverbio u otro modificador) se interpone entre el auxiliar y el verbo principal (*ha siempre sostenido*, *podría fácilmente resolverse*). |

### O. Coordinación

Estos rasgos distinguen entre la coordinación de constituyentes menores (palabras o sintagmas) y la coordinación de cláusulas completas, dos fenómenos con distribuciones muy distintas entre géneros textuales.

| Código | Descripción |
|--------|-------------|
| `f_64_phrasal_coordination` | Coordinación de sintagmas del mismo tipo: N + *y* + N, ADJ + *y* + ADJ (*la autora y el editor*, *clara y precisa*, *rápido pero eficaz*). |
| `f_65_clausal_coordination` | Coordinación de cláusulas independientes mediante conjunciones coordinantes (*y*, *pero*, *sino*, *ni*...). |

### P. Negación

El español presenta un sistema de negación que difiere del inglés en un aspecto fundamental: admite la concordancia negativa, es decir, la acumulación de varios elementos negativos en una misma cláusula (*no sabe nadie nada*). Biber distingue entre dos tipos de negación según su expresión formal.

| Código | Descripción |
|--------|-------------|
| `f_66_neg_synthetic` | Negación sintética: palabras con valor inherentemente negativo (*nadie*, *nunca*, *jamás*, *tampoco*, *ninguno*, *nada*...). |
| `f_67_neg_analytic` | Negación analítica: el adverbio *no* antepuesto al verbo (*no sabe*, *no llegó*, *no se publicó*). |

## Limitaciones conocidas

Las situaciones descritas a continuación reflejan decisiones de diseño documentadas o comportamientos del modelo `spanish-gsd`. No se trata de errores: el comportamiento observado es el esperado.

- **f_18_by_passives**: `spanish-gsd` no emite la relación `obl:agent`, por lo que el agente (*por el comité*) y la causa (*por incumplimiento*) son indistinguibles (ambos `obl`); el rasgo puede incluir falsos positivos causales. Se evaluó y descartó un filtro de concordancia de número entre el sujeto pasivo y el sintagma agente: falla en casos legítimos y comunes (*"los proyectos fueron aprobados por el comité"*, *"el informe fue aprobado por los ingenieros"*) y no distingue el caso que se quería filtrar (*"el accidente fue causado por la lluvia"*).
- **f_26_past_participle** y **f_27_past_participle_whiz**: la relación de dependencia del participio varía según el contexto sintáctico. La detección es correcta cuando el análisis de dependencias acierta, pero puede presentar inconsistencias en construcciones límite.
- **f_50_discourse_particles**: la implementación actual no aplica filtro posicional, por lo que algunos marcadores monoléxicos (*bueno*, *claro*) pueden contabilizarse fuera de la posición inicial de cláusula. Este comportamiento está autorizado por la especificación.
- **f_29 y f_30 en cláusulas con sujeto elidido**: cuando el sujeto es nulo y no hay un `nsubj` explícito, la función sintáctica de la relativa no es recuperable; en ese caso se imputa a `f_29_that_subj`.
- **f_57_verb_suasive**: UDPipe lematiza las formas diptongadas de *recomendar* de manera irregular e impredecible (*recomienda*→*recomienda*, *recomiendan*→*recomiendar*, *recomiendo*→*recomir*, no al infinitivo *recomendar*), por lo que oraciones como *"El jefe recomienda que trabajen más"* no se cuentan pese a que `recomendar` sí está en el diccionario. No se parcheó con una lista de formas de superficie por resultar demasiado impredecible caso por caso; queda documentado como límite del modelo.
- **Rasgos sin equivalente en español**: las 10 columnas que siempre devuelven cero (f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, f_62) están justificadas lingüísticamente en la tabla de rasgos de la sección anterior.

## Funciones complementarias

El paquete incluye dos funciones adicionales que complementan a `biber_es()` en escenarios específicos. Ambas utilizan el mismo catálogo de detectores y producen recuentos equivalentes; sus diferencias residen en el tipo de entrada que aceptan y en el formato de la salida.

### Cuándo usar cada función

- **`biber_es()`** es la función base: recibe directamente la salida de `udpipe_annotate()` y devuelve un `data.frame`. Es la opción más eficiente cuando la anotación morfosintáctica ya está disponible.
- **`biber_es_batch()`** es la opción natural para corpus extensos. Combina en una sola llamada la lectura del corpus (desde un CSV o desde un `data.frame`), la anotación con UDPipe y la extracción de rasgos.
- **`biber_es_traced()`** es útil cuando se necesita auditar qué tokens concretos activaron cada detector: para verificar el extractor en un género nuevo, para documentar ejemplos en una publicación, o para depurar casos inesperados. Devuelve los mismos recuentos que `biber_es()` y añade un tibble con los tokens que respaldaron cada detección.

### `biber_es_batch()`: procesamiento por lotes

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
| `input` | Ruta a un archivo CSV o `data.frame` en memoria. Se admiten ambos formatos de entrada. Los archivos CSV se leen con codificación UTF-8. |
| `model` | Modelo UDPipe cargado previamente mediante `udpipe::udpipe_load_model()`. Parámetro obligatorio. |
| `text_column` | Nombre de la columna que contiene el texto de cada documento. Valor por defecto: `"text"`. |
| `id_column` | Nombre de la columna de identificadores. Si es `NULL` (valor por defecto), se generan identificadores automáticos con el patrón `doc_0001`, `doc_0002`, etc. |
| `trace` | Lógico. Si `TRUE`, el resultado incluye un elemento `evidence` con la trazabilidad token a token. Valor por defecto: `FALSE`. |
| `safe` | Lógico. `FALSE` (modo rápido, valor por defecto): procesa todos los documentos en una sola pasada de UDPipe; un fallo individual interrumpe la ejecución. `TRUE` (modo robusto): procesa los documentos uno a uno con `tryCatch`; los fallos individuales se recogen en `result$failed_docs`. |
| `measure` | Medida de diversidad léxica para `f_43`. Equivalente al parámetro homónimo de `biber_es()`. |
| `normalize` | Lógico. Normalización de los recuentos a frecuencia por 1 000 tokens léxicos. Valor por defecto: `TRUE`. |
| `progress` | Lógico. Muestra una barra de progreso durante el procesamiento en modo robusto cuando el paquete `cli` está disponible. Valor por defecto: `TRUE`. Sin efecto en modo rápido. |

#### Estructura de retorno

Una lista con los siguientes elementos:

- `counts`: `data.frame` con una fila por documento. Contiene `doc_id`, las columnas de metadatos del corpus de entrada (si las hay), los 67 rasgos `f_NN_*`, y las columnas auxiliares `n_tokens` y `n_lex_tokens`.
- `evidence` (solo si `trace = TRUE`): tibble en formato largo con el esquema descrito en `biber_es_traced()`.
- `failed_docs` (solo si `safe = TRUE`): `data.frame` con tres columnas (`doc_id`, `error_message`, `stage`). Está vacío si todos los documentos se procesaron correctamente.

#### Modos de operación

- **Modo rápido** (`safe = FALSE`, valor por defecto): adecuado para corpus curados y homogéneos. Procesa todos los documentos en una sola pasada de UDPipe, con el máximo rendimiento. Un fallo en cualquier documento interrumpe la ejecución.
- **Modo robusto** (`safe = TRUE`): adecuado para corpus heterogéneos o de procedencia incierta. Los fallos individuales se aíslan y se registran en `result$failed_docs`; el resto del corpus se procesa con normalidad.

#### Exportación a XLSX

La escritura del resultado a un archivo Excel se realiza mediante la función auxiliar `write_biber_xlsx()`:

```r
write_biber_xlsx(resultado, path = "salida.xlsx", include_per_1k = TRUE)
```

El archivo generado contiene las hojas `raw` (recuentos brutos o normalizados según corresponda), `metadata` (fecha de generación, versión del paquete, dimensiones del corpus) y, opcionalmente, `per_1k` (frecuencias normalizadas adicionales), `evidence` (si se invocó con `trace = TRUE`) y `failed_docs` (si se invocó con `safe = TRUE` y existen fallos). Todas las hojas tienen una fila por documento, lo que permite reimportarlas directamente con `readxl::read_xlsx()` en R, `pandas.read_excel()` en Python o el importador estándar de SPSS sin necesidad de transposiciones manuales.

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

Esta función es útil cuando se desea saber qué tokens concretos activaron cada detector: para auditar el extractor sobre un género nuevo, documentar ejemplos en un trabajo de investigación o depurar detecciones inesperadas. Devuelve los mismos recuentos que `biber_es()` y añade un tibble en formato largo con los tokens que respaldaron cada detección.

#### Firma

```r
biber_es_traced(tokens,
                measure   = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                normalize = TRUE)
```

Los parámetros tienen el mismo significado que en `biber_es()`.

#### Estructura de retorno

Una lista con dos elementos:

- `counts`: `data.frame` idéntico al producido por `biber_es()`, con una fila por documento y 70 columnas (`doc_id`, 67 rasgos, `n_tokens`, `n_lex_tokens`). Los valores numéricos son idénticos a los de `biber_es()` para los mismos parámetros.
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

No todos los rasgos generan filas de evidencia. La siguiente tabla resume el comportamiento según el tipo de detector:

| Tipo de rasgo | Comportamiento de la evidencia | Ejemplos |
|---------------|--------------------------------|----------|
| Rasgos de detección directa | Una fila por token detectado. Se cumple la igualdad `count == nrow(evidence)`. | f_01–f_03, f_13, f_14, f_16, f_17–f_20, f_21, f_23–f_27, f_29, f_30, f_33–f_42, f_52, f_54, f_63–f_67. |
| Rasgos con apoyo de suplementos léxicos | Número variable de filas, acotado superiormente por el recuento (`1 ≤ nrow(evidence) ≤ count`). | f_06, f_07, f_08, f_10, f_11, f_51, f_53, f_55–f_58. |
| Métricas continuas | No producen filas de evidencia. El valor se encuentra exclusivamente en `counts`. | f_43 (diversidad léxica), f_44 (longitud media). |
| Rasgos sin equivalente en español | No producen filas de evidencia. La columna en `counts` siempre vale cero. | f_09, f_12, f_15, f_28, f_31, f_32, f_59–f_62. |

Para los rasgos de la categoría K (f_45–f_50), cuya detección se realiza por búsqueda en diccionario, la evidencia token a token no se produce en la versión actual. Para recuperarla manualmente:

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

El paquete usa [`testthat`](https://testthat.r-lib.org/) para las pruebas unitarias e integración. Los recursos fuente (oraciones de ejemplo, diccionarios, listas léxicas) se mantienen en `data-raw/`. El paquete distribuye únicamente los objetos `.rda` compilados en `data/` y los fixtures sintéticos declarados directamente en los archivos de prueba.

### Estructura de pruebas

- `tests/testthat/test-spanish-basic.R`: fixtures sintéticos para rasgos principales, sin necesidad de UDPipe.
- `tests/testthat/test-spanish-examples.R`: pruebas de integración con UDPipe, alimentadas por `data-raw/spanish_examples.yaml`.
- `tests/testthat/test-spanish-modals.R`: cobertura de perífrasis modales (*poder + infinitivo*, *deber + infinitivo*, *tener que + infinitivo*, *ir a + infinitivo*, futuro sintético).
- `tests/testthat/test-feature-coverage-evidence.R`: verificación de que los 55 rasgos con detección activa generan evidencia token a token cuando corresponde.
- `tests/testthat/test-evidence-audit-regression.R`: pruebas de regresión para los detectores revisados durante el proceso de auditoría (f_06, f_23, f_24, f_47).

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

> Cordovez, M. (2026). *pseudobibeR.es: Extracción de rasgos morfológicos de Biber para español*. R package version 0.1.0. <https://github.com/browndw/pseudobibeR.es>

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
