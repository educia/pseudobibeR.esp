# pseudobibeR.es

[![R-CMD-check](https://github.com/browndw/pseudobibeR.es/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Tests](https://github.com/browndw/pseudobibeR.es/workflows/Tests/badge.svg)](https://github.com/browndw/pseudobibeR.es/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Paquete compañero de `pseudobibeR` orientado a textos en **español**. Calcula los rasgos lexicogramaticales y funcionales descritos por Biber (1985) y ampliamente usados para el análisis de variación de registro y género textual. Comparte el mismo catálogo de referencia que la versión inglesa, pero se apoya en los recursos morfosintácticos de Universal Dependencies para español provistos por UDPipe y en heurísticas específicas del idioma implementadas en los bloques `*_es`.

## Descripción general

Este paquete **no** realiza el etiquetado gramatical por sí mismo. En su lugar, aprovecha los etiquetadores y analizadores de dependencias existentes —principalmente [udpipe](https://bnosac.github.io/udpipe/en/) con el modelo `spanish-gsd`— para extraer y agregar patrones lingüísticos. La precisión de los rasgos extraídos depende directamente de la calidad del etiquetado y del análisis de dependencias subyacente.

El español plantea desafíos específicos respecto al inglés o el francés: el sujeto nulo (*pro-drop*) hace innecesario un pronombre expletivo equivalente al inglés *it*; la negación opera con palabras negativas preverbales o posverbales; y las relativas introducidas por *que* funcionan tanto en posición de sujeto como de objeto. Estas diferencias se han tenido en cuenta en el diseño del extractor:

- **57 rasgos de Biber adaptados** (de los 67 originales): 11 rasgos intraducibles han sido eliminados.
- **2 fusiones** de rasgos: `f_29+f_31 → f_29_that_subj`; `f_30+f_32 → f_30_that_obj`.
- **1 extensión** calculada internamente: `f_71_preterit` (pretérito imperfecto), disponible en la salida de `biber_es()` aunque no mostrada en la aplicación Shiny.

**Nota:** Textos con ortografía muy irregular, puntuación no normativa o lenguaje muy especializado pueden producir salidas menos fiables salvo que el modelo UD se haya ajustado a ese dominio.

## Instalación

Instala la versión de desarrollo desde GitHub:

```r
# install.packages("devtools")
devtools::install_github("browndw/pseudobibeR.es")
```

### Instalar desde un tarball de versión

1. Descarga el archivo `pseudobibeR.es_<version>.tar.gz` más reciente desde la [página de releases de GitHub](https://github.com/browndw/pseudobibeR.es/releases).
2. En R, instala el archivo descargado con:

   ```r
   install.packages("/ruta/a/pseudobibeR.es_<version>.tar.gz", repos = NULL, type = "source")
   ```

   En macOS y Linux puedes arrastrar el archivo al terminal para completar la ruta.
3. Reinicia tu sesión de R para que el paquete recién instalado quede disponible.

## Inicio rápido

La función principal es `biber_es()`, que toma la salida de `udpipe::udpipe_annotate()` con un modelo UD de español y devuelve un `data.frame` de rasgos por documento.

### Con UDPipe (recomendado)

Recomendamos **UDPipe** para la mayoría de los usuarios por su sencillez (implementación pura en R, sin dependencias Python) y su fiabilidad con el esquema UD sobre el que están calibradas las heurísticas `_es`.

```r
library(udpipe)
library(pseudobibeR.es)

# Descarga del modelo (solo una vez)
model <- udpipe_download_model(language = "spanish-gsd")
ud_model <- udpipe_load_model(model$file_model)

# Datos de texto
text_data <- data.frame(
  doc_id = c("doc_1", "doc_2"),
  text = c(
    "El informe fue redactado por el equipo de investigación.",
    "Nos gustaría aclarar lo que se propuso en la última sesión."
  )
)

# Parsear el texto
parsed_data <- udpipe_annotate(
  ud_model,
  x      = text_data$text,
  doc_id = text_data$doc_id,
  tagger = "default",
  parser = "default"
)

# Extraer rasgos de Biber
features <- biber_es(parsed_data, measure = "none", normalize = FALSE)
print(features)
```

## Notas de parseo para español

- **Modelo UDPipe recomendado:** `spanish-gsd-ud-2.5-191206.udpipe`. Aunque existen versiones más recientes de modelos UD para español (AnCora, etc.), las heurísticas del paquete están validadas contra `spanish-gsd`.
- **`feats` obligatorio:** el extractor depende de las características morfológicas del campo `feats` (p.ej. `Tense=Past`, `Mood=Ind`, `VerbForm=Fin`). Asegúrate de usar `parser = "default"` y `tagger = "default"` en `udpipe_annotate()`.
- **Pro-drop:** el español es una lengua de sujeto nulo; solo se cuentan los pronombres explícitos en f_06–f_08. El rasgo inglés `f_09_pronoun_it` (pronombre expletivo) ha sido eliminado.
- **Relativas con *que*:** UDPipe Spanish-GSD etiqueta siempre el relativo *que* como `SCONJ/mark` (no como `PRON`). El extractor maneja este comportamiento: todas las relativas con *que* se cuentan en `f_29_that_subj`; `f_30_that_obj` captura relativas con *quien/cual* en posición oblicua.
- **Condicional *si*:** cuando *si* aparece al inicio de cláusula puede ser etiquetado como `CCONJ` por el parser; el extractor es más robusto con *si* en posición media de oración.
- **Copulativas:** en UD español el adjetivo predicativo es el nodo raíz de la construcción copulativa (*ser/estar* depende como `cop`). El extractor detecta correctamente ambas estructuras para `f_41_adj_pred`.

## Características principales

- **57 rasgos de Biber adaptados al español**, organizados en 16 categorías (A–P)
- **Normalización automática** a conteos por 1 000 tokens (opcional)
- **Varias medidas de type-token ratio** (MATTR, TTR, CTTR, MSTTR)
- **Integración con UDPipe** (y potencialmente spaCy en el futuro)
- **Diccionarios y listas léxicas** en español bajo `data-raw/`
- **Suite de tests** con ejemplos sintéticos reproducibles en `tests/testthat/`
- **Aplicación Shiny** integrada (`app.R`) para exploración interactiva de rasgos

## Dependencias y requisitos

### Núcleo

- R (>= 3.5.0)
- dplyr, purrr, quanteda, quanteda.textstats, rlang, stringr, tibble, magrittr

### Para el parseo de texto

- Paquete [udpipe](https://bnosac.github.io/udpipe/en/)
- Modelo UDPipe para español (`spanish-gsd-ud-2.5`); descárgalo con `udpipe::udpipe_download_model("spanish-gsd")`

## Argumentos de la función

`biber_es()` acepta los siguientes argumentos:

- `tokens`: datos pre-parseados provenientes de `udpipe::udpipe_annotate()` (como `data.frame` o objeto `udpipe_connlu`)
- `measure`: medida de type-token ratio (`"MATTR"`, `"TTR"`, `"CTTR"`, `"MSTTR"` o `"none"`)
- `normalize`: si se normalizan los conteos a por 1 000 tokens (`TRUE`/`FALSE`)

```r
# Ejemplo con parámetros personalizados
features <- biber_es(parsed_data,
                     measure   = "MATTR",
                     normalize = TRUE)
```

## Desarrollo y tests

pseudobibeR.es usa [testthat](https://testthat.r-lib.org/) para los tests unitarios. Los materiales fuente (oraciones de ejemplo, diccionarios, scripts de construcción) viven bajo `data-raw/`; el paquete distribuye solo los objetos `.rda` compilados en `data/` y los fixtures sintéticos declarados directamente en los archivos de test.

### Estructura de tests

- `tests/testthat/test-spanish-basic.R`: fixtures sintéticos (data.frames con columnas UD) para los rasgos principales sin necesidad de UDPipe: tiempos verbales (f_01, f_03, f_71), pronombres explícitos (f_06–f_08), pasivas (f_17, f_18), modales (f_52, f_53) y negación (f_66, f_67).
- `tests/testthat/test-spanish-examples.R`: test de integración con UDPipe real, alimentado por `data-raw/spanish_examples.yaml`. Los rasgos con divergencias conocidas del parser se registran como informativos (*relaxed*), no como fallos.
- `tests/testthat/test-spanish-modals.R`: cubre las perífrasis modales del español: `poder + inf`, `deber + inf`, `tener que + inf`, `ir a + inf` y el futuro sintético.

### Divergencias conocidas de UDPipe Spanish-GSD

Algunas construcciones generan conteos inconsistentes en el parser. Estos rasgos se marcan como `relaxed` en los tests y se documentan en `data-raw/spanish_examples.yaml`:

| Rasgo | Comportamiento conocido |
|-------|------------------------|
| `f_22_that_adj_comp` | El nodo padre de *que* no siempre es `ADJ` |
| `f_23_wh_clause` | Lematización de *quién/cuándo* variable con tilde |
| `f_26_past_participle` | Participio absoluto con `dep_rel` variable |
| `f_37_if` | *Si* inicial de oración puede etiquetarse como `CCONJ/case` |
| `f_39_prepositions` | Conteo variable según la estructura del SN |

### Actualizar datos y fixtures

Si modificas algún archivo YAML bajo `data-raw/`, regenera los datos del paquete ejecutando desde la raíz del repositorio:

```r
source("data-raw/build_french_dictionaries.R")
```

Después de editar `data-raw/spanish_examples.yaml`, actualiza también los fixtures de test con UDPipe:

```r
Rscript data-raw/generate_edge_case_fixture.R
```

Confirma siempre tanto los archivos YAML editados como los artefactos `.rda` regenerados para mantener sincronizados CI y los usuarios.

### Ejecutar los tests

```r
# Todos los tests
testthat::test_package("pseudobibeR.es")

# Archivos específicos
testthat::test_file("tests/testthat/test-spanish-basic.R")
testthat::test_file("tests/testthat/test-spanish-examples.R")
testthat::test_file("tests/testthat/test-spanish-modals.R")
```

## Datos del paquete

El paquete incluye varios conjuntos de datos integrados:

- `dict`: patrones de diccionario para la detección de rasgos (conjunciones, adverbios, pronombres…)
- `word_lists`: listas léxicas de apoyo (sufijos de nominalización, verbos modales, verbos de actitud, etc.)

Puedes explorarlos así:

```r
library(pseudobibeR.es)

# Ver diccionarios disponibles
names(dict)

# Explorar listas léxicas
word_lists$nominalization_suffixes   # sufijos de nominalización
word_lists$f_06_first_person_pronouns
```

## Actualizar diccionarios y ejemplos

Los colaboradores pueden ampliar los recursos léxicos que alimentan el extractor. Los archivos YAML viven en `data-raw/` y se convierten en datos del paquete (`dict.rda`, `word_lists.rda`) mediante el script de construcción.

### 1. Editar los archivos YAML

- `data-raw/dict.yaml`: asocia cada ID de rasgo (p.ej. `f_45_conjuncts`) con una lista de lemas o patrones multipalabra separados por guiones bajos. Todas las entradas se normalizan a minúsculas automáticamente. Los comentarios con `#` están permitidos.
- **Importante:** el extractor convierte las entradas del diccionario a lemas antes de comparar. Solo se retiene el token final de cada cadena multipalabra. Los indicios perifrásticos complejos (p.ej. `tener que`, `ir a`) se gestionan directamente en `block_modals_es()` y similares; no es necesario duplicarlos en `dict.yaml`.
- `data-raw/word_lists.yaml`: reúne listas auxiliares (pronombres, sufijos, listas de exclusión, etc.).
- `data-raw/spanish_examples.yaml`: proporciona oraciones ilustrativas para los tests. Cada entrada tiene la forma:

  ```yaml
  - feature: f_01_past_tense
    example: "María llegó tarde a la reunión."
    count: 1.0
  ```

  El campo `count` indica el número esperado de ocurrencias del rasgo en ese ejemplo.

### 2. Regenerar los datos del paquete

Desde la raíz del repositorio:

```r
source("data-raw/build_french_dictionaries.R")
```

### 3. Validar los cambios

- `devtools::test()` confirma que los detectores de rasgos siguen pasando la suite de tests.
- `devtools::document()` actualiza la documentación de ayuda.
- Al modificar los diccionarios de modales u otras listas de lemas, recuerda añadir fixtures en `test-spanish-modals.R` o `test-spanish-basic.R` para evitar regresiones.

Confirma siempre los archivos YAML editados *y* los activos `.rda` regenerados en `data/`.

## Aplicación Shiny

El repositorio incluye una aplicación Shiny (`app.R`) para la exploración interactiva de los rasgos sobre texto libre en español.

```r
# Lanzar la aplicación (desde el directorio del paquete)
shiny::runApp()
```

La aplicación requiere que el modelo UDPipe `spanish-gsd-ud-2.5-191206.udpipe` esté en el directorio de trabajo. Descárgalo previamente con:

```r
udpipe::udpipe_download_model("spanish-gsd")
```

La tabla de resultados muestra los 57 rasgos de Biber adaptados al español, organizados por grupo (A–P), con la posibilidad de filtrar por categoría y exportar a CSV o Excel.

## Cita sugerida

Al usar `pseudobibeR.es` en tu investigación, cita:

**El artículo original de Biber (1985):**
> Biber, D. (1985). Investigating macroscopic textual variation through multifeature/multidimensional analyses. *Linguistics*, 23(2), 337–360. DOI: [10.1515/ling.1985.23.2.337](https://doi.org/10.1515/ling.1985.23.2.337)

**Este paquete:**
> Cordovez, M. (2024). pseudobibeR.es: Extracción de rasgos morfológicos de Biber para español. R package version 0.1.0. <https://github.com/browndw/pseudobibeR.es>

## Rasgos lingüísticos extraídos

El paquete extrae **57 rasgos de Biber adaptados al español**, organizados en 16 categorías. Once rasgos del catálogo original han sido eliminados por ser intraducibles al español (f_09, f_12, f_15, f_28, f_31, f_32, f_59, f_60, f_61, f_62, f_68); dos pares de rasgos se han fusionado (f_29+f_31; f_30+f_32).

### Categorías de rasgos

| Categoría | Rasgos | Descripción |
|-----------|--------|-------------|
| **A. Tiempo y aspecto** | f_01–f_03 | Pretérito indefinido, aspecto perfecto, presente de indicativo |
| **B. Adverbiales de lugar y tiempo** | f_04–f_05 | Adverbiales espaciales y temporales |
| **C. Pronombres** | f_06–f_11 | 1.ª, 2.ª y 3.ª persona, demostrativos, indefinidos |
| **D. Interrogativas** | f_13 | Preguntas directas con palabra *qu-* |
| **E. Formas nominales** | f_14, f_16 | Nominalizaciones, otros sustantivos |
| **F. Pasivas** | f_17–f_18 | Pasiva sin agente y pasiva con *por* |
| **G. Formas estativas** | f_19–f_20 | *Ser/estar* como verbo principal; existencial con *haber* |
| **H. Subordinación** | f_21–f_38 | Completivas, relativas, subordinadas adverbiales |
| **I. Sintagmas prep., adj. y adv.** | f_39–f_42 | Preposiciones, adjetivo atributivo/predicativo, adverbios |
| **J. Especificidad léxica** | f_43–f_44 | TTR (type-token ratio), longitud media de palabra |
| **K. Clases léxicas** | f_45–f_51 | Conjuntos textuales, atenuadores, *hedges*, amplificadores, enfáticos, demostrativos |
| **L. Modales** | f_52–f_54 | Posibilidad (*poder*), necesidad (*deber*, *tener que*), predictivos (*ir a* + inf., futuro) |
| **M. Verbos especializados** | f_55–f_58 | Verbos públicos, privados, suasivos, de apariencia |
| **N. Formas reducidas** | f_63 | Auxiliar escindido |
| **O. Coordinación** | f_64–f_65 | Coordinación frasal y clausal |
| **P. Negación** | f_66–f_67 | Negación sintética (*nadie*, *nunca*…) y analítica (*no* + V) |

### Lista detallada de rasgos

| Rasgo | Ejemplos en español |
|-------|---------------------|
| **A. Tiempo y aspecto** | |
| f_01_past_tense | *llegó*, *presentaron*, *fue* (pretérito indefinido) |
| f_02_perfect_aspect | *ha terminado*, *han llegado* (haber + participio) |
| f_03_present_tense | *explica*, *muestran*, *funciona* (presente de indicativo) |
| **B. Adverbiales de lugar y tiempo** | |
| f_04_place_adverbials | *aquí*, *allí*, *encima*, *lejos*, *afuera*… |
| f_05_time_adverbials | *ayer*, *hoy*, *siempre*, *nunca*, *después*… |
| **C. Pronombres** | |
| f_06_first_person_pronouns | *yo*, *nosotros*, *me*, *nos*, *mí*… |
| f_07_second_person_pronouns | *tú*, *usted*, *te*, *ti*, *vosotros*… |
| f_08_third_person_pronouns | *él*, *ella*, *ellos*, *lo*, *le*, *les*… |
| f_10_demonstrative_pronoun | *este*, *ese*, *aquel*, *esto*, *eso*… |
| f_11_indefinite_pronouns | *alguien*, *nadie*, *algo*, *nada*, *todo*… |
| **D. Interrogativas** | |
| f_13_wh_question | *¿Quién llamó?* / *¿Qué hora es?* / *¿Cuándo llega?* |
| **E. Formas nominales** | |
| f_14_nominalizations | *organización*, *distribución*, *productividad* (-ción, -idad…) |
| f_16_other_nouns | Otros sustantivos léxicos |
| **F. Pasivas** | |
| f_17_agentless_passives | *La propuesta fue aprobada sin debate* |
| f_18_by_passives | *La novela fue escrita por García Márquez* |
| **G. Formas estativas** | |
| f_19_be_main_verb | *El libro es interesante* / *El autor es conocido* |
| f_20_existential_there | *Hay tres errores en el documento* |
| **H. Subordinación** | |
| f_21_that_verb_comp | *Creo que el proyecto tendrá éxito* |
| f_22_that_adj_comp | *Es importante que todos participen* |
| f_23_wh_clause | *No sé quién llamó ni cuándo llegó* |
| f_24_infinitives | *Quiero terminar el trabajo para poder descansar* |
| f_25_present_participle | *Caminando por el parque, encontré a mi vecino* |
| f_26_past_participle | *Terminado el examen, los alumnos salieron* |
| f_27_past_participle_whiz | *El informe redactado ayer es muy completo* |
| f_29_that_subj | *El estudiante que llegó tarde…* [f_29+f_31 fusionados] |
| f_30_that_obj | *La persona con quien hablé…* [f_30+f_32 fusionados] |
| f_33_pied_piping | *el asunto del que habló* |
| f_34_sentence_relatives | *…, lo que es sorprendente* |
| f_35_because | *No pudo asistir porque estaba enfermo* |
| f_36_though | *Aunque llovía, salimos a caminar* |
| f_37_if | *Lo aprobarás si estudias con dedicación* |
| f_38_other_adv_sub | *cuando*, *mientras*, *como*, *aunque* (otros subordinantes adv.) |
| **I. Sintagmas prep., adj. y adv.** | |
| f_39_prepositions | *de*, *en*, *con*, *para*, *sobre*, *entre*… |
| f_40_adj_attr | *un brillante estudiante*, *una nueva solución* (amod) |
| f_41_adj_pred | *El resultado es positivo* / *parece correcto* |
| f_42_adverbs | *muy*, *rápidamente*, *siempre*, *puntualmente*… |
| **J. Especificidad léxica** | |
| f_43_type_token | Ratio tipos/tokens del documento |
| f_44_mean_word_length | Longitud media de palabra (tokens léxicos) |
| **K. Clases léxicas** | |
| f_45_conjuncts | *sin embargo*, *además*, *por tanto*, *en cambio*… |
| f_46_downtoners | *casi*, *apenas*, *algo*, *ligeramente*… |
| f_47_hedges | *quizás*, *tal vez*, *probablemente*, *en cierta medida*… |
| f_48_amplifiers | *muy*, *totalmente*, *enormemente*, *absolutamente*… |
| f_49_emphatics | *de hecho*, *sin duda*, *realmente*, *evidentemente*… |
| f_50_discourse_particles | *bueno*, *pues*, *claro*, *mira*… |
| f_51_demonstratives | *este*, *ese*, *aquel* (determinantes demostrativos) |
| **L. Modales** | |
| f_52_modal_possibility | *poder + inf.* («podemos mejorar») |
| f_53_modal_necessity | *deber + inf.*, *tener que + inf.*, *hay que + inf.* |
| f_54_modal_predictive | *ir a + inf.*, futuro sintético (*presentará*) |
| **M. Verbos especializados** | |
| f_55_verb_public | *afirmar*, *declarar*, *anunciar*, *señalar*… |
| f_56_verb_private | *creer*, *pensar*, *saber*, *suponer*… |
| f_57_verb_suasive | *recomendar*, *pedir*, *sugerir*, *ordenar*… |
| f_58_verb_seem | *parecer*, *resultar* («parece que los resultados…») |
| **N. Formas reducidas** | |
| f_63_split_auxiliary | *ha probablemente sido analizado* |
| **O. Coordinación** | |
| f_64_phrasal_coordination | *los estudiantes y los profesores* (N y N) |
| f_65_clausal_coordination | *Llovía y hacía frío* (prop. independientes con *y/o*) |
| **P. Negación** | |
| f_66_neg_synthetic | *Nadie sabe nada* / *Nunca llegó* / *Tampoco avisó* |
| f_67_neg_analytic | *No llegó* / *El resultado no es válido* |

### Rasgos eliminados (intraducibles al español)

| Rasgo | Motivo de eliminación |
|-------|-----------------------|
| f_09_pronoun_it | El español es lengua de sujeto nulo; no existe pronombre expletivo equivalente |
| f_12_proverb_do | *Hacer* pro-verbal no tiene el mismo comportamiento que *do* en inglés |
| f_15_gerunds | El gerundio español tiene distribución distinta; no existe categoría equivalente a los gerundios nominales del inglés |
| f_28_present_participle_whiz | Fusionado en f_25 o eliminado por baja frecuencia en UD español |
| f_31_wh_subj | Fusionado en f_29_that_subj (*que* cubre sujeto y objeto en español) |
| f_32_wh_obj | Fusionado en f_30_that_obj (*quien/cual* en posición oblicua) |
| f_59_contractions | No existen contracciones ortográficas equivalentes en español estándar escrito |
| f_60_that_deletion | La omisión de *que* complementante es marginal en español escrito |
| f_61_stranded_preposition | La preposición varada no existe en español (siempre precede al relativo) |
| f_62_split_infinitive | El infinitivo no se escinde en español |
| f_68 | No aplicable al español |

## Contribuciones

¡Las contribuciones son bienvenidas! Puedes abrir *issues* o *pull requests* en [GitHub](https://github.com/browndw/pseudobibeR.es).

### Al reportar problemas

Por favor incluye:

- Un ejemplo mínimo reproducible
- Tu versión de R y de los paquetes relevantes
- El modelo UDPipe utilizado y su versión

## Licencia

Este paquete está bajo la licencia MIT. Consulta el archivo [LICENSE](LICENSE) para más detalles.

## Recursos relacionados

- [Biber (1985) — artículo original](https://doi.org/10.1515/ling.1985.23.2.337)
- [udpipe](https://bnosac.github.io/udpipe/en/) — herramienta de NLP para R
- [quanteda](https://quanteda.io/) — marco de análisis de texto usado internamente
- [pseudobibeR](https://github.com/browndw/pseudobibeR) — versión original en inglés
- [pseudobibeR.fr](https://github.com/browndw/pseudobibeR.fr) — versión en francés
