# Guía de instalación y prueba — `pseudobibeR.es`

## Requisitos previos

- **R ≥ 3.5.0** — [cran.r-project.org](https://cran.r-project.org)
- **RStudio** (recomendado) — [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop)
- Conexión a internet para instalar dependencias

---

## Opción A — Instalar desde GitHub

```r
install.packages("devtools")
devtools::install_github("browndw/pseudobibeR.es")
```

---

## Opción B — Instalar desde ZIP

1. Descomprimir el archivo recibido → queda una carpeta `pseudobibeR.esp/`
2. En R, ejecutar:

```r
install.packages("devtools")
devtools::install_deps("/ruta/a/la/carpeta/pseudobibeR.esp")
devtools::install("/ruta/a/la/carpeta/pseudobibeR.esp")
```

> **Atajo en RStudio:** ir a **Tools → Install Packages → Install from: Package Archive File** y seleccionar el ZIP directamente (sin descomprimir).

---

## Correr los tests

Con el repositorio clonado o descomprimido localmente:

```r
library(devtools)
devtools::test()
```

Para un archivo específico:

```r
testthat::test_file("tests/testthat/test-spanish-basic.R")
testthat::test_file("tests/testthat/test-feature-coverage.R")
```

> Los tests `f_22` y `f_50` están marcados como `skip()` — es esperado, corresponde a una limitación conocida del parser.

---

## Prueba rápida de funcionamiento

```r
library(udpipe)
library(pseudobibeR.es)

# Descargar el modelo UDPipe (solo la primera vez, ~15 MB)
model <- udpipe_download_model(language = "spanish-gsd")
ud_model <- udpipe_load_model(model$file_model)

# Texto de prueba
texto <- data.frame(
  doc_id = "tesis_01",
  text = "En este contexto, los textos producidos mediante LLMs emergen como una nueva modalidad de escritura: géneros informativos como columnas de opinión y noticias periodísticas pueden redactarse en su totalidad con estas herramientas. No obstante, gran parte del debate académico sobre la IA ha privilegiado el estudio de capacidades de razonamiento, resolución de problemas matemáticos, programación o diagnóstico médico, entre otros, desatendiendo el análisis sistemático del estilo lingüístico de estos modelos."
)

# Parsear
parsed <- udpipe_annotate(
  ud_model,
  x      = texto$text,
  doc_id = texto$doc_id,
  tagger = "default",
  parser = "default"
)

# Extraer rasgos de Biber
rasgos <- biber_es(parsed, measure = "none", normalize = FALSE)
print(rasgos)
```

El resultado esperado es un `data.frame` con 1 fila y ~57 columnas de rasgos lingüísticos.

---

## Aplicación Shiny (opcional)

```r
install.packages("shiny")
shiny::runApp("/ruta/a/la/carpeta/pseudobibeR.esp/app.R")
```

---

## Problemas comunes

| Problema | Solución |
|---|---|
| Error al instalar `quanteda` | `install.packages("quanteda")` manualmente |
| El modelo UDPipe ya existe | Usar `udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")` directamente |
| Tests `f_22` / `f_50` aparecen como skipped | Es esperado — limitación documentada del parser |
| Error de ruta con espacios | Envolver la ruta en comillas: `"/ruta con espacios/carpeta"` |
