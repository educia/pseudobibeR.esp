# pseudobibeR.es 0.1.0

## Primera version funcional

* Implementacion de los 67 rasgos Biber (1988) para espanol via UDPipe (Spanish-GSD).
* Funcion principal `biber_es()` con soporte para `udpipe_connlu`.
* Diccionarios en YAML con cobertura de espanol peninsular y latinoamericano.
* Validacion empirica sobre corpus de 4 generos: 17/20 predicciones Biber verificadas.

## Correcciones aplicadas durante desarrollo

* f_10 / f_51: lista de demostrativos separada de pronombres personales.
* f_54: filtro temporal excluye desplazamiento fisico con *ir* en indefinido.
* f_59: deteccion de MWT (*al*, *del*) corregida en preprocesado UDPipe.
* f_43: eliminada colision de columnas `.x`/`.y` en `parse_functions.R`.

## Decisiones de diseno

* f_01 captura solo preterito imperfecto (ver `docs/DECISIONES_ES.md`).
* Para proxy de "total pasado" equivalente al ingles: `f_01 + f_02 + f_71`.

---

# pseudobibeR.fr 0.0.0.93

## Major Changes

### Cross-Parser Validation
* Validated feature extraction across UDPipe and spaCy parsers
* Nearly identical MDA results (R² = 0.715 vs 0.703)
* Demonstrates robustness across NLP pipelines
* French Factor 1 aligns with Biber's English Dimension 1 (Interactional vs Information Production)

### Documentation Website
* Complete Quarto documentation site with:
  - Getting Started guide
  - Cross-Parser Validation vignette
  - Chambers–Le Baron corpus walkthrough
  - Feature categories with French equivalents and examples
  - Data sources with corpus composition tables

### Code Reorganization
* Modularized all parsing code into block functions (e.g., `block_contractions_fr`, `block_passives_fr`)
* Each block is self-contained and independently testable
* Improved maintainability and troubleshooting
* Comprehensive inline documentation explaining linguistic rationale for each block

## Bug Fixes

### Contraction Detection (f_59_contractions)
* **Fixed**: Over-counting due to confusion between contractions and elisions
* **Problem**: Was counting grammatical elisions (l', d', qu') as informal contractions
* **Solution**: Implemented POS-based filtering to distinguish:
  - **Elisions** (excluded): Grammatical function words with POS tags DET, ADP, PRON, ADV, SCONJ
  - **Contractions** (counted): Informal lexical forms with POS tags ADJ, NOUN, PROPN (e.g., p'tit, m'sieur)
* **Validation**: Analyzed 74K apostrophe tokens showing clear POS-based pattern
* **Cross-parser**: Works identically with both UDPipe and spaCy

## Documentation Improvements

* Added comprehensive feature descriptions in French (67 features)
* Token counts and composition tables for all corpora:
  - Chambers–Le Baron: 125,445 tokens, 10 disciplines
  - French Register Corpus: 1.6M tokens, 6 registers
* Cross-references between vignettes and data sources
* Implementation details with extensive code comments

## Infrastructure

* GitHub Actions workflows for:
  - Automated testing with UDPipe model download
  - Quarto documentation deployment (triggered on version tags)
  - CRAN-ready package releases
* All dependencies properly specified in DESCRIPTION
* Conditional evaluation in vignettes for reproducibility

---

# pseudobibeR.fr 0.0.0.92

Initial development version with core feature extraction framework.
