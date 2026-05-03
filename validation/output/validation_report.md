# Reporte de validación: pseudobibeR.es

Generado: 2026-05-03 08:30:37
Modelo UDPipe: spanish-gsd-ud-2.5-191206.udpipe
Textos evaluados: 7

## Resumen global

- **Total comparaciones**: 172
- **OK**: 151 (87.8%)
- **TOLERANCE**: 16 (9.3%)
- **FAIL**: 5 (2.9%)
- **MISSING**: 0 (0.0%)

**Leyenda**:
- OK: |observed - expected| <= tolerance
- TOLERANCE: dentro de 2x tolerance
- FAIL: fuera de tolerancia
- MISSING: rasgo no presente en output del paquete

## Resultados por texto

| Texto | Registro | Total | OK | Tol | Fail | Miss | Pass rate |
|-------|----------|-------|-----|-----|------|------|-----------|
| text_01_narrativa | Narrativa (3ª persona, pasado) | 55 | 51 | 3 | 1 | 0 | 98.2% |
| text_02_academico | Académico/expositivo | 21 | 19 | 2 | 0 | 0 | 100% |
| text_03_conversacional | Conversacional/interactivo | 23 | 22 | 0 | 1 | 0 | 95.7% |
| text_04_instruccional | Instruccional/prescriptivo | 18 | 15 | 2 | 1 | 0 | 94.4% |
| text_05_futuro_modales | Futuro y modales predictivos | 17 | 14 | 2 | 1 | 0 | 94.1% |
| text_06_relativos_negacion | Demostrativos, relativos y negación | 22 | 17 | 4 | 1 | 0 | 95.5% |
| text_07_verbos_especializados | Verbos especializados, hedges y downtoners | 16 | 13 | 3 | 0 | 0 | 100% |

## Rasgos con problemas (al menos 1 fallo)

| Rasgo | Tests | OK | Tol | Fail | Miss | Fail rate | Δ medio |
|-------|-------|-----|-----|------|------|-----------|---------|
| `f_42_adverbs` | 2 | 1 | 0 | 1 | 0 | 50% | +2.00 |
| `f_39_prepositions` | 4 | 3 | 0 | 1 | 0 | 25% | -2.25 |
| `f_56_verb_private` | 4 | 2 | 1 | 1 | 0 | 25% | +1.75 |
| `f_01_past_tense` | 7 | 6 | 0 | 1 | 0 | 14.3% | -0.29 |
| `f_03_present_tense` | 7 | 4 | 2 | 1 | 0 | 14.3% | +0.71 |

## Detalle por texto

### text_01_narrativa — Narrativa (3ª persona, pasado)

#### Fallos

| Rasgo | Esperado | Observado | Δ | Tolerancia | Estado |
|-------|----------|-----------|---|------------|--------|
| `f_42_adverbs` | 1 | 5 | +4 | 2 | FAIL |

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_21_that_verb_comp` | 1 | 3 | +2 |
| `f_26_past_participle` | 1 | 3 | +2 |
| `f_56_verb_private` | 2 | 5 | +3 |

### text_02_academico — Académico/expositivo

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_03_present_tense` | 2 | 4 | +2 |
| `f_55_verb_public` | 1 | 3 | +2 |

### text_03_conversacional — Conversacional/interactivo

#### Fallos

| Rasgo | Esperado | Observado | Δ | Tolerancia | Estado |
|-------|----------|-----------|---|------------|--------|
| `f_56_verb_private` | 4 | 8 | +4 | 2 | FAIL |

### text_04_instruccional — Instruccional/prescriptivo

#### Fallos

| Rasgo | Esperado | Observado | Δ | Tolerancia | Estado |
|-------|----------|-----------|---|------------|--------|
| `f_39_prepositions` | 9 | 5 | -4 | 1 | FAIL |

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_40_adj_attr` | 9 | 7 | -2 |
| `f_53_modal_necessity` | 3 | 1 | -2 |

### text_05_futuro_modales — Futuro y modales predictivos

#### Fallos

| Rasgo | Esperado | Observado | Δ | Tolerancia | Estado |
|-------|----------|-----------|---|------------|--------|
| `f_03_present_tense` | 3 | 6 | +3 | 1 | FAIL |

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_11_indefinite_pronouns` | 1 | 3 | +2 |
| `f_14_nominalizations` | 3 | 5 | +2 |

### text_06_relativos_negacion — Demostrativos, relativos y negación

#### Fallos

| Rasgo | Esperado | Observado | Δ | Tolerancia | Estado |
|-------|----------|-----------|---|------------|--------|
| `f_01_past_tense` | 9 | 6 | -3 | 1 | FAIL |

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_03_present_tense` | 4 | 6 | +2 |
| `f_11_indefinite_pronouns` | 1 | 3 | +2 |
| `f_14_nominalizations` | 4 | 6 | +2 |
| `f_21_that_verb_comp` | 1 | 3 | +2 |

### text_07_verbos_especializados — Verbos especializados, hedges y downtoners

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_14_nominalizations` | 6 | 8 | +2 |
| `f_21_that_verb_comp` | 8 | 6 | -2 |
| `f_46_downtoners` | 2 | 4 | +2 |

## Problemas sistémicos (fallan en >=3 textos)

No hay problemas sistémicos.

