# Reporte de validación: pseudobibeR.es

Generado: 2026-05-03 08:37:14
Modelo UDPipe: spanish-gsd-ud-2.5-191206.udpipe
Textos evaluados: 7

## Resumen global

- **Total comparaciones**: 172
- **OK**: 157 (91.3%)
- **TOLERANCE**: 15 (8.7%)
- **FAIL**: 0 (0.0%)
- **MISSING**: 0 (0.0%)

**Leyenda**:
- OK: |observed - expected| <= tolerance
- TOLERANCE: dentro de 2x tolerance
- FAIL: fuera de tolerancia
- MISSING: rasgo no presente en output del paquete

## Resultados por texto

| Texto | Registro | Total | OK | Tol | Fail | Miss | Pass rate |
|-------|----------|-------|-----|-----|------|------|-----------|
| text_01_narrativa | Narrativa (3ª persona, pasado) | 55 | 52 | 3 | 0 | 0 | 100% |
| text_02_academico | Académico/expositivo | 21 | 19 | 2 | 0 | 0 | 100% |
| text_03_conversacional | Conversacional/interactivo | 23 | 23 | 0 | 0 | 0 | 100% |
| text_04_instruccional | Instruccional/prescriptivo | 18 | 17 | 1 | 0 | 0 | 100% |
| text_05_futuro_modales | Futuro y modales predictivos | 17 | 15 | 2 | 0 | 0 | 100% |
| text_06_relativos_negacion | Demostrativos, relativos y negación | 22 | 18 | 4 | 0 | 0 | 100% |
| text_07_verbos_especializados | Verbos especializados, hedges y downtoners | 16 | 13 | 3 | 0 | 0 | 100% |

## Rasgos con problemas (al menos 1 fallo)

No hay rasgos con fallos.

## Detalle por texto

### text_01_narrativa — Narrativa (3ª persona, pasado)

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

Todos los rasgos correctos.

### text_04_instruccional — Instruccional/prescriptivo

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_40_adj_attr` | 9 | 7 | -2 |

### text_05_futuro_modales — Futuro y modales predictivos

#### Dentro de tolerancia (revisar)

| Rasgo | Esperado | Observado | Δ |
|-------|----------|-----------|---|
| `f_11_indefinite_pronouns` | 1 | 3 | +2 |
| `f_14_nominalizations` | 3 | 5 | +2 |

### text_06_relativos_negacion — Demostrativos, relativos y negación

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

