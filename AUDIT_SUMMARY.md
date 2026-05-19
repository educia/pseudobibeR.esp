# AUDIT_SUMMARY.md — Auditoría completa pseudobibeR.es

**Fecha:** 2026-05-19 · **Modelo:** UDPipe `spanish-gsd`
**Fuente de verdad:** `biber_espanol_completo.md`

## Resultado global

| Métrica | Valor |
|---|---|
| Rasgos auditados | 57 |
| Rasgos ✅ OK (cobertura pos/neg) | 55 |
| Ruido aceptable por spec (⚠️) | 1 (f_50, §f_50) |
| Limitación UDPipe documentada (❌) | 1 (f_22, §f_22) |
| **Bugs reales pendientes** | **0** |
| Validación: comparaciones | 181 |
| Validación: FAIL | **0** |
| Validación: OK | 170 (93.9 %) |
| Validación: TOLERANCE | 11 (6.1 %) |
| **Pass rate por texto** | **8/8 = 100 %** |

## Pass rate por texto (corpus de validación)

| Texto | Registro | OK | Tol | Fail | Pass |
|---|---|---|---|---|---|
| text_01_narrativa | Narrativa 3ª/pasado | 52 | 3 | 0 | 100 % |
| text_02_academico | Académico | 19 | 2 | 0 | 100 % |
| text_03_conversacional | Conversacional | 22 | 1 | 0 | 100 % |
| text_04_instruccional | Instruccional | 17 | 1 | 0 | 100 % |
| text_05_futuro_modales | Futuro/modales | 17 | 0 | 0 | 100 % |
| text_06_relativos_negacion | Relativos/negación | 20 | 2 | 0 | 100 % |
| text_07_verbos_especializados | Verbos/hedges | 14 | 2 | 0 | 100 % |
| text_08_casos_limite | Casos límite multi-token | 9 | 0 | 0 | 100 % |

## Fases ejecutadas

**Fase 1 — Limpieza** (`CLEANUP_REPORT.md`)
- Salida confirmada en 57 rasgos exactos (fixes f_69/f_70/f_71 + doble
  normalización f_43/f_44 ya aplicados en sesión previa).
- Eliminados stubs `block_contractions_es` / `block_stranded_split_es`.
- Tests franceses → `tests/testthat/fr/`; helpers split es/fr.
- Reorg de arquitectura (Pasos 1–5) **pospuesta** (opción C del usuario):
  premisa falsa — no hay archivos `block_*.R` ni `block_*_fr` definidos
  en este fork; se actualizará desde el repo original post-auditoría.

**Fase 2 — Cobertura 57 rasgos** (`FEATURE_AUDIT.md`,
`tests/testthat/test-feature-coverage.R`)
- Estado inicial: 45 OK, 8 OVER, 4 FAIL.
- 10 bugs reales identificados vs fuente de verdad y corregidos:

| Rasgo | Bug | Commit |
|---|---|---|
| f_11 | artículos un/una contados | `300709c` |
| f_37 | si interrogativo indirecto | `b078a7e` |
| f_52 | sustantivo poder | `ad760c2` |
| f_51 | neutros como determinante | `a9e0ed5` |
| f_30 | relativas objeto → f_29 (merge) | `d0af907` |
| f_21 | relativas → complemento verbal | `83a5df7` |
| f_14 | regex -ado/-ido (lado/grado) | `9c69171` |
| f_34 | lo cual/lo que oracional | `62613d4` |
| f_65 | y/e clause-initial tras punto | `72f4bd9` |
| f_23 | interrogativa directa → f_13 | `a78f8c3` |

- Estado final: **54 OK, 1 OVER (ruido spec), 2 FAIL** (f_22 limitación
  documentada; f_30 = artefacto del script throwaway, OK con frase
  correcta verificada).

**Fase 3 — Multi-token** (`MULTIWORD_DIAGNOSIS.md`)
- Infraestructura `tokens_compound` + `flag_mwe_tokens` funciona.
- 7/7 diagnósticos pasan; regresiones críticas f_19 (*o sea*) y f_20
  (*hay que*) = 0 correctas.
- Fix adicional f_38 (`mientras_que` CCONJ): commit `a284f81`.

**Fase 4 — Validación cruzada**
- 8 textos, 181 comparaciones, **0 FAIL**, 100 % pass rate por texto.
- Mejora acumulada: 166 → **170 OK** durante los fixes (sin regresiones).

## Limitaciones documentadas (no bugs)

- **f_22_that_adj_comp**: UDPipe no etiqueta el head de *que* como ADJ en
  copulativas (`biber_espanol_completo.md §f_22` lo reconoce explícitamente).
- **f_50_discourse_particles**: sin filtro posicional en baseline; spec
  §f_50 autoriza el ruido. Mejora opcional futura.
- **f_29/f_30 pro-drop**: cuando el sujeto está elidido y no hay nsubj
  externo, el hueco no es distinguible; queda en f_29 (§2.5/§6/§f_29).

## Pendiente post-auditoría

- Opción C: traer `block_*_fr` reales desde el repo original
  `pseudobibeR.fr` y luego separar arquitectura es/fr.
- Re-cablear discovery de testthat para `tests/testthat/fr/` (subdir no
  auto-descubierto).

## Historial de commits de la auditoría

```
f482a92 chore: baseline limpio
2b7d41b chore: eliminar stubs vestigiales
72d9644 chore: separar suite tests por idioma
28accb3 test: auditoría Fase 2 cobertura 57 rasgos
300709c fix(f_11)   b078a7e fix(f_37)   ad760c2 fix(f_52)
a9e0ed5 fix(f_51)   d0af907 fix(f_30)   83a5df7 fix(f_21)
9c69171 fix(f_14)   62613d4 fix(f_34)   72f4bd9 fix(f_65)
a78f8c3 fix(f_23)   a284f81 fix(f_38)
```
