# Tabla de rasgos de pseudobibeR.es (etiquetas en español)

Documentación de los 67 rasgos de Biber (1988) adaptados al español, tras una
revisión lingüística del paquete. Refleja el
**comportamiento real** del extractor sobre el modelo UDPipe `spanish-gsd`, no
solo la intención teórica.

## Invariantes (no negociables)

1. **La salida son exactamente 67 columnas de rasgos**, con los **nombres
   idénticos** a `pseudobibeR` en inglés (`f_31_wh_subj`/`f_32_wh_obj` aunque
   cuenten *quien*/*el cual*).
   Los renombrados de esta tabla afectan **solo a la etiqueta y descripción en
   español**, nunca al identificador de columna ni a su orden.
2. Los subtotales (modo/tiempo/aspecto) se calculan dentro de la función y se
   **suman antes de devolver**: no se exponen como columnas nuevas.
3. Metodología: cada regla se verificó empíricamente contra las etiquetas de UD
   antes de implementarse; cuando `spanish-gsd` no soporta una distinción, **no
   se fuerza** — se documenta como limitación (marcado ⚠️).

---

## A. Tiempo y aspecto

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_01_past_tense` | Tiempos del pasado de indicativo y subjuntivo | `VERB/AUX` finito con `Tense ∈ {Past, Imp, Pqp}`, sin filtrar modo. Cubre perfecto simple, imperfecto (ind. y subj.) y el auxiliar de los pluscuamperfectos. | ⚠️ `spanish-gsd` no emite `Tense=Pqp`; algunos imperfectos en `-ía` (`corría`) se confunden con condicional y se pierden. Solapamiento deliberado con f_02 (el auxiliar `había` cuenta en ambos). |
| `f_02_perfect_aspect` | Tiempos compuestos (conjugación del verbo auxiliar haber + participio) | `haber` (aux) + participio; excluye `estar` copulativo. | No se incluye *tener/llevar* + participio (decisión de comparabilidad con el *have* inglés). |
| `f_03_present_tense` | Verbos en presente de indicativo | `VERB/AUX`, `Tense=Pres, Mood=Ind, VerbForm=Fin`. | Excluye el presente de subjuntivo. |

## B. Adverbiales de lugar y tiempo

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_04_place_adverbials` | Adverbios y locuciones de lugar | Diccionario (adverbios simples + locuciones multipalabra: `aquí`, `en mitad de`, `a lo lejos`…). Las locuciones se compactan vía `multiword_patterns`. | La evidencia trazada no cubre las locuciones (solo el conteo). |
| `f_05_time_adverbials` | Adverbios y locuciones de tiempo | Ídem con inventario de tiempo (`ayer`, `de vez en cuando`, `hace poco`…). | — |

## C. Pronombres

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_06_first_person_pronouns` | Formas pronominales y posesivas de primera persona | Pronombres (`yo`, `me`, `nos`…) **+ posesivos** por `Poss=Yes` + `Person=1` (`mi`, `nuestro`) y tónicos por lema (`mío`). | — |
| `f_07_second_person_pronouns` | Formas pronominales y posesivas de segunda persona | `tú`, `usted(es)`, `te`, `os`… + posesivos `Person=2` (`tu`, `vuestro`) + rescate de voseo (`vos`, mal-etiquetado NOUN). | `usted(es)` cuentan aquí aunque concuerden en 3ª. |
| `f_08_third_person_pronouns` | Formas pronominales y posesivas de tercera persona | `él/ella/ellos`, clíticos `lo/la/le`… + posesivos `Person=3` (`su`, incluido el de *usted*). | ⚠️ El `se` reflexivo argumental **no se cuenta**: es indistinguible de la pasiva refleja/impersonal (todos `Reflex=Yes`, `iobj`). |
| `f_09_pronoun_it` | *(0)* Pronombre expletivo *it* | Siempre 0. | Sin equivalente en español (sujeto nulo). Las funciones de *it* se reparten entre sujeto nulo, `lo/ello`, `esto/eso`, `su`, `se` — sin forma única contabilizable sin solapar con f_08/f_10. |
| `f_10_demonstrative_pronoun` | Demostrativos en uso pronominal | `esto/eso/aquello/este/ese` sin sustantivo. | No confundir con f_51 (determinantes demostrativos). |
| `f_11_indefinite_pronouns` | Indefinidos en uso pronominal | `alguien/nadie/algo/nada/cualquiera…` con función **pronominal independiente**; excluye el uso determinante (`todo el día`) y adjetival (`libro cualquiera`). Es *code-only* para que la superficie no anule el control. | ⚠️ El `algo` adverbial de grado (`está algo cansado`) el modelo lo mis-etiqueta `PRON nsubj` → inseparable. |
| `f_12_proverb_do` | *(0)* Pro-verbo *do* | Siempre 0. | El español resuelve la anáfora verbal por elipsis; sin pro-verbo equivalente contabilizable de forma unívoca. |

## D. Interrogativas

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_13_wh_question` | Interrogativas directas parciales | Palabra interrogativa en oración con `?`; el interrogativo **no** tiene que ser el primer token (`¿Y por qué…?` cuenta). | — |

## E. Formas nominales

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_14_nominalizations` | Nominalizaciones derivadas | Sustantivos con sufijos productivos (`-ción`, `-idad`, `-miento`…). | — |
| `f_15_gerunds` | *(0)* Infinitivos en función nominal | Siempre 0 (revertido a diseño original por decisión del usuario). | Etiqueta técnica: el nombre refleja la función que este rasgo cubriría en inglés (nominalización de una forma verbal no finita), no que se esté detectando algo. El gerundio español (*-ando/-iendo*) no admite función nominal; la función nominal de una forma verbal no finita la cubre en español el infinitivo (f_24) o una nominalización derivada (f_14). Un intento de activarlo vía infinitivo-sujeto (`csubj`) se probó en la revisión y se revirtió. |
| `f_16_other_nouns` | Otros sustantivos | `NOUN/PROPN` no contabilizados en f_14. | — |

## F. Pasivas

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_17_agentless_passives` | Pasivas sin complemento agente | Perifrástica (`fue redactado`) **+ pasiva refleja** (`se publicaron los informes`), sin `por`. | ⚠️ La **impersonal con se** se excluye (criterio: la refleja tiene sujeto paciente `nsubj`; la impersonal no: `se entrevistó a los candidatos`, `se recomienda leer`). |
| `f_18_by_passives` | Pasivas con complemento agente introducido por *por* | Pasiva + `por` en las dos posiciones siguientes. | ⚠️ **Bloqueada por el modelo:** `spanish-gsd` no emite `obl:agent`; agente (`por el comité`) y causa (`por incumplimiento`) son indistinguibles (ambos `obl`). Puede contar falsos positivos causales. Se evaluó y **descartó** un filtro de concordancia de número entre el sujeto pasivo y el sintagma agente (ver "Limitaciones conocidas"). |

## G. Formas estativas

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_19_be_main_verb` | Usos copulativos de *ser* y *estar* | Cópula nominal, adjetival, preposicional (`es de madera`), adverbial (`está bien`), de infinitivo (`su objetivo es ganar`) y locativa (`está en Lima`). Excluye usos auxiliares (pasiva/progresivo) y *parecer* (f_58). | — |
| `f_20_existential_there` | Haber impersonal en construcciones existenciales | `hay/había/hubo/habrá/haya…`. Excluye `haber` auxiliar y `haber que` (→ f_53). | — |

## H. Subordinación

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_21_that_verb_comp` | Completivas con *que* dependientes de un verbo | *que* complementante de un verbo, incl. término de preposición (`insistió en que…`, `se alegró de que…`). Se busca el predicado real en el "abuelo" de *que* (ver f_22), exigiendo que sea VERBO/AUX. | Corregido: antes absorbía también los casos adjetivales de f_22 (ver f_22). |
| `f_22_that_adj_comp` | Completivas con *que* dependientes de un predicado adjetival | `que` (SCONJ, mark) siempre tiene como head el verbo de su propia cláusula (V); el adjetivo real vive en el head DE ESE VERBO (el "abuelo" de *que*, vía `csubj`/`advcl`). Se comprueba que ese abuelo sea ADJ: `es probable que llueva`, `estoy seguro de que ganaremos`. | Corregido (antes: 0% de recall, siempre daba 0 — la regla comprobaba el head inmediato de *que*, que nunca es un ADJ). Verificado en 8 construcciones distintas. |
| `f_23_wh_clause` | Interrogativas indirectas | Palabra interrogativa **acentuada** en cláusula subordinada. | ⚠️ Las **relativas libres** (`lo que dijo`, `donde me indiques`) NO se incluyen: separarlas de f_29-f_34 sin coreferencia es de alto riesgo (deferido). |
| `f_24_infinitives` | Infinitivos y oraciones de infinitivo | Todo `VerbForm=Inf`: xcomp, perífrasis, complemento del nombre/adjetivo, finales (incluye el infinitivo-sujeto, ya que f_15 no lo separa). | — |
| `f_25_present_participle` | Gerundio en función adverbial | Gerundio `advcl/ccomp`; excluye predicativo (`lo vi entrando`) y perifrástico (`está trabajando`). | — |
| `f_26_past_participle` | Participio en construcción absoluta o adverbial | Participio `advcl/acl` no pasivo (`terminada la reunión…`). | ⚠️ Detección variable según el análisis de dependencias. |
| `f_27_past_participle_whiz` | Participios postnominales con valor de relativa reducida | Participio `acl` con head NOUN (`el artículo publicado ayer`). | — |
| `f_28_present_participle_whiz` | *(0)* Gerundio postnominal | Siempre 0. | El español carece de un mecanismo productivo general de gerundio postnominal modificador. |
| `f_29_that_subj` | Relativas introducidas por *que* en las que el relativo funciona como sujeto (absorbe f_31) | *que* relativo (SCONJ/mark en `acl:relcl`) sin `nsubj` propio en la relativa, **más** `quien/cual` (`PronType=Rel`) con rol `nsubj` (fusionado desde f_31; ver f_31). | ⚠️ En pro-drop puro, el objeto-relativa sin sujeto explícito se imputa aquí. |
| `f_30_that_obj` | Relativas introducidas por *que* en las que el relativo funciona como complemento directo (absorbe f_32) | *que* relativo cuya relativa tiene un `nsubj` explícito (el hueco es el objeto), **más** `quien/cual` con rol `obj/iobj` sin preposición (fusionado desde f_32; ver f_32). | Solo dispara con sujeto explícito en la relativa. |
| `f_31_wh_subj` | *(0)* Integrado en f_29 | Siempre 0. Revertido: vuelve a fusionarse en f_29 (decisión del usuario en la revisión v2, no artefacto). | El conteo de `quien/cual` con rol `nsubj` vive en f_29 (ver f_29). Esta columna se mantiene solo por compatibilidad de esquema (67 columnas fijas). |
| `f_32_wh_obj` | *(0)* Integrado en f_30 | Siempre 0. Revertido: vuelve a fusionarse en f_30 (decisión del usuario en la revisión v2, no artefacto). | El conteo de `quien/cual` con rol `obj/iobj` vive en f_30 (ver f_30). Esta columna se mantiene solo por compatibilidad de esquema (67 columnas fijas). |
| `f_33_pied_piping` | Relativas con preposición antepuesta al relativo | ADP + (DET) + pronombre relativo (`por el cual`, `con quien`, `a quien`). | *a quien* (CD de persona) cuenta aquí por precedencia del pied-piping. |
| `f_34_sentence_relatives` | Relativas con antecedente oracional | `lo que`/`lo cual` con antecedente proposicional (`…, lo cual molestó…`). | Las relativas libres sin antecedente proposicional no cuentan aquí. |
| `f_35_because` | Cláusulas causales introducidas por *porque* | *porque*. | — |
| `f_36_though` | Cláusulas concesivas introducidas por *aunque* | *aunque*. | — |
| `f_37_if` | Cláusulas condicionales (*si*, *a menos que*, *salvo que*) | *si* condicional (excluye el *si* interrogativo indirecto) + locuciones `a menos que` / `salvo que` (multipalabra, detectadas por secuencia y excluidas de f_38). | Aclaración de redacción (sin cambio de detección): el condicional en la oración principal (f_54/Mood=Cnd) suele acompañar a la subordinada con *si*; no se implementa como patrón conjunto, es solo una nota descriptiva. |
| `f_38_other_adv_sub` | Otros subordinadores adverbiales | `cuando`, `mientras`, `según`, `para que`… (SCONJ/ADP/ADV mark no contados en f_21/f_22/f_35/f_36/f_37). | Excluye los componentes de `a menos que`/`salvo que`. |

## I. Sintagmas preposicionales, adjetivos y adverbios

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_39_prepositions` | Preposiciones | Todos los ADP; las **locuciones preposicionales** (`a causa de`, `en relación con`…) cuentan **una vez** (léxico acotado, resta del exceso). `al`/`del` cuentan una cada una. | Fix (revisión v2): la deduplicación de locuciones fallaba con contracciones *al/del* (`"viajó a través del país"` daba 2 en vez de 1) porque la fila "fantasma" del token multi-palabra (`token_id` tipo `4-5`, `pos=NA`) se colaba en el texto reconstruido para la búsqueda; se excluyen ahora esas filas al reconstruir el texto. Las locuciones acentuadas requieren locale UTF-8 (los literales del código usan escapes `\uXXXX`, no caracteres acentuados directos, para evitar corrupción de encoding al cargar el paquete). |
| `f_40_adj_attr` | Adjetivos en función de modificador nominal | ADJ `amod`. | — |
| `f_41_adj_pred` | Adjetivos en función predicativa: atributos y complementos predicativos | ADJ atributo de cópula (`es positivo`, `está cansada`) **más** complementos predicativos, ampliado en la revisión v2 (`dep_rel ∈ {xcomp, acomp}` sin restricción de lema, y la rama de cópula invertida sin restricción a `linking_verbs`): `llegó cansada`, `encontraron abierta la puerta`, `se considera relevante`. | Fix (revisión v2): antes se exigía `lemma %in% linking_verbs` (ser/estar/parecer…) en la rama de cópula invertida (sujeto tácito o pospuesto, donde UDPipe reanaliza el ADJ como `root` y el verbo real como su `cop`); eso excluía predicativos con verbos como `llegar`/`encontrar`/`considerar`. Se quitó el filtro de lema: si UDPipe etiquetó `dep_rel=="cop"`, ya está afirmando estructuralmente que ese verbo funciona como cópula ahí. |
| `f_42_adverbs` | Total de adverbios | **Todos** los `ADV`, con solapamientos deliberados (`hoy` en f_05 y f_42; `muy` en f_48 y f_42; `no` en f_67 y f_42). | — |

## J. Especificidad léxica

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_43_type_token` | Diversidad léxica | Medida del argumento `measure`; por defecto **MATTR** (no la TTR original). Desde la revisión v2, el cálculo incluye la puntuación (`textstat_lexdiv(..., remove_punct = FALSE, remove_numbers = FALSE, remove_symbols = FALSE)`), fiel a la definición de Biber (1988): "type-token ratio, including punctuation". | Fix (revisión v2): `textstat_lexdiv()` tiene esos tres parámetros en `TRUE` por defecto y la llamada nunca los sobreescribía, así que la puntuación jamás entraba al cálculo pese a que la tokenización previa (`quanteda::tokens(..., remove_punct = FALSE)`) sí la retenía. Verificado: MATTR sin cambio (0.08→0.08) con el default al añadir puntuación a un texto de prueba; con `remove_punct = FALSE` sí cambia (0.08→0.16). Ventana móvil; sensible a la definición de *type*. |
| `f_44_mean_word_length` | Longitud media de palabras | Media de caracteres por token, excluida la puntuación. | — |

## K. Clases léxicas

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_45_conjuncts` | Conectores textuales | `sin embargo`, `por tanto`, `no obstante`… (diccionario). | — |
| `f_46_downtoners` | Atenuadores de grado y aproximativos | `apenas`, `ligeramente`, `un poco`… (`casi` ya **no** está aquí). | — |
| `f_47_hedges` | Modalizadores epistémicos que corresponden a expresiones de aproximación, imprecisión o reserva | `quizás`, `tal vez`, `probablemente`, `más o menos`, `una especie de`, **`casi`** (por coherencia con *almost*). | — |
| `f_48_amplifiers` | Intensificadores de grado | `muy`, `totalmente`, `absolutamente`… | — |
| `f_49_emphatics` | Expresiones de énfasis o refuerzo asertivo | `de hecho`, `sin duda`, `realmente`… | — |
| `f_50_discourse_particles` | Partículas discursivas conversacionales | Diccionario de superficie (`bueno`, `pues`, `claro`, `mira`…). | ⚠️ **Sin filtro posicional fiable:** `spanish-gsd` no marca `discourse` y mis-etiqueta las formas ambiguas (`Bueno`→ADJ, `Mira`→PROPN, `Claro`→PROPN). Cuenta algún falso positivo (autorizado por la especificación base). |
| `f_51_demonstratives` | Determinantes demostrativos antepuestos a un sustantivo | `este/ese/aquel` + sustantivo. | No confundir con f_10 (pronominales). |

## L. Modales (perífrasis)

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_52_modal_possibility` | Usos modales de *poder* + infinitivo | `poder` + infinitivo **+ `puede que`** + subjuntivo (analizado como expresión fija `poder` + `que` fixed). | — |
| `f_53_modal_necessity` | Perífrasis modales de necesidad, obligación o conveniencia | `deber` + inf, `tener que`, `haber que`, `haber de` + inf; incluye `deber de` (probabilidad). | — |
| `f_54_modal_predictive` | Futuro, condicional e *ir a* + infinitivo | Futuro sintético, condicional (`Mood=Cnd`) y `ir a` + inf. Excluye `ir a` de desplazamiento (`Tense=Past`). | — |

## M. Verbos especializados

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_55_verb_public` | Verbos de lengua o de comunicación pública | Lemas de comunicación (`afirmar`, `señalar`, `sostener`, `decir`…). Fix (revisión v2): se agregó `decir` al diccionario (`dict$f_55_verb_public`), confirmado ausente empíricamente (*"El testigo dijo que todo era cierto."* daba 0). | ⚠️ La desambiguación por modo (`decir`+subj→f_57 / `decir`+cnd→f_55) no se implementó: el modelo mis-etiqueta verbos polisémicos (`sostuvo`→ADJ). |
| `f_56_verb_private` | Verbos de pensamiento | `creer`, `pensar`, `saber`, `suponer`… | ⚠️ `esperar` cuenta siempre (no se separa 'aguardar' de 'tener expectativa'). |
| `f_57_verb_suasive` | Verbos suasivos | `pedir`, `recomendar`, `sugerir`, `proponer`… | Ver límite de f_55 (desambiguación por modo no implementada). ⚠️ **Límite de lematización confirmado:** UDPipe lematiza formas diptongadas de `recomendar` de forma irregular e impredecible (`recomienda`→`recomienda`, `recomiendan`→`recomiendar`, `recomiendo`→`recomir`), por lo que *"El jefe recomienda que trabajen más."* da 0 pese a que `recomendar` sí está en el diccionario. No se parcheó con una lista de formas de superficie por ser demasiado impredecible caso por caso; documentado como limitación del modelo. |
| `f_58_verb_seem` | Verbos de apariencia o inferencia: *parecer* y ciertos usos de *resultar* | `parecer` (siempre) y `resultar` **solo en uso copulativo** de apariencia (`resulta adecuada`); excluye `resultó ganador` y `resultó de`. | — |

## N. Formas reducidas

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_59_contractions` | *(0)* Contracciones | Siempre 0. | `al`/`del` **sí** son contracciones para la RAE; lo que el español no tiene es un sistema productivo de contracciones verbales/negativas (`can't`, `I've`), que es lo que mide Biber. |
| `f_60_that_deletion` | *(0)* Omisión restringida de la conjunción completiva *que* | Siempre 0. | La omisión de *que* existe marginalmente en español formal (`espero sepan disculpar`); se mantiene 0 como decisión de diseño (baja productividad/comparabilidad). |
| `f_61_stranded_preposition` | *(0)* Preposición pospuesta | Siempre 0. | El español estándar carece de un mecanismo productivo equivalente. |
| `f_62_split_infinitive` | *(0)* Infinitivo escindido | Siempre 0. | El infinitivo español es una forma con afijo `-r`, no partícula + verbo. |
| `f_63_split_auxiliary` | Adverbios interpuestos entre el verbo auxiliar y el verbo auxiliado | ADV entre auxiliar y verbo auxiliado, **incl. perífrasis modal** (`podría fácilmente resolver`). Excluye sujetos interpuestos (`podía yo saberlo`). | — |

## O. Coordinación

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_64_phrasal_coordination` | Coordinación de palabras o grupos no oracionales | Coordinación de nombres, adjetivos, adverbios **y verbos** (SV con sujeto compartido, `lee y escribe`). | `!has_subject` la separa de f_65. Fix (revisión v2): *"Un procedimiento rápido, pero eficaz."* daba 0 (debía dar 1). Causa raíz: cuando el primer ADJ (`rápido`) es `amod` de un sustantivo, UDPipe cuelga el segundo ADJ (`eficaz`) del sustantivo (`conj`), no del primer ADJ, así que la regla original (comparar el POS del "head del head" del CCONJ) veía NOUN vs. ADJ y descartaba el caso. Se agregó una rama que, cuando el head del segundo conjunto es un NOUN con un `amod` propio, usa ese `amod` como "primer conjunto" para la comparación de POS. |
| `f_65_clausal_coordination` | Coordinación de cláusulas independientes mediante conjunciones coordinantes | CCONJ (`y/e/o/pero`) entre cláusulas con sujeto propio, o `y/e` en posición inicial. | Depende de la calidad del parseo (nombres propios pueden romperlo). |

## P. Negación

| Código | Etiqueta ES | Regla real | Notas / límites |
|---|---|---|---|
| `f_66_neg_synthetic` | Negación mediante palabras negativas distintas de *no* | `nadie/nunca/jamás/tampoco/ningún(o/a)/nada` + `ni`. Solapamientos deliberados con f_11/f_67 (concordancia negativa). | — |
| `f_67_neg_analytic` | Negación mediante el adverbio *no* | `no` preverbal **y de foco** sobre otros constituyentes (`respuesta no definitiva`, `no muy lejos`). Excluye `no` como respuesta y como sustantivo (`el no del comité`). | — |

---

## Limitaciones conocidas (resumen)

Situaciones que reflejan límites del modelo `spanish-gsd`, no errores del
extractor. No se fuerzan (§1.2/§10 de las instrucciones):

- **`se` argumental vs. pasiva refleja/impersonal** (f_08, f_17): morfológicamente
  idénticos (`Reflex=Yes`, `iobj`). f_08 excluye todo `se`; f_17 usa la presencia
  de sujeto paciente (`nsubj`) para separar refleja (cuenta) de impersonal (no).
- **Agente vs. causa con *por*** (f_18): el modelo no emite `obl:agent`; ambos son
  `obl`. f_18 puede incluir falsos positivos causales. Se probó y **descartó** un
  filtro de concordancia de número (sujeto pasivo vs. sintagma agente) propuesto en
  la revisión: falla en casos legítimos y comunes — *"los proyectos fueron
  aprobados por el comité"* (agente colectivo singular, sujeto plural) y *"el
  informe fue aprobado por los ingenieros"* (agente plural, sujeto singular) se
  excluirían indebidamente — y no distingue el caso que se quería filtrar
  (*"el accidente fue causado por la lluvia"*, singular-singular, sigue colando).
- **Relativas con pro-drop** (f_29/f_30): sin sujeto explícito, el objeto-relativa
  se etiqueta `nsubj` y cae en f_29 en vez de f_30.
- **Relativas libres** (f_23): no separables de f_29–f_34 sin coreferencia (deferido).
- **Imperfectos en `-ía`** (f_01): a veces mal etiquetados `Mood=Cnd`.
- **Verbos polisémicos** (f_55/f_57): `sostuvo` se mis-etiqueta ADJ; la
  desambiguación de `decir` por modo no se implementó. `recomendar` (f_57) tiene
  un límite de lematización confirmado en sus formas diptongadas (ver f_57).
- **Partículas discursivas ambiguas** (f_50): sin señal `discourse`; el filtro
  posicional no es fiable (`Bueno`→ADJ, `Mira`→PROPN).
- **Encoding**: las locuciones acentuadas de f_39 requieren locale UTF-8; los
  literales del código fuente usan escapes `\uXXXX` en vez de caracteres
  acentuados directos para evitar corrupción al cargar el paquete bajo locales
  no UTF-8 (p. ej. `LC_CTYPE=C`).

## Columnas siempre en cero (10)

`f_09`, `f_12`, `f_15`, `f_28`, `f_31`, `f_32`, `f_59`, `f_60`, `f_61`, `f_62` —
justificadas lingüísticamente arriba. (`f_31`/`f_32` volvieron a fusionarse en
f_29/f_30 en la revisión v2, tras haberse des-fusionado brevemente en la Fase 2;
`f_15` se activó en la Fase 3 y se revirtió por decisión del usuario.)
