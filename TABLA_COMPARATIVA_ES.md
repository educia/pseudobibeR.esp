# Comparativa de rasgos: antes → nuevo

Comparación de los 67 rasgos de `pseudobibeR.es` **antes** de la revisión (tabla
original) y **después** de implementarla. El
"nuevo" refleja el **comportamiento real** verificado sobre UDPipe `spanish-gsd`,
no solo la intención teórica.

> **Invariante:** los identificadores de columna (`f_NN_*`) y su orden **no
> cambian**. Los renombrados afectan solo a la etiqueta y descripción en
> español. ✳️ = cambió el comportamiento (no solo la etiqueta). ⚠️ = límite del
> modelo documentado.

---

## A. Tiempo y aspecto

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_01` ✳️ | Verbos en pretérito indefinido | Tiempos del pasado de indicativo y subjuntivo | Solo pretérito perfecto simple (`Tense=Past`) | Todo pasado finito `Tense ∈ {Past, Imp, Pqp}`, ind. **y** subj. ⚠️ imperfectos `-ía` a veces se pierden (mal-etiquetados condicional) | *corrí, corría, corriera, había corrido* |
| `f_02` | Aspecto perfecto (perífrasis haber + participio) | Tiempos compuestos (conjugación del verbo auxiliar haber + participio) | "perífrasis haber + participio" | Igual regla; se corrige la terminología ("tiempos compuestos", no perífrasis). NO incluye *tener/llevar* + participio (comparabilidad) | *ha escrito, habían terminado* |
| `f_03` | Verbos en presente de indicativo | Verbos en presente de indicativo | `Tense=Pres, Mood=Ind` | Igual (excluye subjuntivo, por decisión de comparabilidad) | *habla, se observa* |

## B. Adverbiales de lugar y tiempo

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_04` ✳️ | Adverbios y locuciones de lugar | Adverbios y locuciones de lugar | Diccionario de lugar | Igual + **inventario ampliado** (`en mitad de`, `más allá`, `por todas partes`, `a lo lejos`). Las locuciones sí se compactan y cuentan | *aquí, en mitad de* |
| `f_05` ✳️ | Adverbios y locuciones de tiempo | Adverbios y locuciones de tiempo | Diccionario de tiempo | Igual + `hace poco`, `hace tiempo` | *ayer, de vez en cuando* |

## C. Pronombres

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_06` ✳️ | Pronombres personales de 1ª persona | Formas pronominales y posesivas de primera persona | Solo pronombres explícitos | Pronombres **+ posesivos** (`mi`, `nuestro`, `mío`) por `Poss=Yes`+`Person` | *yo, nos, **mi** propuesta* |
| `f_07` ✳️ | Pronombres personales de 2ª persona | Formas pronominales y posesivas de segunda persona | Solo pronombres | + posesivos (`tu`, `vuestro`) + rescate de **voseo** (`vos`) | *tú, usted, **tu** informe, vos* |
| `f_08` ✳️ | Pronombres personales de 3ª persona | Formas pronominales y posesivas de tercera persona | Solo pronombres | + posesivos (`su`, incl. el de *usted*). ⚠️ el `se` reflexivo argumental no se cuenta (indistinguible de la pasiva refleja) | *él, lo, **su** plan* |
| `f_09` | Pronombre expletivo *it* (0) | *(igual, 0)* | Sin equivalente | Redacción corregida: las funciones de *it* se reparten (sujeto nulo, `lo/ello`, `esto`, `su`, `se`) | — |
| `f_10` | Demostrativos pronominales | Demostrativos en uso pronominal | `esto/eso/aquello…` | Igual | *eso es importante* |
| `f_11` ✳️ | Pronombres indefinidos | Indefinidos en uso pronominal | Lista léxica de indefinidos | + **control sintáctico**: solo función pronominal; excluye determinante (`todo el día`) y adjetival (`libro cualquiera`) | *alguien, nadie, algo* |
| `f_12` | Pro-verbo *do* (0) | *(igual, 0)* | Sin equivalente | Redacción: existen correlatos (`hacerlo`) pero no contabilizables unívocamente | — |

## D. Interrogativas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_13` | Preguntas con palabra interrogativa | Interrogativas directas parciales | Empieza con palabra interrogativa | El interrogativo **no** tiene que ir primero (`¿Y por qué…?` cuenta) | *¿Qué quieres?* |

## E. Formas nominales

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_14` | Nominalizaciones derivadas | Nominalizaciones derivadas | Sufijos productivos | Igual | *producción, evaluación* |
| `f_15` | *(0)* Gerundio nominal | *(0)* Infinitivos en función nominal | Siempre 0 | Sin cambio de comportamiento: la revisión lo activó vía infinitivo-sujeto (`csubj`), pero se **revirtió** a pedido del usuario. Sigue siempre 0. Solo cambia la etiqueta, para reflejar que la función nominal que cubriría (nominalización de una forma verbal no finita) la cubre en español el infinitivo, no el gerundio | — |
| `f_16` | Otros sustantivos | Otros sustantivos | NOUN/PROPN no en f_14 | Igual | *perro, mesa* |

## F. Pasivas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_17` ✳️ | Pasivas sin agente (perifrásticas y se-pasivas) | Pasivas sin complemento agente | Perifrásticas **y toda** se-pasiva/impersonal | Perifrásticas + pasiva refleja; **excluye la impersonal** (criterio: la refleja tiene sujeto paciente `nsubj`) | *fue redactado; se publicaron los informes* |
| `f_18` ⚠️ | Pasivas con agente (por) | Pasivas con complemento agente introducido por *por* | `por` + agente | ⚠️ **Bloqueada:** el modelo no emite `obl:agent`; agente y causa (`por incumplimiento`) son indistinguibles. Se evaluó y **descartó** un filtro de concordancia de número (falla en *"los proyectos fueron aprobados por el comité"* y en *"el informe fue aprobado por los ingenieros"*, y no filtra *"causado por la lluvia"*) | *fue aprobado por el comité* |

## G. Formas estativas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_19` | *Ser/estar* copulativos | Usos copulativos de *ser* y *estar* | Nominal y adjetival | + preposicional (`es de madera`), adverbial (`está bien`), infinitivo, locativo (`está en Lima`) | *es profesor; está en Lima* |
| `f_20` | Haber impersonal existencial | Haber impersonal en construcciones existenciales | `hay/había/hubo…` | Igual; excluye `haber que` (→ f_53) | *hay un problema* |

## H. Subordinación

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_21` ✳️ | Completivas con *que* (CD de verbo) | Completivas con *que* dependientes de un verbo | *que* como CD de verbo (sobrecontaba: absorbía también los casos adjetivales de f_22) | + término de preposición (`insistió en que…`). **Corregido**: ahora exige que el predicado real ("abuelo" de *que*) sea VERBO/AUX, no ADJETIVO — deja de solaparse con f_22 | *cree que…, insistió en que…* |
| `f_22` ✳️ | Completivas con *que* (de adjetivo) | Completivas con *que* dependientes de un predicado adjetival | ADJ + que (regla nunca disparaba: 0% recall) | **Corregido**: se busca el adjetivo en el "abuelo" de *que* (el head del verbo subordinado), no en el head inmediato de *que* (que siempre es el verbo). f_21 se corrigió en espejo para dejar de absorber estos casos | *es probable que llueva; estoy seguro de que ganaremos* |
| `f_23` ⚠️ | Interrogativas indirectas | Interrogativas indirectas | Palabra interrogativa subordinada | Igual. ⚠️ las **relativas libres** (`lo que dijo`) NO se incluyen (riesgo con f_29-f_34) | *no sé quién vino* |
| `f_24` | Infinitivos (complemento/perífrasis) | Infinitivos y oraciones de infinitivo | Complemento verbal, perífrasis | Sin cambio final: la revisión probó excluir `csubj` (exclusión mutua con f_15), pero al revertir f_15 esa exclusión también se deshizo | *quiere estudiar* |
| `f_25` | Gerundio adverbial/predicativo | Gerundio en función adverbial | Adverbial o predicativo | Solo adjunto (`advcl/ccomp`); excluye predicativo y perifrástico | *llegando tarde, salió* |
| `f_26` ⚠️ | Participio absoluto/adverbial | Participio en construcción absoluta o adverbial | `advcl/acl` no pasivo | Igual. ⚠️ detección variable | *terminado el examen…* |
| `f_27` | Participio postnominal | Participios postnominales con valor de relativa reducida | Reduce relativa de objeto | Igual; admite inacusativos | *el artículo publicado ayer* |
| `f_28` | *(0)* Gerundio postnominal | *(igual, 0)* | Agramatical | Redacción: "carece de mecanismo productivo" | — |
| `f_29` | Relativas *que* sujeto (absorbe f_31) | Relativas introducidas por *que* en las que el relativo funciona como sujeto (absorbe f_31) | *que* sujeto **+ absorbe f_31** | **Sin cambio neto de conteo** respecto del original: durante la revisión se probó separar quien/cual a f_31 (des-fusión, ver f_31), pero se **revirtió** por decisión del usuario — f_29 vuelve a fusionar *que* + *quien/cual* sujeto, igual que "antes". Solo cambia la etiqueta. ⚠️ pro-drop: el objeto sin sujeto explícito se imputa aquí | *el libro que está…* |
| `f_30` | Relativas *que* objeto (absorbe f_32) | Relativas introducidas por *que* en las que el relativo funciona como complemento directo (absorbe f_32) | *quien/cual* oblicuo **+ absorbe f_32** | **Sin cambio neto de conteo** respecto del original: mismo revert que f_29 (ver f_31/f_32) — f_30 vuelve a fusionar *que* objeto + *quien/cual* objeto, igual que "antes". Solo cambia la etiqueta | *el libro que María escribió* |
| `f_31` | *(0)* Integrado en f_29 | *(0)* Integrado en f_29 | Siempre 0 | **Sin cambio neto**: durante la revisión se des-fusionó brevemente (contaba *quien/cual* sujeto por separado), pero se **revirtió** por decisión del usuario tras confirmarse que era la decisión correcta para esta versión — el conteo final es idéntico al original (fusionado en f_29, siempre 0) | *la autora, quien presentó…* |
| `f_32` | *(0)* Integrado en f_30 | *(0)* Integrado en f_30 | Siempre 0 | **Sin cambio neto**: mismo revert que f_31 — el conteo final es idéntico al original (fusionado en f_30, siempre 0) | *el cual revisamos* |
| `f_33` | Pied-piping | Relativas con preposición antepuesta al relativo | prep + relativo | Igual; `a quien` cuenta aquí por precedencia | *por el cual, con quien* |
| `f_34` | Relativas oracionales | Relativas con antecedente oracional | `lo que/lo cual` proposicional | Igual | *…, lo cual molestó* |
| `f_35` | Causales *porque* | Cláusulas causales introducidas por *porque* | *porque* | Igual | *porque llovía* |
| `f_36` | Concesivas *aunque* | Cláusulas concesivas introducidas por *aunque* | *aunque* | Igual | *aunque llovía* |
| `f_37` ✳️ | Condicionales *si* | Cláusulas condicionales (*si*, *a menos que*, *salvo que*) | Solo *si* | + `a menos que`/`salvo que` (multipalabra); excluye el *si* interrogativo. Aclaración de redacción (sin nuevo patrón de detección): el condicional en la oración principal suele acompañar a la subordinada con *si* | *si llueve; a menos que pare* |
| `f_38` ✳️ | Otros subordinadores adverbiales | Otros subordinadores adverbiales | Incluía `a menos que` | Igual, **pero** `a menos que` sale a f_37 | *cuando, mientras, según* |

## I. Sintagmas preposicionales, adjetivos y adverbios

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_39` ✳️ | Preposiciones | Preposiciones | Todos los ADP | Igual, **pero** las locuciones (`a causa de`) cuentan **una vez**. Fix (v2): la deduplicación fallaba con contracciones *al/del* (`"viajó a través del país"` daba 2) por una fila "fantasma" del token multi-palabra; se excluye ahora del texto reconstruido | *de, en, a causa de* |
| `f_40` | Adjetivo modificador nominal | Adjetivos en función de modificador nominal | ADJ `amod` | Igual | *libro interesante* |
| `f_41` ✳️ | Adjetivos predicativos | Adjetivos en función predicativa: atributos y complementos predicativos | Atributo copulativo | **Ampliado (v2)**: ya no excluye predicativos — se quitó el filtro `lemma %in% linking_verbs` en la rama de cópula invertida, ya que si UDPipe etiquetó `dep_rel=="cop"` eso basta para contarlo | *es positivo, está cansada, llegó cansada, se considera relevante* |
| `f_42` ✳️ | Adverbios residuales | Total de adverbios | Excluía f_04/f_05/f_46-f_50/f_67 | **Todos** los ADV (solapamientos deliberados) | *rápidamente, muy, hoy, no* |

## J. Especificidad léxica

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_43` ✳️ | Diversidad léxica | Diversidad léxica | Medida `measure` | Igual (por defecto MATTR, no TTR), **pero ahora incluye la puntuación** en el cálculo (`remove_punct/remove_numbers/remove_symbols = FALSE` en `textstat_lexdiv()`), fiel a Biber (1988) ("type-token ratio, including punctuation"). Antes se excluía silenciosamente pese a que la tokenización previa sí la retenía | — |
| `f_44` | Longitud media de palabra | Longitud media de palabras | Caracteres por token | Igual | — |

## K. Clases léxicas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_45` | Conectores textuales | Conectores textuales | Diccionario | Igual | *sin embargo, por tanto* |
| `f_46` ✳️ | Atenuadores (incluía *casi*) | Atenuadores de grado y aproximativos | `casi, apenas, ligeramente…` | **`casi` sale** (pasa a f_47) | *apenas, un poco* |
| `f_47` ✳️ | Modalizadores epistémicos | Modalizadores epistémicos que corresponden a expresiones de aproximación, imprecisión o reserva | `quizás, tal vez…` | + **`casi`** (por coherencia con *almost*) | *quizás, tal vez, casi* |
| `f_48` | Amplificadores | Intensificadores de grado | Diccionario | Igual | *muy, totalmente* |
| `f_49` | Expresiones enfáticas | Expresiones de énfasis o refuerzo asertivo | Diccionario | Igual | *de hecho, sin duda* |
| `f_50` ⚠️ | Partículas discursivas | Partículas discursivas conversacionales | `bueno, pues, claro…` | Igual. ⚠️ **sin filtro posicional fiable** (el modelo no marca `discourse`: `Bueno`→ADJ, `Mira`→PROPN) | *bueno, pues, claro* |
| `f_51` | Determinantes demostrativos | Determinantes demostrativos antepuestos a un sustantivo | `este/ese/aquel` + N | Igual | *este libro* |

## L. Modales (perífrasis)

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_52` ✳️ | Posibilidad: *poder* + inf | Usos modales de *poder* + infinitivo | `poder` + infinitivo | + **`puede que`** + subjuntivo | *puede llover; puede que llueva* |
| `f_53` | Necesidad/obligación | Perífrasis modales de necesidad, obligación o conveniencia | `deber/tener que/haber que` + inf | Igual (+ `haber de`, `deber de`) | *debe terminar, hay que actuar* |
| `f_54` | Predicción (futuro/condicional/ir a) | Futuro, condicional e *ir a* + infinitivo | Futuro sintético, condicional, `ir a` | Igual; excluye `ir a` de desplazamiento | *llegará; va a llover* |

## M. Verbos especializados

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_55` ✳️ ⚠️ | Verbos de comunicación pública | Verbos de lengua o de comunicación pública | `afirmar, declarar…` | **`decir` agregado (v2)**, confirmado ausente empíricamente. ⚠️ desambiguación por modo (decir+subj→f_57) sigue sin implementarse (`sostuvo`→ADJ) | *afirmar, señalar, decir* |
| `f_56` ⚠️ | Verbos cognitivos | Verbos de pensamiento | `creer, pensar, saber…` | Igual. ⚠️ `esperar` cuenta siempre (no se separa 'aguardar') | *creer, suponer* |
| `f_57` ⚠️ | Verbos suasivos | Verbos suasivos | `recomendar, pedir…` | Igual (ver límite de f_55). ⚠️ **Límite de lematización confirmado (v2)**: UDPipe lematiza formas diptongadas de `recomendar` de forma irregular e impredecible (`recomienda`→`recomienda`, no `recomendar`); no se parchea por ser impredecible caso por caso | *recomendar, sugerir* |
| `f_58` ✳️ | Verbos de apariencia (*parecer, resultar*) | Verbos de apariencia o inferencia: *parecer* y ciertos usos de *resultar* | `parecer, resultar` (todos) | `parecer` siempre; **`resultar` solo copulativo** (`resulta adecuada`); excluye `resultó ganador`/`resultó de` | *parece difícil; resulta adecuada* |

## N. Formas reducidas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_59` | *(0)* Contracciones | *(igual, 0)* | "al/del no son contracciones" | **Corregido:** al/del **sí** lo son; lo ausente es el sistema de contracciones verbales/negativas (`can't`) | — |
| `f_60` | *(0)* Omisión de *that* | *(0)* Omisión restringida de la conjunción completiva *que* | "omisión agramatical" | **Corregido:** existe marginalmente (`espero sepan…`); 0 por decisión de diseño | — |
| `f_61` | *(0)* Preposición pospuesta | *(igual, 0)* | "no admite" | "carece de mecanismo productivo equivalente" | — |
| `f_62` | *(0)* Infinitivo escindido | *(igual, 0)* | "imposible" | "carece de equivalente formal" (afijo `-r`) | — |
| `f_63` ✳️ | Auxiliar escindido | Adverbios interpuestos entre el verbo auxiliar y el verbo auxiliado | ADV entre auxiliar y verbo | + **perífrasis modal** (`podría fácilmente resolver`); excluye sujetos interpuestos | *ha siempre sostenido* |

## O. Coordinación

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_64` ✳️ | Coordinación de sintagmas (N, ADJ) | Coordinación de palabras o grupos no oracionales | N/ADJ | + **verbos** (SV compartido, `lee y escribe`) y adverbios. Fix (v2): coordinación de ADJ con *pero* cuando el primero es `amod` de un sustantivo (`"un procedimiento rápido, pero eficaz"` daba 0; UDPipe cuelga el 2º ADJ del sustantivo, no del 1º) | *la autora y el editor; lee y escribe; rápido, pero eficaz* |
| `f_65` | Coordinación de cláusulas | Coordinación de cláusulas independientes mediante conjunciones coordinantes | `y, pero, sino, ni` | Igual (incl. `o`); depende del parseo | *llegó tarde y se fue* |

## P. Negación

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_66` | Negación sintética | Negación mediante palabras negativas distintas de *no* | `nadie, nunca, ninguno…` | Igual (ya incluye `ningún(o/a)` y `ni`) | *nadie, nunca, ni* |
| `f_67` ✳️ | Negación analítica (*no* preverbal) | Negación mediante el adverbio *no* | *no* antepuesto al verbo | + **`no` de foco** sobre otros constituyentes (`respuesta no definitiva`); excluye `no` respuesta/sustantivo | *no sabe; no definitiva* |

---

## Resumen de cambios de comportamiento (✳️)

Recalculado al cierre de la revisión v2 (código estabilizado, incluye 8.1–8.5).

Rasgos cuyo **conteo final** cambió respecto del original (no solo la
etiqueta): **f_01, f_04, f_05, f_06, f_07, f_08, f_11, f_17, f_21, f_22, f_37,
f_38, f_39, f_41, f_42, f_43, f_46, f_47, f_52, f_55, f_58, f_63, f_64, f_67**.

Rasgos que quedaron como estaban por **límite del modelo** (⚠️), documentado y no
forzado: **f_18** (obl:agent; se probó y descartó un filtro de concordancia de
número), **f_50** (sin señal discourse), **f_55** (desambiguación de `decir`
por modo), **f_57** (límite de lematización de `recomendar` confirmado en v2),
**f_23** (relativas libres), **f_56** (esperar). Ver `TABLA_RASGOS_ES.md` para
el detalle.

**Rasgos revertidos por decisión del usuario (sin cambio neto de conteo final
respecto del original):**
- **f_15**: se activó en la Fase 3 (infinitivo nominal-sujeto) pero se
  revirtió a su comportamiento original de columna siempre-cero; f_24
  recuperó en consecuencia su regla original (sin excluir `csubj`). Solo
  cambia la etiqueta (ver 9.1).
- **f_31/f_32**: en la Fase 2 se des-fusionaron de f_29/f_30 (empezaron a
  contar *quien/cual* por separado); en la revisión v2 el usuario decidió
  revertir esa des-fusión — f_31/f_32 vuelven a fusionarse en f_29/f_30 y a
  ser columnas siempre-cero, exactamente como en el diseño original. f_29/f_30
  ya no llevan ✳️ porque su conteo final es idéntico al de "antes"; solo
  cambió la etiqueta (ver 9.1).
