# Comparativa de rasgos: antes → nuevo (para revisión de Hernán)

Comparación de los 67 rasgos de `pseudobibeR.es` **antes** de la revisión (tabla
original) y **después** de implementarla (rama `feat/revision-hernan`). El
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
| `f_01` ✳️ | Verbos en pretérito indefinido | Verbos en tiempos de pasado (indicativo y subjuntivo) | Solo pretérito perfecto simple (`Tense=Past`) | Todo pasado finito `Tense ∈ {Past, Imp, Pqp}`, ind. **y** subj. ⚠️ imperfectos `-ía` a veces se pierden (mal-etiquetados condicional) | *corrí, corría, corriera, había corrido* |
| `f_02` | Aspecto perfecto (perífrasis haber + participio) | Tiempos compuestos (haber + participio) | "perífrasis haber + participio" | Igual regla; se corrige la terminología ("tiempos compuestos", no perífrasis). NO incluye *tener/llevar* + participio (comparabilidad) | *ha escrito, habían terminado* |
| `f_03` | Verbos en presente de indicativo | Verbos en presente de indicativo | `Tense=Pres, Mood=Ind` | Igual (excluye subjuntivo, por decisión de comparabilidad) | *habla, se observa* |

## B. Adverbiales de lugar y tiempo

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_04` ✳️ | Adverbios y locuciones de lugar | Adverbios y locuciones de lugar | Diccionario de lugar | Igual + **inventario ampliado** (`en mitad de`, `más allá`, `por todas partes`, `a lo lejos`). Las locuciones sí se compactan y cuentan | *aquí, en mitad de* |
| `f_05` ✳️ | Adverbios y locuciones de tiempo | Adverbios y locuciones de tiempo | Diccionario de tiempo | Igual + `hace poco`, `hace tiempo` | *ayer, de vez en cuando* |

## C. Pronombres

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_06` ✳️ | Pronombres personales de 1ª persona | Formas pronominales y **posesivas** de 1ª persona | Solo pronombres explícitos | Pronombres **+ posesivos** (`mi`, `nuestro`, `mío`) por `Poss=Yes`+`Person` | *yo, nos, **mi** propuesta* |
| `f_07` ✳️ | Pronombres personales de 2ª persona | Formas pronominales y posesivas de 2ª persona | Solo pronombres | + posesivos (`tu`, `vuestro`) + rescate de **voseo** (`vos`) | *tú, usted, **tu** informe, vos* |
| `f_08` ✳️ | Pronombres personales de 3ª persona | Formas pronominales y posesivas de 3ª persona | Solo pronombres | + posesivos (`su`, incl. el de *usted*). ⚠️ el `se` reflexivo argumental no se cuenta (indistinguible de la pasiva refleja) | *él, lo, **su** plan* |
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
| `f_15` | *(0)* Gerundio nominal | *(0)* Gerundio nominal | Siempre 0 | Sin cambio final: la revisión lo activó vía infinitivo-sujeto (`csubj`), pero se **revirtió** a pedido del usuario. Sigue siempre 0 | — |
| `f_16` | Otros sustantivos | Otros sustantivos | NOUN/PROPN no en f_14 | Igual | *perro, mesa* |

## F. Pasivas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_17` ✳️ | Pasivas sin agente (perifrásticas y se-pasivas) | Pasivas sin complemento agente | Perifrásticas **y toda** se-pasiva/impersonal | Perifrásticas + pasiva refleja; **excluye la impersonal** (criterio: la refleja tiene sujeto paciente `nsubj`) | *fue redactado; se publicaron los informes* |
| `f_18` ⚠️ | Pasivas con agente (por) | Pasivas con complemento agente (por) | `por` + agente | ⚠️ **Bloqueada:** el modelo no emite `obl:agent`; agente y causa (`por incumplimiento`) son indistinguibles | *fue aprobado por el comité* |

## G. Formas estativas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_19` | *Ser/estar* copulativos | Usos copulativos de *ser* y *estar* | Nominal y adjetival | + preposicional (`es de madera`), adverbial (`está bien`), infinitivo, locativo (`está en Lima`) | *es profesor; está en Lima* |
| `f_20` | Haber impersonal existencial | Haber impersonal existencial | `hay/había/hubo…` | Igual; excluye `haber que` (→ f_53) | *hay un problema* |

## H. Subordinación

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_21` ✳️ | Completivas con *que* (CD de verbo) | Completivas con *que* dependientes de un verbo | *que* como CD de verbo (sobrecontaba: absorbía también los casos adjetivales de f_22) | + término de preposición (`insistió en que…`). **Corregido**: ahora exige que el predicado real ("abuelo" de *que*) sea VERBO/AUX, no ADJETIVO — deja de solaparse con f_22 | *cree que…, insistió en que…* |
| `f_22` ✳️ | Completivas con *que* (de adjetivo) | Completivas con *que* dependientes de un predicado adjetival | ADJ + que (regla nunca disparaba: 0% recall) | **Corregido**: se busca el adjetivo en el "abuelo" de *que* (el head del verbo subordinado), no en el head inmediato de *que* (que siempre es el verbo). f_21 se corrigió en espejo para dejar de absorber estos casos | *es probable que llueva; estoy seguro de que ganaremos* |
| `f_23` ⚠️ | Interrogativas indirectas | Interrogativas indirectas | Palabra interrogativa subordinada | Igual. ⚠️ las **relativas libres** (`lo que dijo`) NO se incluyen (riesgo con f_29-f_34) | *no sé quién vino* |
| `f_24` | Infinitivos (complemento/perífrasis) | Infinitivos (complemento verbal, perífrasis, final) | Complemento verbal, perífrasis | Sin cambio final: la revisión probó excluir `csubj` (exclusión mutua con f_15), pero al revertir f_15 esa exclusión también se deshizo | *quiere estudiar* |
| `f_25` | Gerundio adverbial/predicativo | Gerundio en función adverbial | Adverbial o predicativo | Solo adjunto (`advcl/ccomp`); excluye predicativo y perifrástico | *llegando tarde, salió* |
| `f_26` ⚠️ | Participio absoluto/adverbial | Participio absoluto o adverbial | `advcl/acl` no pasivo | Igual. ⚠️ detección variable | *terminado el examen…* |
| `f_27` | Participio postnominal | Participio postnominal (relativa reducida) | Reduce relativa de objeto | Igual; admite inacusativos | *el artículo publicado ayer* |
| `f_28` | *(0)* Gerundio postnominal | *(igual, 0)* | Agramatical | Redacción: "carece de mecanismo productivo" | — |
| `f_29` ✳️ | Relativas *que* sujeto (absorbe f_31) | Relativas con *que* en función de sujeto | *que* sujeto **+ absorbe f_31** | Solo *que* (quien/cual salen a f_31). ⚠️ pro-drop: el objeto sin sujeto explícito se imputa aquí | *el libro que está…* |
| `f_30` ✳️ | Relativas *que* objeto (absorbe f_32) | Relativas con *que* en función de CD | *quien/cual* oblicuo **+ absorbe f_32** | Solo *que* objeto (con sujeto explícito en la relativa) | *el libro que María escribió* |
| `f_31` ✳️ | *(0)* Integrado en f_29 | Relativas con *quien*/*el cual* (sujeto) | Siempre 0 | **Deja de ser 0**: quien/cual sujeto (des-fusión) | *la autora, quien presentó…* |
| `f_32` ✳️ | *(0)* Integrado en f_30 | Relativas con *quien*/*el cual* (CD) | Siempre 0 | Des-fusión, pero ⚠️ casi siempre 0 (el modelo taguea `cual` objeto como `nsubj`) | *el cual revisamos* |
| `f_33` | Pied-piping | Relativas con preposición antepuesta al relativo | prep + relativo | Igual; `a quien` cuenta aquí por precedencia | *por el cual, con quien* |
| `f_34` | Relativas oracionales | Relativas con antecedente oracional | `lo que/lo cual` proposicional | Igual | *…, lo cual molestó* |
| `f_35` | Causales *porque* | Cláusulas causales con *porque* | *porque* | Igual | *porque llovía* |
| `f_36` | Concesivas *aunque* | Cláusulas concesivas con *aunque* | *aunque* | Igual | *aunque llovía* |
| `f_37` ✳️ | Condicionales *si* | Cláusulas condicionales (*si*, *a menos que*, *salvo que*) | Solo *si* | + `a menos que`/`salvo que` (multipalabra); excluye el *si* interrogativo | *si llueve; a menos que pare* |
| `f_38` ✳️ | Otros subordinadores adverbiales | Otros subordinadores adverbiales | Incluía `a menos que` | Igual, **pero** `a menos que` sale a f_37 | *cuando, mientras, según* |

## I. Sintagmas preposicionales, adjetivos y adverbios

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_39` ✳️ | Preposiciones | Preposiciones | Todos los ADP | Igual, **pero** las locuciones (`a causa de`) cuentan **una vez** | *de, en, a causa de* |
| `f_40` | Adjetivo modificador nominal | Adjetivos en función de modificador nominal | ADJ `amod` | Igual | *libro interesante* |
| `f_41` | Adjetivos predicativos | Adjetivos en función de atributo | Atributo copulativo | Igual; excluye predicativos (`llegó cansada`) | *es positivo, está cansada* |
| `f_42` ✳️ | Adverbios residuales | Total de adverbios | Excluía f_04/f_05/f_46-f_50/f_67 | **Todos** los ADV (solapamientos deliberados) | *rápidamente, muy, hoy, no* |

## J. Especificidad léxica

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_43` | Diversidad léxica | Diversidad léxica | Medida `measure` | Igual (por defecto MATTR, no TTR) | — |
| `f_44` | Longitud media de palabra | Longitud gráfica media de las palabras | Caracteres por token | Igual | — |

## K. Clases léxicas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_45` | Conectores textuales | Conectores textuales | Diccionario | Igual | *sin embargo, por tanto* |
| `f_46` ✳️ | Atenuadores (incluía *casi*) | Atenuadores de grado y aproximativos | `casi, apenas, ligeramente…` | **`casi` sale** (pasa a f_47) | *apenas, un poco* |
| `f_47` ✳️ | Modalizadores epistémicos | Expresiones de aproximación, imprecisión o reserva | `quizás, tal vez…` | + **`casi`** (por coherencia con *almost*) | *quizás, tal vez, casi* |
| `f_48` | Amplificadores | Intensificadores de grado | Diccionario | Igual | *muy, totalmente* |
| `f_49` | Expresiones enfáticas | Expresiones de énfasis o refuerzo asertivo | Diccionario | Igual | *de hecho, sin duda* |
| `f_50` ⚠️ | Partículas discursivas | Partículas discursivas conversacionales | `bueno, pues, claro…` | Igual. ⚠️ **sin filtro posicional fiable** (el modelo no marca `discourse`: `Bueno`→ADJ, `Mira`→PROPN) | *bueno, pues, claro* |
| `f_51` | Determinantes demostrativos | Determinantes demostrativos | `este/ese/aquel` + N | Igual | *este libro* |

## L. Modales (perífrasis)

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_52` ✳️ | Posibilidad: *poder* + inf | Usos modales de posibilidad | `poder` + infinitivo | + **`puede que`** + subjuntivo | *puede llover; puede que llueva* |
| `f_53` | Necesidad/obligación | Perífrasis de necesidad/obligación | `deber/tener que/haber que` + inf | Igual (+ `haber de`, `deber de`) | *debe terminar, hay que actuar* |
| `f_54` | Predicción (futuro/condicional/ir a) | Futuro, condicional e *ir a* + infinitivo | Futuro sintético, condicional, `ir a` | Igual; excluye `ir a` de desplazamiento | *llegará; va a llover* |

## M. Verbos especializados

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_55` ⚠️ | Verbos de comunicación pública | Verbos de lengua o comunicación | `afirmar, declarar…` | Igual. ⚠️ desambiguación por modo (decir+subj→f_57) NO implementada (`decir` fuera del inventario; `sostuvo`→ADJ) | *afirmar, señalar* |
| `f_56` ⚠️ | Verbos cognitivos | Verbos de pensamiento | `creer, pensar, saber…` | Igual. ⚠️ `esperar` cuenta siempre (no se separa 'aguardar') | *creer, suponer* |
| `f_57` | Verbos suasivos | Verbos de influencia | `recomendar, pedir…` | Igual (ver límite de f_55) | *recomendar, sugerir* |
| `f_58` ✳️ | Verbos de apariencia (*parecer, resultar*) | Verbos de apariencia/inferencia | `parecer, resultar` (todos) | `parecer` siempre; **`resultar` solo copulativo** (`resulta adecuada`); excluye `resultó ganador`/`resultó de` | *parece difícil; resulta adecuada* |

## N. Formas reducidas

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_59` | *(0)* Contracciones | *(igual, 0)* | "al/del no son contracciones" | **Corregido:** al/del **sí** lo son; lo ausente es el sistema de contracciones verbales/negativas (`can't`) | — |
| `f_60` | *(0)* Omisión de *that* | *(igual, 0)* | "omisión agramatical" | **Corregido:** existe marginalmente (`espero sepan…`); 0 por decisión de diseño | — |
| `f_61` | *(0)* Preposición pospuesta | *(igual, 0)* | "no admite" | "carece de mecanismo productivo equivalente" | — |
| `f_62` | *(0)* Infinitivo escindido | *(igual, 0)* | "imposible" | "carece de equivalente formal" (afijo `-r`) | — |
| `f_63` ✳️ | Auxiliar escindido | Adverbio interpuesto entre auxiliar y verbo | ADV entre auxiliar y verbo | + **perífrasis modal** (`podría fácilmente resolver`); excluye sujetos interpuestos | *ha siempre sostenido* |

## O. Coordinación

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_64` ✳️ | Coordinación de sintagmas (N, ADJ) | Coordinación de palabras o grupos no oracionales | N/ADJ | + **verbos** (SV compartido, `lee y escribe`) y adverbios | *la autora y el editor; lee y escribe* |
| `f_65` | Coordinación de cláusulas | Coordinación de cláusulas independientes | `y, pero, sino, ni` | Igual (incl. `o`); depende del parseo | *llegó tarde y se fue* |

## P. Negación

| Código | Nombre antes | Nombre nuevo | Descripción antes | Descripción nueva | Ejemplo |
|---|---|---|---|---|---|
| `f_66` | Negación sintética | Negación por palabras negativas distintas de *no* | `nadie, nunca, ninguno…` | Igual (ya incluye `ningún(o/a)` y `ni`) | *nadie, nunca, ni* |
| `f_67` ✳️ | Negación analítica (*no* preverbal) | Negación mediante el adverbio *no* | *no* antepuesto al verbo | + **`no` de foco** sobre otros constituyentes (`respuesta no definitiva`); excluye `no` respuesta/sustantivo | *no sabe; no definitiva* |

---

## Resumen de cambios de comportamiento (✳️)

Rasgos cuyo **conteo** cambió (no solo la etiqueta): **f_01, f_04, f_05, f_06,
f_07, f_08, f_11, f_17, f_21, f_22, f_29, f_30, f_31, f_32, f_37, f_38, f_39,
f_42, f_46, f_47, f_52, f_58, f_63, f_64, f_67**.

Rasgos que quedaron como estaban por **límite del modelo** (⚠️), documentado y no
forzado: **f_18** (obl:agent), **f_50** (sin señal discourse), **f_55/f_57**
(desambiguación por modo), **f_23** (relativas libres), **f_56** (esperar). Ver
`TABLA_RASGOS_ES.md` para el detalle.

**Rasgo revertido por decisión del usuario:** **f_15** se activó en la Fase 3
(infinitivo nominal-sujeto) pero se revirtió a su comportamiento original de
columna siempre-cero; f_24 recuperó en consecuencia su regla original (sin
excluir `csubj`).
