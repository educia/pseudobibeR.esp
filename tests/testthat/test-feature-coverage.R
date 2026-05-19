# Auditoría Fase 2 — cobertura positiva/negativa de los 57 rasgos ES.
# Casos derivados de biber_espanol_completo.md (fuente de verdad).
# pos_text → debe detectar (>= pos_min). neg_text → NO debe detectar (<= neg_max).
# neg_text = NA → rasgo residual/métrico sin negativo limpio (solo sanity).

fc_cases <- list(
  list("f_01_past_tense",          "Juan llegó tarde y habló con ella.",            1, "Juan hablaba mientras ella leía.",            0),
  list("f_02_perfect_aspect",      "He terminado el informe esta mañana.",          1, "Terminé el informe ayer.",                    0),
  list("f_03_present_tense",       "María trabaja en la oficina central.",          1, "María trabajó ayer en casa.",                 0),
  list("f_04_place_adverbials",    "El perro está aquí y el gato allí afuera.",     1, "Hoy comimos muy temprano.",                   0),
  list("f_05_time_adverbials",     "Llegó ayer y se marchará mañana temprano.",     1, "El libro está aquí dentro.",                  0),
  list("f_06_first_person_pronouns","Yo creo que nosotros ganaremos.",              1, "Él dijo que ella vendría.",                   0),
  list("f_07_second_person_pronouns","Tú sabes que usted tiene razón.",             1, "Yo sé que él viene.",                         0),
  list("f_08_third_person_pronouns","Él la vio y ella le habló a ellos.",           1, "Yo te di el libro a ti.",                     0),
  list("f_10_demonstrative_pronoun","Eso es importante. Esto me gusta mucho.",      1, "Este libro es mío y esa casa es tuya.",       0),
  list("f_11_indefinite_pronouns", "Alguien llamó pero nadie respondió nada.",      1, "Un perro y una casa grande.",                 0),
  list("f_13_wh_question",         "¿Qué quieres? ¿Dónde estás ahora?",             1, "La casa que compré es grande.",               0),
  list("f_14_nominalizations",     "La producción y la evaluación del conocimiento.",1,"El lado del grado del partido.",              0),
  list("f_16_other_nouns",         "El perro corre en el parque con la pelota.",    2, NA,                                            NA),
  list("f_17_agentless_passives",  "Se publicaron los resultados. La tarea fue realizada.",1,"Juan comió la manzana roja.",          0),
  list("f_18_by_passives",         "El libro fue escrito por María González.",      1, "La tarea fue realizada rápido.",              0),
  list("f_19_be_main_verb",        "El libro es interesante y la casa está limpia.",1, "El informe ha sido escrito hoy.",             0),
  list("f_20_existential_there",   "Hay un problema grave en el sistema.",          1, "Hay que esperar el resultado final.",         0),
  list("f_21_that_verb_comp",      "Dijo que vendría mañana sin falta.",            1, "La casa que compré es vieja.",                0),
  list("f_22_that_adj_comp",       "Es importante que vengas pronto.",              1, "Corrió porque tenía prisa.",                  0),
  list("f_23_wh_clause",           "No sé qué quieres ni dónde vives.",             1, "¿Qué quieres exactamente?",                   0),
  list("f_24_infinitives",         "Quiero comer y necesito dormir ahora.",         1, "Como pan todos los días.",                    0),
  list("f_25_present_participle",  "Caminando por la calle, pensó en todo.",        1, "Está hablando por teléfono ahora.",           0),
  list("f_26_past_participle",     "Terminada la reunión, todos se fueron.",        1, "Ha terminado la reunión temprano.",           0),
  list("f_27_past_participle_whiz","Los métodos utilizados fueron buenos.",         1, "Terminada la sesión, salieron.",              0),
  list("f_29_that_subj",           "El hombre que vino ayer era alto.",             1, "Dijo que vino tarde.",                        0),
  list("f_30_that_obj",            "La casa que compré es muy grande.",             1, "Es importante que vengas.",                   0),
  list("f_33_pied_piping",         "La casa en la que vivo. La persona con quien hablé. El motivo por el cual vino. Los temas de los cuales hablamos.",2,"El libro que leí ayer.",0),
  list("f_34_sentence_relatives",  "Llegó tarde, lo cual molestó a todos.",         1, "Lo que quieras está bien.",                   0),
  list("f_35_because",             "No vino porque estaba enfermo.",                1, "Aunque llovía, salió igual.",                 0),
  list("f_36_though",              "Aunque llovía, salió a la calle.",              1, "Porque llovía, no salió.",                    0),
  list("f_37_if",                  "Si llueve mañana, no salgo.",                   1, "Preguntó si vendría a la fiesta.",            0),
  list("f_38_other_adv_sub",       "Mientras trabajaba, escuchaba música.",         1, "Comió pan y bebió agua.",                     0),
  list("f_39_prepositions",        "El libro de María está en la mesa con cuidado.",3, NA,                                           NA),
  list("f_40_adj_attr",            "Un libro interesante y una casa grande.",       1, "El libro es interesante.",                    0),
  list("f_41_adj_pred",            "El libro es interesante y útil.",               1, "Un libro interesante llegó.",                 0),
  list("f_42_adverbs",             "Habló rápidamente y actuó claramente.",         1, NA,                                           NA),
  list("f_43_type_token",          "El gato y el perro corren juntos felices.",     0, NA,                                           NA),
  list("f_44_mean_word_length",    "La producción de conocimiento riguroso.",       1, NA,                                           NA),
  list("f_45_conjuncts",           "Llegó tarde. Sin embargo, lo intentó. Además ganó.",1,"Salió sin nada en las manos.",            0),
  list("f_46_downtoners",          "Es casi imposible y apenas visible.",           1, "Es muy claro y totalmente cierto.",           0),
  list("f_47_hedges",              "Quizás venga. A lo mejor llega mañana.",        1, "Vendrá seguro y sin dudas.",                  0),
  list("f_48_amplifiers",          "Es muy bueno y totalmente claro.",              1, "Es casi bueno y apenas claro.",               0),
  list("f_49_emphatics",           "Realmente lo hizo. De verdad que sí.",          1, "Lo hizo ayer por la tarde.",                  0),
  list("f_50_discourse_particles", "Bueno, o sea, es decir, llegó tarde.",          2, "El alumno bueno estudió mucho.",              0),
  list("f_51_demonstratives",      "Este libro y esa casa son nuevos.",             1, "Eso es mío y esto es tuyo.",                  0),
  list("f_52_modal_possibility",   "Puedo ir y podríamos hablar luego.",            1, "Tiene mucho poder político.",                 0),
  list("f_53_modal_necessity",     "Hay que esperar. Tengo que irme. Debe salir.",  2, "Tengo un perro y un gato.",                   0),
  list("f_54_modal_predictive",    "Hablará mañana. Vamos a comer pronto.",         1, "Habló ayer y comió tarde.",                   0),
  list("f_55_verb_public",         "Dijo que sí y afirmó lo contrario.",            1, "Comió pan y durmió bien.",                    0),
  list("f_56_verb_private",        "Creo que sé la respuesta y pienso mucho.",      1, "Corrió rápido y saltó alto.",                 0),
  list("f_57_verb_suasive",        "Le pedí que viniera y recomendé esperar.",      1, "Comió pan y bebió agua.",                     0),
  list("f_58_verb_seem",           "Parece que va a llover pronto.",                1, "Apareció en la reunión tarde.",               0),
  list("f_63_split_auxiliary",     "Ha probablemente sido analizado el caso.",      1, "Fue mostrado el resultado.",                  0),
  list("f_64_phrasal_coordination","Juan y María vinieron temprano.",               1, "Vino pero se fue rápido.",                    0),
  list("f_65_clausal_coordination","Llegó tarde. Y luego se fue.",                  1, "Juan y María vinieron.",                      0),
  list("f_66_neg_synthetic",       "Nadie vino y nada pasó. Nunca lo hizo.",        2, "Vino alguien con algo.",                      0),
  list("f_67_neg_analytic",        "No vino y no lo hizo nunca.",                   1, "No obstante, finalmente llegó.",              0)
)

for (cs in fc_cases) {
  feat <- cs[[1]]; pos_t <- cs[[2]]; pos_min <- cs[[3]]; neg_t <- cs[[4]]; neg_max <- cs[[5]]

  test_that(paste0(feat, " — cobertura positiva"), {
    r <- run_biber(pos_t)
    expect_true(feat %in% names(r), info = paste(feat, "ausente del output"))
    val <- as.numeric(r[[feat]][1])
    if (feat == "f_43_type_token") {
      expect_gt(val, 0); expect_lte(val, 1)
    } else if (feat == "f_44_mean_word_length") {
      expect_gt(val, 3); expect_lt(val, 15)
    } else {
      expect_gte(val, pos_min)
    }
  })

  if (!is.na(neg_t)) {
    test_that(paste0(feat, " — cobertura negativa"), {
      r <- run_biber(neg_t)
      val <- as.numeric(r[[feat]][1])
      expect_lte(val, neg_max)
    })
  }
}
