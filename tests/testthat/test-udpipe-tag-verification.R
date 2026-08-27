# test-udpipe-tag-verification.R
#
# Gate empírico del §1.2 de INSTRUCCIONES_CLAUDE_CODE_pseudobiber_es.md:
# "Verifica empíricamente las etiquetas de UD antes de escribir cualquier
#  condición." Este archivo ANOTA oraciones diagnósticas con el modelo real
# spanish-gsd y ASERTA las etiquetas de las que dependen las reglas de
# extracción. Si una futura versión del modelo cambia una etiqueta, o si
# alguien asume una etiqueta que el modelo no produce, estos tests fallan.
#
# También DOCUMENTA (vía comentarios y expectativas) los límites conocidos
# del modelo que obligaron a desviarse de la revisión lingüística de Hernán:
#   - `se` reflexivo argumental vs. pasiva refleja: INDISTINGUIBLES.
#   - agente de pasiva (`por`) vs. causa (`por`): INDISTINGUIBLES (ambos `obl`).
#   - relativo `que`: SCONJ/mark, sin PronType=Rel (no separable sujeto/objeto).
#   - Tense=Pqp: el modelo no lo emite (pluscuamperfecto sale como Tense=Imp).
#
# nolint start: line_length_linter, object_name_linter

# Reutiliza el modelo memoizado de helper-feature-coverage.R (.fc_ud), que
# hace skip() si el modelo no está disponible. Así no se depende de helpers en
# subdirectorios que testthat no auto-carga.
annotate_es <- function(text) {
  ud <- .fc_ud()
  as.data.frame(udpipe::udpipe_annotate(ud, x = text, doc_id = "d1"))
}

feat_of <- function(df, token_lc) {
  row <- df[tolower(df$token) == token_lc, , drop = FALSE]
  if (nrow(row) == 0) return(NA_character_)
  row$feats[[1]]
}

has_feat <- function(feats, key_eq) {
  !is.na(feats) && grepl(key_eq, feats, fixed = TRUE)
}

get_model_or_skip <- function() {
  skip_if_not_installed("udpipe")
  invisible(NULL)
}

test_that("posesivos: DET con Poss=Yes y Person (base de f_06/f_07/f_08)", {
  mp <- get_model_or_skip()
  df <- annotate_es("Nuestro equipo revisó mi propuesta y tu informe y su plan.")

  # mi/tu/su/nuestro son DET, Poss=Yes, con Person morfológico fiable.
  expect_true(has_feat(feat_of(df, "nuestro"), "Poss=Yes"))
  expect_true(has_feat(feat_of(df, "nuestro"), "Person=1"))
  expect_true(has_feat(feat_of(df, "mi"),  "Person=1"))
  expect_true(has_feat(feat_of(df, "tu"),  "Person=2"))
  expect_true(has_feat(feat_of(df, "su"),  "Person=3"))
})

test_that("f_01: pluscuamperfecto e imperfecto de subjuntivo salen como Tense=Imp, no Pqp", {
  mp <- get_model_or_skip()
  df <- annotate_es("Ya había corrido y ojalá hubiera corrido; quería que corriera.")

  # 'había' (pluscuamperfecto ind.) y 'hubiera'/'corriera' (subjuntivo) → Tense=Imp.
  expect_true(has_feat(feat_of(df, "había"),   "Tense=Imp"))
  expect_true(has_feat(feat_of(df, "hubiera"), "Tense=Imp"))
  expect_true(has_feat(feat_of(df, "corriera"),"Tense=Imp"))
  # El modelo NO emite Tense=Pqp: la rama Pqp de la regla de f_01 es inofensiva
  # pero prácticamente rama muerta con spanish-gsd.
  expect_false(any(grepl("Tense=Pqp", df$feats, fixed = TRUE), na.rm = TRUE))
})

test_that("LIMITACIÓN: los usos de 'se' son morfológicamente indistinguibles", {
  mp <- get_model_or_skip()
  refl  <- annotate_es("Ella se vio en el espejo.")
  prefl <- annotate_es("Se publicaron los informes.")

  se_refl  <- refl[tolower(refl$token) == "se", , drop = FALSE]
  se_prefl <- prefl[tolower(prefl$token) == "se", , drop = FALSE]

  # Reflexivo argumental y pasiva refleja: MISMA etiqueta (Reflex=Yes, iobj).
  # Por eso f_06-f_08 excluyen todo 'se' Reflex=Yes (documentado): contar el
  # reflexivo argumental exigiría separar la pasiva refleja, y no hay señal.
  expect_true(has_feat(se_refl$feats[[1]],  "Reflex=Yes"))
  expect_true(has_feat(se_prefl$feats[[1]], "Reflex=Yes"))
  expect_identical(se_refl$dep_rel[[1]], se_prefl$dep_rel[[1]])
})

test_that("LIMITACIÓN: agente y causa con 'por' son indistinguibles (f_18)", {
  mp <- get_model_or_skip()
  ag <- annotate_es("El informe fue aprobado por el comité.")
  ca <- annotate_es("El informe fue sancionado por incumplimiento.")

  # spanish-gsd NO usa obl:agent; agente y causa salen ambos como 'obl'.
  expect_false(any(ag$dep_rel == "obl:agent"))
  expect_true(any(ag$dep_rel == "obl"))
  expect_true(any(ca$dep_rel == "obl"))
})

test_that("relativas: 'que' es SCONJ/mark; 'quien' es PronType=Rel (f_29-f_34)", {
  mp <- get_model_or_skip()
  q  <- annotate_es("Leí el libro que está en la mesa.")
  wh <- annotate_es("La autora, quien presentó el proyecto, respondió.")

  que_row <- q[tolower(q$token) == "que", , drop = FALSE]
  expect_identical(que_row$upos[[1]], "SCONJ")
  expect_identical(que_row$dep_rel[[1]], "mark")
  expect_false(has_feat(que_row$feats[[1]], "PronType=Rel"))
  expect_true(any(q$dep_rel == "acl:relcl"))

  quien_row <- wh[tolower(wh$token) == "quien", , drop = FALSE]
  expect_true(grepl("PronType=Int,Rel|PronType=Rel", quien_row$feats[[1]]))
})

test_that("f_57 vs f_55: subjuntivo vs condicional en la subordinada son separables", {
  mp <- get_model_or_skip()
  sub <- annotate_es("Me pidió que volviera pronto.")
  cnd <- annotate_es("Me dijo que volvería pronto.")

  expect_true(has_feat(feat_of(sub, "volviera"), "Mood=Sub"))
  expect_true(has_feat(feat_of(cnd, "volvería"), "Mood=Cnd"))
})

# nolint end
