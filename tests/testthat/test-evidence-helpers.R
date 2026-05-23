# Tests para los helpers de evidencia (Phase 1).
#
# Verifica el contrato de make_block_result/as_block_result/extract_*/
# bind_evidence/count_feature_traced. Estos helpers seran consumidos por
# parse_biber_features() y los bloques migrados en Phase 2.

library(testthat)

empty_evidence <- pseudobibeR.es:::empty_evidence_tibble
make_block     <- pseudobibeR.es:::make_block_result
as_block       <- pseudobibeR.es:::as_block_result
is_block       <- pseudobibeR.es:::is_block_result
get_counts     <- pseudobibeR.es:::extract_counts
get_evidence   <- pseudobibeR.es:::extract_evidence
stack_evidence <- pseudobibeR.es:::bind_evidence
count_traced   <- pseudobibeR.es:::count_feature_traced
EV_COLS        <- pseudobibeR.es:::.EVIDENCE_COLS

# ----------------------------------------------------------------------
# empty_evidence_tibble()
# ----------------------------------------------------------------------

test_that("empty_evidence_tibble() tiene 0 filas y las 9 columnas canonicas", {
  ev <- empty_evidence()
  expect_s3_class(ev, "tbl_df")
  expect_equal(nrow(ev), 0L)
  expect_equal(colnames(ev), EV_COLS)
})

test_that("empty_evidence_tibble() tiene tipos coherentes para bind_rows()", {
  ev <- empty_evidence()
  expect_type(ev$doc_id,        "character")
  expect_type(ev$feature,       "character")
  expect_type(ev$sentence_id,   "integer")
  expect_type(ev$token_id,      "integer")
  expect_type(ev$token,         "character")
  expect_type(ev$lemma,         "character")
  expect_type(ev$upos,          "character")
  expect_type(ev$feats,         "character")
  expect_type(ev$head_token_id, "integer")
})

# ----------------------------------------------------------------------
# make_block_result()
# ----------------------------------------------------------------------

test_that("make_block_result() construye un block_result con clase y elementos correctos", {
  counts <- data.frame(doc_id = "d1", f_01_past_tense = 3L)
  br <- make_block(counts)
  expect_true(is_block(br))
  expect_s3_class(br, "block_result")
  expect_equal(br$counts, counts)
  expect_equal(nrow(br$evidence), 0L)
  expect_equal(colnames(br$evidence), EV_COLS)
})

test_that("make_block_result() rechaza counts no-dataframe", {
  expect_error(make_block(counts = list(a = 1L)), "'counts' debe ser un data.frame")
})

test_that("make_block_result() rechaza evidence con schema incompleto", {
  bad_ev <- tibble::tibble(doc_id = "d1", feature = "f_01_past_tense")
  expect_error(
    make_block(counts = data.frame(doc_id = "d1", f_01 = 1L), evidence = bad_ev),
    "schema canonico"
  )
})

# ----------------------------------------------------------------------
# as_block_result()
# ----------------------------------------------------------------------

test_that("as_block_result() envuelve data.frames antiguos sin cambios en counts", {
  df_old <- data.frame(doc_id = "d1", f_01_past_tense = 3L)
  br <- as_block(df_old)
  expect_true(is_block(br))
  expect_equal(br$counts, df_old)
  expect_equal(nrow(br$evidence), 0L)
})

test_that("as_block_result() es idempotente sobre block_results existentes", {
  ev <- tibble::tibble(
    doc_id = "d1", feature = "f_01_past_tense", sentence_id = 1L,
    token_id = 2L, token = "llego", lemma = "llegar", upos = "VERB",
    feats = "Tense=Past", head_token_id = 0L
  )
  br <- make_block(data.frame(doc_id = "d1", f_01_past_tense = 1L), ev)
  br2 <- as_block(br)
  expect_identical(br, br2)
})

test_that("as_block_result() rechaza tipos no esperados", {
  expect_error(as_block(42), "Tipo recibido")
  expect_error(as_block("string"), "Tipo recibido")
})

# ----------------------------------------------------------------------
# extract_counts() / extract_evidence()
# ----------------------------------------------------------------------

test_that("extract_counts() recupera el data.frame de cualquier formato", {
  df_old <- data.frame(doc_id = "d1", f_01 = 1L)
  br_new <- make_block(df_old)
  expect_equal(get_counts(df_old), df_old)
  expect_equal(get_counts(br_new), df_old)
})

test_that("extract_evidence() devuelve vacio para formato antiguo y el tibble real para nuevo", {
  df_old <- data.frame(doc_id = "d1", f_01 = 1L)
  ev <- tibble::tibble(
    doc_id = "d1", feature = "f_01_past_tense", sentence_id = 1L,
    token_id = 2L, token = "llego", lemma = "llegar", upos = "VERB",
    feats = "Tense=Past", head_token_id = 0L
  )
  br <- make_block(df_old, ev)
  expect_equal(nrow(get_evidence(df_old)), 0L)
  expect_equal(colnames(get_evidence(df_old)), EV_COLS)
  expect_equal(get_evidence(br), ev)
})

# ----------------------------------------------------------------------
# bind_evidence()
# ----------------------------------------------------------------------

test_that("bind_evidence() apila tibbles preservando schema", {
  ev1 <- tibble::tibble(
    doc_id = "d1", feature = "f_01_past_tense", sentence_id = 1L,
    token_id = 2L, token = "llego", lemma = "llegar", upos = "VERB",
    feats = "Tense=Past", head_token_id = 0L
  )
  ev2 <- tibble::tibble(
    doc_id = "d1", feature = "f_03_present_tense", sentence_id = 2L,
    token_id = 1L, token = "come", lemma = "comer", upos = "VERB",
    feats = "Tense=Pres", head_token_id = 0L
  )
  out <- stack_evidence(ev1, ev2)
  expect_equal(nrow(out), 2L)
  expect_equal(colnames(out), EV_COLS)
  expect_setequal(out$feature, c("f_01_past_tense", "f_03_present_tense"))
})

test_that("bind_evidence() sin argumentos devuelve el tibble vacio canonico", {
  out <- stack_evidence()
  expect_equal(nrow(out), 0L)
  expect_equal(colnames(out), EV_COLS)
})

test_that("bind_evidence() filtra NULLs silenciosamente", {
  ev1 <- tibble::tibble(
    doc_id = "d1", feature = "f_01_past_tense", sentence_id = 1L,
    token_id = 2L, token = "llego", lemma = "llegar", upos = "VERB",
    feats = "Tense=Past", head_token_id = 0L
  )
  out <- stack_evidence(ev1, NULL, NULL)
  expect_equal(nrow(out), 1L)
})

# ----------------------------------------------------------------------
# count_feature_traced()
# ----------------------------------------------------------------------

test_that("count_feature_traced() devuelve counts identicos a count_feature()", {
  tbl <- tibble::tibble(
    doc_id            = c("d1", "d1", "d2"),
    sentence_id       = c(1L, 1L, 1L),
    token_id_int      = c(2L, 5L, 3L),
    token             = c("llego", "salio", "comio"),
    lemma             = c("llegar", "salir", "comer"),
    pos               = c("VERB", "VERB", "VERB"),
    feats             = c("Tense=Past|Mood=Ind", "Tense=Past|Mood=Ind", "Tense=Past|Mood=Ind"),
    head_token_id_int = c(0L, 0L, 0L)
  )
  br <- count_traced(tbl, "f_01_past_tense")
  expect_true(is_block(br))
  expect_equal(br$counts$doc_id, c("d1", "d2"))
  expect_equal(br$counts$f_01_past_tense, c(2L, 1L))
})

test_that("count_feature_traced() genera evidence con una fila por token superviviente al dedup", {
  tbl <- tibble::tibble(
    doc_id            = c("d1", "d1", "d2"),
    sentence_id       = c(1L, 1L, 1L),
    token_id_int      = c(2L, 5L, 3L),
    token             = c("llego", "salio", "comio"),
    lemma             = c("llegar", "salir", "comer"),
    pos               = c("VERB", "VERB", "VERB"),
    feats             = c("Tense=Past", "Tense=Past", "Tense=Past"),
    head_token_id_int = c(0L, 0L, 0L)
  )
  br <- count_traced(tbl, "f_01_past_tense")
  expect_equal(nrow(br$evidence), 3L)
  expect_equal(colnames(br$evidence), EV_COLS)
  expect_true(all(br$evidence$feature == "f_01_past_tense"))
  expect_setequal(br$evidence$token, c("llego", "salio", "comio"))
})

test_that("count_feature_traced() satisface count == nrow(evidence) (invariante critica)", {
  tbl <- tibble::tibble(
    doc_id            = c("d1", "d1", "d1", "d2", "d2"),
    sentence_id       = c(1L, 1L, 2L, 1L, 1L),
    token_id_int      = c(2L, 5L, 3L, 1L, 4L),
    token             = c("a", "b", "c", "d", "e"),
    lemma             = c("a", "b", "c", "d", "e"),
    pos               = c("VERB", "VERB", "VERB", "VERB", "VERB"),
    feats             = "Tense=Past",
    head_token_id_int = 0L
  )
  br <- count_traced(tbl, "f_01_past_tense")
  for (doc in br$counts$doc_id) {
    expected <- br$counts[br$counts$doc_id == doc, "f_01_past_tense", drop = TRUE]
    observed <- sum(br$evidence$doc_id == doc & br$evidence$feature == "f_01_past_tense")
    expect_equal(observed, expected, info = paste0("doc=", doc))
  }
})

test_that("count_feature_traced() maneja tbl vacio sin error", {
  tbl <- tibble::tibble(
    doc_id            = character(0),
    sentence_id       = integer(0),
    token_id_int      = integer(0),
    token             = character(0),
    lemma             = character(0),
    pos               = character(0),
    feats             = character(0),
    head_token_id_int = integer(0)
  )
  br <- count_traced(tbl, "f_01_past_tense")
  expect_equal(nrow(br$counts), 0L)
  expect_equal(nrow(br$evidence), 0L)
  expect_equal(colnames(br$evidence), EV_COLS)
})
