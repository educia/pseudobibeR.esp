# Tests for lexical disambiguation functions

test_that("donc disambiguation works correctly", {
  skip_if_not_installed("udpipe")
  skip_on_cran()
  
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  
  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }
  
  # Test 1: donc as conjunct (mid-sentence after comma)
  text_conjunct <- "Il pleut, donc nous restons dedans."
  ann_conjunct <- udpipe::udpipe_annotate(model, x = text_conjunct, doc_id = "test_conjunct")
  result_conjunct <- biber(ann_conjunct, normalize = FALSE)
  
  expect_equal(result_conjunct$f_45_conjuncts, 1, 
               info = "donc after comma should be classified as conjunct")
  expect_equal(result_conjunct$f_50_discourse_particles, 0,
               info = "donc after comma should NOT be discourse particle")
  
  # Test 2: donc as discourse particle (sentence-initial)
  text_discourse <- "Donc, que faisons-nous maintenant?"
  ann_discourse <- udpipe::udpipe_annotate(model, x = text_discourse, doc_id = "test_discourse")
  result_discourse <- biber(ann_discourse, normalize = FALSE)
  
  expect_equal(result_discourse$f_45_conjuncts, 0,
               info = "Sentence-initial donc should NOT be conjunct")
  expect_equal(result_discourse$f_50_discourse_particles, 1,
               info = "Sentence-initial donc should be discourse particle")
  
  # Test 3: donc in question (discourse particle)
  text_question <- "Pourquoi donc es-tu là?"
  ann_question <- udpipe::udpipe_annotate(model, x = text_question, doc_id = "test_question")
  result_question <- biber(ann_question, normalize = FALSE)
  
  expect_equal(result_question$f_45_conjuncts, 0,
               info = "donc in question should NOT be conjunct")
  expect_equal(result_question$f_50_discourse_particles, 1,
               info = "donc in question should be discourse particle")
})

test_that("ensuite disambiguation works correctly", {
  skip_if_not_installed("udpipe")
  skip_on_cran()
  
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  
  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }
  
  # Test 1: ensuite as time adverbial (default usage)
  text_time <- "Ensuite nous irons au parc."
  ann_time <- udpipe::udpipe_annotate(model, x = text_time, doc_id = "test_time")
  result_time <- biber(ann_time, normalize = FALSE)
  
  expect_equal(result_time$f_05_time_adverbials, 1,
               info = "Sentence-initial ensuite should be time adverbial")
  expect_equal(result_time$f_45_conjuncts, 0,
               info = "Sentence-initial ensuite should NOT be conjunct")
  
  # Test 2: ensuite as conjunct (after comma)
  text_conjunct <- "Il a plu, ensuite il a neigé."
  ann_conjunct <- udpipe::udpipe_annotate(model, x = text_conjunct, doc_id = "test_conjunct")
  result_conjunct <- biber(ann_conjunct, normalize = FALSE)
  
  expect_equal(result_conjunct$f_05_time_adverbials, 0,
               info = "Post-comma ensuite should NOT be time adverbial")
  expect_equal(result_conjunct$f_45_conjuncts, 1,
               info = "Post-comma ensuite should be conjunct")
})

test_that("vraiment disambiguation works correctly", {
  skip_if_not_installed("udpipe")
  skip_on_cran()
  
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  
  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }
  
  # Test 1: vraiment as amplifier (modifies adjective)
  text_amplifier <- "C'est vraiment grand."
  ann_amplifier <- udpipe::udpipe_annotate(model, x = text_amplifier, doc_id = "test_amplifier")
  result_amplifier <- biber(ann_amplifier, normalize = FALSE)
  
  expect_equal(result_amplifier$f_48_amplifiers, 1,
               info = "vraiment before adjective should be amplifier")
  expect_equal(result_amplifier$f_49_emphatics, 0,
               info = "vraiment before adjective should NOT be emphatic")
  
  # Test 2: vraiment as emphatic (standalone)
  text_emphatic <- "Je l'ai vraiment fait hier."
  ann_emphatic <- udpipe::udpipe_annotate(model, x = text_emphatic, doc_id = "test_emphatic")
  result_emphatic <- biber(ann_emphatic, normalize = FALSE)
  
  expect_equal(result_emphatic$f_48_amplifiers, 0,
               info = "Standalone vraiment should NOT be amplifier")
  expect_equal(result_emphatic$f_49_emphatics, 1,
               info = "Standalone vraiment should be emphatic")
})

test_that("disambiguation prevents double-counting", {
  skip_if_not_installed("udpipe")
  skip_on_cran()
  
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  
  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }
  
  # Test with donc (appears twice: once as conjunct, once as discourse)
  text_donc <- "Donc, il pleut. Il fait froid, donc nous restons dedans."
  ann_donc <- udpipe::udpipe_annotate(model, x = text_donc, doc_id = "test_donc")
  result_donc <- biber(ann_donc, normalize = FALSE)
  
  donc_total <- result_donc$f_45_conjuncts + result_donc$f_50_discourse_particles
  expect_equal(donc_total, 2, 
               label = "donc should be counted exactly once in either f_45 or f_50, total 2 instances")
  
  # Test with ensuite (appears once as time adverbial)
  text_ensuite <- "Ensuite nous irons au parc."
  ann_ensuite <- udpipe::udpipe_annotate(model, x = text_ensuite, doc_id = "test_ensuite")
  result_ensuite <- biber(ann_ensuite, normalize = FALSE)
  
  ensuite_total <- result_ensuite$f_05_time_adverbials + result_ensuite$f_45_conjuncts
  expect_equal(ensuite_total, 1,
               label = "ensuite should be counted exactly once in either f_05 or f_45")
  
  # Test with vraiment (appears once as amplifier)
  text_vraiment <- "C'est vraiment grand."
  ann_vraiment <- udpipe::udpipe_annotate(model, x = text_vraiment, doc_id = "test_vraiment")
  result_vraiment <- biber(ann_vraiment, normalize = FALSE)
  
  vraiment_total <- result_vraiment$f_48_amplifiers + result_vraiment$f_49_emphatics
  expect_equal(vraiment_total, 1,
               label = "vraiment should be counted exactly once in either f_48 or f_49")
})
