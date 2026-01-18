# Adjective, preposition, adverb, verb class, and modal features

#' Extract adjective, preposition, and adverb features
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param dict_lookup Dictionary lookup
#' @param word_lists_lookup Word lists lookup
#' @param negation_adverbs Vector of negation adverbs
#' @return Data frame with f_39_prepositions through f_42_adverbs
#' @keywords internal
block_adj_prep_adv_fr <- function(
    tokens,
    doc_ids,
    dict_lookup,
    word_lists_lookup,
    negation_adverbs) {
  f39 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADP",
      dplyr::coalesce(.data$dep_rel, "") %in% c("case", "fixed")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_39_prepositions = "n")

  f40 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      (
        dplyr::lead(.data$pos == "NOUN") |
          dplyr::lead(.data$pos == "ADJ") |
          (
            dplyr::lead(.data$token == ",") &
              dplyr::lead(.data$pos == "ADJ", 2)
          )
      )
    ) %>%
    dplyr::filter(stringr::str_detect(.data$token, "-") == FALSE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_40_adj_attr = "n")

  linking_verbs <- get_word_list(word_lists_lookup, "linking_matchlist")

  f41 <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::lag(.data$pos == "VERB" | .data$pos == "AUX"),
      dplyr::lag(.data$lemma %in% linking_verbs),
      dplyr::lead(.data$pos != "NOUN"),
      dplyr::lead(.data$pos != "ADJ"),
      dplyr::lead(.data$pos != "ADV")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_41_adj_pred = "n")

  adverb_exclusions <- unique(c(
    dictionary_to_lemmas(dict_lookup, "f_46_downtoners"),
    dictionary_to_lemmas(dict_lookup, "f_47_hedges"),
    dictionary_to_lemmas(dict_lookup, "f_48_amplifiers"),
    dictionary_to_lemmas(dict_lookup, "f_49_emphatics"),
    dictionary_to_lemmas(dict_lookup, "f_50_discourse_particles"),
    negation_adverbs
  ))

  f42 <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV",
      !.data$lemma %in% adverb_exclusions
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_42_adverbs = "n")

  doc_ids %>%
    dplyr::left_join(f39, by = "doc_id") %>%
    dplyr::left_join(f40, by = "doc_id") %>%
    dplyr::left_join(f41, by = "doc_id") %>%
    dplyr::left_join(f42, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

#' Extract specialized verb class features
#'
#' @param tokens Annotated token data frame
#' @param doc_ids Document IDs
#' @param dict_lookup Dictionary lookup
#' @return Data frame with f_55_verb_public through f_58_verb_seem
#' @keywords internal
block_specialized_verbs_fr <- function(tokens, doc_ids, dict_lookup) {
  verb_public_lemmas <- dictionary_to_lemmas(dict_lookup, "f_55_verb_public")
  verb_private_lemmas <- dictionary_to_lemmas(dict_lookup, "f_56_verb_private")
  verb_suasive_lemmas <- dictionary_to_lemmas(dict_lookup, "f_57_verb_suasive")
  verb_seem_lemmas <- dictionary_to_lemmas(dict_lookup, "f_58_verb_seem")

  f55 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_public_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_55_verb_public = "n")

  f56 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_private_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_56_verb_private = "n")

  f57 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_suasive_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_57_verb_suasive = "n")

  f58 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_seem_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_58_verb_seem = "n")

  doc_ids %>%
    dplyr::left_join(f55, by = "doc_id") %>%
    dplyr::left_join(f56, by = "doc_id") %>%
    dplyr::left_join(f57, by = "doc_id") %>%
    dplyr::left_join(f58, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}

#' Extract modal verb features
#'
#' @param tokens Annotated token data frame with context columns
#' @param doc_ids Document IDs
#' @param dict_lookup Dictionary lookup
#' @return Data frame with f_52_modal_possibility through f_54_modal_predictive
#' @keywords internal
block_modals_fr <- function(tokens, doc_ids, dict_lookup) {
  # All modal detection is lemma-driven; dictionary_to_lemmas() already reduces
  # entries to their final token. Periphrastic cues (risquer de, \u00eatre susceptible
  # de, avoir la possibilit\u00e9 de, etc.) are recognized here via dependency
  # patterns, so collaborators should not duplicate surface forms;
  # see data-raw/dict.yaml for the canonical entries.
  modal_counts <- function(lemmas) {
    if (length(lemmas) == 0) {
      return(tibble::tibble(doc_id = character(), n = integer()))
    }
    tokens %>%
      dplyr::filter(
        .data$lemma %in% lemmas,
        .data$pos %in% c("VERB", "AUX")
      ) %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::tally()
  }

  sum_counts <- function(tbls) {
    valid <- purrr::compact(tbls)
    if (length(valid) == 0) {
      return(tibble::tibble(doc_id = character(), n = integer()))
    }
    purrr::reduce(valid, function(acc, tbl) {
      dplyr::full_join(acc, tbl, by = "doc_id") %>%
        dplyr::mutate(n = dplyr::coalesce(.data$n.x, 0L) + dplyr::coalesce(.data$n.y, 0L)) %>%
        dplyr::select("doc_id", "n")
    })
  }

  has_de_inf <- function(df) {
    df %>%
      dplyr::filter(
        .data$next_lemma == "de",
        .data$next_pos == "ADP",
        .data$next2_pos %in% c("VERB", "AUX"),
        .data$next2_morph_verbform == "Inf"
      )
  }

  risquer_pattern <- tokens %>%
    dplyr::filter(.data$lemma == "risquer") %>%
    has_de_inf() %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally()

  possibilite_pattern <- tokens %>%
    dplyr::filter(
      .data$lemma == "possibilit\u00e9",
      .data$prev2_lemma == "avoir"
    ) %>%
    has_de_inf() %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally()

  etre_support_pattern <- function(adj_lemmas) {
    tokens %>%
      dplyr::filter(
        .data$lemma %in% adj_lemmas,
        (.data$prev_lemma == "\u00eatre" | .data$prev2_lemma == "\u00eatre")
      ) %>%
      has_de_inf() %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::tally()
  }

  etre_oblige_pattern <- etre_support_pattern(c("obliger"))
  etre_necessaire_pattern <- etre_support_pattern(c("n\u00e9cessaire"))
  etre_susceptible_pattern <- etre_support_pattern(c("susceptible"))

  etre_future_pattern <- tokens %>%
    dplyr::filter(
      .data$token %in% c("sera", "serait"),
      .data$lemma == "\u00eatre"
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally()

  possibility_lemmas <- dictionary_to_lemmas(dict_lookup, "f_52_modal_possibility")
  necessity_lemmas <- dictionary_to_lemmas(dict_lookup, "f_53_modal_necessity")
  predictive_lemmas <- dictionary_to_lemmas(dict_lookup, "f_54_modal_predictive")

  f52 <- sum_counts(list(
    modal_counts(possibility_lemmas),
    possibilite_pattern
  )) %>%
    dplyr::rename(f_52_modal_possibility = "n")

  f53 <- sum_counts(list(
    modal_counts(necessity_lemmas),
    etre_oblige_pattern,
    etre_necessaire_pattern
  )) %>%
    dplyr::rename(f_53_modal_necessity = "n")

  predictive_counts <- modal_counts(predictive_lemmas)

  predictive_aller <- tokens %>%
    dplyr::filter(
      .data$lemma == "aller",
      .data$pos %in% c("VERB", "AUX"),
      .data$next_pos %in% c("VERB", "AUX"),
      .data$next_morph_verbform == "Inf"
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally()

  f54 <- sum_counts(list(
    predictive_counts,
    predictive_aller,
    risquer_pattern,
    etre_susceptible_pattern,
    etre_future_pattern
  )) %>%
    dplyr::rename(f_54_modal_predictive = "n")

  doc_ids %>%
    dplyr::left_join(f52, by = "doc_id") %>%
    dplyr::left_join(f53, by = "doc_id") %>%
    dplyr::left_join(f54, by = "doc_id") %>%
    dplyr::mutate(dplyr::across(-dplyr::any_of("doc_id"), ~ dplyr::coalesce(., 0L)))
}
