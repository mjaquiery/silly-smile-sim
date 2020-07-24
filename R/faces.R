# Functions for defining and updating faces

FEATURES <- c('smiling', 'frowning', 'pursed lips', 'eye contact')

#' Return a target facial expression for an event
#' @param event tbl row of the event to which we are responding
#' @return tbl of feature-value pairs describing a facial expression
#' @importFrom dplyr tibble case_when
event_facial_response <- function(event) {
  values <- NULL
  if (event$name == "decision_time") {
    values <- case_when(
      event$player_a_cooperates ~ c(1, 0, 0, 0),
      T ~ c(0, 0, 1, -1)
    )
  } else if (event$name == "reveal_time") {
    outcome <- get_outcome_description(event$player_a_cooperates, event$player_b_cooperates)
    values <- case_when(
      outcome == 'Mutual betrayal' ~ c(-1, 1, .25, 0),
      outcome == 'Outcome stolen' ~ c(-1, 1, 1, 1),
      outcome == 'Stole outcome' ~ c(.5, -1, 0, -1),
      outcome == 'Outcome shared' ~ c(1, -1, -1, 1)
    )
  } else
    # round start
    values <- c(0, 0, 0, 0)

  tibble(feature = FEATURES, value = values)
}

#' Update a face by moving some amount from the current value to the target
#' @param face face to update
#' @return updated \code{face}
#' @importFrom dplyr case_when mutate %>%
#' @importFrom rlang .data
update_face <- function(face) {
  .update <- function(value, target, delta) {
    dplyr::case_when(
      # If we're close enough to the target then snap there
      abs(value - target) < abs(delta) ~ value,
      # Update using delta in the correct direction
      sign(value - target) == -1 ~ value + abs(delta),
      T ~ value + -abs(delta)
    )
  }
  face %>%
    mutate(
      value = .update(.data$value, .data$target, .data$delta)
    )
}

#' Simulate a single round of data for a single participant
#' @param behavioural_data tbl of behavioural data (player, round, and temporal event markers)
#' @return tbl of a simulated round with a column for each feature and a row for each frame
#' @importFrom dplyr tibble %>% select filter case_when mutate pull bind_rows contains
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
simulate_feature_data <- function(behavioural_data, ms_between_expressions = 150) {
  face <- tibble(
    feature = FEATURES,
    value = 0,
    target = value,
    delta = 0
  )
  last_event <- NULL
  last_decision <- NULL
  last_outcome <- NULL

  events <- behavioural_data %>%
    select(-.data$partner_decision_time, -.data$round_end_time) %>%
    pivot_longer(cols = contains('time'), values_to = 'time')

  out <- NULL
  frames <- 1:ms_to_frames(max(behavioural_data$round_end_time))
  frame_times <- frames_to_ms(frames)
  for (i in frames) {

    last_frame_end <- if (i > 1) frame_times[i - 1] else -1
    # Check if a new event occurred we can respond to
    event <- events %>% filter(.data$time > last_frame_end,
                               .data$time <= frame_times[i])
    if (nrow(event)) {
      last_event <- case_when(
        'reveal_time' %in% pull(event, .data$name) ~ 'reveal_time',
        'decision_time' %in% pull(event, .data$name) ~ 'decision_time',
        T ~ pull(event, name)[1]
      )
      event <- filter(event, .data$name == last_event)[1, ]
      last_decision <- pull(event, .data$player_a_cooperates)
      last_partner_decision <- pull(event, .data$player_b_cooperates)
      last_outcome <- pull(event, .data$outcome)
      face <- face %>%
        mutate(
          # we can map quickly between target and event_facial_response because
          # event_facial_response returns a tbl with the same row order
          target = event_facial_response(event)$value,
          delta = 1 / ms_to_frames(ms_between_expressions)
        )
    }

    # Update face to animate towards target
    face <- update_face(face)
    out <- bind_rows(
      out,
      face %>% mutate(
        frame = i,
        last_event = last_event,
        last_decision = last_decision,
        last_partner_decision = last_partner_decision,
        last_outcome = last_outcome
      )
    )
  }

  out %>%
    select(-target, -delta) %>%
    pivot_wider(names_from = .data$feature, values_from = .data$value)
}

#' Simulate facial data for players
#' @param behavioural_data tbl of behavioural data (player, round, and temporal event markers)
#' @return tbl of a simulated round with a column for each feature and a row for each frame
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom rlang .data
simulate_faces <- function(behavioural_data, ms_between_expression = 150) {
  behavioural_data %>%
    nest(d = -.data$id) %>%
    mutate(x = map(.data$d, ~ simulate_feature_data(., ms_between_expressions = ms_between_expression))) %>%
    unnest(cols = c(.data$x))
}
