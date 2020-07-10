library(tidyverse)

n_subjects <- 3
n_rounds <- 5
fps <- 30

features <- c('smiling', 'blinking')

#' Simulate the underlying tendencies for subjects
#' @param n_subjects number of subjects to simulate
#' @return tbl of $id, $z_decision_speed
simulate_subjects <- function(n_subjects) {
  tibble(
    id = 1:n_subjects,
    z_decision_speed = rnorm(n_subjects)
  )
}

#' Convert a duration in milliseconds to a frame count
#' @param ms milliseconds
#' @param fps frame rate
#' @param floor whether to return the answer as maximum number of frames in the duration (T) or exact answer including partial frames (F)
#' @return frames in \code{ms}
ms_to_frames <- function(ms, fps = 30, floor = T) {
  f <- fps * ms / 1000
  if (floor)
    floor(f)
  else
    f
}

#' Find the millisecond at which frames begin
#' @param frameNumbers frames to get the beginnings of
frames_to_ms <- function(frameNumbers, fps = 30) {
  frameNumbers / fps * 1000
}

#' Return a target facial expression for an event
#' @param event_name name of the event to which we are responding
#'
#' !TODO[Fix this - needs to use values that animate between current and target, using 0 means nothing changes!]
event_facial_response <- function(event_name) {
  if (event_name == "decision_time") {
    tibble(
      feature = features,
      value = 1
    )
  } else if (event_name == "reveal_time") {
    tibble(
      feature = features,
      value = -1
    )
  } else
    tibble(
      feature = features,
      value = 0
    )
}

#' Simulate a single round of data for a single participant
#' @param behavioural_data tbl of behavioural data (subject, round, and temporal event markers)
#' @return tbl of a simulated round with a column for each feature and a row for each frame
simulate_feature_data <- function(behavioural_data, ms_between_expressions = 150) {
  face <- tibble(
    feature = features,
    value = 0,
    target = value,
    delta = 0
  )
  last_event <- NULL

  events <- behavioural_data %>%
    select(-partner_decision_time, -round_end_time) %>%
    pivot_longer(cols = contains('time'), values_to = 'time')

  out <- NULL
  frames <- 1:ms_to_frames(max(behavioural_data$round_end_time))
  frame_times <- frames_to_ms(frames)
  for (i in frames) {
    last_frame_end <- if (i > 1) frame_times[i - 1] else -1
    # Check if a new event occurred we can respond to
    event <- events %>% filter(time > last_frame_end, time <= frame_times[i])
    if (nrow(event)) {
      event_name <- case_when(
        'reveal_time' %in% pull(event, name) ~ 'reveal_time',
        'decision_time' %in% pull(event, name) ~ 'decision_time',
        T ~ pull(event, name)[1]
      )
      last_event <- paste0(event_name, '.', i)
      face <- face %>%
        mutate(
          target = event_facial_response(event_name)$value,
          delta = target / ms_to_frames(ms_between_expressions)
        )
    }

    # Update face to animate towards target
    face <- face %>%
      mutate(
        value = if_else(value + delta != target &
                          (value == target |
                             sign(value + delta - target) != sign(value - target)),
                        value,
                        value + delta)
      )
    out <- bind_rows(
      out,
      face %>% mutate(frame = i, last_event = last_event)
    )
  }

  out %>%
    mutate(
      subject_id = behavioural_data$id[1]
      ) %>%
    pivot_wider(names_from = feature, values_from = value)
}

#' Convert a standardized decision time to a real one in ms
#' @param z_times vector of standardized times
#' @param cap whether to cap the resulting decision times to minimum 1sd ms
#' @return vector of decision times in ms
get_decision_time <- function(z_times, cap = TRUE) {
  times <- z_times * decision_time_sd + decision_time_mean
  if (cap)
    times <- ifelse(times < decision_time_sd, decision_time_sd, times)

  times
}

#' Simulate behavioural data
#' @param subject tbl row of the subject
#' @param partner tbl row of the subject's partner (another subject)
#' @param n_rounds number of rounds to play
#' @return tbl with $id, $round_id, and markers for $decision_time, $partner_decision_time, $reveal_time, and $round_end_time all in ms.
simulate_behavioural_markers <- function(subject, partner, n_rounds) {
  behaviour <- tibble(
    id = rep(subject$id, n_rounds),
    round_id = 1:n_rounds,
    z_decision_time = rnorm(n_rounds, subject$z_decision_speed),
    z_partner_decision_time = rnorm(n_rounds, partner$z_decision_speed),
    .round_end_time = 5000,
    .decision_time = get_decision_time(z_decision_time),
    .partner_decision_time = get_decision_time(z_partner_decision_time),
    round_end_time = NA_real_,
    round_start_time = NA_real_,
    decision_time = NA_real_,
    partner_decision_time = NA_real_,
  )

  for (r in 1:n_rounds) {
    behaviour$round_start_time[r] <- if (r == 1) 0 else behaviour$round_end_time[r - 1]
    behaviour$decision_time[r] <-
      sum(behaviour$.decision_time[1:r]) + behaviour$round_start_time[r]
    behaviour$partner_decision_time[r] <-
      sum(behaviour$.partner_decision_time[1:r]) + behaviour$round_start_time[r]
    behaviour$round_end_time[r] <- behaviour$.round_end_time[r] + behaviour$round_start_time[r]
  }

  behaviour %>%
    mutate(
      reveal_time = pmax(decision_time, partner_decision_time)
    ) %>%
    select(id, round_id, round_start_time, decision_time, partner_decision_time, reveal_time, round_end_time)
}

#' Simulate the data
#' @param subjects tbl of subjects
#' @param n_rounds number of rounds to simulate
#' @param fps frame rate of simulations
#' @return tbl of experiment data
simulate_rounds <- function(subjects, n_rounds, fps = 30) {
  decision_time_mean <- 1000
  decision_time_sd <- 200

  partners <- unlist(
    sapply(subjects$id, function(x) sample(subjects$id[subjects$id != x], 1))
  )

  behaviour <- NULL

  for (s in 1:nrow(subjects))
    behaviour <- bind_rows(
      behaviour,
      simulate_behavioural_markers(subjects[s, ], subjects[partners[s], ], n_rounds)
    )
}

s <- simulate_subjects(n_subjects)
r <- simulate_rounds(s, n_rounds)
