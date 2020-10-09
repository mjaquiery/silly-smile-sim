# Functions for defining and updating faces

FEATURES <- c(
  "Smile",
	"Valence",
	"Attention",
	"Anger",
	"Sadness",
	"Disgust",
	"Joy",
	"Surprise",
	"Fear",
	"Contempt",
	"Brow.Furrow",
	"Brow.Raise",
	"Cheek.Raise",
	"Chin.Raise",
	"Dimpler",
	"Eye.Closure",
	"Eye.Widen",
	"Inner.Brow.Raise",
	"Jaw.Drop",
	"Lip.Corner.Depressor",
	"Lip.Press",
	"Lip.Pucker",
	"Lip.Stretch",
	"Lip.Suck",
	"Lid.Tighten",
	"Mouth.Open",
	"Nose.Wrinkle",
	"Upper.Lip.Raise",
	"Smirk"
)

#' Generate a random resting face
#' @param seed used to generate the random numbers
#' @param means single number or vector of means for each feature
#' @param sds single number or vector of standard deviations for each feature
#' @return tbl of feature-value pairs describing the log-odds that resting face matches a feature expression
#' @importFrom stats rnorm
#' @importFrom dplyr tibble
#' @importFrom R.utils withSeed
generate_resting_face <- function(seed, means, sds) {
  withSeed(
    tibble(
      feature = FEATURES,
      value = rnorm(length(FEATURES), means, sds)
    ),
    seed = seed
  )
}

#' Return a target facial expression for an event
#' @param event tbl row of the event to which we are responding
#' @param resting_face returned when nothing special is happening
#' @return tbl of feature-value pairs describing the log-odds that data match a feature expression
#' @importFrom dplyr tibble case_when
#' @importFrom stats rnorm
event_facial_response <- function(event, resting_face) {

  values <- NULL
  facial_volatility <- 25

  if (event$name == "decision_time") {
    values <- case_when(
      event$player_cooperates ~ c(100, 100, 95, 0, 0, 0, 65, 3, 0, 0, 0,
                                    5, 45, 30, 10, 0, 0, 0, 5, 0, 0, 0, 5,
                                    0, 0, 40, 0, 3, 0),
      T ~ c(40, 45, 95, 0, 0, 5, 20, 10, 0, 0, 0, 10, 20, 3, 5, 25, 0, 60,
            0, 7, 7, 0, 15, 0, 0, 0, 0, 2, 0)
    )
  } else if (event$name == "reveal_time") {
    outcome <- get_outcome_description(
      event$player_cooperates,
      event$partner_cooperates
    )
    values <- case_when(
      outcome == 'Mutual betrayal' ~ c(100, 70, 100, 0, 9, 2, 70, 15, 2.5,
                                       0, 5, 6, 90, 80, 25, 0, 100, 100,
                                       80, 0, 0, 0, 0, 0, 0, 80, 0, 0, 0),
      outcome == 'Outcome stolen' ~ c(0, -50, 95, 15, 20, 3, 0, 5, 0, 93,
                                      70, 0, 0, 0, 2, 13, 65, 70, 25, 0,
                                      30, 40, 0, 90, 30, 0, 0, 0, 0),
      outcome == 'Stole outcome' ~ c(100, -10, 95, 0, 0, 4, 20, 30, 15, 0,
                                     5, 30, 0, 0, 50, 22, 3, 15, 0, 30, 70,
                                     40, 100, 80, 12, 0, 25, 13, 100),
      outcome == 'Outcome shared' ~ c(100, 80, 100, 0, 0, 0, 75, 3, 0, 0, 0,
                                      4, 10, 15, 0, 3, 10, 0, 20, 0, 0, 0, 0,
                                      0, 0, 30, 0, 5, 0)
    )
  } else
    # round start
    values <- generate_resting_face(resting_face, 0, facial_volatility)$value

  tibble(
    feature = FEATURES,
    value = rnorm(length(FEATURES), values, facial_volatility)
  )

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
  noise <- 5
  face %>%
    mutate(
      value = .update(.data$value, .data$target, .data$delta),
      value = rnorm(length(.data$value), .data$value, noise)
    )
}

#' Simulate a single round of data for a single participant
#' @param behavioural_data tbl of behavioural data (player, round, and temporal event markers)
#' @param ms_between_expressions milliseconds between expressions
#' @return tbl of a simulated round with a column for each feature and a row for each frame
#' @importFrom dplyr tibble %>% select filter case_when mutate pull bind_rows contains
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
#' @export
.simulate_feature_data <- function(behavioural_data, ms_between_expressions = 150) {
  face <- tibble(
    feature = FEATURES,
    value = 0,
    target = .data$value,
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
        'player_decision_time' %in% pull(event, .data$name) ~
          'player_decision_time',
        T ~ pull(event, name)[1]
      )
      event <- filter(event, .data$name == last_event)[1, ]
      last_decision <- pull(event, .data$player_cooperates)
      last_partner_decision <- pull(event, .data$partner_cooperates)
      last_outcome <- pull(event, .data$outcome)
      resting_face_seed <- pull(event, .data$player)[[1]]$resting_face_seed
      face <- face %>%
        mutate(
          # we can map quickly between target and event_facial_response because
          # event_facial_response returns a tbl with the same row order
          target = ifelse(
            !is.null(event$player[[1]]$face_event_funs) &&
              event$name %in% names(event$player[[1]]$face_event_funs),
            # use player's custom face function
            event$player[[1]]$face_event_funs[[event$name]](event)$value,
            # use generic facial response function + resting face seed
            event_facial_response(event, resting_face_seed)$value
          ),
          delta = 100 / ms_to_frames(ms_between_expressions)
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
    select(-.data$target, -.data$delta) %>%
    pivot_wider(names_from = .data$feature, values_from = .data$value)
}

#' Simulate facial data for players
#' @param players list of players in the game
#' @param behavioural_data tbl of behavioural data (player, round, and temporal event markers)
#' @param ms_between_expressions milliseconds between expressions
#' @return tbl of a simulated round with a column for each feature and a row for each frame
#' @export
#' @importFrom dplyr %>% mutate rename_with
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map2
#' @importFrom rlang .data
#' @importFrom stringr str_replace
simulate_faces <- function(
  players, behavioural_data, ms_between_expressions = 150
) {
  behavioural_data <- behavioural_data %>%
    pivot_longer(
      cols = matches('[ab]_id'),
      names_to = 'position',
      names_pattern = '([ab])_id',
      values_to = 'id'
    ) %>%
    mutate(
      player = map(.data$id, ~get_player_by_id(., players))
    ) %>%
    nest(d = c(-.data$id, -.data$position))

  # Rearrange data to replace a_ and b_ variables with player_ and partner_ as
  # required by the player's position
  .rename <- function(df, position) {
    df %>%
      mutate(
        d = map(
          .data$d,
          ~rename_with(
            ., ~str_replace(., paste0('^', position, '_'), 'player_')
          ) %>%
            rename_with(~str_replace(., '^[ab]_', 'partner_'))
        )
      )
  }

  behavioural_data <- behavioural_data %>%
    nest(d = -.data$position) %>%
    mutate(
      d = map2(.data$d, .data$position, .rename)
    ) %>%
    unnest(cols = .data$d)

  if (getOption('sillySmileSim.useParallel')) {
    cl <- parallel::makeCluster(getOption('sillySmileSim.nCores'))
    on.exit({parallel::stopCluster(cl)})
    behavioural_data$x <- parallel::parLapply(
      cl,
      behavioural_data$d,
      function(x) {
        library(sillySmileSim)
        .simulate_feature_data(
          x,
          ms_between_expressions
        )
      }
    )

  } else {
    behavioural_data <- behavioural_data %>%
      mutate(x = map(.data$d, ~ .simulate_feature_data(
        .,
        ms_between_expressions = ms_between_expressions
      )))
  }

  behavioural_data %>%
    unnest(cols = c(.data$x))
}
