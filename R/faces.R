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

#' Vector of facial feature dimensions
#' @export
features <- function() FEATURES

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
  round_id <- NULL

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
      round_id <- pull(event, .data$round_id)
      if (event$name %in% names(event$player[[1]]$face_event_funs))
        face <- face %>%
        mutate(
          # we can map quickly between target and player's face_event_fun because
          # player's face_event_fun returns a tbl with the same row order
          target = event$player[[1]]$face_event_funs[[event$name]](
            events,
            which(events$time == event$time & events$name == event$name)
          )$value,
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
        last_outcome = last_outcome,
        round_id = round_id
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
