# Functions for defining player properties

#' Simulate the underlying tendencies for players
#' @param n_players number of players to simulate
#' @param .forceN whether to suppress errors concerning odd/singular n_players
#' @return tbl of $id, $z_decision_speed, $forgivingness, $sneakiness,
#'   $resting_face_seed
#' @importFrom dplyr %>% tibble mutate_at if_else vars
#' @importFrom rlang .data
#' @importFrom stats rgamma runif
#' @export
simulate_players <- function(n_players, .forceN = F) {
  if (!.forceN) {
    if (n_players < 2)
      stop('At least two players must be present to play a game.')
    if (n_players %% 2 > 0)
      stop('n_players must be even to allow pairs to be made.')
  }

  forgive_gamma_shape <- 1
  forgive_gamma_rate <- 5
  forgive_min <- .01
  sneak_gamma_shape <- .4
  sneak_gamma_rate <- 5
  sneak_min <- .01
  values <- tibble(
    id = 1:n_players,
    z_decision_speed = rnorm(n_players),
    # Forgivingness is the likelihood of cooperating after being defected against
    forgivingness = rgamma(
      n_players, shape = forgive_gamma_shape, rate = forgive_gamma_rate
    ) + forgive_min,
    # Sneakiness is the likelihood of defecting after successful cooperation
    sneakiness = rgamma(
      n_players, shape = sneak_gamma_shape, rate = sneak_gamma_rate
    ) + sneak_min,
    facial_volatility = rnorm(n_players, 10, 5),
    resting_face_seed = round(runif(n_players, 1e6, 1e9))
  ) %>%
    mutate_at(
      vars(.data$forgivingness, .data$sneakiness),
      ~ if_else(. > 1, 1, .)
    )

  players <- list()

  for (p in 1:n_players) {
    players[[p]] <- as.list(values[p, ])
    # The player face_event_funs are functions which take an events dataframe
    # with fields round_id, player_cooperates, partner_cooperates, outcome,
    # (event) name, time, and player, as well as a row index for the current
    # event, and return a table of length(FEATURES) feature-value pairs
    # specifying the target facial expression following that event.
    #
    # The round history is not available through event, nor the other player's
    # facial information, but the player's own properties are accessible via
    # event$player.
    players[[p]]$face_event_funs <- list(
      'round_start_time' =
        function(events, i) {
          e <- events[i, ]
          values <- generate_resting_face(
            e$player[[1]]$resting_face_seed,
            0,
            e$player[[1]]$facial_volatility
          )$value
          tibble(
            feature = FEATURES,
            value = rnorm(
              length(FEATURES),
              values,
              e$player[[1]]$facial_volatility
            )
          )
        },
      'player_decision_time' =
        function(events, i) {
          e <- events[i, ]
          values <- case_when(
            e$player_cooperates ~ c(100, 100, 95, 0, 0, 0, 65, 3, 0, 0, 0,
                                    5, 45, 30, 10, 0, 0, 0, 5, 0, 0, 0, 5,
                                    0, 0, 40, 0, 3, 0),
            T ~ c(40, 45, 95, 0, 0, 5, 20, 10, 0, 0, 0, 10, 20, 3, 5, 25, 0, 60,
                  0, 7, 7, 0, 15, 0, 0, 0, 0, 2, 0)
          )
          tibble(
            feature = FEATURES,
            value = rnorm(
              length(FEATURES),
              values,
              e$player[[1]]$facial_volatility
            )
          )
        },
      'reveal_time' =
        function(events, i) {
          e <- events[i, ]
          outcome <- get_outcome_description(
            e$player_cooperates,
            e$partner_cooperates
          )
          values <- case_when(
            outcome == 'DD' ~ c(100, 70, 100, 0, 9, 2, 70, 15, 2.5,
                                             0, 5, 6, 90, 80, 25, 0, 100, 100,
                                             80, 0, 0, 0, 0, 0, 0, 80, 0, 0, 0),
            outcome == 'DC' ~ c(0, -50, 95, 15, 20, 3, 0, 5, 0, 93,
                                            70, 0, 0, 0, 2, 13, 65, 70, 25, 0,
                                            30, 40, 0, 90, 30, 0, 0, 0, 0),
            outcome == 'CD' ~ c(100, -10, 95, 0, 0, 4, 20, 30, 15, 0,
                                           5, 30, 0, 0, 50, 22, 3, 15, 0, 30, 70,
                                           40, 100, 80, 12, 0, 25, 13, 100),
            outcome == 'CC' ~ c(100, 80, 100, 0, 0, 0, 75, 3, 0, 0, 0,
                                            4, 10, 15, 0, 3, 10, 0, 20, 0, 0, 0, 0,
                                            0, 0, 30, 0, 5, 0)
          )
          tibble(
            feature = FEATURES,
            value = rnorm(
              length(FEATURES),
              values,
              e$player[[1]]$facial_volatility
            )
          )
        }
    )
  }

  players
}
