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
    resting_face_seed = round(runif(n_players, 1e6, 1e9))
  ) %>%
    mutate_at(
      vars(.data$forgivingness, .data$sneakiness),
      ~ if_else(. > 1, 1, .)
    )

  players <- list()

  for (p in 1:n_players) {
    players[[p]] <- as.list(values[p, ])
    # The player face_event_funs are functions which take an event with fields
    # round_id, player_cooperates, partner_cooperates, outcome, (event) name,
    # time, and player, and return a table of length(FEATURES) feature-value
    # pairs specifying the target facial expression following that event.
    #
    # The round history is not available through event, nor the other player's
    # facial information, but the player's own properties are accessible via
    # event$player.
    players[[p]]$face_event_funs <- list(
      'round_start_time' =
        function(x) {tibble(value = rep(50, 29))},
      'player_decision_time' =
        function(x) {tibble(value = rep(0, 29))},
      'reveal_time' =
        function(x) {tibble(value = rep(100, 29))}
    )
  }

  players
}
