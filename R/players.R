# Functions for defining player properties

#' Simulate the underlying tendencies for players
#' @param n_players number of players to simulate
#' @return tbl of $id, $z_decision_speed, $forgivingness, $sneakiness
#' @importFrom dplyr %>% tibble mutate_at if_else vars
#' @export
simulate_players <- function(n_players) {
  forgive_gamma_shape <- 1
  forgive_gamma_rate <- 5
  forgive_min <- .01
  sneak_gamma_shape <- .4
  sneak_gamma_rate <- 5
  sneak_min <- .01
  tibble(
    id = 1:n_players,
    z_decision_speed = rnorm(n_players),
    # Forgivingness is the likelihood of cooperating after being defected against
    forgivingness = rgamma(
      n_players, shape = forgive_gamma_shape, rate = forgive_gamma_rate
    ) + forgive_min,
    # Sneakiness is the likelihood of defecting after successful cooperation
    sneakiness = rgamma(
      n_players, shape = sneak_gamma_shape, rate = sneak_gamma_rate
    ) + sneak_min
  ) %>%
    mutate_at(
      vars(forgivingness, sneakiness),
      ~ if_else(. > 1, 1, .)
    )
}
