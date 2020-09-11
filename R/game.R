# Game rules and simulations

#' Names of the outcomes listed in order so that matrix(X, 2, 2) gives the
#' outcome matrix with 1 = steal, 2 = split
OUTCOME_NAMES <- c('Mutual betrayal', 'Outcome stolen', 'Stole outcome', 'Outcome shared')

#' Return a string describing the outcome
#' @param a_cooperates whether player a cooperates
#' @param b_cooperates whether player b cooperates
#' @return string describing the outcome
#' @export
get_outcome_description <- function(a_cooperates, b_cooperates) {
  if (length(a_cooperates) > 1)
    return(sapply(1:length(a_cooperates), function(i)
      get_outcome_description(a_cooperates[i], b_cooperates[i])))

  outcomes <- matrix(OUTCOME_NAMES, 2, 2)
  outcomes[a_cooperates + 1, b_cooperates + 1]
}

#' Convert a standardized decision time to a real one in ms
#' @param z_times vector of standardized times
#' @param M mean decision time
#' @param SD standard deviation of decision time
#' @param cap whether to cap the resulting decision times to minimum 1sd ms
#' @return vector of decision times in ms
get_decision_time <- function(z_times, M, SD, cap = TRUE) {
  times <- z_times * SD + M
  if (cap)
    times <- ifelse(times < SD, SD, times)

  times
}

#' Does \code{player} cooperate with the other player on this round of the game?
#' @param player tbl row of the player whose cooperation we are checking
#' @param previous_round tbl of the previous round of the game
#' @details Uses a version of forgiving tit-for-tat. Basic strategy is to copy
#'   the partner's previous move, but with a chance to change. Where the partner
#'   cooperated, players defect based on their sneakiness. Where the partner
#'   defected, players cooperate based on their forgivingness.
#' @return logical of T = cooperate, F = defect
#' @importFrom dplyr if_else
#' @importFrom stats runif
does_player_cooperate <- function(player, previous_round) {
  partner_cooperated_before <- if_else(
    player$id == previous_round$player_a_id,
    previous_round$player_b_cooperates, previous_round$player_a_cooperates
  )

  if (partner_cooperated_before)
    runif(1) >= player$sneakiness
  else
    runif(1) >= player$forgivingness
}

#' Simulate behavioural data
#' @param player tbl row of the player
#' @param partner tbl row of the player's partner (another player)
#' @param n_rounds number of rounds to play
#' @param decision_time_mean mean decision time of the sample
#' @param decision_time_sd standard deviation of the above
#' @return tbl with $id, $round_id, and markers for $decision_time, $partner_decision_time, $reveal_time, and $round_end_time all in ms.
#' @importFrom dplyr tibble %>% mutate select
#' @importFrom rlang .data
#' @importFrom stats rnorm
simulate_behavioural_markers <- function(
  player,
  partner,
  n_rounds,
  decision_time_mean = 750,
  decision_time_sd = 100
) {
  behaviour <- tibble(
    id = rep(player$id, n_rounds),
    resting_face_seed = player$resting_face_seed,  # needed for face data stuff later
    round_id = 1:n_rounds,
    z_decision_time = rnorm(n_rounds, player$z_decision_speed),
    z_partner_decision_time = rnorm(n_rounds, partner$z_decision_speed),
    .round_end_time = 5000,
    .decision_time = get_decision_time(.data$z_decision_time, decision_time_mean, decision_time_sd),
    .partner_decision_time = get_decision_time(.data$z_partner_decision_time, decision_time_mean, decision_time_sd),
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
      reveal_time = pmax(.data$decision_time, .data$partner_decision_time)
    ) %>%
    select(
      .data$id,
      .data$resting_face_seed,
      .data$round_id,
      .data$round_start_time,
      .data$decision_time,
      .data$partner_decision_time,
      .data$reveal_time,
      .data$round_end_time
    )
}

#' Simulate the cooperation/defecting behaviour in the game
#' @param player_a player taking the role of player a
#' @param player_b player taking the role of player b
#' @param n_rounds number of rounds to simulate
#' @return tbl with $round_id, $player_a_id, $player_b_id, $player_a_cooperates, $player_b_cooperates, $outcome
#' @importFrom dplyr tibble
simulate_game <- function(player_a, player_b, n_rounds) {
  rounds <- tibble(
    round_id = 1:n_rounds,
    player_a_id = player_a$id,
    player_b_id = player_b$id,
    player_a_cooperates = NA,
    player_b_cooperates = NA,
    outcome = NA
  )
  # Simulate serially because player's decisions depend on previous decisions
  for (r in 1:nrow(rounds)) {
    if (r == 1) {
      prev <- tibble(
        player_a_id = player_a$id,
        player_b_id = player_b$id,
        player_a_cooperates = T,
        player_b_cooperates = T,
        outcome = T
      )
    } else {
      prev <- rounds[r - 1, ]
    }

    rounds$player_a_cooperates[r] <- does_player_cooperate(player_a, prev)
    rounds$player_b_cooperates[r] <- does_player_cooperate(player_b, prev)
    rounds$outcome[r] <- as.logical(
      rounds$player_a_cooperates[r] * rounds$player_b_cooperates[r]
    )
  }

  rounds
}

#' Simulate the data
#' @param players tbl of players
#' @param n_rounds number of rounds to simulate
#' @param fps frame rate of simulations
#' @return tbl of experiment data
#' @importFrom dplyr tibble bind_rows %>% left_join
#' @export
simulate_rounds <- function(players, n_rounds, fps = 30) {
  decision_time_mean <- 1000
  decision_time_sd <- 200

  partners <- unlist(
    sapply(players$id, function(x) sample(players$id[players$id != x], 1))
  )

  behaviour <- NULL

  for (s in 1:nrow(players))
    behaviour <- bind_rows(
      behaviour,
      simulate_behavioural_markers(
        players[s, ],
        players[partners[s], ],
        n_rounds
      ) %>%
        left_join(
          simulate_game(players[s, ],
                        players[partners[s], ],
                        n_rounds),
          by = c('round_id', id = 'player_a_id')
        )
    )

  behaviour
}
