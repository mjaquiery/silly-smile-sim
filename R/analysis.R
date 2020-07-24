# Analysis and visualisations

#' Print a neat EEG-style plot of how facial features are moving throughout the
#' trials
#' @param feature_data facial feature data to plot
#' @return ggPlot object with a feature_data plot
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% if_else arrange lag all_of
#' @importFrom rlang .data
#' @importFrom scales hue_pal
#' @export
feature_plot <- function(feature_data) {
  requireNamespace("ggplot2")
  events <- feature_data %>%
    select(.data$frame,
           .data$last_event,
           .data$last_decision,
           .data$last_partner_decision,
           .data$last_outcome) %>%
    filter(.data$last_event != lag(.data$last_event)) %>%
    bind_rows(tibble(last_event = 'round_start_time', frame = 0)) %>%
    arrange(.data$frame) %>%
    mutate(
      event = case_when(
        .data$last_event == 'round_start_time' ~
          'Round start',
        .data$last_event == 'decision_time' ~
          if_else(.data$last_decision, 'Choose split', 'Choose steal'),
        .data$last_event == 'reveal_time' ~
          get_outcome_description(.data$last_decision, .data$last_partner_decision)
      )
    )

  feature_data %>%
    pivot_longer(all_of(FEATURES), names_to = 'feature') %>%
    ggplot(aes(x = .data$frame, y = .data$value)) +
    geom_vline(aes(xintercept = .data$frame, colour = .data$event, linetype = .data$event), data = events, size = .75) +
    geom_line(aes(group = .data$id)) +
    scale_colour_manual(name = 'Event',
                        limits = c(
                          'Round start',
                          'Choose split', 'Choose steal',
                          OUTCOME_NAMES
                        ),
                        drop = F,
                        values = c('black', hue_pal()(6))
    ) +
    scale_linetype_manual(name = 'Event',
                          limits = c(
                            'Round start',
                            'Choose split', 'Choose steal',
                            OUTCOME_NAMES
                          ),
                          drop = F,
                          values = c('dashed', rep('solid', 6))
    ) +
    theme_light() +
    facet_grid(feature ~ .) +
    labs(
      title = paste0('Facial trajectory plot (participant ', feature_data$id[1], ')')
    )
}
