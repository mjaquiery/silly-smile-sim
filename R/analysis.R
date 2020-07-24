# Analysis and visualisations

#' Print a neat EEG-style plot of how facial features are moving throughout the
#' trials
#' @param feature_data facial feature data to plot
feature_plot <- function(feature_data) {
  events <- feature_data %>%
    select(frame, last_event, last_decision, last_partner_decision, last_outcome) %>%
    filter(last_event != lag(last_event)) %>%
    bind_rows(tibble(last_event = 'round_start_time', frame = 0)) %>%
    arrange(frame) %>%
    mutate(
      event = case_when(
        last_event == 'round_start_time' ~
          'Round start',
        last_event == 'decision_time' ~
          if_else(last_decision, 'Choose split', 'Choose steal'),
        last_event == 'reveal_time' ~
          get_outcome_description(last_decision, last_partner_decision)
      )
    )

  feature_data %>%
    pivot_longer(all_of(FEATURES), names_to = 'feature') %>%
    ggplot(aes(x = frame, y = value)) +
    geom_vline(aes(xintercept = frame, colour = event, linetype = event), data = events, size = .75) +
    geom_line(aes(group = id)) +
    scale_colour_manual(name = 'Event',
                        limits = c(
                          'Round start',
                          'Choose split', 'Choose steal',
                          OUTCOME_NAMES
                        ),
                        drop = F,
                        values = c('black', scales::hue_pal()(6))
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
