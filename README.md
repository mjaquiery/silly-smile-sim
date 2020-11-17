<!-- badges: start -->
![R-CMD-check](https://github.com/mjaquiery/silly-smile-sim/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/mjaquiery/silly-smile-sim/branch/master/graph/badge.svg)](https://codecov.io/gh/mjaquiery/silly-smile-sim)
<!-- badges: end -->

# Silly Smile Sim
## Matt Jaquiery and Olly Robertson
### Created 10/07/2020

## Introduction

Olly and co collect lots of exciting facial expression data from prisoner's dilemma games, but it can be quite tricky to figure out an appropriate analytical approach. This project simulates slightly less (or more) exciting expression data for testing analytical approaches on.

## Installation

The easiest way to install is directly from GitHub using remotes:

```r
remotes::install_github('mjaquiery/silly-smile-sim')
```

## Example

The simplest simulation is run by creating some players for the game, getting them to play the game, and simulating their facial responses to the game:

``` r
library(sillySmileSim)
n_players <- 4
n_rounds <- 5

players <- simulate_players(n_players)
behaviour <- simulate_rounds(players, n_rounds)
faces <- simulate_faces(players, behaviour)
```

We can then view a graph of a player's face throughout the game:

``` r
library(dplyr)
# Split faces by player ID (while preserving id column)
x <- faces %>% mutate(i = id) %>% nest(d = -i)
# View player 1's facial data
feature_plot(x$d[[1]])
```

### Custom player response functions

We can also have one or more players using custom response functions. 
For example we can have a player whose response is always a blank face, regardless of what actually happens.

``` r
players[[1]]$face_event_funs$reveal_time <-
  #' Function to process facial response to reveal_time event
  #' @param events dataframe of events
  #' @param row_id of the current event
  #' @return 29 x 2 tibble of features and their target values
  function(events, i) {
    tibble::tibble(
      feature = features(),
      # rnorm adds a bit of variety around the flatness, based on the player's
      # facial_volatility property
      value = rnorm(
        length(features()), 
        0, 
        events[i, 'player'][[1]]$facial_volatility
      )
    )
  }
  
# now simulate with that new data
behaviour <- simulate_rounds(players, n_rounds)
faces <- simulate_faces(players, behaviour)

# and plot the graph
x <- faces %>% mutate(i = id) %>% nest(d = -i) %>% filter(i == 1)
# View player 1's facial data
feature_plot(x$d[[1]])

```

## Limitations

At the moment the package has support for individual players with individual responses to game events, and can make those responses dependent upon the player's own characteristics. 
These custom functions cannot, however, depend upon the partner's facial configuration information. 
The facial responses are thus discrete, idiosyncratic responses rather than truly being enmeshed temporally within an evolving game and interlinked socially with another player.
