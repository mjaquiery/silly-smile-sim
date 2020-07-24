context("Basic simulation behaviour")
library(sillySmileSim)

n_players <- 3
n_rounds <- 5
fps <- 30

test_that("simulate_players returns a tbl", {
  s <- simulate_players(n_players)
  expect_equal("tbl" %in% class(s), T)
})

test_that("simulate_rounds returns a tbl", {
  load('data/player_data.Rdata')
  r <- simulate_rounds(s, n_rounds)
  expect_equal("tbl" %in% class(r), T)
})

test_that("simulate_faces returns a tbl", {
  load('data/behavioural_data.Rdata')
  f <- simulate_faces(r)
  expect_equal("tbl" %in% class(f), T)
})

test_that("feature_plot returns a ggplot object", {
  load('data/facial_data.Rdata')
  x <- f %>% mutate(i = id) %>% nest(d = -i)
  g <- feature_plot(x$d[[1]])
  expect_equal("ggplot" %in% class(g), T)
})

