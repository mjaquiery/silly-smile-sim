context("Basic simulation behaviour")
library(sillySmileSim)

n_players <- 4
n_rounds <- 5
fps <- 30

test_that("simulate_players returns a list", {
  s <- simulate_players(n_players)
  expect_equal("list" %in% class(s), T)
})

test_that("simulate_rounds returns a tbl", {
  load('data/player_data.Rdata')
  r <- simulate_rounds(s, n_rounds)
  expect_equal("tbl" %in% class(r), T)
})

test_that("simulate_faces returns a tbl", {
  load('data/player_data.Rdata')
  load('data/behavioural_data.Rdata')
  op <- options()
  options(list(sillySmileSim.useParallel = T, sillySmileSim.nCores = 2L))
  f <- simulate_faces(s, r)
  expect_equal("tbl" %in% class(f), T)
  options(op)
})

test_that("simulate_faces runs in serial", {
  load('data/player_data.Rdata')
  load('data/behavioural_data.Rdata')
  op <- options()
  options(list(sillySmileSim.useParallel = F))
  f <- simulate_faces(s, r)
  expect_equal("tbl" %in% class(f), T)
  options(op)
})

test_that("generate_resting_face is consistent", {
  load('data/player_data.Rdata')
  expect_equal(
    generate_resting_face(s[[1]]$resting_face_seed, 0, 1),
    generate_resting_face(s[[1]]$resting_face_seed, 0, 1)
  )
})

test_that("feature_plot returns a ggplot object", {
  load('data/facial_data.Rdata')
  x <- f %>% mutate(i = id) %>% nest(d = -i)
  g <- feature_plot(x$d[[1]])
  expect_equal("ggplot" %in% class(g), T)
})

test_that("everything runs as set", {
  s <- simulate_players(n_players)
  r <- simulate_rounds(s, n_rounds)
  f <- simulate_faces(s, r)
  expect_equal("tbl" %in% class(f), T)
})

# Look at individual graphs
if (F) {
  feature_plot(x$d[[1]], features = c(
    'Smile', 'Cheek.Raise', 'Mouth.Open', 'Lip.Press', 'Dimpler', 'Lip.Stretch',
    'Lip.Suck', 'Chin.Raise', 'Brow.Furrow', 'Nose.Wrinkle', 'Upper.Lip.Raise'
  )) +
    theme_light() +
    theme(
      panel.grid = element_blank()
    )
}

# Generate the data for other tests
if (F) {
  save(s, file = 'tests/testthat/data/player_data.Rdata')
  save(r, file = 'tests/testthat/data/behavioural_data.Rdata')
  save(f, file = 'tests/testthat/data/facial_data.Rdata')
}
