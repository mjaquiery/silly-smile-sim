% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faces.R
\name{.simulate_feature_data}
\alias{.simulate_feature_data}
\title{Simulate a single round of data for a single participant}
\usage{
.simulate_feature_data(behavioural_data, ms_between_expressions = 150)
}
\arguments{
\item{behavioural_data}{tbl of behavioural data (player, round, and temporal event markers)}

\item{ms_between_expressions}{milliseconds between expressions}
}
\value{
tbl of a simulated round with a column for each feature and a row for each frame
}
\description{
Simulate a single round of data for a single participant
}
