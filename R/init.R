#' Set options for working with Silly Smile Sim
.onLoad <- function(libname, pkgname) {
  parallel <- 'parallel' %in% rownames(installed.packages())

  op <- options()

  op.sillySmileSim <- list(
    sillySmileSim.useParallel = parallel,
    sillySmileSim.nCores = if (parallel) parallel::detectCores() else 2
  )

  toset <- !(names(op.sillySmileSim) %in% names(op))

  if (any(toset))
    options(op.sillySmileSim[toset])

  invisible(NULL)
}
