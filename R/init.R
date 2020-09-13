#' Set options for working with Silly Smile Sim
#' @param libname	a character string giving the library directory where the
#'   package defining the namespace was found.
#' @param pkgname	a character string giving the name of the package.
#' @importFrom utils installed.packages
.onLoad <- function(libname, pkgname) {
  coreLimit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  op <- options()

  op.sillySmileSim <- list(
    sillySmileSim.useParallel = T,
    sillySmileSim.nCores =
      if (nchar(coreLimit) & coreLimit == "TRUE")
        2L
    else
      parallel::detectCores()
  )

  toset <- !(names(op.sillySmileSim) %in% names(op))

  if (any(toset))
    options(op.sillySmileSim[toset])

  invisible(NULL)
}
