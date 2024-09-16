
#' mclapply with progress bar
#' 
#' `pmclapply` adds a progress bar to [mclapply()] in Rstudio environment using
#' output to the console. It is designed to add very little overhead.
#' 
#' @param X a vector (atomic or list) or an expressions vector. Other objects
#'   (including classed objects) will be coerced by `as.list`.
#' @param FUN the function to be applied via [mclapply()] to each element of `X`
#'   in parallel.
#' @param ... Optional arguments passed to `FUN`.
#' @param progress Logical whether to show the progress bar.
#' @param spinner Logical whether to show a spinner which moves each time a
#'   parallel process is completed.
#' @param title Title for the progress bar.
#' @param mc.cores The number of cores to use, i.e. at most how many child
#'   processes will be run simultaneously. The option is initialized from
#'   environment variable `MC_CORES` if set. Must be at least one, and
#'   parallelization requires at least two cores.
#' @param mc.preschedule,mc.set.seed,mc.silent,mc.cleanup,mc.allow.recursive,affinity.list
#'   See [mclapply()].
#' @return A list of the same length as `X` and named by `X`.
#' @seealso [mclapply()]
#' @importFrom parallel mclapply
#' @export
pmclapply <- function(X, FUN, ..., progress = TRUE, spinner = TRUE, title = "",
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                      mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                      affinity.list = NULL) {
  if (!progress) {
    return(mclapply(X, FUN,
                    mc.preschedule = mc.preschedule,
                    mc.set.seed = mc.set.seed,
                    mc.silent = mc.silent, mc.cores = mc.cores,
                    mc.cleanup = mc.cleanup,
                    mc.allow.recursive = mc.allow.recursive,
                    affinity.list = affinity.list, ...))
  }
  start <- Sys.time()
  s <- seq_along(X)
  FUN1 <- function(i, ...) {
    out <- FUN(X[[i]], ...)
    mcProgressBar(i, length(s), mc.cores, title = title, spinner = spinner)
    out
  }
  mcProgressBar(0, title = title, spinner = spinner)
  ret <- mclapply(s, FUN1,
                  mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
                  mc.silent = mc.silent, mc.cores = mc.cores,
                  mc.cleanup = mc.cleanup,
                  mc.allow.recursive = mc.allow.recursive,
                  affinity.list = affinity.list, ...)
  closeProgress(start, title = title)
  ret
}
