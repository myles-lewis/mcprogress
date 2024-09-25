
#' mclapply with progress bar
#' 
#' `pmclapply()` adds a progress bar to [mclapply()] in Rstudio or Linux
#' environments using output to the console. It is designed to add very little
#' overhead.
#' 
#' This function can be used in an identical manner to [mclapply()]. It is ideal
#' for use if the length of `X` is comparably > cores. As processes are spawned
#' in a block and most code for each process completes at roughly the same time,
#' processes move along in blocks as determined by `mc.cores`. To track
#' progress, `pmclapply()` only tracks the nth process, where n=`mc.cores`. For
#' example, with 4 cores, `pmclapply()` reports progress when the 4th, 8th,
#' 12th, 16th etc process has completed. If the length of `X` is very large
#' (e.g. in the 1000s), then the progress bar will only update for each 1% of
#' progress in order to reduce overhead.
#' 
#' However, in some scenarios the length of `X` is comparable to the number of
#' cores and each process may take a long time. For example, machine learning
#' applied to each of 8 cross-validation folds on an 8-core machine will open 8
#' processes from the outset. Each process will often complete at roughly the
#' same time. In this case `pmclapply()` is much less informative as it only
#' shows completion at the end of 1 round of processes, so it will go from 0%
#' straight to 100%. For this scenario, we recommend users use [mcProgressBar()]
#' which allows more fine-grained reporting of subprogress from within a block
#' of parallel processes.
#' 
#' ETA is approximate. As part of minimising overhead, it is only updated with
#' each change in progress (i.e. each time a block of processes completes). It
#' is not updated by interrupt.
#' 
#' @param X a vector (atomic or list) or an expressions vector. Other objects
#'   (including classed objects) will be coerced by `as.list()`.
#' @param FUN the function to be applied via [mclapply()] to each element of `X`
#'   in parallel.
#' @param ... Optional arguments passed to `FUN`.
#' @param progress Logical whether to show the progress bar.
#' @param spinner Logical whether to show a spinner which moves each time a
#'   parallel process is completed. More useful if the length of time for each
#'   process to complete is variable.
#' @param title Title for the progress bar.
#' @param eta Logical whether to show estimated time to completion.
#' @param mc.cores The number of cores to use, i.e. at most how many child
#'   processes will be run simultaneously. The option is initialized from
#'   environment variable `MC_CORES` if set. Must be at least one, and
#'   parallelization requires at least two cores.
#' @param mc.preschedule,mc.set.seed,mc.silent,mc.cleanup,mc.allow.recursive,affinity.list
#'   See [mclapply()].
#' @return A list of the same length as `X` and named by `X`.
#' @seealso [mclapply()] [mcProgressBar()]
#' @author Myles Lewis
#' @examples
#' if (Sys.info()["sysname"] != "Windows") {
#' 
#' res <- pmclapply(letters[1:20], function(i) {
#'                  Sys.sleep(0.2 + runif(1) * 0.1)
#'                  setNames(rnorm(5), paste0(i, 1:5))
#'                  }, mc.cores = 2, title = "Working")
#' 
#' }
#' @importFrom parallel mclapply
#' @export
pmclapply <- function(X, FUN, ..., progress = TRUE, spinner = FALSE, title = "",
                      eta = TRUE,
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
    mcProgressBar(i, length(s), mc.cores, title = title, spinner = spinner,
                  eta = eta, start = start)
    out
  }
  mcProgressBar(0, title = title, spinner = spinner, eta = eta)
  ret <- mclapply(s, FUN1,
                  mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed,
                  mc.silent = mc.silent, mc.cores = mc.cores,
                  mc.cleanup = mc.cleanup,
                  mc.allow.recursive = mc.allow.recursive,
                  affinity.list = affinity.list, ...)
  closeProgress(start, title = title, eta = eta)
  ret
}
