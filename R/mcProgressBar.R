
#' Show progress bar during parallel processing 
#' 
#' Uses `echo` to safely output a progress bar to Rstudio or Linux console
#' during parallel processing.
#' 
#' This package provides 2 main methods to show progress during parallelised
#' code using [mclapply()]. If `X` (the list object looped over in a call to
#' `mclapply`) has many elements compared to the number of cores, then it is
#' easiest to use [pmclapply()]. However, in some use cases the length of `X` is
#' comparable to the number of cores and each process may take a long time. For
#' example, machine learning applied to each of 8 folds on an 8-core machine
#' will open 8 processes from the outset. Each process will often complete at
#' roughly the same time. In this case `pmclapply` is much less informative as
#' it only shows completion at the end of 1 round of processes so it will go
#' from 0% to 100%. In this example, if each process code is long and
#' subprogress can be reported along the way, for example during nested loops,
#' then `mcProgressBar` provides a way to show the subprogress during the inner
#' loop. The example below shows how to write code involving an outer call to
#' `mclapply` and an inner loop whose subprogress is tracked via calls to
#' `mcProgressBar`.
#' 
#' Technically only 1 process can be tracked. If `cores` is set to 4 and
#' `subval` is invoked, then the 1st, 5th, 9th, 13th etc process is tracked.
#' Subprogress of this process is computed as part of the number of blocks of
#' processes required.
#'
#' @param val Integer measuring progress
#' @param len Total number of processes to be executed overall.
#' @param cores Number of cores used for parallel processing.
#' @param subval Optional subvalue ranging from 0 to 1 to enable granularity
#'   during long processes. Especially useful if `len` is small relative to
#'   `cores`.
#' @param title Optional title for the progress bar.
#' @param spinner Logical whether to show a spinner which moves when each core
#'   completes a process. Not shown if `subval` is used.
#' @param start Used to pass the system time from the start of the call to show
#'   a total time elapsed. See the example below.
#' @returns No return value. Prints a progress bar to the console if called
#'   within the Rstudio environment.
#' @seealso [pmclapply()] [mclapply()]
#' @author Myles Lewis
#' @examples
#' if (Sys.info()["sysname"] != "Windows") {
#' 
#' ## Example function with mclapply wrapped around another nested function
#' library(parallel)
#' 
#' my_fun <- function(x, cores) {
#'   start <- Sys.time()
#'   mcProgressBar(0, title = "my_fun")  # initialise progress bar
#'   res <- mclapply(seq_along(x), function(i) {
#'     # inner loop of calculation
#'     y <- 1:4
#'     inner <- lapply(seq_along(y), function(j) {
#'       Sys.sleep(0.2 + runif(1) * 0.1)
#'       mcProgressBar(val = i, len = length(x), cores, subval = j / length(y),
#'                     title = "my_fun")
#'       rnorm(4)
#'     })
#'     inner
#'   }, mc.cores = cores)
#'   closeProgress(start, title = "my_fun")  # finalise the progress bar
#'   res
#' }
#' 
#' res <- my_fun(letters[1:4], cores = 2)
#' 
#' ## Example of long function
#' longfun <- function(x, cores) {
#'   start <- Sys.time()
#'   mcProgressBar(0, title = "longfun")  # initialise progress bar
#'   res <- mclapply(seq_along(x), function(i) {
#'     # long sequential calculation in parallel with 3 major steps
#'     Sys.sleep(0.2)
#'     mcProgressBar(val = i, len = length(x), cores, subval = 0.33,
#'                   title = "longfun")  # 33% complete
#'     Sys.sleep(0.2)
#'     mcProgressBar(val = i, len = length(x), cores, subval = 0.66,
#'                   title = "longfun")  # 66% complete
#'     Sys.sleep(0.2)
#'     mcProgressBar(val = i, len = length(x), cores, subval = 1,
#'                   title = "longfun")  # 100% complete
#'     return(rnorm(4))
#'   }, mc.cores = cores)
#'   closeProgress(start, title = "longfun")  # finalise the progress bar
#'   res
#' }
#' 
#' res <- longfun(letters[1:2], cores = 2)
#' 
#' }
#' @export

mcProgressBar <- function(val, len = 1L, cores = 1L, subval = NULL, title = "",
                          spinner = TRUE) {
  width <- getOption("width") - 22L - nchar(title)
  if (is.null(subval)) {
    if (val %% cores != 0) return(if (spinner) mcSpinner(val, title))
    nb <- round(width * val / len)
    pc <- round(100 * val / len)
    i <- val %% 4 +1
    sp <- if (!spinner || pc == 0 || pc == 100) "  " else c("/ ", "- ", "\\\ ", "| ")[i]
  } else {
    # with subvalue
    if (subval < 0 | subval > 1) mcstop("impossible subval")
    if (cores == 1) {
      val2 <- (val + subval -1) / len
    } else {
      if (val %% cores != 1) return()
      nround <- ceiling(len / cores)
      val2 <- (val-1) / cores / nround + subval / nround
    }
    nb <- round(width * val2)
    pc <- round(100 * val2)
    sp <- "  "
  }
  if (pc > 100) mcstop("impossible percent progress")
  if (title != "") title <- paste0(title, " ")
  
  # standard
  p <- paste(c(title, sp, "|", rep.int("=", nb), rep.int(" ", width - nb),
               sprintf("| %3d%%", pc)), collapse = "")
  if (Sys.getenv("RSTUDIO") == "1" && rstudioapi::isAvailable()) {
    if (rstudioapi::getThemeInfo()$dark) {
      # colour
      p <- paste(c("\\x1b[37m", title, sp, "|\\x1b[36m", rep.int("=", nb),
                   rep.int(" ", width - nb),
                   sprintf("\\x1b[37m| %3d%%", pc)), collapse = "")
    }
  }
  over_parallel(p)
}

#' @rdname mcProgressBar
#' @export
closeProgress <- function(start = NULL, title = "") {
  end <- Sys.time()
  mcProgressBar(1, title = title)
  if (!is.null(start)) {
    message_parallel("  (", format(end - start, digits = 3), ")")
  }
}


mcSpinner <- function(val, title) {
  i <- val %% 4 +1
  sp <- c("/", "-", "\\\ ", "|")[i]
  if (title != "") title <- paste0(title, " ")
  over_parallel(title, sp)
}


# returns TRUE if in Rstudio or Linux
checkenv <- function() {
  Sys.getenv("RSTUDIO") == "1" || Sys.info()['sysname'] == "Linux"
}
