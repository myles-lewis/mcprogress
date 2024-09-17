# prints using shell echo from inside mclapply when run in Rstudio

#' Versions of `cat` and `message` for parallel processing
#' 
#' Prints messages to the console using `echo` during to enable messages to be
#' printed during parallel processing. Text is only printed if the Rstudio
#' environment is detected.
#' 
#' @param ... zero or more objects which can be coerced to character and which
#'   are pasted together.
#' @return Prints a message to the console. `cat_parallel` uses no line feed,
#'   while `message_parallel` always adds a newline.
#' @export
cat_parallel <- function(...) {
  if (!checkenv()) return()
  system(sprintf('echo "%s', paste0(..., '\\c"', collapse = "")))
}

#' @rdname cat_parallel
#' @export
message_parallel <- function(...) {
  if (!checkenv()) return()
  system(sprintf('echo "%s"', paste0(..., collapse = "")))
}

over_parallel <- function(...) {
  if (!checkenv()) return()
  p <- paste0('\\r', ..., '\\c"', collapse = "")
  system(sprintf('echo "%s', p))
}

#' Stop and print message during parallel processing
#' 
#' `mcstop` is a multicore version of `stop` which prints to the console using
#' `echo` during parallel commands such as [mclapply()], to allow error messages
#' to be more visible.
#' 
#' @param ... Objects coerced to character and pasted together and printed to
#'   the console using `echo`.
#' @return Prints an error message.
#' @export
mcstop <- function(...) {
  system(sprintf('echo "%s"', paste0('\\nError: ', ..., collapse = "")))
  stop(...)
}
