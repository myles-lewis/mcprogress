# prints using shell echo from inside mclapply when run in Rstudio
cat_parallel <- function(...) {
  if (!checkenv()) return()
  system(sprintf('echo "%s', paste0(..., '\\c"', collapse = "")))
}

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
#' to be seen more clearly.
#' 
#' @param ... Objects coerced to character and pasted together and printed to
#'   the console using `echo`.
#' @return Produces an error message.
#' @export
mcstop <- function(...) {
  system(sprintf('echo "%s"', paste0('\\nError: ', ..., collapse = "")))
  stop(...)
}
