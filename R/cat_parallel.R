# prints using shell echo from inside mclapply when run in Rstudio

#' Versions of [cat()] and [message()] for parallel processing
#' 
#' Prints messages to the console using `echo` during to enable messages to be
#' printed during parallel processing. Text is only printed if the Rstudio
#' environment is detected.
#' 
#' @param ... zero or more objects which can be coerced to character and which
#'   are pasted together.
#' @return Prints a message to the console. `cat_parallel()` uses no line feed,
#'   while `message_parallel()` always adds a newline.
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


#' Stop and print error message during parallel processing
#' 
#' `mcstop()` is a multicore version of [stop()] which prints to the console
#' using 'echo' during parallel commands such as [mclapply()], to allow error
#' messages to be more visible.
#' 
#' @param ... Objects coerced to character and pasted together and printed to
#'   the console using `echo`.
#' @return Prints an error message.
#' @export
mcstop <- function(...) {
  system(sprintf('echo "%s"', paste0(..., collapse = "")))
  stop(...)
}


#' Catch error messages during parallel processing
#' 
#' Allows an expression to be wrapped in [try()] to catch error messages. Any
#' error messages are printed to the console using [mcstop()].
#' 
#' @param expr An expression to be wrapped in [try()] to allow execution and
#'   catch error messages.
#' @param ... Optional objects to be tracked if you want to know state of
#'   objects at the point error messages are generated.
#' @return Prints error messages during parallel processing. If there is no
#'   error, the result of the evaluated expression is returned.
#' @seealso [mcstop()]
#' @export
catchError <- function(expr, ...) {
  out <- try(expr, silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  call <- match.call(expand.dots = TRUE)
  dots <- list(...)
  labs <- call[-c(1:2)]
  mcstop(c(out[1], paste(paste(labs, dots, sep = "="), collapse = ", ")))
}
