# mcprogress

Adds a progress bar to `mclapply()` using `echo` to output to the console in 
Rstudio or Linux environments. Simply replace your original call to `mclapply()`
with `pmclapply()`.

A progress bar can also be displayed with parallelisation via the `foreach` 
package.

Also included are functions to safely print messages (including error messages)
from within parallelised code. This can be very useful for debugging 
parallelised R code.

### Installation

Install direct from Github.
```
devtools::install_github("myles-lewis/mcprogress")
```

### Example

```
# toy example
res <- pmclapply(letters[1:20], function(i) {
                 Sys.sleep(0.2 + runif(1) * 0.1)
                 setNames(rnorm(5), paste0(i, 1:5))
                 }, mc.cores = 2, title = "Working")
```
```
Working / |================================                 |  60%  eta 3.1 secs
```

Another example using the `foreach` package with `doMC` backend.

```
# Example from doMC vignette
library(doMC)
library(foreach)
registerDoMC(4)

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 10000

{
  start <- Sys.time()
  r <- foreach(i = seq_len(trials), .combine = cbind) %dopar% {
    ind <- sample(100, 100, replace = TRUE)
    result1 <- glm(x[ind, 2] ~ x[ind, 1], family = binomial(logit))
    mcProgressBar(i, trials, cores = getDoParWorkers(), start = start)
    coefficients(result1)
  }
  closeProgress(start)
}
```
