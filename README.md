# mcprogress

Adds a progress bar to `mclapply()` using `echo` to output to the console in 
Rstudio or Linux environments. Simply replace your original call to `mclapply()`
with `pmclapply()`.

Also includes functions to safely print messages (including error messages)
from within parallelised code. This can be very useful for debugging parallel R
code.

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
