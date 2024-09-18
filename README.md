# mcprogress

Adds a progress bar to `mclapply()` using `echo` to output to the console in 
Rstudio or Linux environments. Simply replace your original call to `mclapply()`
with `pmclapply()`.

### Installation

Clone the repo using Github desktop etc.

Install using R/devtools.

```
# set path to the repo
setwd("../github/mcprogress")
devtools::install()
```

Or install direct from Github (this will only work once the package is public 
and not private).
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
