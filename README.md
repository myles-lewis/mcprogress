# mcprogress

Adds a progress bar to `mclapply()` using `echo` to output to the console in 
Rstudio or Linux environments. Simply replace your original call to `mclapply()`
with `pmclapply()`.

### Installation

Clone the repo using github desktop etc.

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
