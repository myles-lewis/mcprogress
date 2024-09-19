# mcprogress

Adds a progress bar to `mclapply()` using `echo` to output to the console in 
Rstudio or Linux environments. Simply replace your original call to `mclapply()`
with `pmclapply()`.

Also includes functions to safely print messages (including error messages)
from within parallelised code. This can be very useful for debugging parallel R
code.

### Installation

Install from Github
```
devtools::install_github("myles-lewis/mcprogress")
```
