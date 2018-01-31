
<!-- README.md is generated from README.Rmd. Please edit that file -->
<style>
  .col2 {
    columns: 2 200px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 200px; /* chrome, safari */
    -moz-columns: 2 200px;    /* firefox */
  }
  .col3 {
    columns: 3 100px;
    -webkit-columns: 3 100px;
    -moz-columns: 3 100px;
  }
</style>
launcheR
========

The goal of launcheR is to easily launch R scripts with 2 available features :

1.  Waiting queue management
    -   `queue` can belong to a group. A group can run only one queue at once.
    -   A `batch` can be parallelizable among all groups or not
    -   *"first come, first served"*, then `queue` or `batch` are set in a waiting queue and launch ASAP.

2.  Easy set of parameters
    -   No more trouble with `args` in `Rscript`
    -   All class can be set as parameters

**NB : ** `Rscript` is run in background, you get your prompt back 😄

Installation
------------

You can install launcheR from github with:

``` r
# install.packages("devtools")
devtools::install_github("qfazille/launcheR")
```

The management of waiting queue & waiting batch can be shared by multiple users if you all use the same machine.

Basic example
-------------

This creates a queue containing 2 batchs

``` r
library(launcheR)

# create queue
q <- queue()

# Add batchs script1.R & script2.R
q <- add.batch(queue = q, path = "/path/to/script1.R")
q <- add.batch(queue = q, path = "/path/to/script2.R")

# launch queue
launch.queue(q)
```

Using arguments on a batch
--------------------------

Add a batch with arguments (parameters)

``` r
# Some random values in params list (must be named)
username <- "mylogin"
country <- "france"
age <- 99
other <- list(a = 1, b = "hello", d = "world")

# params must be a named list
params <- list(username = username, country = country, age = age, other = other)

# Add batch script1.R with those parameters
q <- add.batch(queue = q, path = "/path/to/script1.R", params = params)
```

Launch queue on same group
--------------------------

**NB : ** Both users are on the same machine

**user1**

``` r
library(launcheR)

# create queue on group 'demo'
q <- queue(group = "demo")

# Add batchs script1.R & script2.R
q <- add.batch(queue = q, path = "./script1.R")
q <- add.batch(queue = q, path = "./script2.R")

# Launch
launch.queue(q)
```

**user2**

``` r
library(launcheR)

# create queue on group 'demo'
q <- queue(group = "demo")

# Add batchs script1.R & script3.R
q <- add.batch(queue = q, path = "./script1.R")
q <- add.batch(queue = q, path = "./script3.R")

# Launch
launch.queue(q)
```
