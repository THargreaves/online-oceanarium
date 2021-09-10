# Online Oceanarium

[![Language](https://img.shields.io/badge/Language-R_(4.1.0%2B)-orange.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/THargreaves/online-oceanarium/workflows/R-CMD-check/badge.svg)](https://github.com/THargreaves/online-oceanarium/actions)
[![Documentation](https://img.shields.io/badge/docs-stable-blue.svg)](http://thargreaves.github.io/online-oceanarium/)
[![Code Quality](https://github.com/THargreaves/online-oceanarium/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter)
[![Code Coverage](https://img.shields.io/codecov/c/github/THargreaves/online-oceanarium)](https://app.codecov.io/gh/THargreaves/online-oceanarium/)

## Overview

![Online Oceanarium Logo](https://user-images.githubusercontent.com/38204689/118853426-a6b9d300-b8cb-11eb-97f1-6420cd8c59a1.png)

Online Oceanarium is an esoteric R package collecting examples of [online algorthms](https://en.wikipedia.org/wiki/Online_algorithm) (also known as streaming algorithms).

In short, online algorithms process their input one piece at a time; a desirable property when processing an ammount of data that is too large to store in memory.
This is also useful when data is streamed from a sensor or updating data source and you wish to update your algorithms state without running from scratch.

The focus of this package is largely on online algorithms used for:

- Machine learning (including reinforcement learning)
- Statistics
- Data processing

We hope that this package can be used for both the practical application of online algorithms as well as a demonstration of the implementation of such methods for educational purposes.

## Package Design

The package is composed of multiple [R6 classes](https://adv-r.hadley.nz/r6.html), each corresponding to a particular online algorithm. An instance of these classes is called a _streamer_ and has three core public methods: `initialize`, `update`, `value`.
Respectively, these are used to create a new streamer with initial values (by calling `<class>$new()`), update the state of the streamer by providing new data, and return the current value of the algorithm.

For example, a simple streamer for calculating a running mean is below (note `CMA` stands for cumulative moving average).

```r
library(onlineoceanarium)
mean <- CMA$new(c(1, 2))
mean$value()
#> [1] 1.5
mean$update(c(3, 4))
mean$value()
#> [1] 2.5
```

## Contributing

Contributions to the package are welcome. Before writing code, we suggest opening an issue detailing the algorithm you wish to implement or selecting an already open issue.

Please ensure that all contributions are documented, have full coverage with unit tests, and follow the style guide set out in `.lintr`.
