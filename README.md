
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statguide

<!-- badges: start -->

<!-- badges: end -->

`statguide` is an R package designed to be used as a guidance tool, to
help beginners choose, understand, and run basic statistical tests.  
It is designed to support learning and decision-making, not to replace
statistical judgement.

It guides the user through four steps:

1.  **Identify variable types**  
2.  **Choose an appropriate statistical test**  
3.  **Explain *why* that test was chosen**  
4.  **Generate diagnostic plots and run the test**

The goal is to make statistical reasoning transparent, accessible, and
reproducible.

## Overview of the workflow

The package handles a limited set of introductory-level scenarios and
requires the user to explicitly specify the outcome and predictor
variables, ensuring that no incorrect assumptions are made about
variable roles.

The package is built around a simple, consistent pipeline:

- choose_test(data, outcome, predictor)  
- explain_choice(data, outcome, predictor)  
- plot_diagnostics(data, outcome, predictor)  
- run_test(data, outcome, predictor)

Each function focuses on one clear task.

## Installation

You can install the development version of statguide from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("adves182/statguide")
```

## Example

Here is a complete example using the built‑in `PlantGrowth` dataset:

``` r
library(statguide)

# Inspect the data
head(PlantGrowth)

# Step 1: Choose a test
choose_test(PlantGrowth, outcome = "weight", predictor = "group")

# Step 2: Explain the reasoning
explain_choice(PlantGrowth, "weight", "group")

# Step 3: Visual diagnostics
plots <- plot_diagnostics(PlantGrowth, "weight", "group")
plots$boxplot
plots$qq

# Step 4: Run the test
run_test(PlantGrowth, "weight", "group")
```

## Why use `statguide`?

Beginners may struggle with:

- choosing the right test
- understanding why it is appropriate
- checking assumptions
- interpreting the results

This package aims to simplify these steps and guide the user through the
process.

## Supported tests

Currently, `statguide` supports:

- t-test  
- ANOVA  
- correlation  
- chi-square test  
- logistic regression
