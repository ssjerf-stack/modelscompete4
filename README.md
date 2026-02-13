---
title: "modelscompete4"
output: github_document
---

# modelscompete4: Compare SEM Models

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

An R package for comparing multiple Structural Equation Models (SEM), supporting both nested and non-nested model comparisons with comprehensive fit indices and statistical tests.

## Installation

### From GitHub (Recommended)
```r
# Install from GitHub
# devtools::install_github("ssjerf-stack/modelscompete4")
```

## From Local Source

```r
# Install from local package file
install.packages("modelscompete4_0.1.0.tar.gz", repos = NULL, type = "source")
```
## Quick Start

```r
library(modelscompete4)
library(lavaan)

# Load example data
data(HolzingerSwineford1939)

# Fit a simple CFA model
model <- 'visual =~ x1 + x2 + x3'
fit <- cfa(model, data = HolzingerSwineford1939)

# Extract latent parameters
results <- extract_latent_parameters(fit)
print(results)
```

## Features

Extract latent variable parameters from lavaan models

Calculate fit indices for model comparison

Export results to data frames for further analysis

Compatible with all lavaan model types (cfa, sem, growth)

## Vignette

For detailed examples and usage, see the package vignette:

```r
# View the vignette
vignette("getting-started", package = "modelscompete4")
```

## Functions

extract_latent_parameters() - Extract parameters from lavaan models
Additional functions for model comparison and analysis

## Author

Jerf Yeung

## License

MIT License
