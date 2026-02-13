
[![R-CMD-check](https://github.com/ssjerf-stack/modelscompete4/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ssjerf-stack/modelscompete4/actions/workflows/R-CMD-check.yaml)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# modelscompete4

**Compare Nested and Non‑Nested Structural Equation Models**  
Built on `lavaan` – automatic nesting detection, chi‑square difference
tests, Vuong tests, and comprehensive fit indices.

------------------------------------------------------------------------

## Installation

``` r
# install.packages("remotes")
remotes::install_github("ssjerf-stack/modelscompete4")
```

------------------------------------------------------------------------

## Quick Start

``` r
library(modelscompete4)
library(lavaan)

# Example data
data(HolzingerSwineford1939)

# Define models as named list of lavaan syntax strings
model_list <- list(
  "OneFactor" = "g =~ x1 + x2 + x3 + x4 + x5 + x6",
  "TwoFactor" = "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
)

# Compare them – the function fits the models automatically
result <- compare_models(model_list, data = HolzingerSwineford1939)

# Print summary
print(result)

# Access detailed results
result$fit_table          # fit indices for all models
result$comparison_matrix  # pairwise test results
```

------------------------------------------------------------------------

## Features

- [x] **Nested models** → chi‑square difference test (`lavTestLRT`)
- [x] **Non‑nested models** → Vuong test (`nonnest2::vuongtest`)
- [x] **Fit indices**: AIC, BIC, CFI, TLI, RMSEA, SRMR, χ², df, p‑value
- [x] **Automatic model naming** – use your own names
- [x] **Bootstrap support** – standard errors and confidence intervals
- [x] **S3 methods** – `print()`, `summary()`

------------------------------------------------------------------------

## Advanced Example

``` r
# Three non‑nested models with different factor structures
models <- list(
  "M1" = "F1 =~ x1 + x2 + x3",
  "M2" = "F1 =~ x1 + x2 + x3 + x4",
  "M3" = "
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  "
)

result <- compare_models(models, data = HolzingerSwineford1939)
print(result)
```

------------------------------------------------------------------------

## Output Structure

The returned object of class `modelscompete4` contains:

| Component            | Description                                |
|----------------------|--------------------------------------------|
| `$fit_list`          | List of fitted `lavaan` objects            |
| `$fit_table`         | Data frame with fit indices for all models |
| `$comparison_matrix` | Matrix with pairwise test results          |
| `$test_results`      | Detailed statistical tests (LRT / Vuong)   |
| `$bootstrap_summary` | Bootstrapped estimates (if requested)      |

------------------------------------------------------------------------

## License

This package is licensed under the **GNU General Public License
v3.0**.  
See the [LICENSE](LICENSE) file for details.
