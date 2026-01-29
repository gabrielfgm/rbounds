# Coding Plan for rbounds Package

This document outlines the plan for improving the existing `rbounds` package and extending it to support bounds estimation on Average Treatment Effects (ATE).

## Overview

The `rbounds` package currently provides:
- `pidoutcomes()`: Estimates bounds on conditional means with missing outcomes
- S3 methods for printing and plotting rbounds objects
- Confidence interval calculations based on Imbens-Manski (2004)

## Part 1: Clean Up Existing Functions

### 1.1 Fix S3 Method Signatures

**File:** `R/S3_methods.R`

**Issues:**
- `print.rbounds(m)` uses non-standard parameter name `m` instead of `x`
- `plot.rbounds(m)` uses non-standard parameter name `m` instead of `x`
- `print.rbounds()` does not return the object invisibly (standard for print methods)
- Missing `...` parameter in both methods (required by S3 generics)

**Fix:**
```r
print.rbounds <- function(x, ...) {
  # ... existing code with m -> x ...
  invisible(x)
}

plot.rbounds <- function(x, ...) {
  # ... existing code with m -> x ...
}
```

### 1.2 Fix Confidence Interval Calculation

**File:** `R/confidence_intervals.R` and `R/estimate_bounds.R`

**Issues:**
- Sample size `N` is hardcoded to `1` in the call to `conf_int_bounds()`
- This makes the confidence intervals independent of sample size
- Should pass actual sample size from data

**Fix:**
- Pass `nrow(data)` to the confidence interval function
- Consider whether bandwidth-adjusted effective sample size is more appropriate

### 1.3 Fix Standard Error Estimation

**File:** `R/estimate_bounds.R`

**Issues:**
- Variance formula for bounds may be incorrect
- Current formula: `sigma_l <- sigma^2 * p + mu^2 * p * not_p`
- Need to verify against delta method derivation (see `econometric_models.md`)
- The `sigma` from `np::se()` is for the regression, but we also need variance of `p`

**Fix:**
- Add variance estimation for the probability model `np_missing`
- Correctly apply delta method for product of estimated quantities
- Consider covariance terms if using joint estimation

### 1.4 Clean Up Code Style

**Files:** All R files

**Issues:**
- Inconsistent spacing and formatting
- `capture.output(mes <- ...)` pattern is clunky
- Some commented-out code in `test_data.R`

**Fix:**
- Use `suppressMessages()` instead of `capture.output()` for cleaner output suppression
- Move test data to proper test files
- Add consistent roxygen documentation

### 1.5 Fix DESCRIPTION Typo

**File:** `DESCRIPTION`

**Issue:** "implimentation" should be "implementation"

## Part 2: Fix README Rendering

### 2.1 LaTeX Rendering on GitHub

**File:** `README.Rmd`

**Issue:** GitHub markdown does not render LaTeX math in display mode. The current `\[...\]` syntax may not render properly.

**Options:**
1. Use GitHub's math rendering with `$$...$$` syntax (now supported)
2. Pre-render equations as images
3. Use simpler inline notation that renders better

**Recommended Fix:**
- Update to use `$$...$$` for display math
- Verify equations render on GitHub after pushing
- The current README.md seems to render reasonably but could be improved

### 2.2 Re-knit README

**Process:**
1. Open `README.Rmd` in RStudio
2. Verify `knitr` and required packages installed
3. Knit to regenerate `README.md` and plot

## Part 3: Extend to ATE Bounds Estimation

### 3.1 New Function: `ate_bounds()`

**Purpose:** Estimate Manski bounds on Average Treatment Effects

**Signature:**
```r
ate_bounds <- function(
  formula,          # outcome ~ covariates
  treatment,        # name of treatment variable
  data,             # data frame
  y_min = 0,        # minimum possible outcome value
  y_max = 1,        # maximum possible outcome value
  alpha = 0.95,     # confidence level
  ...               # passed to np functions
)
```

**Returns:** Object of class `rbounds_ate` containing:
- `lower`: lower bound on ATE at each covariate value
- `upper`: upper bound on ATE at each covariate value
- `lower_ci`: lower confidence interval endpoint
- `upper_ci`: upper confidence interval endpoint
- `ate_lower`: average lower bound (averaged over X)
- `ate_upper`: average upper bound (averaged over X)
- Intermediate estimates for diagnostics

### 3.2 Implementation Steps

1. **Parse inputs and validate:**
   - Check treatment is binary
   - Check outcome is within [y_min, y_max]
   - Extract formula components

2. **Estimate conditional means:**
   - `E[Y|D=1, X]` using `np::npreg` on treated subsample
   - `E[Y|D=0, X]` using `np::npreg` on control subsample

3. **Estimate treatment probabilities:**
   - `P(D=1|X)` using `np::npreg` (propensity score)

4. **Compute bounds on potential outcomes:**
   ```r
   # E[Y(1)] bounds
   ey1_lower <- ey_d1 * p_d1 + y_min * (1 - p_d1)
   ey1_upper <- ey_d1 * p_d1 + y_max * (1 - p_d1)

   # E[Y(0)] bounds
   ey0_lower <- y_min * p_d1 + ey_d0 * (1 - p_d1)
   ey0_upper <- y_max * p_d1 + ey_d0 * (1 - p_d1)
   ```

5. **Compute ATE bounds:**
   ```r
   ate_lower <- ey1_lower - ey0_upper
   ate_upper <- ey1_upper - ey0_lower
   ```

6. **Compute confidence intervals:**
   - Derive variance formulas using delta method
   - Apply Imbens-Manski CI construction

### 3.3 New S3 Methods

**File:** `R/S3_methods.R`

Add methods for class `rbounds_ate`:
- `print.rbounds_ate()`: Display average bounds and CIs
- `plot.rbounds_ate()`: Visualize bounds over observations or covariates
- `summary.rbounds_ate()`: Detailed summary with component estimates

### 3.4 Update NAMESPACE

Export new functions:
```r
export(ate_bounds)
S3method(print, rbounds_ate)
S3method(plot, rbounds_ate)
S3method(summary, rbounds_ate)
```

### 3.5 Add Tests

**File:** `tests/testthat/test_ate_bounds.R`

Test cases:
1. Basic functionality with simulated data
2. Edge cases (all treated, all control)
3. Bounds contain true ATE in simulations
4. Confidence interval coverage
5. Input validation errors

### 3.6 Documentation

- Add roxygen documentation to `ate_bounds()`
- Update README with ATE example
- Add vignette explaining the methodology

## Part 4: Implementation Order

### Phase 1: Bug Fixes (Priority: High)
1. Fix S3 method signatures
2. Fix confidence interval N parameter
3. Fix standard error calculation
4. Fix DESCRIPTION typo

### Phase 2: Code Quality (Priority: Medium)
1. Clean up output suppression
2. Move test code to proper tests
3. Add missing tests for `pidoutcomes()`

### Phase 3: ATE Extension (Priority: High)
1. Create `ate_bounds()` function
2. Add S3 methods for new class
3. Write tests
4. Update documentation

### Phase 4: Documentation (Priority: Medium)
1. Re-render README
2. Improve math notation for GitHub
3. Add examples to function documentation

## Testing Strategy

1. **Unit tests** for helper functions (`conf_int_bounds`, `get_c_consts`)
2. **Integration tests** for main functions with simulated data
3. **Coverage tests** ensure bounds contain true parameter in repeated simulations
4. **Edge case tests** for boundary conditions and error handling

## Dependencies

Current:
- `np`: Non-parametric kernel regression
- `ggplot2`: Plotting

No new dependencies required for ATE extension.
