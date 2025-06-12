# Placeholder and Hardcoded Value Fixes Summary

## Date: December 2024

### Overview
During code review, we identified several placeholder implementations and hardcoded values that could produce misleading results. This document summarizes the fixes applied to ensure the package returns honest, accurate calculations.

## Critical Fixes Applied

### 1. Faith's Phylogenetic Diversity (Fixed ✓)
**File:** `R/calculate_diversity.R`
**Issue:** Faith's PD was calculated as simple species richness (count of present species)
**Fix:** Now properly uses `picante::pd()` for actual phylogenetic diversity calculation
**Behavior:** Returns NA with warning if picante package is not installed

### 2. Universal Component Defaults (Fixed ✓)
**File:** `R/universal_transformations.R`
**Issue:** When transformation matrix unavailable, returned hardcoded 0.25 for all R,E,P,S components
**Fix:** Now returns NA values with appropriate warning
**Impact:** Prevents misleading "equal contribution" results

### 3. Information Theory Components (Fixed ✓)
**File:** `R/information_theory.R`
**Issue:** Used random synthetic data (rnorm) instead of actual component values
**Fix:** Now extracts real components from universal_information$information_components
**Behavior:** Returns NA if components not available

### 4. Assembly Mechanism P-values (Fixed ✓)
**File:** `R/ecological_intelligence.R`
**Issue:** Hardcoded p-value of 0.01 for environmental filtering
**Fix:** Uses actual p-values from correlation tests
**Behavior:** Returns NA if p-value not calculable

## Remaining Issues (Lower Priority)

### 5. Beta Diversity Spatial Parameters
**File:** `R/experimental/beta_diversity_core.R`
**Issue:** Hardcoded max_dist = 100 for spatial calculations
**Status:** Not fixed - experimental code
**Recommendation:** Estimate from actual spatial extent of data

### 6. Beta Diversity Phylogenetic Fallback
**File:** `R/experimental/beta_diversity_core.R`
**Issue:** Returns 0.5 as placeholder for non-information methods
**Status:** Not fixed - experimental code
**Recommendation:** Implement proper UniFrac calculation

## Testing Recommendations

1. **Always test with real data** - avoid using only simulated datasets
2. **Check for variation** - if all samples have identical values, investigate
3. **Verify dependencies** - ensure required packages (picante) are installed
4. **Compare methods** - Faith's PD should differ from observed richness
5. **Inspect NA values** - NA is more honest than a placeholder calculation

## Code Patterns to Avoid

```r
# BAD: Placeholder calculation
values <- sum(x > 0)  # "Simplified" Faith's PD

# GOOD: Proper calculation or NA
if (requireNamespace("picante", quietly = TRUE)) {
  values <- picante::pd(otu_mat, tree)$PD
} else {
  values <- rep(NA, n_samples)
}
```

```r
# BAD: Hardcoded "equal" components
R_component = rep(0.25, n_samples)

# GOOD: Return NA if cannot calculate
R_component = rep(NA_real_, n_samples)
```

## Impact on Package Functionality

These fixes ensure:
1. **Transparency** - Users know when calculations cannot be performed
2. **Accuracy** - No misleading results from placeholder values
3. **Reproducibility** - Results reflect actual data, not hardcoded values
4. **Trust** - Package provides honest assessment of what it can calculate

## Future Development

When implementing new features:
1. Never use placeholder calculations in production code
2. Always return NA with informative warning if calculation impossible
3. Document any simplifying assumptions clearly
4. Use TODO/FIXME comments for incomplete implementations
5. Validate against known test cases

## Verification

Run `test_real_calculations.R` to verify all fixes are working:
```r
source("test_real_calculations.R")
```

This ensures the package now provides reliable, non-placeholder results.