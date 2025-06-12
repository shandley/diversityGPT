# Final Hardcoded Values and Dependencies Report

## Date: December 2024

## Summary of Fixes Applied

### ✅ Fixed High-Priority Issues

1. **Placeholder P-values in Ecological Intelligence**
   - **Fixed:** Changed hardcoded p-values (0.05, 0.1) to `NA_real_`
   - **Files:** `R/ecological_intelligence.R` (lines 253, 300, 345)
   - **Impact:** No more misleading statistical significance claims

2. **Faith's Phylogenetic Diversity**
   - **Fixed:** Now uses `picante::pd()` or returns NA
   - **File:** `R/calculate_diversity.R`
   - **Impact:** Accurate phylogenetic diversity calculations

3. **Universal Component Fallbacks**
   - **Fixed:** Returns NA instead of hardcoded 0.25 values
   - **File:** `R/universal_transformations.R`
   - **Impact:** No fake "equal contribution" results

4. **Information Theory Synthetic Data**
   - **Fixed:** Uses real component data instead of `rnorm()`
   - **File:** `R/information_theory.R`
   - **Impact:** Actual taxa-component relationships

### ✅ Created Infrastructure

1. **Dependency Management System**
   - `check_dependencies()` - Check installation status
   - `setup_diversitygpt()` - One-time setup wizard
   - `safe_use_package()` - Safe optional package usage
   - **File:** `R/check_dependencies.R`, `R/utils_dependencies.R`

2. **Configuration Parameter System**
   - `get_config_parameters()` - Centralized parameters
   - All magic numbers documented with references
   - **File:** `R/config_parameters.R`

3. **Development Guidelines**
   - No-placeholder policy in CLAUDE.md
   - Clear rules for handling missing calculations

## Remaining Issues by Priority

### 🔴 High Priority (Should fix before CRAN)

1. **Spatial Distance Calculation**
   ```r
   # File: R/beta_diversity_decomposition.R, line 345
   max_dist <- 10  # Placeholder - should be based on study extent
   ```
   **Fix needed:** Calculate from actual spatial extent of data

2. **Arbitrary Ecological Thresholds**
   ```r
   # File: R/ecological_intelligence.R
   if (total_var > 0.5 && mean_quality < 0.6)  # line 296
   if (mean_S > 0.3)  # line 343 (now documented but still arbitrary)
   ```
   **Fix needed:** Base on ecological literature or make user-configurable

### 🟡 Medium Priority

1. **Missing requireNamespace Error Handling**
   - Multiple files use `requireNamespace()` without proper fallback
   - **Fix needed:** Use new `safe_use_package()` wrapper

2. **TODO/FIXME Comments**
   - Several files contain unresolved TODO comments
   - **Fix needed:** Resolve or remove before CRAN submission

3. **Shapley Calculation Limits**
   ```r
   # File: R/shapley_values.R
   if (method == "exact" && length(selected_taxa) > 12)
   ```
   **Fix needed:** Make configurable or document limitation

### 🟢 Low Priority (Post-CRAN)

1. **Visualization Parameters**
   - Node sizes, color thresholds hardcoded
   - Generally acceptable but could be configurable

2. **Bootstrap Iterations**
   - Default 1000 is reasonable but could be adaptive

## Verification Checklist

### Before CRAN Submission:
- [ ] Run `test_real_calculations.R` - Verify no placeholders
- [ ] Run `check_dependencies()` - Ensure clean dependency state
- [ ] Search for remaining TODOs: `grep -r "TODO\|FIXME" R/`
- [ ] Check for hardcoded p-values: `grep -r "p_value.*=.*0\.[0-9]" R/`
- [ ] Verify all functions return NA (not fake values) when they can't calculate

### Package Quality Assurance:
```r
# 1. Check dependencies
check_dependencies(verbose = TRUE)

# 2. Run comprehensive tests
devtools::test()

# 3. Check for remaining placeholders
source("test_real_calculations.R")

# 4. R CMD check
devtools::check()
```

## Best Practices Going Forward

1. **Never use placeholder calculations**
   - Return NA with informative warning
   - Document what's needed for proper calculation

2. **Document all thresholds**
   - Use `config_parameters.R` for centralized values
   - Cite literature sources when possible

3. **Handle missing packages gracefully**
   - Use `safe_use_package()` wrapper
   - Provide clear installation instructions

4. **Test with real data**
   - Avoid relying only on simulated examples
   - Check for suspicious patterns (all equal values, etc.)

## Critical Functions to Monitor

These functions previously had placeholders and should be carefully tested:

1. `calculate_diversity()` - Faith's PD calculation
2. `extract_universal_information()` - Component extraction
3. `calculate_taxa_mutual_information()` - Information theory
4. `detect_assembly_mechanisms()` - P-value calculations
5. Beta diversity functions in experimental/

## Conclusion

The package has been significantly improved with:
- Removal of critical placeholder calculations
- Robust dependency management system
- Clear development guidelines against hardcoded values
- Centralized configuration parameters

Remaining issues are mostly medium/low priority and can be addressed in future releases. The package now provides honest, transparent results with clear communication when calculations cannot be performed.