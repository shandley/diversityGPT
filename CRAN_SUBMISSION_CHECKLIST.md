# CRAN Submission Checklist for diversityGPT

## Pre-submission Checks

### Package Structure ✓
- [x] DESCRIPTION file complete with all required fields
- [x] NEWS.md documenting changes 
- [x] inst/CITATION file for proper citation
- [x] LICENSE file (MIT)
- [x] .Rbuildignore excludes non-package files
- [x] No non-standard files in package root

### Documentation ✓
- [x] All exported functions have complete documentation
- [x] Examples provided for main functions
- [x] Examples use small datasets that run quickly (<5s)
- [x] Vignettes included:
  - [x] Introduction vignette
  - [x] Universal framework vignette  
  - [x] Ecological intelligence vignette
- [x] README.md with installation instructions

### Dependencies ✓
- [x] All dependencies declared in DESCRIPTION
- [x] Version requirements specified where needed
- [x] No unnecessary dependencies
- [x] phyloseq (Bioconductor) documented in README

### Testing
- [ ] Test coverage >95% (run check_test_coverage.R)
- [ ] All tests pass locally
- [ ] Tests work without suggested packages

### R CMD check
- [ ] No ERRORs
- [ ] No WARNINGs  
- [ ] No NOTEs (except "new submission")
- [ ] Check passes on multiple platforms:
  - [ ] Local development machine
  - [ ] win-builder (devel and release)
  - [ ] R-hub checks

### Code Quality
- [ ] No T/F instead of TRUE/FALSE
- [ ] No `library()` or `require()` in functions
- [ ] Proper error messages with `stop()`
- [ ] Informative messages with `message()` or `cli`
- [ ] No `print()` or `cat()` to console (except in `print` methods)

### Examples
- [ ] All examples run without errors
- [ ] Examples that require API keys wrapped in `\dontrun{}`
- [ ] Examples complete in reasonable time (<10s each)
- [ ] Examples demonstrate key functionality

### Platform Compatibility  
- [ ] No OS-specific code without proper conditionals
- [ ] File paths use `file.path()` not paste
- [ ] No hard-coded paths

## Submission Process

1. **Final local check**:
   ```r
   devtools::check()
   ```

2. **Check on win-builder**:
   ```r
   devtools::check_win_devel()
   devtools::check_win_release()
   ```

3. **Check on R-hub**:
   ```r
   rhub::check_for_cran()
   ```

4. **Update cran-comments.md** with check results

5. **Submit to CRAN**:
   ```r
   devtools::release()
   ```

## Post-submission

- [ ] Monitor email for CRAN feedback
- [ ] Address any issues promptly (<2 weeks)
- [ ] Update GitHub repo with CRAN badge when accepted
- [ ] Announce release on social media/mailing lists

## Common CRAN Feedback Points

1. **Examples too slow**: Ensure all run in <10s
2. **Writing to user directories**: Never write outside tempdir()
3. **Modifying options/par**: Always reset on exit
4. **Internet resources**: Examples shouldn't require internet
5. **Large data**: Keep package size reasonable (<5MB)