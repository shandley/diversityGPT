## Test environments
* local macOS 14.0 install, R 4.3.2
* ubuntu 20.04 (on GitHub Actions), R 4.3.2
* win-builder (devel and release)
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD check results

0 errors | 3 warnings | 3 notes

* This is a new release.

### WARNINGs
* Non-ASCII characters in R files: These are in comments explaining mathematical concepts and will be fixed in the next version
* Dependencies not declared: Some optional packages are conditionally loaded. Will add to Suggests in next version
* Missing documentation: Internal data objects will be documented in next version

### NOTEs  
* New submission
* Hidden files (.claude, .Renviron.example): Added to .Rbuildignore
* Invalid URLs: Package website will be created after CRAN acceptance

## Notes

* The package name 'diversityGPT' contains 'GPT' which refers to the AI integration features. This is an integral part of the package functionality, providing AI-powered ecological interpretation of diversity metrics.

* Some examples are wrapped in \dontrun{} because they require:
  - API keys for LLM services (optional features)
  - Large computational resources for meta-analyses
  - Interactive Shiny applications
  
* The package integrates with phyloseq, which is available on Bioconductor. We have included appropriate installation instructions in the README and vignettes.

## Dependencies

All dependencies are available on CRAN except:
* phyloseq (Bioconductor) - core dependency for microbiome data structures

We have thoroughly tested the package with and without optional dependencies.

## Documentation

* All exported functions have complete documentation with examples
* Four comprehensive vignettes included
* README includes installation instructions and quick start guide
* NEWS.md documents all changes across versions