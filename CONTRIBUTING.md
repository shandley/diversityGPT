# Contributing to diversityGPT

Thank you for your interest in contributing to diversityGPT! This document provides guidelines and instructions for contributing to the project.

## Code of Conduct

By participating in this project, you agree to abide by our Code of Conduct:

- **Be respectful**: Treat all participants with respect and consideration
- **Be collaborative**: Work together towards improving the project
- **Be inclusive**: Welcome newcomers and help them get started
- **Be professional**: Focus on constructive criticism and solutions

## Ways to Contribute

### 1. Report Bugs

Found a bug? Please [open an issue](https://github.com/shandley/diversityGPT/issues/new) with:

- A clear, descriptive title
- Steps to reproduce the issue
- Expected vs actual behavior
- System information (R version, OS, package versions)
- Minimal reproducible example (reprex)

Example:
```r
# Good bug report includes:
library(diversityGPT)
library(phyloseq)

# Minimal data that reproduces issue
data(GlobalPatterns)
small_gp <- prune_taxa(taxa_names(GlobalPatterns)[1:10], GlobalPatterns)

# Code that triggers the bug
result <- extract_universal_information(small_gp)  # Error: ...
```

### 2. Suggest Features

Have an idea? [Open a feature request](https://github.com/shandley/diversityGPT/issues/new) with:

- Clear description of the feature
- Use cases and benefits
- Potential implementation approach
- Examples of similar features elsewhere

### 3. Improve Documentation

Documentation improvements are always welcome:

- Fix typos or clarify explanations
- Add examples to function documentation
- Write tutorials or blog posts
- Improve vignettes

### 4. Contribute Code

#### Setup Development Environment

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/diversityGPT.git
   cd diversityGPT
   ```

3. Install development dependencies:
   ```r
   install.packages(c("devtools", "testthat", "roxygen2", "lintr", "covr"))
   devtools::install_deps(dependencies = TRUE)
   ```

4. Create a new branch:
   ```bash
   git checkout -b feature/your-feature-name
   ```

#### Development Workflow

1. **Write code** following our style guide (see below)
2. **Add tests** for new functionality
3. **Document** your functions with roxygen2
4. **Check** your changes:
   ```r
   devtools::check()     # Full package check
   devtools::test()      # Run tests
   lintr::lint_package() # Check code style
   ```

#### Code Style Guide

We follow the [tidyverse style guide](https://style.tidyverse.org/) with these specifics:

- Use `snake_case` for function and variable names
- Use explicit returns in functions
- Comment your code generously
- Keep lines under 80 characters when possible

Example:
```r
#' Calculate diversity metric
#'
#' @param physeq A phyloseq object
#' @param metric Character string naming the metric
#' 
#' @return Numeric vector of diversity values
#' @export
#'
#' @examples
#' data(GlobalPatterns)
#' calculate_diversity_metric(GlobalPatterns, "shannon")
calculate_diversity_metric <- function(physeq, metric = "shannon") {
  # Validate inputs
  if (!inherits(physeq, "phyloseq")) {
    stop("Input must be a phyloseq object", call. = FALSE)
  }
  
  # Calculate metric
  result <- switch(metric,
    shannon = diversity(physeq, index = "shannon"),
    simpson = diversity(physeq, index = "simpson"),
    stop("Unknown metric: ", metric, call. = FALSE)
  )
  
  return(result)
}
```

#### Testing Guidelines

Write tests for all new functions:

```r
# In tests/testthat/test-your-function.R
test_that("calculate_diversity_metric works correctly", {
  data(GlobalPatterns)
  
  # Test basic functionality
  result <- calculate_diversity_metric(GlobalPatterns, "shannon")
  expect_length(result, nsamples(GlobalPatterns))
  expect_true(all(result >= 0))
  
  # Test error handling
  expect_error(
    calculate_diversity_metric("not_phyloseq", "shannon"),
    "must be a phyloseq object"
  )
  
  expect_error(
    calculate_diversity_metric(GlobalPatterns, "invalid"),
    "Unknown metric"
  )
})
```

### Pull Request Process

1. **Update documentation**:
   ```r
   devtools::document()  # Generate man pages
   ```

2. **Run checks**:
   ```r
   devtools::check()     # Should pass with 0 errors, 0 warnings
   devtools::test()      # All tests should pass
   ```

3. **Update NEWS.md** with your changes

4. **Commit** with clear messages:
   ```bash
   git add .
   git commit -m "Add universal metric validation function
   
   - Validates transformation matrix structure
   - Checks R² thresholds
   - Returns detailed diagnostics"
   ```

5. **Push** to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

6. **Open Pull Request** with:
   - Clear title and description
   - Reference any related issues
   - Summary of changes
   - Screenshots if applicable

### Review Process

Your PR will be reviewed for:

1. **Functionality**: Does it work as intended?
2. **Tests**: Are changes adequately tested?
3. **Documentation**: Is it well documented?
4. **Style**: Does it follow our guidelines?
5. **Performance**: No significant slowdowns?

## Project Structure

```
diversityGPT/
├── R/                      # R function files
│   ├── universal-info.R    # Core transformation functions
│   ├── visualization.R     # Plotting functions
│   ├── datasets.R          # Dataset management
│   └── ...
├── man/                    # Documentation (auto-generated)
├── tests/                  # Test files
│   └── testthat/
├── vignettes/             # Long-form documentation
├── inst/                  # Installed files
│   ├── shiny/            # Shiny applications
│   └── data/             # Package data
├── data-raw/             # Scripts to prepare data
└── DESCRIPTION           # Package metadata
```

## Adding New Features

### 1. New Diversity Metrics

To add a new diversity metric:

1. Add calculation in `R/diversity-metrics.R`
2. Update transformation matrix calculation
3. Add tests in `tests/testthat/test-diversity-metrics.R`
4. Update documentation

### 2. New Visualizations

To add new plot types:

1. Create function in `R/visualization.R`
2. Follow ggplot2 conventions
3. Return ggplot object (allow customization)
4. Add examples in documentation

### 3. New Datasets

To add datasets to the registry:

1. Update `R/dataset_registry.R`
2. Add processing script in `data-raw/`
3. Document data source and processing
4. Add citation information

## Getting Help

- **Questions**: Open a [discussion](https://github.com/shandley/diversityGPT/discussions)
- **Bugs**: Open an [issue](https://github.com/shandley/diversityGPT/issues)
- **Chat**: Join our [Slack workspace](https://diversitygpt.slack.com) (coming soon)

## Recognition

Contributors will be:
- Listed in the package AUTHORS file
- Acknowledged in release notes
- Credited in relevant documentation

## License

By contributing, you agree that your contributions will be licensed under the same MIT License that covers the project.

Thank you for helping make diversityGPT better! 🎉