# diversityGPT R Package Development

## Project Overview
**diversityGPT** is an R package that solves the critical "decision confusion" problem in microbiome diversity analysis by combining advanced statistical synthesis with AI-powered interpretation. When researchers get conflicting results from multiple diversity metrics (Shannon vs Simpson, Bray-Curtis vs UniFrac), this package provides intelligent synthesis and biological interpretation.

## Current Development Phase
**Phase 1: Foundation (MVP) - Starting**
- Core diversity calculations with Hill number framework
- Basic LLM API integration for result interpretation
- Simple consensus algorithms for metric synthesis
- Pattern recognition framework

## Technology Stack
- **Language**: R (>=4.0.0)
- **Core Dependencies**: phyloseq, vegan, hilldiv, mixOmics, FactoMineR
- **LLM Integration**: httr2, jsonlite, openai R package
- **Statistical Methods**: boot, MCMCpack for advanced analyses
- **Documentation**: roxygen2, pkgdown for package documentation
- **Testing**: testthat for comprehensive test coverage

## Package Architecture

### Core Components
1. **Diversity Analysis Engine**
   - `diversity_suite()`: Main function for unified analysis
   - Hill number profiles (q=0,1,2 and continuous)
   - Multi-metric calculation and storage

2. **Consensus Analysis Module**
   - `consensus_diversity()`: Resolve conflicting metrics
   - Custom algorithms for metric synthesis
   - Adaptive weighting based on reliability

3. **LLM Integration Layer**
   - `configure_llm()`: Set up AI provider
   - `interpret_results()`: Context-aware interpretation
   - `generate_hypotheses()`: Pattern-based insights

4. **Pattern Recognition System**
   - `pattern_detective()`: Identify diversity signatures
   - Common pattern library (e.g., "high richness, low evenness")
   - Biological mechanism suggestions

## Development Principles

### R Package Best Practices
- Follow tidyverse style guide
- Use S3 classes for diversity results objects
- Implement print, plot, and summary methods
- Comprehensive documentation with examples
- Vignettes for common workflows

### Code Organization
```
diversityGPT/
├── R/                   # Function definitions
│   ├── diversity-core.R
│   ├── consensus-algorithms.R
│   ├── llm-integration.R
│   └── pattern-recognition.R
├── man/                 # Documentation (auto-generated)
├── tests/              # Test suite
├── vignettes/          # User guides
├── data/               # Example datasets
└── inst/               # Additional files
```

### Function Design Philosophy
- **Modular**: Each function does one thing well
- **Pipeable**: Compatible with %>% workflows
- **Flexible**: Sensible defaults with customization options
- **Interpretable**: Clear parameter names and outputs
- **Validated**: Input checking and informative errors

## Key Implementation Tasks

### Phase 1 Priorities
1. **Core Diversity Framework**
   - Implement Hill number calculations
   - Create diversity result S3 class
   - Basic plotting methods

2. **Simple Consensus Algorithm**
   - Weighted averaging with bootstrap CI
   - Basic conflict detection
   - Summary statistics

3. **LLM API Wrapper**
   - OpenAI API integration
   - Result formatting for LLM context
   - Response parsing and display

4. **Pattern Library**
   - Define 10-15 common diversity patterns
   - Create pattern matching algorithm
   - Build interpretation templates

### Testing Strategy
- Unit tests for all statistical functions
- Integration tests for LLM features
- Example datasets with known patterns
- Performance benchmarks for large datasets

## Development Workflow

### Getting Started
```r
# Clone and setup
git clone https://github.com/shandley/diversityGPT.git
cd diversityGPT

# Install development dependencies
install.packages(c("devtools", "usethis", "testthat", "roxygen2"))

# Load for development
devtools::load_all()
```

### Daily Development Cycle
1. Write/modify functions in R/
2. Document with roxygen2 comments
3. Run `devtools::document()`
4. Test with `devtools::test()`
5. Check with `devtools::check()`

### Key Commands
```r
# Create new function file
usethis::use_r("function-name")

# Add tests
usethis::use_test("function-name")

# Build and reload
devtools::load_all()

# Run specific tests
testthat::test_file("tests/testthat/test-function-name.R")
```

## API Design Examples

### Main Analysis Function
```r
# Unified diversity analysis
results <- diversity_suite(
  physeq_object,
  groups = "treatment",
  metrics = c("shannon", "simpson", "bray_curtis"),
  consensus = TRUE,
  llm_interpret = TRUE
)
```

### Consensus Analysis
```r
# Resolve conflicting metrics
consensus <- consensus_diversity(
  alpha_metrics = results$alpha,
  beta_metrics = results$beta,
  method = "adaptive",
  confidence = 0.95
)
```

### AI Interpretation
```r
# Get biological interpretation
interpretation <- interpret_results(
  results,
  context = list(
    organism = "human_gut",
    condition = "antibiotic_treatment",
    timepoints = c("pre", "during", "post")
  )
)
```

## Important Implementation Notes

### LLM Integration Considerations
- Cache API responses to minimize costs
- Implement rate limiting for API calls
- Provide offline fallback interpretations
- Allow custom prompts for domain experts

### Statistical Rigor
- All methods must be statistically sound
- Provide references for novel algorithms
- Include diagnostic plots
- Report effect sizes with confidence intervals

### User Experience
- Informative progress messages for long operations
- Clear error messages with solutions
- Sensible defaults for non-experts
- Advanced options for power users

## Success Metrics
- **Functionality**: All core features working reliably
- **Performance**: <30s for typical dataset (1000 samples)
- **Accuracy**: Validated against published datasets
- **Usability**: Clear documentation and intuitive API
- **Adoption**: CRAN submission ready by Phase 4

## Next Steps (Phase 1)
1. Set up R package structure with usethis
2. Implement basic Hill diversity calculations
3. Create diversity result S3 class with methods
4. Write first consensus algorithm (weighted mean)
5. Integrate OpenAI API for basic interpretation
6. Create 2-3 example vignettes
7. Submit to GitHub with CI/CD pipeline

## Resources
- [R Packages (2e)](https://r-pkgs.org/) by Hadley Wickham
- [Advanced R](https://adv-r.hadley.nz/) for S3 classes
- [phyloseq tutorials](https://joey711.github.io/phyloseq/) for microbiome data
- [Hill diversity paper](https://doi.org/10.1890/10-1594.1) for theoretical foundation