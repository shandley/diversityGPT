# diversityGPT R Package Development

## Project Overview
**diversityGPT** is an R package that solves the critical "decision confusion" problem in microbiome diversity analysis by combining:
1. **Universal Information Theory Framework**: Mathematical deconvolution system that establishes relationships between ANY diversity metrics through information theory
2. **AI-Powered Ecological Interpretation**: LLM integration that translates mathematical patterns into ecological theory, biological mechanisms, and testable hypotheses
3. **Advanced Statistical Synthesis**: Consensus algorithms for resolving conflicting metrics
4. **Interactive Visualization**: Network plots, dashboards, and real-time exploration tools

## Revolutionary New Capabilities
### Universal Diversity Metric Deconvolution
- **Any-to-Any Conversion**: Calculate any diversity metric from any other metric(s)
- **Information Component Analysis**: Decompose metrics into Richness (R), Evenness (E), Phylogenetic (P), and Spatial (S) components
- **Mathematical Relationship Discovery**: Automatically identify and quantify relationships between all diversity metrics
- **Cross-Study Standardization**: Convert between different metric sets across studies

### Ecological Intelligence Engine
- **Assembly Mechanism Detection**: Automatically identify environmental filtering, competitive exclusion, neutral processes
- **Hypothesis Generation**: AI-powered generation of testable biological hypotheses
- **Literature Integration**: Connect findings with relevant scientific literature
- **Real-Time Ecological Commentary**: Context-aware interpretation of metric relationships

## Current Development Phase
**Phase 2A: Universal Information Framework - ✅ COMPLETED**
- Revolutionary mathematical deconvolution system (R, E, P, S components) ✅
- Any-to-any diversity metric transformation algorithms ✅
- Universal information component extraction from ANY diversity metrics ✅
- Transformation quality assessment and relationship discovery ✅
- Comprehensive test suite for mathematical framework ✅

**Phase 1: Foundation (MVP) - ✅ COMPLETED**
- Core diversity calculations with Hill number framework ✅
- Universal information component extraction ✅
- Basic LLM API integration for result interpretation ✅
- Simple consensus algorithms for metric synthesis ✅
- Pattern recognition framework ✅

## Technology Stack
- **Language**: R (>=4.0.0)
- **Core Dependencies**: phyloseq, vegan, hilldiv, mixOmics, FactoMineR
- **LLM Integration**: httr2, jsonlite, openai/anthropic R packages
- **Statistical Methods**: boot, MCMCpack, igraph for advanced analyses
- **Visualization**: networkD3, plotly, DiagrammeR, DT, shiny
- **Ecological Analysis**: picante, FD, ape for phylogenetic/functional diversity
- **Documentation**: roxygen2, pkgdown for package documentation
- **Testing**: testthat for comprehensive test coverage

## Package Architecture

### Core Components
1. **Diversity Analysis Engine**
   - `diversity_suite()`: Main function for unified analysis
   - Hill number profiles (q=0,1,2 and continuous)
   - Multi-metric calculation and storage
   - Universal information component extraction

2. **Universal Deconvolution System**
   - `universal_diversity_deconvolution()`: Mathematical relationship discovery
   - `extract_universal_information()`: Decompose metrics into R, E, P, S components
   - `universal_diversity_transform()`: Convert between any metrics
   - `predict_missing_diversity_metrics()`: Impute missing metrics from available ones

3. **Consensus Analysis Module**
   - `consensus_diversity()`: Resolve conflicting metrics
   - Custom algorithms: adaptive, information-theoretic, hierarchical
   - Conflict resolution for significant vs non-significant patterns
   - Bootstrap confidence intervals

4. **Ecological Intelligence Layer**
   - `interpret_diversity_patterns()`: Translate math to ecology
   - `detect_assembly_mechanisms()`: Identify ecological processes
   - `generate_ecological_hypotheses()`: Create testable predictions
   - `integrate_literature_context()`: Connect with scientific literature

5. **LLM Integration System**
   - `configure_llm()`: Set up AI provider (OpenAI/Anthropic/Local)
   - `provide_ecological_commentary()`: Real-time interpretation
   - `predict_ecological_mechanisms()`: Mechanism confidence scoring
   - Context-aware prompting for domain expertise

6. **Visualization and Reporting**
   - `plot_diversity_network()`: Interactive metric relationship networks
   - `plot_information_dashboard()`: Multi-panel information analysis
   - `launch_diversity_explorer()`: Shiny app for exploration
   - `generate_diversity_transformation_report()`: Comprehensive HTML reports

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
├── R/                          # Function definitions
│   ├── diversity-core.R        # Basic diversity calculations
│   ├── information-theory.R    # Universal deconvolution system
│   ├── consensus-algorithms.R  # Metric synthesis methods
│   ├── ecological-intelligence.R # Assembly mechanism detection
│   ├── llm-integration.R       # AI provider interfaces
│   ├── visualization.R         # Plotting functions
│   └── reporting.R            # Report generation
├── man/                       # Documentation (auto-generated)
├── tests/                     # Test suite
├── vignettes/                 # User guides
├── data/                      # Example datasets
├── inst/                      # Additional files
│   ├── shiny/                # Shiny app components
│   └── prompts/              # LLM prompt templates
└── data-raw/                 # Data preparation scripts
```

### Function Design Philosophy
- **Modular**: Each function does one thing well
- **Pipeable**: Compatible with %>% workflows
- **Flexible**: Sensible defaults with customization options
- **Interpretable**: Clear parameter names and outputs
- **Validated**: Input checking and informative errors

## Key Implementation Tasks

### Phase 1 Priorities (Weeks 1-4)
1. **Core Diversity Framework**
   - Implement Hill number calculations
   - Create diversity result S3 class with information components
   - Universal information extraction (R, E, P, S)
   - Basic plotting methods

2. **Mathematical Deconvolution Foundation**
   - Information component extraction algorithms
   - Basic transformation functions (alpha-to-alpha)
   - Transformation quality metrics (R², reliability)
   - Validation framework

3. **Simple Consensus Algorithm**
   - Weighted averaging with bootstrap CI
   - Basic conflict detection
   - Information-theoretic weighting
   - Summary statistics

4. **LLM API Wrapper**
   - OpenAI/Anthropic API integration
   - Ecological prompt templates
   - Result formatting for LLM context
   - Response parsing and caching

### Phase 2 Priorities (Weeks 5-8)
1. **Advanced Deconvolution**
   - Alpha-to-beta transformations
   - Beta-to-beta transformations
   - Missing metric prediction
   - Cross-study standardization

2. **Ecological Intelligence**
   - Assembly mechanism detection
   - Hypothesis generation system
   - Literature integration module
   - Real-time commentary

3. **Visualization Layer**
   - Interactive network plots
   - Information dashboards
   - Prediction validation plots
   - Relationship explorer

### Phase 3 Priorities (Weeks 9-12)
1. **Advanced Features**
   - Shiny app development
   - Comprehensive reporting
   - Advanced prompting strategies
   - Performance optimization

2. **Integration and Polish**
   - Complete test coverage
   - Vignette development
   - Documentation website
   - Example gallery

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

### Main Analysis Function with Full Features
```r
# Unified diversity analysis with deconvolution and ecological intelligence
results <- diversity_suite_with_ecology(
  phyloseq_object,
  groups = "treatment",
  study_context = list(
    environment = "human_gut",
    organism = "bacteria",
    condition = "antibiotic_treatment"
  ),
  deconvolution = TRUE,      # Enable universal metric relationships
  ecological_interpretation = TRUE,
  llm_assist = TRUE
)

# Results include:
# - Mathematical deconvolution of all metrics
# - Assembly mechanism predictions
# - Ecological hypotheses
# - Literature synthesis
# - Interactive visualizations
```

### Universal Metric Deconvolution
```r
# Discover relationships between ALL diversity metrics
deconv_results <- universal_diversity_deconvolution(
  phyloseq_object,
  source_metrics = c("shannon", "simpson", "chao1"),
  target_metrics = c("bray_curtis", "jaccard", "unifrac")
)

# Predict missing metrics from available ones
predicted_metrics <- predict_missing_diversity_metrics(
  available_metrics = list(shannon = 2.3, simpson = 0.8),
  transformation_matrix = deconv_results$transformation_matrix
)
```

### Ecological Intelligence
```r
# Detect assembly mechanisms from diversity patterns
mechanisms <- detect_assembly_mechanisms(
  transformation_results = deconv_results,
  environmental_data = sample_data(phyloseq_object)
)

# Generate testable hypotheses
hypotheses <- generate_ecological_hypotheses(
  patterns = deconv_results$patterns,
  study_context = list(environment = "soil_microbiome"),
  literature_integration = TRUE
)
```

### Interactive Visualization
```r
# Network visualization of metric relationships
plot_diversity_network_with_ecology(deconv_results)

# Launch interactive exploration dashboard
launch_diversity_explorer(results)

# Generate comprehensive report
generate_diversity_transformation_report(
  results,
  output_format = "html",
  include_shiny_app = TRUE
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
- **Mathematical Accuracy**: R² > 0.9 for metric predictions
- **Performance**: <30s for typical dataset (1000 samples)
- **Ecological Validity**: Mechanism predictions validated against literature
- **Usability**: Clear documentation and intuitive API
- **Adoption**: CRAN submission ready by Phase 4

## Expected Revolutionary Impact
1. **Transform Diversity Analysis**: First tool to mathematically relate ALL diversity metrics
2. **Accelerate Discovery**: AI-powered hypothesis generation from patterns
3. **Standardize Field**: Unified framework for diversity interpretation
4. **Enable Meta-Analysis**: Cross-study comparison through metric standardization
5. **Educate Researchers**: Real-time ecological theory integration

## Completed Implementation Status

### ✅ **REVOLUTIONARY UNIVERSAL FRAMEWORK ACHIEVED**
The package successfully implements the world's first **Universal Diversity Metric Transformation System** with:
- **Mathematical Deconvolution**: Any diversity metric can be expressed as R + E + P + S information components
- **Any-to-Any Conversion**: Transform between ANY diversity metrics (Shannon → Simpson, Chao1 → Faith's PD, etc.)
- **Cross-Study Standardization**: Convert metric sets across different studies for meta-analysis
- **Relationship Discovery**: Automatically discover mathematical relationships between all metrics
- **Quality Assessment**: R² scoring and reliability metrics for all transformations

### ✅ **CURRENT WORKING FUNCTIONS**
```r
# Universal Information Extraction
universal_info <- extract_universal_information(phyloseq_obj)

# Any-to-Any Metric Transformation  
predicted <- universal_diversity_transform(
  source_metrics = c(shannon = 2.3),
  target_metrics = c("simpson", "chao1", "faith_pd"),
  transformation_matrix = universal_info$transformation_matrix
)

# Missing Metric Prediction
missing_metrics <- predict_missing_diversity_metrics(
  available_metrics = c(shannon = 2.1, observed = 45),
  phyloseq_reference = reference_data
)

# Relationship Discovery
relationships <- discover_metric_relationships(phyloseq_obj)
```

### ✅ **AI-POWERED INTERPRETATION**
- **Dual Provider Support**: Both Anthropic Claude and OpenAI GPT working seamlessly
- **Ecological Context**: Real-time biological interpretation of mathematical patterns
- **Hypothesis Generation**: AI-generated testable predictions from diversity patterns

### ✅ **Phase 2B: Advanced Visualization - COMPLETED**
1. **Interactive Network Plots**: Visualize metric relationships and transformations ✅
2. **Information Dashboards**: Multi-panel exploration of R, E, P, S components ✅  
3. **Transformation Quality Plots**: Visual assessment of prediction reliability ✅
4. **Plot Methods**: S3 plot methods for all universal objects ✅

### 🚀 **VISUALIZATION CAPABILITIES**
```r
# Network visualization of metric relationships
plot_diversity_network(universal_info, interactive = TRUE)

# Multi-panel information component dashboard
plot_information_components(universal_info, plot_type = "interactive")

# Transformation quality assessment
plot_transformation_quality(universal_info, plot_type = "matrix")

# S3 plot methods
plot(universal_info, type = "network")  # or "components", "quality"
plot(transformation_results)  # Shows predictions with confidence
```

### 📋 **NEXT PRIORITY: Phase 2C - Shiny App & Examples**
1. **Interactive Diversity Explorer**: Full Shiny application for real-time analysis
2. **Example Workflows**: Complete vignettes showing revolutionary capabilities
3. **Performance Optimization**: Speed up large dataset processing
4. **Documentation Website**: pkgdown site with tutorials

## Resources
- [R Packages (2e)](https://r-pkgs.org/) by Hadley Wickham
- [Advanced R](https://adv-r.hadley.nz/) for S3 classes
- [phyloseq tutorials](https://joey711.github.io/phyloseq/) for microbiome data
- [Hill diversity paper](https://doi.org/10.1890/10-1594.1) for theoretical foundation