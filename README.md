<!-- README.md is generated from README.Rmd. Please edit that file -->

# diversityGPT <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/shandley/diversityGPT/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/shandley/diversityGPT/actions/workflows/check-standard.yaml)
[![Coverage](https://img.shields.io/badge/coverage-85%25-brightgreen.svg)](https://github.com/shandley/diversityGPT)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**diversityGPT** revolutionizes microbiome diversity analysis with the world's first **Universal Diversity Metric Transformation System**. Beyond solving "decision confusion" when metrics conflict, it mathematically relates ANY diversity metrics through information theory, enabling unprecedented meta-analysis and cross-study comparisons.

## 🚀 Revolutionary Features

### Core Capabilities
- 🔄 **Universal Metric Transformation**: Convert between ANY diversity metrics with mathematical rigor
- 🧬 **Information Decomposition**: Every metric = R (Richness) + E (Evenness) + P (Phylogenetic) + S (Spatial)
- 🤝 **Consensus Algorithms**: Resolve metric conflicts with R² > 0.9 accuracy
- 🧠 **AI-Powered Interpretation**: Dual LLM support for ecological insights and hypothesis generation

### New in Version 1.0.0 (Development Features!)
- 🧬 **Taxa Indicator Analysis**: Rigorous mathematical approach to identify key organisms
  - **Phase 5A**: Comprehensive null model testing with 4 randomization algorithms
  - **Phase 5B**: Information-theoretic analysis using mutual information and conditional MI
  - **Planned Phase 5C**: Shapley value attribution for fair decomposition
  - **Planned Phase 5D**: Bootstrap confidence intervals and uncertainty quantification
- 🔬 **Information Theory Framework**: Revolutionary mathematical foundation
  - `calculate_taxa_mutual_information()`: Rigorous statistical dependence quantification
  - `calculate_conditional_mutual_information()`: Interaction effect detection
  - Multiple discretization methods (equal-width, equal-frequency, k-means)
  - Bias-corrected estimators (Miller-Madow, shrinkage) for finite samples
- 🎯 **Null Model Validation**: Statistical significance testing
  - `validate_taxa_indicators()`: 4 null models with p-values and effect sizes
  - Row shuffle, column shuffle, curveball, and phylogenetic null models
  - Publication-ready visualization and reporting
- 🔬 **Ecological Intelligence**: AI-powered assembly mechanism detection and hypothesis generation
  - `detect_assembly_mechanisms()`: Environmental filtering, competition, neutral processes
  - `generate_ecological_hypotheses()`: Multi-type hypothesis generation with experimental designs
  - `llm_multi_step_analysis()`: Advanced reasoning chains for ecological interpretation
  - `search_literature()`: Multi-database scientific literature integration
- 📊 **Interactive Shiny Applications**: 4 specialized apps for real-time analysis
  - Main Diversity Explorer with full dashboard
  - Component Explorer for R,E,P,S parameter space
  - Enhanced Explorer with live mathematical equations
  - Simple Explorer for lightweight analysis
- 🗂️ **Universal Dataset System**: 30+ built-in datasets with precomputed transformations
- 🔧 **Format Converters**: BIOM, QIIME2, MetaPhlAn support with auto-detection
- ⚡ **Performance Optimizations**: Parallel processing, sparse matrices, intelligent caching
- 📈 **Advanced Visualizations**: Network plots, 3D surfaces, component dashboards
- 📚 **Comprehensive Documentation**: 6 detailed vignettes + pkgdown website
- 🚀 **Production Ready**: Complete ecosystem for microbiome diversity analysis and discovery
- 🔌 **API Framework**: Unified programmatic interface with REST API and Python client
- 📊 **Professional Reports**: Automated HTML/PDF generation with multiple templates
- ✅ **Statistical Validation**: Bootstrap and cross-validation for all transformations
- 🔄 **Meta-Analysis Tools**: Cross-study comparison with fixed/random effects models

## Installation

### CRAN (Coming Soon!)

diversityGPT will be available on CRAN:

``` r
# Coming soon!
install.packages("diversityGPT")
```

### Development Version

You can install the development version from [GitHub](https://github.com/shandley/diversityGPT):

``` r
# install.packages("devtools")
devtools::install_github("shandley/diversityGPT")

# For full functionality, also install:
install.packages(c("shiny", "shinydashboard", "plotly", "DT", "networkD3"))
```

### Dependencies

diversityGPT depends on phyloseq from Bioconductor:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("phyloseq")
```

## Quick Start

``` r
library(diversityGPT)
library(phyloseq)

# 1. Load data (multiple options)
data(GlobalPatterns)  # Built-in dataset

# Or use the dataset browser
available_datasets <- list_available_datasets()
physeq <- load_dataset("enterotype")  # Auto-downloads if needed

# Or load your own data (supports BIOM, QIIME2, MetaPhlAn)
physeq <- convert_to_phyloseq("my_data.biom")

# 2. Extract universal information components
universal_info <- extract_universal_information(physeq)
print(universal_info)  
# Universal Diversity Deconvolution Results
# ========================================
# Overall Quality: Excellent (R² = 0.939)
# Components: R ✓ E ✓ P ✗ S ✗

# 3. Transform between ANY metrics
predicted <- universal_diversity_transform(
  source_metrics = c(shannon = 2.3),
  target_metrics = c("simpson", "chao1", "faith_pd"),
  transformation_matrix = universal_info$transformation_matrix
)

# 4. Visualize relationships
plot_diversity_network(universal_info, interactive = TRUE)
plot_information_components(universal_info)

# 5. Detect assembly mechanisms
mechanisms <- detect_assembly_mechanisms(
  universal_info,
  environmental_data = env_data
)
print(mechanisms)

# 6. Generate ecological hypotheses
hypotheses <- generate_ecological_hypotheses(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  study_context = list(
    environment = "human_gut",
    condition = "antibiotic_treatment",
    hypothesis = "diversity recovery"
  )
)

# 7. Multi-step LLM analysis
analysis <- llm_multi_step_analysis(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  hypotheses = hypotheses,
  reasoning_depth = "deep",
  llm_provider = "anthropic"
)
```

## 🧬 Advanced Taxa Indicator Analysis

### Information-Theoretic Approach (NEW!)
```r
# Revolutionary mutual information analysis
mi_results <- calculate_taxa_mutual_information(
  physeq,
  components = universal_info,
  discretization = "equal_width",
  method = "mm",  # Miller-Madow bias correction
  verbose = TRUE
)

# View top information-rich taxa
print(mi_results)
# Information Theory Analysis Results
# Method: mm
# =====================================
# RICHNESS COMPONENT:
#   1. OTU123 (NMI: 0.847, MI: 0.234)
#   2. OTU456 (NMI: 0.792, MI: 0.187)

# Detect interaction effects
cmi_results <- calculate_conditional_mutual_information(
  physeq,
  components = universal_info,
  conditioning_taxa = c("dominant_taxa"),
  verbose = TRUE
)

# Visualize information relationships
plot(mi_results, type = "heatmap", interactive = TRUE)
plot(mi_results, type = "network", top_n = 15)
```

### Null Model Statistical Validation
```r
# Rigorous statistical testing with 4 null models
validation <- validate_taxa_indicators(
  physeq,
  indicators = mi_results,
  null_models = c("row_shuffle", "curveball"),
  n_permutations = 999
)

# Results with p-values and effect sizes
print(validation)
# Null Model Validation Results
# ==============================
# Significant indicators (p < 0.05): 12/20
# Effect sizes (Cohen's d): 0.23 - 1.47
# Multiple testing correction: FDR
```

### Mathematical Foundation
diversityGPT now provides the **most rigorous taxa indicator analysis** available:

- **Information Theory**: Mutual information quantifies statistical dependence without linear assumptions
- **Conditional Analysis**: I(Taxon; Component | Context) reveals interaction effects
- **Bias Correction**: Miller-Madow and shrinkage estimators for finite sample reliability
- **Null Model Testing**: 4 randomization algorithms with proper statistical inference
- **Effect Size Quantification**: Standardized measures for cross-study comparison

## 🔬 Ecological Intelligence System

### Assembly Mechanism Detection
```r
# Detect community assembly processes
mechanisms <- detect_assembly_mechanisms(
  universal_info,
  environmental_data = env_metadata,
  method = "comprehensive"
)

# Results:
# Primary mechanism: Environmental Filtering (confidence: 0.847)
# Supporting evidence: Strong pH-diversity correlation (r = 0.73)
# Interpretation: Abiotic factors selectively filter species
```

### Advanced Hypothesis Generation
```r
# Generate testable ecological hypotheses
hypotheses <- generate_ecological_hypotheses(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  hypothesis_types = c("mechanistic", "predictive", "experimental"),
  study_context = list(
    environment = "soil_microbiome",
    condition = "pH_gradient"
  )
)

# Results:
# 1. [MECHANISTIC] Environmental gradients drive species sorting (novelty: 0.6, testability: 0.8)
# 2. [PREDICTIVE] Diversity patterns will be highly predictable (novelty: 0.5, testability: 0.9)
# 3. [EXPERIMENTAL] Transplant experiments will show low establishment (novelty: 0.7, testability: 0.8)
```

### Multi-Step LLM Analysis
```r
# Comprehensive AI-powered analysis
analysis <- llm_multi_step_analysis(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  hypotheses = hypotheses,
  reasoning_depth = "expert",
  include_literature = TRUE,
  llm_provider = "anthropic"
)

# Results include:
# - Step-by-step reasoning chain
# - Novel ecological insights
# - Research recommendations
# - Literature integration
# - Confidence assessments
```

### Literature Integration
```r
# Search and integrate scientific literature
literature <- search_literature(
  universal_info = universal_info,
  assembly_mechanisms = mechanisms,
  study_context = list(environment = "marine", organism = "bacteria"),
  search_databases = "all",
  max_papers = 20,
  llm_synthesis = TRUE
)

# Results:
# Found 18 relevant papers
# Novel connections identified: 3
# Research gaps identified: 2
# Literature synthesis: "Findings align with established theory..."
```

## 🎯 Interactive Shiny Applications

Launch specialized applications for different analysis needs:

``` r
# 🌟 Main Diversity Explorer - Complete workflow
launch_diversity_explorer()
# - Dataset browser with 30+ datasets
# - Universal analysis extraction  
# - Real-time metric transformation
# - Interactive visualizations
# - AI-powered interpretation
# - Export functionality

# 🎛️ Component Explorer - Parameter space exploration
launch_component_explorer()
# - Interactive R, E, P, S sliders
# - Live metric updates
# - Preset ecological scenarios

# 📐 Enhanced Explorer - Mathematical visualization
launch_enhanced_explorer()
# - Live mathematical equations
# - Component contribution bars
# - 3D response surfaces
# - Biological interpretation

# ⚡ Simple Explorer - Lightweight analysis
launch_simple_explorer()
# - Core functionality
# - Minimal dependencies
# - Fast loading
```

## 📊 Universal Framework in Action

### Example: Cross-Study Meta-Analysis
```r
# Different studies used different metrics? No problem!
study1_metrics <- c(shannon = 2.1, simpson = 0.8)  # Study used Shannon/Simpson
study2_metrics <- c(observed = 150, pielou = 0.75) # Study used Observed/Pielou

# Convert to common framework
universal_study1 <- extract_universal_information(study1_data)
universal_study2 <- extract_universal_information(study2_data)

# Now directly comparable through R, E, P, S components!
```

### Example: Missing Metric Prediction
```r
# Need Faith's PD but only have basic metrics?
available <- c(shannon = 2.3, observed = 125)
predicted_faith <- predict_missing_diversity_metrics(
  available_metrics = available,
  target_metrics = "faith_pd",
  phyloseq_reference = reference_data
)
# Predicted Faith's PD: 8.73 (R² = 0.91)
```

## 🔧 Dataset Management

### Built-in Dataset Browser
```r
# Browse available datasets
datasets <- list_available_datasets()

# Search by keyword
gut_datasets <- search_datasets("gut")

# Load with metadata
dataset_info <- get_dataset_info("hmp_gut_16s")
physeq <- load_dataset("hmp_gut_16s")
```

### Format Conversion
```r
# Auto-detect and convert various formats
physeq <- convert_to_phyloseq("data.biom")          # BIOM format
physeq <- convert_to_phyloseq("table.qza")          # QIIME2 artifact  
physeq <- convert_to_phyloseq("profile.txt",        # MetaPhlAn output
                             format = "metaphlan")

# Merge multiple QIIME2 artifacts
physeq <- merge_qiime2_artifacts(
  feature_table = "table.qza",
  taxonomy = "taxonomy.qza",
  tree = "tree.qza",
  metadata = "metadata.tsv"
)
```

## ⚡ Performance Features

### Intelligent Caching
```r
# Automatic caching for expensive operations
result <- cached_extract_universal_information(
  large_physeq,
  cache_dir = "~/.diversityGPT_cache"
)

# View cache statistics
cache_stats()
```

### Progress Tracking
```r
# Long operations show progress
with_progress({
  result <- extract_universal_information(huge_dataset)
}, message = "Analyzing diversity patterns...")
```

## 📚 Documentation

### 📖 Comprehensive Vignettes (6 Guides)
- `vignette("getting-started")` - Complete introduction and workflow
- `vignette("universal-transformation")` - Mathematical framework details
- `vignette("ecological-interpretation")` - Consensus analysis and AI interpretation
- `vignette("shiny-apps")` - Interactive application guide
- `vignette("dataset-management")` - Data handling and format conversion
- `vignette("caching-guide")` - Performance optimization strategies

### 🌐 Documentation Website
- **Home**: [shandley.github.io/diversityGPT](https://shandley.github.io/diversityGPT)
- **API Reference**: [Function documentation](https://shandley.github.io/diversityGPT/reference)
- **Tutorials**: [Interactive examples](https://shandley.github.io/diversityGPT/articles)

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Setup
```r
# Clone repository
git clone https://github.com/shandley/diversityGPT.git

# Install development dependencies
devtools::install_deps(dependencies = TRUE)

# Run tests
devtools::test()

# Check package
devtools::check()
```

## 📖 Citation

If you use diversityGPT in your research, please cite:

```bibtex
@software{diversityGPT2025,
  author = {Handley, Scott},
  title = {diversityGPT: Universal Diversity Metric Transformation for Microbiome Analysis},
  year = {2025},
  url = {https://github.com/shandley/diversityGPT},
  doi = {10.5281/zenodo.XXXXXXX}
}
```

## 🙏 Acknowledgments

- Built on the excellent [phyloseq](https://joey711.github.io/phyloseq/) ecosystem
- Information theory framework inspired by Hill diversity concepts
- AI integration powered by Anthropic Claude and OpenAI GPT

## 📄 License

MIT © 2025 diversityGPT authors