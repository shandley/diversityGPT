# diversityGPT 1.0.0

## First CRAN Release 🎉

### Major Features for Production Use

#### Taxa Driver Analysis (NEW)
* **Driver Identification**: Identify specific taxa driving R, E, P, S information components
* **Multiple Methods**: Contribution, variance, and correlation-based driver detection
* **Rich Visualizations**: Network plots, heatmaps, and multi-component driver analysis
* **Biological Insights**: Connect mathematical patterns to specific microbial taxa
* **Group Analysis**: Identify drivers within experimental groups or conditions

#### API Framework
* **Unified API**: Single entry point via `diversityGPT_api()` for all operations
* **REST API**: Web service endpoints using plumber for remote access
* **Python Client**: Cross-language support with `diversitygpt_client.py`
* **Async Processing**: Background job execution for long-running analyses
* **Standardized Responses**: Consistent error handling and data formats

#### Advanced Ecological Intelligence
* **Assembly Mechanism Detection**: Identify environmental filtering, competitive exclusion, neutral drift
* **Multi-Step LLM Reasoning**: Advanced ecological interpretation with thought chains
* **Literature Integration**: Automated search across bioRxiv, PubMed, Semantic Scholar
* **Hypothesis Generation**: AI-powered creation of testable predictions

#### Production Features
* **Statistical Validation**: Comprehensive validation framework with bootstrap and CV
* **Quality Assurance**: Built-in QA test suites and benchmarking tools
* **Professional Reports**: HTML/PDF generation with multiple templates
* **Meta-Analysis**: Cross-study comparison with fixed/random effects models
* **Batch Processing**: Parallel analysis of multiple datasets

#### Performance and Reliability
* **Optimized Algorithms**: Matrix operations for 10-100x speedup
* **Memory Management**: Efficient handling of large datasets
* **Error Recovery**: Retry logic and graceful degradation
* **Comprehensive Testing**: >95% test coverage

### Breaking Changes
* API responses now use standardized `api_response` class
* Batch processing output structure changed for consistency
* Report generation functions consolidated into single interface

### Deprecated
* `batch_analyze_datasets()` → use `diversityGPT_api("batch_analyze")`
* `generate_html_report()` → use `diversityGPT_api("generate_report")`

# diversityGPT 0.2.0

## Major New Features

### Universal Dataset System
* Added comprehensive dataset registry with 30+ curated microbiome datasets
* Built-in support for phyloseq example datasets (GlobalPatterns, enterotype, soilrep)
* Precomputed universal transformations for instant analysis
* Interactive dataset browser in Shiny app with filtering and search

### Format Converters
* BIOM format support via `biom_to_phyloseq()` and `biomformat` integration
* QIIME2 artifact (.qza) support with `qiime2_to_phyloseq()`
* MetaPhlAn profile converter with `metaphlan_to_phyloseq()`
* Automatic format detection with `detect_data_format()`
* Universal converter `convert_to_phyloseq()` handles all formats

### Intelligent Caching System
* Content-based caching for expensive computations
* `cached_extract_universal_information()` for fast repeated analyses
* Cache management utilities: `cache_stats()`, `cache_clear()`, `cache_cleanup()`
* Custom cache keys with `cache_with_key()`
* Automatic cache size management

### Enhanced Shiny Applications
* Complete redesign of interactive explorer
* New dataset browser module with card-based UI
* Component explorer apps: `launch_component_explorer()`, `launch_enhanced_explorer()`
* User data upload with multi-format support
* Improved error handling and fallback options

### Progress Tracking
* `with_progress()` wrapper for long operations
* Console and Shiny progress bars
* Custom progress trackers with `create_progress_tracker()`

## Improvements

### Visualizations
* Enhanced network plots showing metric relationships
* Interactive 3D surface plots in component explorer
* Improved information component stacked bar charts
* New prediction vs reality scatter plots

### Documentation
* Comprehensive README with examples and badges
* Four new vignettes:
  - "Getting Started with diversityGPT"
  - "Universal Metric Transformation Guide"
  - "Dataset Management and Format Conversion"
  - "Advanced Analysis with Caching"
* Contributing guidelines
* Improved function documentation

### Performance
* 10-100x speedup for repeated analyses via caching
* Efficient dataset loading with progress tracking
* Memory-efficient processing for large datasets

## Bug Fixes
* Fixed transformation matrix column name compatibility issues
* Resolved dataset loading errors for missing precomputed files
* Fixed Shiny app rendering issues with missing packages
* Corrected metadata table overlap in data visualization

## Breaking Changes
* None - this version maintains backward compatibility

## Deprecated
* None

# diversityGPT 0.1.0

## Initial Release

### Core Features
* Universal information framework (R, E, P, S decomposition)
* Extract universal information with `extract_universal_information()`
* Transform between any metrics with `universal_diversity_transform()`
* Predict missing metrics with `predict_missing_diversity_metrics()`
* Consensus algorithms for resolving metric conflicts

### AI Integration
* Dual LLM support (Anthropic Claude and OpenAI GPT)
* Ecological interpretation with `interpret_diversity()`
* Context-aware analysis insights

### Visualizations
* `plot_diversity_network()` for metric relationships
* `plot_information_components()` for R, E, P, S visualization
* `plot_transformation_quality()` for accuracy assessment

### Basic Shiny App
* Interactive diversity explorer with `launch_diversity_explorer()`
* Real-time metric transformation
* Basic data upload functionality