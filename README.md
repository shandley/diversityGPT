<!-- README.md is generated from README.Rmd. Please edit that file -->

# diversityGPT <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/shandley/diversityGPT/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/shandley/diversityGPT/actions/workflows/check-standard.yaml)
<!-- badges: end -->

**diversityGPT** solves the critical "decision confusion" problem in microbiome diversity analysis. When different diversity metrics give conflicting results (e.g., Shannon says significant, Simpson says not), researchers often struggle to interpret what's really happening. This package provides intelligent synthesis and AI-powered interpretation to turn confusion into clarity.

## Key Features

- 📊 **Multi-Metric Analysis**: Calculate all major diversity metrics at once
- 🤝 **Consensus Algorithms**: Resolve conflicts between different metrics  
- 🧠 **AI-Powered Interpretation**: Get plain-language explanations of your results
- 📈 **Smart Visualizations**: See patterns across multiple metrics
- 🔬 **Ecological Intelligence**: Understand biological mechanisms behind patterns

## Installation

You can install the development version of diversityGPT from [GitHub](https://github.com/shandley/diversityGPT) with:

``` r
# install.packages("devtools")
devtools::install_github("shandley/diversityGPT")
```

## Quick Start

``` r
library(diversityGPT)
library(phyloseq)

# Check API setup (one-time)
check_api_setup()

# Load your data (or use example)
data(example_physeq)

# Calculate all diversity metrics at once
div_results <- calculate_diversity(
  example_physeq,
  metrics = c("shannon", "simpson", "chao1", "observed"),
  groups = "Group"
)

# Get consensus across metrics
consensus <- consensus_diversity(div_results)

# Get AI-powered interpretation
interpretation <- interpret_diversity(
  consensus,
  context = list(
    environment = "human_gut",
    condition = "treatment_vs_control"
  )
)

print(interpretation)
```

## The Problem We Solve

Imagine you're analyzing gut microbiome data:
- Shannon index shows p = 0.03 (significant difference)
- Simpson index shows p = 0.12 (no significant difference)  
- Chao1 shows p = 0.008 (highly significant)

What do you conclude? This confusion is extremely common and leads to:
- Cherry-picking favorable results
- Missing important biological patterns
- Inconsistent interpretations across studies

**diversityGPT** provides a principled solution by synthesizing all metrics and explaining what the patterns mean biologically.

## How It Works

1. **Calculate Everything**: Run all diversity metrics in a standardized way
2. **Find Consensus**: Use statistical methods to resolve conflicts
3. **Interpret Patterns**: AI explains what metric combinations mean
4. **Generate Hypotheses**: Get suggestions for follow-up analyses

## Setting Up API Keys

The package uses AI for intelligent interpretation. Add your API key to `.Renviron`:

```bash
ANTHROPIC_API_KEY=your-key-here
# or
OPENAI_API_KEY=your-key-here
```

Then restart R. The package works with either provider.

## Documentation

- [Getting Started Vignette](vignettes/getting-started.html)
- [Understanding Diversity Metrics](vignettes/diversity-metrics.html)
- [API Configuration Guide](vignettes/api-setup.html)

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

## Citation

If you use diversityGPT in your research, please cite:

```
@software{diversityGPT,
  author = {Handley, Scott},
  title = {diversityGPT: AI-Powered Microbiome Diversity Analysis},
  year = {2025},
  url = {https://github.com/shandley/diversityGPT}
}
```

## License

MIT © 2025 diversityGPT authors