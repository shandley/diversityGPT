#' Comprehensive Report Generation for diversityGPT
#'
#' Functions to generate professional HTML and PDF reports combining
#' all diversityGPT analyses with visualizations and interpretations.

#' Generate Comprehensive Diversity Report
#'
#' Creates a professional report combining universal information analysis,
#' ecological intelligence, literature integration, and statistical validation.
#'
#' @param universal_info Universal information object from extract_universal_information()
#' @param phyloseq_obj Original phyloseq object
#' @param assembly_mechanisms Optional output from detect_assembly_mechanisms()
#' @param hypotheses Optional output from generate_ecological_hypotheses()
#' @param literature Optional output from search_literature()
#' @param validation Optional output from validate_universal_analysis()
#' @param output_file Output file path (without extension)
#' @param output_format Format: "html", "pdf", or "both"
#' @param template Report template: "research", "summary", "technical", "presentation"
#' @param include_code Whether to include R code in report
#' @param include_data_summary Whether to include detailed data summary
#' @param include_methodology Whether to include methodology section
#' @param include_citations Whether to generate citations section
#' @param author Author name(s) for report
#' @param title Custom report title
#' @param abstract Custom abstract text
#' @param study_context Study context information
#'
#' @return Path to generated report file(s)
#'
#' @examples
#' \dontrun{
#' # Complete analysis workflow with reporting
#' data(GlobalPatterns)
#' universal_info <- extract_universal_information(GlobalPatterns)
#' mechanisms <- detect_assembly_mechanisms(universal_info)
#' hypotheses <- generate_ecological_hypotheses(universal_info, mechanisms)
#' validation <- validate_universal_analysis(universal_info, GlobalPatterns)
#' 
#' # Generate comprehensive report
#' report_path <- generate_diversity_report(
#'   universal_info = universal_info,
#'   phyloseq_obj = GlobalPatterns,
#'   assembly_mechanisms = mechanisms,
#'   hypotheses = hypotheses,
#'   validation = validation,
#'   output_file = "diversity_analysis_report",
#'   output_format = "html",
#'   template = "research",
#'   author = "Research Team",
#'   title = "Microbial Diversity Analysis Using Universal Framework"
#' )
#' }
#'
#' @export
generate_diversity_report <- function(universal_info,
                                    phyloseq_obj,
                                    assembly_mechanisms = NULL,
                                    hypotheses = NULL,
                                    literature = NULL,
                                    validation = NULL,
                                    output_file = "diversity_report",
                                    output_format = "html",
                                    template = "research",
                                    include_code = FALSE,
                                    include_data_summary = TRUE,
                                    include_methodology = TRUE,
                                    include_citations = TRUE,
                                    author = "diversityGPT User",
                                    title = NULL,
                                    abstract = NULL,
                                    study_context = NULL) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    stop("universal_info must be a universal_information object")
  }
  
  if (!output_format %in% c("html", "pdf", "both")) {
    stop("output_format must be 'html', 'pdf', or 'both'")
  }
  
  if (!template %in% c("research", "summary", "technical", "presentation")) {
    stop("template must be 'research', 'summary', 'technical', or 'presentation'")
  }
  
  # Check required packages
  required_packages <- c("rmarkdown", "knitr")
  if (output_format %in% c("pdf", "both")) {
    required_packages <- c(required_packages, "tinytex")
  }
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Required packages missing: ", paste(missing_packages, collapse = ", "),
         "\nInstall with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
  }
  
  cat("Generating comprehensive diversity report...\n")
  cat("Template:", template, "\n")
  cat("Format:", output_format, "\n\n")
  
  # Prepare report data
  report_data <- .prepare_report_data(
    universal_info, phyloseq_obj, assembly_mechanisms, 
    hypotheses, literature, validation, study_context
  )
  
  # Generate title if not provided
  if (is.null(title)) {
    title <- .generate_report_title(template, report_data)
  }
  
  # Generate abstract if not provided
  if (is.null(abstract)) {
    abstract <- .generate_report_abstract(report_data)
  }
  
  # Create report parameters
  report_params <- list(
    universal_info = universal_info,
    phyloseq_obj = phyloseq_obj,
    assembly_mechanisms = assembly_mechanisms,
    hypotheses = hypotheses,
    literature = literature,
    validation = validation,
    report_data = report_data,
    template = template,
    include_code = include_code,
    include_data_summary = include_data_summary,
    include_methodology = include_methodology,
    include_citations = include_citations,
    author = author,
    title = title,
    abstract = abstract,
    study_context = study_context
  )
  
  # Create temporary Rmd file
  rmd_template <- .create_rmd_template(template, report_params)
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_template, temp_rmd)
  
  # Generate reports
  output_files <- character(0)
  
  if (output_format %in% c("html", "both")) {
    cat("Rendering HTML report...\n")
    html_file <- paste0(output_file, ".html")
    
    tryCatch({
      rmarkdown::render(
        temp_rmd,
        output_format = rmarkdown::html_document(
          toc = TRUE,
          toc_float = TRUE,
          theme = "flatly",
          highlight = "tango",
          code_folding = if (include_code) "hide" else "none",
          fig_width = 10,
          fig_height = 6
        ),
        output_file = html_file,
        params = report_params,
        quiet = TRUE
      )
      output_files <- c(output_files, html_file)
      cat("HTML report generated:", html_file, "\n")
    }, error = function(e) {
      warning("HTML report generation failed: ", e$message)
    })
  }
  
  if (output_format %in% c("pdf", "both")) {
    cat("Rendering PDF report...\n")
    pdf_file <- paste0(output_file, ".pdf")
    
    tryCatch({
      rmarkdown::render(
        temp_rmd,
        output_format = rmarkdown::pdf_document(
          toc = TRUE,
          toc_depth = 3,
          number_sections = TRUE,
          highlight = "tango",
          fig_width = 8,
          fig_height = 5
        ),
        output_file = pdf_file,
        params = report_params,
        quiet = TRUE
      )
      output_files <- c(output_files, pdf_file)
      cat("PDF report generated:", pdf_file, "\n")
    }, error = function(e) {
      warning("PDF report generation failed: ", e$message)
      warning("Make sure you have LaTeX installed (tinytex::install_tinytex())")
    })
  }
  
  # Clean up
  unlink(temp_rmd)
  
  if (length(output_files) == 0) {
    stop("Report generation failed for all requested formats")
  }
  
  cat("\nReport generation complete!\n")
  cat("Generated files:\n")
  for (file in output_files) {
    cat("  -", file, "\n")
  }
  
  return(output_files)
}

# Internal function: Prepare report data
.prepare_report_data <- function(universal_info, phyloseq_obj, assembly_mechanisms, 
                                hypotheses, literature, validation, study_context) {
  
  # Basic dataset information
  data_summary <- list(
    n_samples = phyloseq::nsamples(phyloseq_obj),
    n_taxa = phyloseq::ntaxa(phyloseq_obj),
    taxa_ranks = phyloseq::rank_names(phyloseq_obj),
    sample_variables = if (!is.null(phyloseq::sample_data(phyloseq_obj))) {
      colnames(phyloseq::sample_data(phyloseq_obj))
    } else {
      character(0)
    }
  )
  
  # Universal information summary
  universal_summary <- list(
    components_present = names(universal_info$information_components),
    transformation_quality = if (!is.null(universal_info$deconvolution_quality)) {
      universal_info$deconvolution_quality$mean_r_squared
    } else {
      NA
    },
    overall_quality = if (!is.null(universal_info$deconvolution_quality)) {
      universal_info$deconvolution_quality$overall_quality
    } else {
      "Unknown"
    }
  )
  
  # Assembly mechanisms summary
  mechanisms_summary <- if (!is.null(assembly_mechanisms)) {
    list(
      n_mechanisms = nrow(assembly_mechanisms$mechanisms),
      primary_mechanism = assembly_mechanisms$mechanisms$mechanism[1],
      primary_confidence = assembly_mechanisms$mechanisms$confidence[1],
      high_confidence_count = sum(assembly_mechanisms$mechanisms$confidence > 0.6)
    )
  } else {
    NULL
  }
  
  # Hypotheses summary
  hypotheses_summary <- if (!is.null(hypotheses)) {
    list(
      n_hypotheses = nrow(hypotheses$hypotheses),
      hypothesis_types = unique(hypotheses$hypotheses$type),
      mean_novelty = mean(hypotheses$hypotheses$novelty, na.rm = TRUE),
      mean_testability = mean(hypotheses$hypotheses$testability, na.rm = TRUE)
    )
  } else {
    NULL
  }
  
  # Literature summary
  literature_summary <- if (!is.null(literature)) {
    list(
      n_papers = nrow(literature$relevance_ranking),
      databases_searched = literature$metadata$databases,
      novel_connections = length(literature$novel_connections),
      research_gaps = length(literature$research_gaps)
    )
  } else {
    NULL
  }
  
  # Validation summary
  validation_summary <- if (!is.null(validation)) {
    list(
      overall_score = validation$overall_assessment$overall_score,
      quality_level = validation$overall_assessment$quality_level,
      bootstrap_success = if (!is.null(validation$bootstrap_ci)) {
        validation$bootstrap_ci$bootstrap_summary$n_successful /
        validation$bootstrap_ci$bootstrap_summary$n_iterations
      } else {
        NA
      },
      cv_performance = if (!is.null(validation$cross_validation$summary$overall_performance)) {
        validation$cross_validation$summary$overall_performance$mean_r_squared
      } else {
        NA
      }
    )
  } else {
    NULL
  }
  
  return(list(
    data_summary = data_summary,
    universal_summary = universal_summary,
    mechanisms_summary = mechanisms_summary,
    hypotheses_summary = hypotheses_summary,
    literature_summary = literature_summary,
    validation_summary = validation_summary,
    study_context = study_context,
    generation_time = Sys.time(),
    package_version = packageVersion("diversityGPT")
  ))
}

# Internal function: Generate report title
.generate_report_title <- function(template, report_data) {
  
  base_titles <- list(
    research = "Comprehensive Microbial Diversity Analysis Using Universal Information Framework",
    summary = "Diversity Analysis Summary Report",
    technical = "Technical Report: Universal Diversity Metric Decomposition",
    presentation = "Microbial Community Analysis Results"
  )
  
  title <- base_titles[[template]]
  
  # Add context if available
  if (!is.null(report_data$study_context)) {
    if (!is.null(report_data$study_context$environment)) {
      title <- paste(title, ":", stringr::str_to_title(report_data$study_context$environment), "Environment")
    }
  }
  
  return(title)
}

# Internal function: Generate report abstract
.generate_report_abstract <- function(report_data) {
  
  abstract_parts <- character(0)
  
  # Dataset description
  data_desc <- sprintf(
    "This report presents a comprehensive analysis of microbial diversity patterns in a dataset containing %d samples and %d taxa.",
    report_data$data_summary$n_samples,
    report_data$data_summary$n_taxa
  )
  abstract_parts <- c(abstract_parts, data_desc)
  
  # Universal framework description
  universal_desc <- sprintf(
    "Using the universal information framework, we decomposed diversity into fundamental components (%s) with an overall transformation quality of %s.",
    paste(report_data$universal_summary$components_present, collapse = ", "),
    report_data$universal_summary$overall_quality
  )
  abstract_parts <- c(abstract_parts, universal_desc)
  
  # Assembly mechanisms
  if (!is.null(report_data$mechanisms_summary)) {
    mech_desc <- sprintf(
      "Assembly mechanism analysis identified %s as the primary driver (confidence: %.2f).",
      report_data$mechanisms_summary$primary_mechanism,
      report_data$mechanisms_summary$primary_confidence
    )
    abstract_parts <- c(abstract_parts, mech_desc)
  }
  
  # Hypotheses
  if (!is.null(report_data$hypotheses_summary)) {
    hyp_desc <- sprintf(
      "We generated %d testable ecological hypotheses with mean novelty of %.2f and testability of %.2f.",
      report_data$hypotheses_summary$n_hypotheses,
      report_data$hypotheses_summary$mean_novelty,
      report_data$hypotheses_summary$mean_testability
    )
    abstract_parts <- c(abstract_parts, hyp_desc)
  }
  
  # Literature integration
  if (!is.null(report_data$literature_summary)) {
    lit_desc <- sprintf(
      "Literature integration identified %d relevant papers and revealed %d novel research connections.",
      report_data$literature_summary$n_papers,
      report_data$literature_summary$novel_connections
    )
    abstract_parts <- c(abstract_parts, lit_desc)
  }
  
  # Validation
  if (!is.null(report_data$validation_summary)) {
    val_desc <- sprintf(
      "Statistical validation achieved an overall quality assessment of '%s' with a validation score of %.2f.",
      report_data$validation_summary$quality_level,
      report_data$validation_summary$overall_score
    )
    abstract_parts <- c(abstract_parts, val_desc)
  }
  
  return(paste(abstract_parts, collapse = " "))
}

# Internal function: Create Rmarkdown template
.create_rmd_template <- function(template, params) {
  
  # Common header
  header <- '---
title: "`r params$title`"
author: "`r params$author`"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: flatly
  pdf_document:
    toc: true
    number_sections: true
params:
  universal_info: NA
  phyloseq_obj: NA
  assembly_mechanisms: NA
  hypotheses: NA
  literature: NA
  validation: NA
  report_data: NA
  template: NA
  include_code: FALSE
  include_data_summary: TRUE
  include_methodology: TRUE
  include_citations: TRUE
  author: NA
  title: NA
  abstract: NA
  study_context: NA
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = params$include_code,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 6
)

# Load required libraries
library(diversityGPT)
library(knitr)
library(ggplot2)

# Extract parameters
universal_info <- params$universal_info
phyloseq_obj <- params$phyloseq_obj
assembly_mechanisms <- params$assembly_mechanisms
hypotheses <- params$hypotheses
literature <- params$literature
validation <- params$validation
report_data <- params$report_data
```'
  
  # Template-specific content
  if (template == "research") {
    content <- .create_research_template()
  } else if (template == "summary") {
    content <- .create_summary_template()
  } else if (template == "technical") {
    content <- .create_technical_template()
  } else if (template == "presentation") {
    content <- .create_presentation_template()
  }
  
  return(paste(header, content, sep = "\n\n"))
}

# Internal function: Research template
.create_research_template <- function() {
  
  template <- '
# Abstract

`r params$abstract`

# Introduction

This report presents a comprehensive analysis of microbial diversity using the diversityGPT universal information framework. The universal framework decomposes traditional diversity metrics into fundamental information components, enabling novel insights into community assembly mechanisms and ecological processes.

```{r data-overview, results="asis"}
if (params$include_data_summary) {
  cat("## Dataset Overview\\n\\n")
  cat("**Samples:** ", report_data$data_summary$n_samples, "\\n\\n")
  cat("**Taxa:** ", report_data$data_summary$n_taxa, "\\n\\n")
  cat("**Taxonomic Ranks:** ", paste(report_data$data_summary$taxa_ranks, collapse = ", "), "\\n\\n")
  
  if (length(report_data$data_summary$sample_variables) > 0) {
    cat("**Sample Variables:** ", paste(report_data$data_summary$sample_variables, collapse = ", "), "\\n\\n")
  }
}
```

# Methods

```{r methodology, results="asis"}
if (params$include_methodology) {
  cat("## Universal Information Framework\\n\\n")
  cat("The universal information framework decomposes diversity into fundamental components:\\n\\n")
  cat("- **R (Richness) Component:** Information content related to species number\\n")
  cat("- **E (Evenness) Component:** Information content related to abundance distribution\\n") 
  cat("- **P (Phylogenetic) Component:** Information content related to evolutionary relationships\\n")
  cat("- **S (Spatial) Component:** Information content related to spatial structure\\n\\n")
  
  cat("### Transformation Quality\\n\\n")
  cat("Universal transformations allow prediction of any diversity metric from any other metric(s). ")
  cat("Transformation quality is assessed using R² values, with higher values indicating better predictive accuracy.\\n\\n")
}
```

# Results

## Universal Information Analysis

```{r universal-analysis}
# Universal information components plot
if (inherits(universal_info, "universal_information")) {
  plot(universal_info, type = "components")
}
```

**Components Present:** `r paste(report_data$universal_summary$components_present, collapse = ", ")`

**Transformation Quality:** `r ifelse(is.na(report_data$universal_summary$transformation_quality), "Not available", round(report_data$universal_summary$transformation_quality, 3))`

**Overall Assessment:** `r report_data$universal_summary$overall_quality`

```{r transformation-network}
# Transformation network plot
if (inherits(universal_info, "universal_information")) {
  plot(universal_info, type = "network")
}
```

## Assembly Mechanism Analysis

```{r assembly-mechanisms, results="asis"}
if (!is.null(assembly_mechanisms)) {
  cat("### Detected Mechanisms\\n\\n")
  
  mechanisms_table <- assembly_mechanisms$mechanisms[1:min(5, nrow(assembly_mechanisms$mechanisms)), ]
  kable(mechanisms_table, digits = 3, caption = "Top Assembly Mechanisms")
  
  cat("\\n\\n**Primary Mechanism:** ", report_data$mechanisms_summary$primary_mechanism, "\\n\\n")
  cat("**Confidence:** ", round(report_data$mechanisms_summary$primary_confidence, 3), "\\n\\n")
  cat("**High Confidence Mechanisms:** ", report_data$mechanisms_summary$high_confidence_count, "\\n\\n")
  
  cat("### Biological Interpretation\\n\\n")
  cat(assembly_mechanisms$interpretation$biological_meaning, "\\n\\n")
  
  cat("### Recommendations\\n\\n")
  for (rec in assembly_mechanisms$interpretation$recommendations) {
    cat("- ", rec, "\\n")
  }
  cat("\\n")
} else {
  cat("Assembly mechanism analysis not performed.\\n\\n")
}
```

## Ecological Hypotheses

```{r hypotheses-analysis, results="asis"}
if (!is.null(hypotheses)) {
  cat("### Generated Hypotheses\\n\\n")
  
  hyp_table <- hypotheses$hypotheses
  kable(hyp_table, digits = 2, caption = "Generated Ecological Hypotheses")
  
  cat("\\n\\n**Total Hypotheses:** ", report_data$hypotheses_summary$n_hypotheses, "\\n\\n")
  cat("**Hypothesis Types:** ", paste(report_data$hypotheses_summary$hypothesis_types, collapse = ", "), "\\n\\n")
  cat("**Mean Novelty:** ", round(report_data$hypotheses_summary$mean_novelty, 2), "\\n\\n")
  cat("**Mean Testability:** ", round(report_data$hypotheses_summary$mean_testability, 2), "\\n\\n")
  
  if (length(hypotheses$experimental_designs) > 0) {
    cat("### Experimental Design Recommendations\\n\\n")
    cat("Experimental designs have been generated for all hypotheses. ")
    cat("Key recommendations include controlled manipulation experiments, ")
    cat("temporal monitoring studies, and comparative analyses across environmental gradients.\\n\\n")
  }
} else {
  cat("Ecological hypothesis generation not performed.\\n\\n")
}
```

## Literature Integration

```{r literature-analysis, results="asis"}
if (!is.null(literature)) {
  cat("### Literature Search Results\\n\\n")
  
  cat("**Papers Found:** ", report_data$literature_summary$n_papers, "\\n\\n")
  cat("**Databases Searched:** ", paste(report_data$literature_summary$databases_searched, collapse = ", "), "\\n\\n")
  cat("**Novel Connections:** ", report_data$literature_summary$novel_connections, "\\n\\n")
  cat("**Research Gaps:** ", report_data$literature_summary$research_gaps, "\\n\\n")
  
  if (nrow(literature$relevance_ranking) > 0) {
    top_papers <- head(literature$relevance_ranking, 5)
    kable(top_papers[, c("title", "authors", "year", "relevance_score")], 
          digits = 3, caption = "Top Relevant Papers")
  }
  
  cat("\\n\\n### Key Literature Themes\\n\\n")
  for (theme in literature$synthesis$key_themes) {
    cat("- ", theme, "\\n")
  }
  cat("\\n")
} else {
  cat("Literature integration not performed.\\n\\n")
}
```

## Statistical Validation

```{r validation-analysis, results="asis"}
if (!is.null(validation)) {
  cat("### Validation Results\\n\\n")
  
  cat("**Overall Quality Level:** ", report_data$validation_summary$quality_level, "\\n\\n")
  cat("**Validation Score:** ", round(report_data$validation_summary$overall_score, 3), "\\n\\n")
  
  if (!is.na(report_data$validation_summary$bootstrap_success)) {
    cat("**Bootstrap Success Rate:** ", round(report_data$validation_summary$bootstrap_success * 100, 1), "%\\n\\n")
  }
  
  if (!is.na(report_data$validation_summary$cv_performance)) {
    cat("**Cross-Validation R²:** ", round(report_data$validation_summary$cv_performance, 3), "\\n\\n")
  }
  
  # Plot validation overview
  plot(validation, type = "overview")
  
  cat("### Validation Recommendations\\n\\n")
  for (rec in validation$overall_assessment$recommendations) {
    cat("- ", rec, "\\n")
  }
  cat("\\n")
} else {
  cat("Statistical validation not performed.\\n\\n")
}
```

# Discussion

The universal information framework provides a powerful approach for understanding microbial diversity patterns through mathematical decomposition of traditional metrics. This analysis revealed important insights into community assembly processes and generated testable hypotheses for future research.

```{r discussion-points, results="asis"}
# Generate discussion points based on available analyses
discussion_points <- character(0)

if (!is.null(assembly_mechanisms) && report_data$mechanisms_summary$primary_confidence > 0.6) {
  discussion_points <- c(discussion_points, 
    paste("The identification of", report_data$mechanisms_summary$primary_mechanism, 
          "as the primary assembly mechanism suggests important ecological processes shaping this community."))
}

if (!is.null(hypotheses) && report_data$hypotheses_summary$mean_testability > 0.7) {
  discussion_points <- c(discussion_points,
    "The generated hypotheses show high testability, providing clear directions for experimental validation.")
}

if (!is.null(validation) && report_data$validation_summary$overall_score > 0.8) {
  discussion_points <- c(discussion_points,
    "Statistical validation confirms the robustness of the universal framework analysis.")
}

if (length(discussion_points) > 0) {
  for (point in discussion_points) {
    cat("- ", point, "\\n\\n")
  }
}
```

# Conclusions

This comprehensive analysis using the diversityGPT universal framework has provided novel insights into microbial community structure and assembly processes. The mathematical decomposition approach enables both fundamental understanding and practical applications for microbiome research.

## Key Findings

```{r key-findings, results="asis"}
findings <- character(0)

findings <- c(findings, paste("Universal information decomposition achieved", 
                            report_data$universal_summary$overall_quality, "quality"))

if (!is.null(assembly_mechanisms)) {
  findings <- c(findings, paste(report_data$mechanisms_summary$primary_mechanism, 
                              "identified as primary assembly mechanism"))
}

if (!is.null(hypotheses)) {
  findings <- c(findings, paste(report_data$hypotheses_summary$n_hypotheses, 
                              "testable hypotheses generated"))
}

if (!is.null(literature)) {
  findings <- c(findings, paste(report_data$literature_summary$novel_connections, 
                              "novel research connections identified"))
}

for (finding in findings) {
  cat("- ", finding, "\\n")
}
```

## Future Directions

Based on this analysis, we recommend:

1. Experimental validation of the most testable hypotheses
2. Collection of additional environmental metadata to enhance mechanism detection
3. Temporal sampling to assess community dynamics
4. Cross-system comparison to validate universal patterns

---

*Report generated using diversityGPT v`r report_data$package_version` on `r report_data$generation_time`*'

  return(template)
}

# Internal function: Summary template
.create_summary_template <- function() {
  
  template <- '
# Executive Summary

`r params$abstract`

# Key Results

## Dataset Overview
- **Samples:** `r report_data$data_summary$n_samples`
- **Taxa:** `r report_data$data_summary$n_taxa`
- **Analysis Quality:** `r report_data$universal_summary$overall_quality`

```{r summary-plot}
# Main results visualization
if (inherits(universal_info, "universal_information")) {
  plot(universal_info, type = "components")
}
```

## Primary Findings

```{r primary-findings, results="asis"}
if (!is.null(assembly_mechanisms)) {
  cat("**Primary Assembly Mechanism:** ", report_data$mechanisms_summary$primary_mechanism, 
      " (confidence: ", round(report_data$mechanisms_summary$primary_confidence, 2), ")\\n\\n")
}

if (!is.null(hypotheses)) {
  cat("**Generated Hypotheses:** ", report_data$hypotheses_summary$n_hypotheses, "\\n\\n")
  cat("**Most Testable Hypothesis:** ", hypotheses$hypotheses$hypothesis[1], "\\n\\n")
}

if (!is.null(validation)) {
  cat("**Statistical Validation:** ", report_data$validation_summary$quality_level, 
      " (score: ", round(report_data$validation_summary$overall_score, 2), ")\\n\\n")
}
```

## Recommendations

```{r recommendations, results="asis"}
recommendations <- character(0)

if (!is.null(assembly_mechanisms)) {
  recommendations <- c(recommendations, assembly_mechanisms$interpretation$recommendations[1])
}

if (!is.null(hypotheses) && nrow(hypotheses$hypotheses) > 0) {
  recommendations <- c(recommendations, "Test the highest-scoring ecological hypotheses experimentally")
}

if (!is.null(validation) && report_data$validation_summary$overall_score < 0.8) {
  recommendations <- c(recommendations, "Consider increasing sample size for more robust results")
}

if (length(recommendations) == 0) {
  recommendations <- "Continue monitoring and expand analysis to additional timepoints"
}

for (rec in recommendations) {
  cat("- ", rec, "\\n")
}
```

---

*Summary report generated using diversityGPT v`r report_data$package_version`*'

  return(template)
}

# Internal function: Technical template
.create_technical_template <- function() {
  
  template <- '
# Technical Report: Universal Diversity Decomposition

## Algorithm Overview

The universal information framework implements mathematical decomposition of diversity metrics into fundamental information components. This technical report details the computational methods and validation results.

## Data Processing Pipeline

```{r technical-pipeline, results="asis"}
cat("### Input Data\\n\\n")
cat("- Phyloseq object with", report_data$data_summary$n_samples, "samples and", 
    report_data$data_summary$n_taxa, "taxa\\n")
cat("- Taxonomic ranks:", paste(report_data$data_summary$taxa_ranks, collapse = ", "), "\\n\\n")

cat("### Processing Steps\\n\\n")
cat("1. Universal information extraction\\n")
cat("2. Component decomposition (R, E, P, S)\\n")
cat("3. Transformation matrix computation\\n")
cat("4. Quality assessment\\n\\n")
```

## Mathematical Results

### Transformation Matrix Properties

```{r transformation-matrix}
if (!is.null(universal_info$transformation_matrix)) {
  if (is.matrix(universal_info$transformation_matrix)) {
    cat("Transformation matrix dimensions:", dim(universal_info$transformation_matrix), "\\n")
    cat("Matrix condition number:", kappa(universal_info$transformation_matrix), "\\n")
    cat("Matrix rank:", qr(universal_info$transformation_matrix)$rank, "\\n")
  } else {
    cat("Transformation matrix: data.frame format\\n")
    cat("Number of transformations:", nrow(universal_info$transformation_matrix), "\\n")
  }
}
```

### Component Statistics

```{r component-stats}
if (inherits(universal_info, "universal_information")) {
  components <- universal_info$information_components
  
  component_stats <- data.frame(
    Component = names(components),
    Mean = sapply(components, mean, na.rm = TRUE),
    SD = sapply(components, sd, na.rm = TRUE),
    Min = sapply(components, min, na.rm = TRUE),
    Max = sapply(components, max, na.rm = TRUE)
  )
  
  kable(component_stats, digits = 3, caption = "Component Statistics")
}
```

## Validation Results

```{r validation-technical, results="asis"}
if (!is.null(validation)) {
  cat("### Bootstrap Analysis\\n\\n")
  if (!is.null(validation$bootstrap_ci)) {
    cat("Bootstrap iterations:", validation$bootstrap_ci$bootstrap_summary$n_iterations, "\\n")
    cat("Successful iterations:", validation$bootstrap_ci$bootstrap_summary$n_successful, "\\n")
    cat("Success rate:", round(validation$bootstrap_ci$bootstrap_summary$n_successful / 
                               validation$bootstrap_ci$bootstrap_summary$n_iterations * 100, 1), "%\\n\\n")
  }
  
  cat("### Cross-Validation\\n\\n")
  if (!is.null(validation$cross_validation$summary$overall_performance)) {
    cv_perf <- validation$cross_validation$summary$overall_performance
    cat("Mean R²:", round(cv_perf$mean_r_squared, 3), "\\n")
    cat("Mean RMSE:", round(cv_perf$mean_rmse, 3), "\\n")
    cat("Consistency:", round(cv_perf$consistency, 3), "\\n\\n")
  }
  
  cat("### Mathematical Consistency\\n\\n")
  if (!is.null(validation$consistency_checks)) {
    cat("Checks passed:", validation$consistency_checks$n_passed, "/", 
        validation$consistency_checks$n_checks, "\\n")
    cat("Overall status:", validation$consistency_checks$overall_passed, "\\n\\n")
  }
}
```

## Performance Metrics

```{r performance-metrics, results="asis"}
# Report computational performance if available
perf_report <- get_performance_report()
if (!is.null(perf_report) && perf_report$n_operations > 0) {
  cat("### Computational Performance\\n\\n")
  cat("Total operations:", perf_report$n_operations, "\\n")
  cat("Total time:", round(perf_report$total_time, 2), "seconds\\n\\n")
  
  # Top operations
  if (length(perf_report$operations) > 0) {
    op_summary <- data.frame(
      Operation = sapply(perf_report$operations, function(x) x$operation),
      Time_sec = sapply(perf_report$operations, function(x) x$elapsed),
      stringsAsFactors = FALSE
    )
    
    kable(head(op_summary), digits = 3, caption = "Operation Performance")
  }
}
```

## Technical Specifications

```{r tech-specs, results="asis"}
cat("### Software Environment\\n\\n")
cat("- R version:", as.character(getRversion()), "\\n")
cat("- diversityGPT version:", as.character(report_data$package_version), "\\n")
cat("- Platform:", R.version$platform, "\\n")
cat("- Analysis date:", as.character(report_data$generation_time), "\\n\\n")

cat("### Package Dependencies\\n\\n")
# List key dependencies
key_packages <- c("phyloseq", "vegan", "ggplot2", "dplyr")
for (pkg in key_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("- ", pkg, ": ", as.character(packageVersion(pkg)), "\\n")
  }
}
```

---

*Technical report generated using diversityGPT v`r report_data$package_version`*'

  return(template)
}

# Internal function: Presentation template
.create_presentation_template <- function() {
  
  template <- '
# Microbial Community Analysis Results

## Overview {.tabset}

### Dataset
- **`r report_data$data_summary$n_samples` samples**
- **`r report_data$data_summary$n_taxa` taxa**
- **Analysis quality: `r report_data$universal_summary$overall_quality`**

### Key Finding
```{r main-result, results="asis"}
if (!is.null(assembly_mechanisms)) {
  cat("**", report_data$mechanisms_summary$primary_mechanism, "**\\n\\n")
  cat("Primary assembly mechanism\\n\\n")
  cat("Confidence: ", round(report_data$mechanisms_summary$primary_confidence, 2))
} else {
  cat("**Universal Diversity Decomposition**\\n\\n")
  cat("Components: ", paste(report_data$universal_summary$components_present, collapse = ", "))
}
```

## Visualizations {.tabset}

### Components
```{r viz-components, fig.height=5}
if (inherits(universal_info, "universal_information")) {
  plot(universal_info, type = "components")
}
```

### Network
```{r viz-network, fig.height=5}
if (inherits(universal_info, "universal_information")) {
  plot(universal_info, type = "network")
}
```

### Validation
```{r viz-validation, fig.height=5}
if (!is.null(validation)) {
  plot(validation, type = "overview")
}
```

## Results Summary {.tabset}

### Mechanisms
```{r results-mechanisms, results="asis"}
if (!is.null(assembly_mechanisms)) {
  top_mechanisms <- head(assembly_mechanisms$mechanisms, 3)
  for (i in 1:nrow(top_mechanisms)) {
    cat("**", i, ". ", top_mechanisms$mechanism[i], "**\\n\\n")
    cat("Confidence: ", round(top_mechanisms$confidence[i], 2), "\\n\\n")
    cat(top_mechanisms$description[i], "\\n\\n")
  }
} else {
  cat("Assembly mechanism analysis not performed.")
}
```

### Hypotheses
```{r results-hypotheses, results="asis"}
if (!is.null(hypotheses)) {
  top_hypotheses <- head(hypotheses$hypotheses, 3)
  for (i in 1:nrow(top_hypotheses)) {
    cat("**", i, ". ", top_hypotheses$hypothesis[i], "**\\n\\n")
    cat("Type: ", top_hypotheses$type[i], "\\n\\n")
    cat("Novelty: ", round(top_hypotheses$novelty[i], 2), 
        " | Testability: ", round(top_hypotheses$testability[i], 2), "\\n\\n")
  }
} else {
  cat("Hypothesis generation not performed.")
}
```

### Next Steps
```{r next-steps, results="asis"}
next_steps <- character(0)

if (!is.null(assembly_mechanisms)) {
  next_steps <- c(next_steps, 
                 paste("Investigate", report_data$mechanisms_summary$primary_mechanism, "mechanism experimentally"))
}

if (!is.null(hypotheses) && nrow(hypotheses$hypotheses) > 0) {
  next_steps <- c(next_steps, "Test top-ranked ecological hypotheses")
}

if (!is.null(literature)) {
  next_steps <- c(next_steps, "Follow up on novel research connections")
}

if (length(next_steps) == 0) {
  next_steps <- c("Expand analysis to additional timepoints",
                 "Collect environmental metadata",
                 "Validate findings with independent datasets")
}

for (step in next_steps[1:3]) {
  cat("- ", step, "\\n")
}
```

---

*Generated with diversityGPT v`r report_data$package_version`*'

  return(template)
}

#' Generate Quick Summary Report
#'
#' Create a concise single-page summary of diversityGPT analysis results
#'
#' @param universal_info Universal information object
#' @param output_file Output file path
#' @param format Output format: "html" or "pdf"
#'
#' @export
generate_quick_summary <- function(universal_info, output_file = "diversity_summary", format = "html") {
  
  cat("Generating quick summary report...\n")
  
  # Create minimal report data
  report_data <- list(
    n_samples = nrow(universal_info$information_components),
    n_components = ncol(universal_info$information_components),
    quality = universal_info$deconvolution_quality$overall_quality %||% "Unknown",
    timestamp = Sys.time()
  )
  
  # Simple HTML template
  html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>Diversity Analysis Summary</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background-color: #f0f8ff; padding: 20px; border-radius: 10px; }
        .metric { background-color: #f9f9f9; padding: 15px; margin: 10px 0; border-left: 4px solid #007acc; }
        .quality-%s { border-left-color: %s; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Diversity Analysis Summary</h1>
        <p>Generated on %s using diversityGPT</p>
    </div>
    
    <div class="metric">
        <h3>Dataset Size</h3>
        <p><strong>%d samples</strong> with <strong>%d components</strong> analyzed</p>
    </div>
    
    <div class="metric quality-%s">
        <h3>Analysis Quality</h3>
        <p><strong>%s</strong> - Universal transformation quality assessment</p>
    </div>
    
    <div class="metric">
        <h3>Components Identified</h3>
        <p>%s</p>
    </div>
    
    <p><em>For detailed analysis, generate a comprehensive report using generate_diversity_report()</em></p>
</body>
</html>',
    tolower(report_data$quality),
    ifelse(report_data$quality == "Excellent", "#28a745",
           ifelse(report_data$quality == "Good", "#ffc107", "#dc3545")),
    report_data$timestamp,
    report_data$n_samples,
    report_data$n_components,
    tolower(report_data$quality),
    report_data$quality,
    paste(names(universal_info$information_components), collapse = ", ")
  )
  
  # Write file
  output_path <- paste0(output_file, ".html")
  writeLines(html_content, output_path)
  
  cat("Quick summary generated:", output_path, "\n")
  return(output_path)
}