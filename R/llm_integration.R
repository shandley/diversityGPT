#' LLM-powered interpretation of diversity patterns
#'
#' @description
#' Uses Large Language Models to provide biological interpretation of diversity
#' metric patterns and consensus results. Translates mathematical patterns into
#' ecological insights and generates testable hypotheses.
#'
#' @param results Either a diversity_results or consensus_results object
#' @param context List containing study context information:
#'   - environment: "human_gut", "soil", "marine", "plant_associated", etc.
#'   - condition: Brief description of experimental condition
#'   - organism: "bacteria", "fungi", "archaea", etc.
#'   - additional_info: Any other relevant context
#' @param provider Character string: "anthropic" (default) or "openai"
#' @param hypothesis_generation Logical: whether to generate testable hypotheses
#' @param temperature Numeric: LLM creativity level (0.1-1.0, default 0.7)
#'
#' @return A list containing:
#'   - interpretation: Plain-language explanation of patterns
#'   - biological_insights: Potential biological mechanisms
#'   - hypotheses: Testable predictions (if requested)
#'   - confidence: AI confidence in interpretation
#'   - recommendations: Suggested follow-up analyses
#'
#' @export
#' @examples
#' \dontrun{
#' data(example_physeq)
#' div_results <- calculate_diversity(example_physeq, groups = "Group")
#' 
#' interpretation <- interpret_diversity(
#'   div_results,
#'   context = list(
#'     environment = "human_gut",
#'     condition = "probiotic_treatment",
#'     organism = "bacteria"
#'   )
#' )
#' print(interpretation)
#' }
interpret_diversity <- function(results,
                              context = list(),
                              provider = c("anthropic", "openai"),
                              hypothesis_generation = TRUE,
                              temperature = 0.7) {
  
  provider <- match.arg(provider)
  
  # Check API key availability
  api_key <- get_api_key(provider)
  if (is.null(api_key)) {
    cli::cli_abort("No {provider} API key found. Use check_api_setup() for instructions.")
  }
  
  # Extract patterns from results
  if (inherits(results, "diversity_results")) {
    patterns <- extract_diversity_patterns(results)
    analysis_type <- "diversity_metrics"
  } else if (inherits(results, "consensus_results")) {
    patterns <- extract_consensus_patterns(results)
    analysis_type <- "consensus_analysis"
  } else {
    cli::cli_abort("Input must be diversity_results or consensus_results object")
  }
  
  # Build context prompt
  context_prompt <- build_context_prompt(context)
  
  # Generate interpretation
  cli::cli_alert_info("Generating AI interpretation with {provider}...")
  
  interpretation_result <- tryCatch({
    if (provider == "anthropic") {
      interpret_with_anthropic(patterns, context_prompt, analysis_type, hypothesis_generation, temperature, api_key)
    } else {
      interpret_with_openai(patterns, context_prompt, analysis_type, hypothesis_generation, temperature, api_key)
    }
  }, error = function(e) {
    cli::cli_alert_danger("API request failed: {e$message}")
    return(generate_fallback_interpretation(patterns, context))
  })
  
  # Add metadata
  interpretation_result$provider <- provider
  interpretation_result$timestamp <- Sys.time()
  interpretation_result$context <- context
  
  class(interpretation_result) <- c("diversity_interpretation", "list")
  
  return(interpretation_result)
}

#' Extract patterns from diversity results
#' @keywords internal
extract_diversity_patterns <- function(results) {
  
  metrics <- attr(results, "metrics")
  if (is.null(metrics)) {
    numeric_cols <- sapply(results, is.numeric)
    metrics <- names(results)[numeric_cols]
  }
  
  patterns <- list()
  
  # Calculate summary statistics
  for (metric in metrics) {
    if (metric %in% names(results)) {
      values <- results[[metric]]
      patterns[[metric]] <- list(
        mean = mean(values, na.rm = TRUE),
        sd = sd(values, na.rm = TRUE),
        min = min(values, na.rm = TRUE),
        max = max(values, na.rm = TRUE),
        cv = sd(values, na.rm = TRUE) / mean(values, na.rm = TRUE)
      )
    }
  }
  
  # Group comparisons if groups available
  if ("group" %in% names(results)) {
    patterns$group_comparison <- analyze_group_patterns(results, metrics)
  }
  
  # Metric correlations
  metric_data <- results[, metrics, drop = FALSE]
  patterns$correlations <- cor(metric_data, use = "complete.obs")
  
  patterns$n_samples <- nrow(results)
  patterns$metrics_calculated <- metrics
  
  return(patterns)
}

#' Extract patterns from consensus results
#' @keywords internal
extract_consensus_patterns <- function(results) {
  
  patterns <- list(
    method = results$method,
    dominant_metric = results$interpretation$dominant_metric,
    weight_distribution = results$interpretation$weight_distribution,
    conflict_status = results$interpretation$conflict_status,
    metric_weights = results$method_weights,
    n_samples = results$n_samples
  )
  
  if (!is.null(results$conflict_analysis)) {
    patterns$conflict_details <- results$conflict_analysis
  }
  
  return(patterns)
}

#' Build context prompt for LLM
#' @keywords internal
build_context_prompt <- function(context) {
  
  # Default context
  default_context <- list(
    environment = "microbiome",
    condition = "experimental_study", 
    organism = "microbes",
    additional_info = ""
  )
  
  # Merge with provided context
  context <- modifyList(default_context, context)
  
  prompt <- paste0(
    "You are analyzing a ", context$environment, " microbiome study focusing on ", 
    context$organism, ". The experimental condition involves ", context$condition, "."
  )
  
  if (context$additional_info != "") {
    prompt <- paste0(prompt, " Additional context: ", context$additional_info)
  }
  
  return(prompt)
}

#' Interpret with Anthropic Claude
#' @keywords internal
interpret_with_anthropic <- function(patterns, context_prompt, analysis_type, hypothesis_generation, temperature, api_key) {
  
  # Build the main prompt
  main_prompt <- generate_interpretation_prompt(patterns, context_prompt, analysis_type, hypothesis_generation)
  
  # Anthropic API request
  url <- "https://api.anthropic.com/v1/messages"
  
  body <- list(
    model = "claude-3-5-sonnet-20241022",
    max_tokens = 1500,
    temperature = temperature,
    messages = list(
      list(
        role = "user",
        content = main_prompt
      )
    )
  )
  
  response <- make_api_request(url, body, api_key, "anthropic")
  
  if (is.null(response)) {
    stop("Failed to get response from Anthropic API")
  }
  
  # Parse response
  content <- response$content[[1]]$text
  return(parse_llm_response(content, hypothesis_generation))
}

#' Interpret with OpenAI
#' @keywords internal
interpret_with_openai <- function(patterns, context_prompt, analysis_type, hypothesis_generation, temperature, api_key) {
  
  # Build the main prompt
  main_prompt <- generate_interpretation_prompt(patterns, context_prompt, analysis_type, hypothesis_generation)
  
  # OpenAI API request
  url <- "https://api.openai.com/v1/chat/completions"
  
  body <- list(
    model = "gpt-4",
    max_tokens = 1500,
    temperature = temperature,
    messages = list(
      list(
        role = "system",
        content = "You are an expert microbiome ecologist specialized in diversity analysis and biological interpretation."
      ),
      list(
        role = "user", 
        content = main_prompt
      )
    )
  )
  
  response <- make_api_request(url, body, api_key, "openai")
  
  if (is.null(response)) {
    stop("Failed to get response from OpenAI API")
  }
  
  # Parse response
  content <- response$choices[[1]]$message$content
  return(parse_llm_response(content, hypothesis_generation))
}

#' Generate interpretation prompt
#' @keywords internal
generate_interpretation_prompt <- function(patterns, context_prompt, analysis_type, hypothesis_generation) {
  
  prompt <- paste0(
    context_prompt, "\n\n",
    
    "Please interpret the following diversity analysis results:\n\n"
  )
  
  if (analysis_type == "diversity_metrics") {
    prompt <- paste0(prompt, "DIVERSITY METRICS SUMMARY:\n")
    for (metric in names(patterns$metrics_calculated)) {
      if (!is.null(patterns[[metric]])) {
        prompt <- paste0(prompt, 
          metric, ": mean=", round(patterns[[metric]]$mean, 3),
          ", sd=", round(patterns[[metric]]$sd, 3),
          ", cv=", round(patterns[[metric]]$cv, 3), "\n"
        )
      }
    }
    
    if (!is.null(patterns$group_comparison)) {
      prompt <- paste0(prompt, "\nGROUP COMPARISONS:\n")
      for (i in seq_len(nrow(patterns$group_comparison))) {
        row <- patterns$group_comparison[i, ]
        prompt <- paste0(prompt,
          row$metric, ": p-value=", round(row$p_value, 4),
          ", significant=", row$significant,
          ", effect_size=", round(row$effect_size, 3), "\n"
        )
      }
    }
    
  } else if (analysis_type == "consensus_analysis") {
    prompt <- paste0(prompt, "CONSENSUS ANALYSIS RESULTS:\n")
    prompt <- paste0(prompt, "Method: ", patterns$method, "\n")
    prompt <- paste0(prompt, "Dominant metric: ", patterns$dominant_metric, "\n")
    prompt <- paste0(prompt, "Weight distribution: ", patterns$weight_distribution, "\n")
    prompt <- paste0(prompt, "Conflict status: ", patterns$conflict_status, "\n")
    
    prompt <- paste0(prompt, "\nMETRIC WEIGHTS:\n")
    for (i in seq_along(patterns$metric_weights)) {
      prompt <- paste0(prompt, names(patterns$metric_weights)[i], ": ", 
                      round(patterns$metric_weights[i], 3), "\n")
    }
  }
  
  prompt <- paste0(prompt, "\n\nPlease provide:\n")
  prompt <- paste0(prompt, "1. INTERPRETATION: A clear, plain-language explanation of what these patterns mean biologically\n")
  prompt <- paste0(prompt, "2. BIOLOGICAL_INSIGHTS: Potential ecological mechanisms or processes that could explain these patterns\n")
  prompt <- paste0(prompt, "3. CONFIDENCE: Your confidence level (Low/Medium/High) in this interpretation\n")
  prompt <- paste0(prompt, "4. RECOMMENDATIONS: Suggested follow-up analyses or experiments\n")
  
  if (hypothesis_generation) {
    prompt <- paste0(prompt, "5. HYPOTHESES: 2-3 specific, testable hypotheses based on these patterns\n")
  }
  
  prompt <- paste0(prompt, "\nPlease structure your response with clear section headers.")
  
  return(prompt)
}

#' Parse LLM response into structured format
#' @keywords internal
parse_llm_response <- function(content, hypothesis_generation) {
  
  # Simple parsing - in production you might want more sophisticated parsing
  sections <- list(
    interpretation = extract_section(content, "INTERPRETATION"),
    biological_insights = extract_section(content, "BIOLOGICAL_INSIGHTS"),
    confidence = extract_section(content, "CONFIDENCE"),
    recommendations = extract_section(content, "RECOMMENDATIONS")
  )
  
  if (hypothesis_generation) {
    sections$hypotheses <- extract_section(content, "HYPOTHESES")
  }
  
  # If parsing fails, return the full content
  if (all(sapply(sections, is.null))) {
    sections$interpretation <- content
    sections$confidence <- "Medium"
  }
  
  return(sections)
}

#' Extract section from LLM response
#' @keywords internal
extract_section <- function(content, section_name) {
  
  # Look for section headers
  pattern <- paste0("\\b", section_name, "\\b:?\\s*([\\s\\S]*?)(?=\\n\\d+\\.|\\n[A-Z_]+:|$)")
  match <- regexpr(pattern, content, ignore.case = TRUE, perl = TRUE)
  
  if (match > 0) {
    start <- attr(match, "capture.start")[1]
    length <- attr(match, "capture.length")[1]
    if (start > 0 && length > 0) {
      section_text <- substr(content, start, start + length - 1)
      return(trimws(section_text))
    }
  }
  
  # Fallback: return NULL if section not found
  return(NULL)
}

#' Generate fallback interpretation when API fails
#' @keywords internal
generate_fallback_interpretation <- function(patterns, context) {
  
  interpretation <- "Basic rule-based interpretation: "
  
  if (inherits(patterns, "list") && "metrics_calculated" %in% names(patterns)) {
    # Simple rules for diversity patterns
    shannon_mean <- patterns$shannon$mean
    if (!is.null(shannon_mean)) {
      if (shannon_mean > 2.5) {
        interpretation <- paste0(interpretation, "High Shannon diversity suggests a diverse, even community. ")
      } else if (shannon_mean < 1.5) {
        interpretation <- paste0(interpretation, "Low Shannon diversity suggests dominance by few species. ")
      }
    }
  }
  
  return(list(
    interpretation = paste0(interpretation, "API unavailable - using basic rules."),
    biological_insights = "Limited insights available without AI interpretation.",
    confidence = "Low",
    recommendations = "Retry with working API connection for detailed analysis.",
    hypotheses = "Hypotheses generation requires AI interpretation."
  ))
}

#' Analyze group patterns
#' @keywords internal
analyze_group_patterns <- function(results, metrics) {
  
  groups <- results$group
  pattern_results <- data.frame(
    metric = character(0),
    p_value = numeric(0),
    significant = logical(0),
    effect_size = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (metric in metrics) {
    if (metric %in% names(results)) {
      values <- results[[metric]]
      
      # Simple t-test or ANOVA
      if (length(unique(groups)) == 2) {
        test_result <- tryCatch({
          t.test(values ~ groups)
        }, error = function(e) NULL)
        
        if (!is.null(test_result)) {
          pattern_results <- rbind(pattern_results, data.frame(
            metric = metric,
            p_value = test_result$p.value,
            significant = test_result$p.value < 0.05,
            effect_size = abs(diff(tapply(values, groups, mean, na.rm = TRUE))) / 
                         sqrt(var(values, na.rm = TRUE)),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(pattern_results)
}

#' Print method for diversity interpretation
#'
#' @param x A diversity_interpretation object
#' @param ... Additional arguments (unused)
#' @export
print.diversity_interpretation <- function(x, ...) {
  cli::cli_h1("AI-Powered Diversity Interpretation")
  cli::cli_text("Provider: {x$provider}")
  cli::cli_text("Timestamp: {x$timestamp}")
  cli::cli_text("")
  
  cli::cli_h2("Interpretation")
  cat(x$interpretation, "\n\n")
  
  cli::cli_h2("Biological Insights")
  cat(x$biological_insights, "\n\n")
  
  if (!is.null(x$hypotheses)) {
    cli::cli_h2("Generated Hypotheses")
    cat(x$hypotheses, "\n\n")
  }
  
  cli::cli_h2("Recommendations")
  cat(x$recommendations, "\n\n")
  
  cli::cli_text("Confidence: {x$confidence}")
}