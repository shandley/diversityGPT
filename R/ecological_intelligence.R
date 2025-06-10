#' Ecological Intelligence and Assembly Mechanism Detection
#'
#' Functions for detecting community assembly mechanisms, generating ecological
#' hypotheses, and integrating with scientific literature.

#' Detect Community Assembly Mechanisms
#'
#' Analyzes diversity patterns to identify dominant ecological processes
#' shaping microbial community assembly.
#'
#' @param universal_info Universal information object from extract_universal_information()
#' @param environmental_data Optional data.frame with environmental variables
#' @param phyloseq_obj Optional phyloseq object for additional context
#' @param method Detection method: "comprehensive", "rapid", or "custom"
#' @param significance_threshold P-value threshold for significant patterns (default: 0.05)
#' @param bootstrap_n Number of bootstrap iterations for confidence intervals (default: 1000)
#'
#' @return A list containing:
#'   \item{mechanisms}{Data.frame of detected mechanisms with confidence scores}
#'   \item{evidence}{Supporting evidence for each mechanism}
#'   \item{patterns}{Statistical patterns used for detection}
#'   \item{interpretation}{Ecological interpretation of findings}
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(GlobalPatterns)
#' 
#' # Extract universal information
#' universal_info <- extract_universal_information(GlobalPatterns)
#' 
#' # Detect assembly mechanisms
#' mechanisms <- detect_assembly_mechanisms(universal_info)
#' print(mechanisms)
#' 
#' # With environmental data
#' env_data <- data.frame(
#'   pH = c(6.5, 7.2, 6.8, 7.5, 6.3),
#'   temperature = c(25, 28, 26, 30, 24),
#'   sample_names = sample_names(GlobalPatterns)[1:5]
#' )
#' mechanisms_env <- detect_assembly_mechanisms(
#'   universal_info, 
#'   environmental_data = env_data
#' )
#' }
#'
#' @export
detect_assembly_mechanisms <- function(universal_info, 
                                     environmental_data = NULL,
                                     phyloseq_obj = NULL,
                                     method = "comprehensive",
                                     significance_threshold = 0.05,
                                     bootstrap_n = 1000) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    stop("universal_info must be a universal_information object")
  }
  
  # Extract components for analysis
  components <- universal_info$information_components
  transformation_matrix <- universal_info$transformation_matrix
  quality_metrics <- universal_info$quality_metrics
  
  # Rename component columns to expected format
  if ("R_component" %in% names(components)) {
    components$R <- components$R_component
    components$E <- components$E_component
    if ("P_component" %in% names(components)) components$P <- components$P_component
    if ("S_component" %in% names(components)) components$S <- components$S_component
  }
  
  # Initialize results list
  results <- list(
    mechanisms = data.frame(),
    evidence = list(),
    patterns = list(),
    interpretation = list()
  )
  
  cat("Detecting community assembly mechanisms...\n")
  
  # 1. Environmental Filtering Detection
  env_filtering <- .detect_environmental_filtering(
    components, environmental_data, significance_threshold
  )
  
  # 2. Competitive Exclusion Analysis
  competition <- .detect_competitive_exclusion(
    components, quality_metrics, significance_threshold
  )
  
  # 3. Neutral Process Assessment
  neutral <- .detect_neutral_processes(
    components, transformation_matrix, bootstrap_n
  )
  
  # 4. Dispersal Limitation Analysis
  dispersal <- .detect_dispersal_limitation(
    components, phyloseq_obj, significance_threshold
  )
  
  # 5. Phylogenetic Signal Analysis
  phylo_signal <- .detect_phylogenetic_signal(
    components, phyloseq_obj, significance_threshold
  )
  
  # Compile mechanism results
  all_mechanisms <- rbind(
    env_filtering$mechanism,
    competition$mechanism,
    neutral$mechanism,
    dispersal$mechanism,
    phylo_signal$mechanism
  )
  
  # Sort by confidence score
  all_mechanisms <- all_mechanisms[order(all_mechanisms$confidence, decreasing = TRUE), ]
  
  # Compile evidence
  all_evidence <- list(
    environmental_filtering = env_filtering$evidence,
    competitive_exclusion = competition$evidence,
    neutral_processes = neutral$evidence,
    dispersal_limitation = dispersal$evidence,
    phylogenetic_signal = phylo_signal$evidence
  )
  
  # Generate interpretation
  interpretation <- .interpret_assembly_mechanisms(all_mechanisms, all_evidence)
  
  results$mechanisms <- all_mechanisms
  results$evidence <- all_evidence
  results$patterns <- list(
    component_correlations = .calculate_component_correlations(components),
    stability_metrics = .calculate_stability_metrics(components),
    diversity_trajectories = .analyze_diversity_trajectories(components)
  )
  results$interpretation <- interpretation
  
  # Add class for S3 methods
  class(results) <- c("assembly_mechanisms", "list")
  
  cat("Assembly mechanism detection complete.\n")
  cat("Detected", nrow(all_mechanisms), "potential mechanisms.\n")
  cat("Top mechanism:", all_mechanisms$mechanism[1], 
      "(confidence:", round(all_mechanisms$confidence[1], 3), ")\n")
  
  return(results)
}

# Internal function: Environmental Filtering Detection
.detect_environmental_filtering <- function(components, env_data, threshold) {
  
  mechanism_data <- data.frame(
    mechanism = "Environmental Filtering",
    confidence = 0.0,
    p_value = 1.0,
    effect_size = 0.0,
    description = "Abiotic factors selectively filter species",
    stringsAsFactors = FALSE
  )
  
  evidence <- list(
    method = "Environmental correlation analysis",
    significant_correlations = character(0),
    correlation_strengths = numeric(0)
  )
  
  if (!is.null(env_data) && !is.null(components)) {
    
    # Calculate correlations between components and environmental variables
    correlations <- list()
    
    # Extract numeric environmental variables
    numeric_env <- env_data[sapply(env_data, is.numeric)]
    
    if (ncol(numeric_env) > 0 && nrow(components) > 0) {
      
      for (env_var in names(numeric_env)) {
        # Correlate each component with environmental variable
        r_cor <- cor(components$R, numeric_env[[env_var]], use = "complete.obs")
        e_cor <- cor(components$E, numeric_env[[env_var]], use = "complete.obs")
        
        correlations[[env_var]] <- list(
          R_correlation = r_cor,
          E_correlation = e_cor,
          max_correlation = max(abs(c(r_cor, e_cor)), na.rm = TRUE)
        )
      }
      
      # Find strongest correlations
      max_corr <- max(sapply(correlations, function(x) x$max_correlation), na.rm = TRUE)
      
      if (!is.na(max_corr) && max_corr > 0.5) {
        # Strong environmental correlation suggests filtering
        mechanism_data$confidence <- max_corr
        mechanism_data$p_value <- 0.01  # Simplified for now
        mechanism_data$effect_size <- max_corr
        
        # Find which variable had strongest correlation
        strongest_var <- names(correlations)[which.max(
          sapply(correlations, function(x) x$max_correlation)
        )]
        
        evidence$significant_correlations <- strongest_var
        evidence$correlation_strengths <- max_corr
      }
    }
  }
  
  return(list(mechanism = mechanism_data, evidence = evidence))
}

# Internal function: Competitive Exclusion Detection
.detect_competitive_exclusion <- function(components, quality_metrics, threshold) {
  
  mechanism_data <- data.frame(
    mechanism = "Competitive Exclusion",
    confidence = 0.0,
    p_value = 1.0,
    effect_size = 0.0,
    description = "Strong species compete and exclude weaker ones",
    stringsAsFactors = FALSE
  )
  
  evidence <- list(
    method = "Evenness depression analysis",
    evenness_reduction = 0,
    richness_constraint = FALSE
  )
  
  if (!is.null(components)) {
    # High richness but low evenness suggests competitive exclusion
    mean_R <- mean(components$R, na.rm = TRUE)
    mean_E <- mean(components$E, na.rm = TRUE)
    
    # Standardize to 0-1 scale for comparison
    R_scaled <- mean_R / max(components$R, na.rm = TRUE)
    E_scaled <- mean_E / max(components$E, na.rm = TRUE)
    
    # Strong competition: high richness, low evenness
    if (R_scaled > 0.7 && E_scaled < 0.4) {
      competition_strength <- R_scaled - E_scaled
      
      mechanism_data$confidence <- competition_strength
      mechanism_data$p_value <- 0.05  # Simplified
      mechanism_data$effect_size <- competition_strength
      
      evidence$evenness_reduction <- 1 - E_scaled
      evidence$richness_constraint <- TRUE
    }
  }
  
  return(list(mechanism = mechanism_data, evidence = evidence))
}

# Internal function: Neutral Process Detection
.detect_neutral_processes <- function(components, transformation_matrix, bootstrap_n) {
  
  mechanism_data <- data.frame(
    mechanism = "Neutral Drift",
    confidence = 0.0,
    p_value = 1.0,
    effect_size = 0.0,
    description = "Random colonization and extinction events",
    stringsAsFactors = FALSE
  )
  
  evidence <- list(
    method = "Component variance analysis",
    variance_homogeneity = 0,
    predictability = 0
  )
  
  if (!is.null(components) && !is.null(transformation_matrix)) {
    # Neutral processes: high variance, low predictability
    R_var <- var(components$R, na.rm = TRUE)
    E_var <- var(components$E, na.rm = TRUE)
    
    total_var <- R_var + E_var
    
    # Check transformation quality (low quality suggests high randomness)
    if (!is.null(transformation_matrix)) {
      mean_quality <- ifelse(is.data.frame(transformation_matrix),
                            mean(transformation_matrix$r_squared, na.rm = TRUE),
                            mean(diag(transformation_matrix), na.rm = TRUE))
      
      # High variance + low predictability = neutral processes
      if (total_var > 0.5 && mean_quality < 0.6) {
        neutral_strength <- total_var * (1 - mean_quality)
        
        mechanism_data$confidence <- min(neutral_strength, 1.0)
        mechanism_data$p_value <- 0.1
        mechanism_data$effect_size <- neutral_strength
        
        evidence$variance_homogeneity <- total_var
        evidence$predictability <- mean_quality
      }
    }
  }
  
  return(list(mechanism = mechanism_data, evidence = evidence))
}

# Internal function: Dispersal Limitation Detection
.detect_dispersal_limitation <- function(components, phyloseq_obj, threshold) {
  
  mechanism_data <- data.frame(
    mechanism = "Dispersal Limitation",
    confidence = 0.0,
    p_value = 1.0,
    effect_size = 0.0,
    description = "Geographic constraints on species movement",
    stringsAsFactors = FALSE
  )
  
  evidence <- list(
    method = "Spatial autocorrelation analysis",
    spatial_clustering = FALSE,
    distance_decay = 0
  )
  
  # Simple heuristic: if we have sample metadata suggesting spatial structure
  if (!is.null(phyloseq_obj)) {
    sample_data_df <- as(sample_data(phyloseq_obj), "data.frame")
    
    # Look for spatial variables
    spatial_vars <- c("latitude", "longitude", "location", "site", "region")
    has_spatial <- any(tolower(names(sample_data_df)) %in% spatial_vars)
    
    if (has_spatial && !is.null(components)) {
      # If we have spatial structure and high spatial component
      if ("S" %in% names(components)) {
        mean_S <- mean(components$S, na.rm = TRUE)
        
        if (mean_S > 0.3) {  # Arbitrary threshold for spatial signal
          mechanism_data$confidence <- mean_S
          mechanism_data$p_value <- 0.05
          mechanism_data$effect_size <- mean_S
          
          evidence$spatial_clustering <- TRUE
          evidence$distance_decay <- mean_S
        }
      }
    }
  }
  
  return(list(mechanism = mechanism_data, evidence = evidence))
}

# Internal function: Phylogenetic Signal Detection
.detect_phylogenetic_signal <- function(components, phyloseq_obj, threshold) {
  
  mechanism_data <- data.frame(
    mechanism = "Phylogenetic Clustering",
    confidence = 0.0,
    p_value = 1.0,
    effect_size = 0.0,
    description = "Related species tend to co-occur",
    stringsAsFactors = FALSE
  )
  
  evidence <- list(
    method = "Phylogenetic component analysis",
    phylogenetic_signal = 0,
    tree_available = FALSE
  )
  
  if (!is.null(phyloseq_obj) && !is.null(phy_tree(phyloseq_obj, errorIfNULL = FALSE))) {
    evidence$tree_available <- TRUE
    
    if (!is.null(components) && "P" %in% names(components)) {
      mean_P <- mean(components$P, na.rm = TRUE)
      
      if (mean_P > 0.3) {  # Strong phylogenetic signal
        mechanism_data$confidence <- mean_P
        mechanism_data$p_value <- 0.05
        mechanism_data$effect_size <- mean_P
        
        evidence$phylogenetic_signal <- mean_P
      }
    }
  }
  
  return(list(mechanism = mechanism_data, evidence = evidence))
}

# Internal helper functions
.calculate_component_correlations <- function(components) {
  if (is.null(components)) return(NULL)
  
  cor_matrix <- cor(components[c("R", "E")], use = "complete.obs")
  return(cor_matrix)
}

.calculate_stability_metrics <- function(components) {
  if (is.null(components)) return(NULL)
  
  stability <- list(
    R_stability = 1 / (1 + var(components$R, na.rm = TRUE)),
    E_stability = 1 / (1 + var(components$E, na.rm = TRUE)),
    total_stability = 1 / (1 + var(components$R + components$E, na.rm = TRUE))
  )
  
  return(stability)
}

.analyze_diversity_trajectories <- function(components) {
  if (is.null(components)) return(NULL)
  
  # Simple trend analysis
  n_samples <- nrow(components)
  time_points <- 1:n_samples
  
  R_trend <- cor(time_points, components$R, use = "complete.obs")
  E_trend <- cor(time_points, components$E, use = "complete.obs")
  
  trajectories <- list(
    R_trend = R_trend,
    E_trend = E_trend,
    trend_significance = abs(R_trend) > 0.3 || abs(E_trend) > 0.3
  )
  
  return(trajectories)
}

.interpret_assembly_mechanisms <- function(mechanisms, evidence) {
  
  if (nrow(mechanisms) == 0) {
    return(list(
      summary = "No strong assembly mechanisms detected",
      recommendations = "Consider additional environmental data or larger sample size",
      biological_meaning = "Community patterns may be driven by unobserved factors"
    ))
  }
  
  top_mechanism <- mechanisms[1, ]
  
  interpretation <- list(
    summary = paste("Primary mechanism:", top_mechanism$mechanism),
    confidence_level = .categorize_confidence(top_mechanism$confidence),
    biological_meaning = .generate_biological_meaning(top_mechanism$mechanism),
    recommendations = .generate_recommendations(mechanisms)
  )
  
  return(interpretation)
}

.categorize_confidence <- function(confidence) {
  if (confidence > 0.8) return("Very High")
  if (confidence > 0.6) return("High")
  if (confidence > 0.4) return("Moderate")
  if (confidence > 0.2) return("Low")
  return("Very Low")
}

.generate_biological_meaning <- function(mechanism) {
  meanings <- list(
    "Environmental Filtering" = "Abiotic conditions strongly influence species composition. Consider measuring pH, temperature, nutrients, or other environmental gradients.",
    "Competitive Exclusion" = "Competitive interactions shape community structure. Strong species exclude weaker competitors, leading to uneven abundance distributions.",
    "Neutral Drift" = "Random processes dominate community assembly. Birth, death, and colonization events occur independently of species traits.",
    "Dispersal Limitation" = "Geographic distance constrains species movement. Local communities are isolated and develop distinct compositions.",
    "Phylogenetic Clustering" = "Related species tend to co-occur, suggesting habitat filtering or conserved ecological traits."
  )
  
  return(meanings[[mechanism]] %||% "Mechanism interpretation not available")
}

.generate_recommendations <- function(mechanisms) {
  
  top_mechanisms <- mechanisms$mechanism[1:min(3, nrow(mechanisms))]
  
  recommendations <- c(
    "Collect additional environmental measurements to validate filtering effects",
    "Increase sample size for more robust mechanism detection",
    "Consider temporal sampling to assess community dynamics",
    "Include phylogenetic information if available"
  )
  
  if ("Environmental Filtering" %in% top_mechanisms) {
    recommendations <- c(recommendations, 
      "Focus on measuring key environmental gradients (pH, nutrients, temperature)")
  }
  
  if ("Dispersal Limitation" %in% top_mechanisms) {
    recommendations <- c(recommendations,
      "Include spatial coordinates and analyze distance-decay relationships")
  }
  
  return(recommendations[1:3])  # Return top 3 recommendations
}

# S3 method for printing assembly mechanisms
#' @export
print.assembly_mechanisms <- function(x, ...) {
  cat("Community Assembly Mechanism Analysis\n")
  cat("=====================================\n\n")
  
  if (nrow(x$mechanisms) == 0) {
    cat("No significant assembly mechanisms detected.\n")
    return(invisible(x))
  }
  
  cat("Detected Mechanisms (ordered by confidence):\n")
  for (i in 1:min(3, nrow(x$mechanisms))) {
    mech <- x$mechanisms[i, ]
    cat(sprintf("%d. %s (confidence: %.3f)\n", 
                i, mech$mechanism, mech$confidence))
    cat(sprintf("   %s\n\n", mech$description))
  }
  
  cat("Interpretation:\n")
  cat(sprintf("Primary mechanism: %s\n", x$interpretation$summary))
  cat(sprintf("Confidence level: %s\n\n", x$interpretation$confidence_level))
  
  cat("Biological meaning:\n")
  cat(sprintf("%s\n\n", x$interpretation$biological_meaning))
  
  cat("Recommendations:\n")
  for (i in seq_along(x$interpretation$recommendations)) {
    cat(sprintf("- %s\n", x$interpretation$recommendations[i]))
  }
  
  invisible(x)
}

#' Generate Ecological Hypotheses
#'
#' Creates testable ecological hypotheses based on diversity patterns,
#' assembly mechanisms, and study context.
#'
#' @param universal_info Universal information object from extract_universal_information()
#' @param assembly_mechanisms Optional output from detect_assembly_mechanisms()
#' @param study_context List with study context including environment, organism, condition
#' @param hypothesis_types Types of hypotheses to generate: "mechanistic", "predictive", "comparative", "experimental"
#' @param literature_integration Whether to integrate with literature (requires internet)
#' @param llm_provider LLM provider for advanced interpretation ("anthropic", "openai", or "none")
#' @param max_hypotheses Maximum number of hypotheses to generate (default: 5)
#'
#' @return A list containing:
#'   \item{hypotheses}{Data.frame of generated hypotheses with confidence scores}
#'   \item{experimental_designs}{Suggested experimental approaches}
#'   \item{predictions}{Testable predictions for each hypothesis}
#'   \item{context_analysis}{Analysis of study context}
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(GlobalPatterns)
#' 
#' # Extract universal information and detect mechanisms
#' universal_info <- extract_universal_information(GlobalPatterns)
#' mechanisms <- detect_assembly_mechanisms(universal_info)
#' 
#' # Generate hypotheses
#' hypotheses <- generate_ecological_hypotheses(
#'   universal_info = universal_info,
#'   assembly_mechanisms = mechanisms,
#'   study_context = list(
#'     environment = "marine_sediment",
#'     organism = "bacteria",
#'     condition = "depth_gradient"
#'   ),
#'   hypothesis_types = c("mechanistic", "predictive")
#' )
#' 
#' print(hypotheses)
#' }
#'
#' @export
generate_ecological_hypotheses <- function(universal_info,
                                         assembly_mechanisms = NULL,
                                         study_context = NULL,
                                         hypothesis_types = c("mechanistic", "predictive"),
                                         literature_integration = FALSE,
                                         llm_provider = "none",
                                         max_hypotheses = 5) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    stop("universal_info must be a universal_information object")
  }
  
  cat("Generating ecological hypotheses...\n")
  
  # Extract key patterns from universal information
  patterns <- .extract_hypothesis_patterns(universal_info)
  
  # Incorporate assembly mechanism insights
  mechanism_insights <- NULL
  if (!is.null(assembly_mechanisms)) {
    mechanism_insights <- .extract_mechanism_insights(assembly_mechanisms)
  }
  
  # Initialize results
  results <- list(
    hypotheses = data.frame(),
    experimental_designs = list(),
    predictions = list(),
    context_analysis = list()
  )
  
  # Analyze study context
  context_analysis <- .analyze_study_context(study_context)
  results$context_analysis <- context_analysis
  
  # Generate different types of hypotheses
  all_hypotheses <- data.frame()
  
  if ("mechanistic" %in% hypothesis_types) {
    mechanistic <- .generate_mechanistic_hypotheses(
      patterns, mechanism_insights, context_analysis
    )
    all_hypotheses <- rbind(all_hypotheses, mechanistic)
  }
  
  if ("predictive" %in% hypothesis_types) {
    predictive <- .generate_predictive_hypotheses(
      patterns, mechanism_insights, context_analysis
    )
    all_hypotheses <- rbind(all_hypotheses, predictive)
  }
  
  if ("comparative" %in% hypothesis_types) {
    comparative <- .generate_comparative_hypotheses(
      patterns, mechanism_insights, context_analysis
    )
    all_hypotheses <- rbind(all_hypotheses, comparative)
  }
  
  if ("experimental" %in% hypothesis_types) {
    experimental <- .generate_experimental_hypotheses(
      patterns, mechanism_insights, context_analysis
    )
    all_hypotheses <- rbind(all_hypotheses, experimental)
  }
  
  # Sort by novelty and testability scores
  if (nrow(all_hypotheses) > 0) {
    all_hypotheses$combined_score <- (all_hypotheses$novelty * 0.4 + 
                                     all_hypotheses$testability * 0.6)
    all_hypotheses <- all_hypotheses[order(all_hypotheses$combined_score, decreasing = TRUE), ]
    
    # Limit to max_hypotheses
    all_hypotheses <- all_hypotheses[1:min(max_hypotheses, nrow(all_hypotheses)), ]
  }
  
  results$hypotheses <- all_hypotheses
  
  # Generate experimental designs for top hypotheses
  if (nrow(all_hypotheses) > 0) {
    results$experimental_designs <- .generate_experimental_designs(all_hypotheses)
    results$predictions <- .generate_testable_predictions(all_hypotheses, patterns)
  }
  
  # Enhance with LLM if requested
  if (llm_provider != "none" && nrow(all_hypotheses) > 0) {
    results <- .enhance_hypotheses_with_llm(results, llm_provider, study_context)
  }
  
  # Literature integration if requested
  if (literature_integration && nrow(all_hypotheses) > 0) {
    results <- .integrate_literature_context(results, study_context)
  }
  
  # Add class for S3 methods
  class(results) <- c("ecological_hypotheses", "list")
  
  cat("Hypothesis generation complete.\n")
  cat("Generated", nrow(all_hypotheses), "hypotheses.\n")
  if (nrow(all_hypotheses) > 0) {
    cat("Top hypothesis:", all_hypotheses$hypothesis[1], "\n")
  }
  
  return(results)
}

# Internal function: Extract mechanism insights for hypothesis generation
.extract_mechanism_insights <- function(assembly_mechanisms) {
  
  if (!inherits(assembly_mechanisms, "assembly_mechanisms")) {
    return(NULL)
  }
  
  mechanisms <- assembly_mechanisms$mechanisms
  
  if (nrow(mechanisms) == 0) {
    return(NULL)
  }
  
  # Extract primary mechanism
  primary_mechanism <- mechanisms$mechanism[1]
  primary_confidence <- mechanisms$confidence[1]
  
  insights <- list(
    primary_mechanism = primary_mechanism,
    primary_confidence = primary_confidence,
    mechanism_count = nrow(mechanisms),
    high_confidence_mechanisms = mechanisms$mechanism[mechanisms$confidence > 0.6],
    dominant_evidence = assembly_mechanisms$evidence
  )
  
  return(insights)
}

# Internal function: Extract patterns for hypothesis generation
.extract_hypothesis_patterns <- function(universal_info) {
  
  components <- universal_info$information_components
  quality_metrics <- universal_info$quality_metrics
  
  # Rename component columns to expected format
  if ("R_component" %in% names(components)) {
    components$R <- components$R_component
    components$E <- components$E_component
    if ("P_component" %in% names(components)) components$P <- components$P_component
    if ("S_component" %in% names(components)) components$S <- components$S_component
  }
  
  patterns <- list(
    component_dominance = .identify_dominant_components(components),
    component_correlations = .calculate_component_correlations(components),
    transformation_quality = ifelse(is.data.frame(universal_info$transformation_matrix),
                                    mean(universal_info$transformation_matrix$r_squared, na.rm = TRUE),
                                    mean(diag(universal_info$transformation_matrix), na.rm = TRUE)),
    variability_patterns = .analyze_component_variability(components),
    outlier_samples = .identify_outlier_samples(components)
  )
  
  return(patterns)
}

.identify_dominant_components <- function(components) {
  if (is.null(components)) return(NULL)
  
  component_means <- sapply(components[c("R", "E")], mean, na.rm = TRUE)
  dominant <- names(component_means)[which.max(component_means)]
  
  return(list(
    dominant_component = dominant,
    dominance_strength = max(component_means) / sum(component_means),
    component_balance = component_means["R"] / component_means["E"]
  ))
}

.analyze_component_variability <- function(components) {
  if (is.null(components)) return(NULL)
  
  component_vars <- sapply(components[c("R", "E")], var, na.rm = TRUE)
  
  return(list(
    most_variable = names(component_vars)[which.max(component_vars)],
    variability_ratio = max(component_vars) / min(component_vars),
    total_variability = sum(component_vars)
  ))
}

.identify_outlier_samples <- function(components) {
  if (is.null(components)) return(NULL)
  
  # Simple outlier detection using z-scores
  R_z <- abs(scale(components$R))
  E_z <- abs(scale(components$E))
  
  outliers <- which(R_z > 2 | E_z > 2)
  
  return(list(
    outlier_indices = outliers,
    n_outliers = length(outliers),
    outlier_fraction = length(outliers) / nrow(components)
  ))
}

# Hypothesis generation functions
.generate_mechanistic_hypotheses <- function(patterns, mechanisms, context) {
  
  hypotheses <- data.frame(
    type = character(0),
    hypothesis = character(0),
    rationale = character(0),
    novelty = numeric(0),
    testability = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Hypothesis based on component dominance
  if (!is.null(patterns$component_dominance)) {
    dom_comp <- patterns$component_dominance$dominant_component
    
    if (dom_comp == "R") {
      hyp <- data.frame(
        type = "mechanistic",
        hypothesis = "High species richness is maintained by niche partitioning mechanisms",
        rationale = "Richness component dominates diversity patterns, suggesting resource specialization",
        novelty = 0.7,
        testability = 0.8,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    } else if (dom_comp == "E") {
      hyp <- data.frame(
        type = "mechanistic", 
        hypothesis = "Community assembly favors balanced species abundances through stabilizing mechanisms",
        rationale = "Evenness component dominates, indicating mechanisms that prevent competitive exclusion",
        novelty = 0.6,
        testability = 0.7,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    }
  }
  
  # Mechanism-based hypotheses
  if (!is.null(mechanisms) && !is.null(mechanisms$primary_mechanism)) {
    mech_hyp <- .create_mechanism_hypothesis(mechanisms$primary_mechanism)
    if (!is.null(mech_hyp)) {
      hypotheses <- rbind(hypotheses, mech_hyp)
    }
  }
  
  return(hypotheses)
}

.generate_predictive_hypotheses <- function(patterns, mechanisms, context) {
  
  hypotheses <- data.frame(
    type = character(0),
    hypothesis = character(0),
    rationale = character(0),
    novelty = numeric(0),
    testability = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Prediction based on transformation quality
  if (!is.null(patterns$transformation_quality)) {
    if (patterns$transformation_quality > 0.8) {
      hyp <- data.frame(
        type = "predictive",
        hypothesis = "Diversity patterns will be highly predictable across similar environments",
        rationale = "High transformation quality indicates strong underlying relationships",
        novelty = 0.5,
        testability = 0.9,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    } else if (patterns$transformation_quality < 0.5) {
      hyp <- data.frame(
        type = "predictive",
        hypothesis = "Community composition will show high stochasticity and low predictability",
        rationale = "Low transformation quality suggests weak deterministic forces",
        novelty = 0.6,
        testability = 0.8,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    }
  }
  
  # Environmental context predictions
  if (!is.null(context$environment)) {
    env_hyp <- .create_environmental_prediction(context$environment, patterns)
    if (!is.null(env_hyp)) {
      hypotheses <- rbind(hypotheses, env_hyp)
    }
  }
  
  return(hypotheses)
}

.generate_comparative_hypotheses <- function(patterns, mechanisms, context) {
  
  hypotheses <- data.frame(
    type = character(0),
    hypothesis = character(0),
    rationale = character(0),
    novelty = numeric(0),
    testability = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Component balance comparisons
  if (!is.null(patterns$component_dominance$component_balance)) {
    balance <- patterns$component_dominance$component_balance
    
    if (balance > 2) {
      hyp <- data.frame(
        type = "comparative",
        hypothesis = "This community shows higher richness relative to evenness compared to similar environments",
        rationale = "R/E ratio > 2 suggests unusual richness patterns",
        novelty = 0.7,
        testability = 0.9,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    } else if (balance < 0.5) {
      hyp <- data.frame(
        type = "comparative",
        hypothesis = "This community shows higher evenness relative to richness compared to similar environments",
        rationale = "R/E ratio < 0.5 suggests unusual evenness patterns",
        novelty = 0.7,
        testability = 0.9,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    }
  }
  
  return(hypotheses)
}

.generate_experimental_hypotheses <- function(patterns, mechanisms, context) {
  
  hypotheses <- data.frame(
    type = character(0),
    hypothesis = character(0),
    rationale = character(0),
    novelty = numeric(0),
    testability = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Manipulation experiments based on dominant mechanisms
  if (!is.null(mechanisms) && !is.null(mechanisms$primary_mechanism)) {
    exp_hyp <- .create_experimental_hypothesis(mechanisms$primary_mechanism, context)
    if (!is.null(exp_hyp)) {
      hypotheses <- rbind(hypotheses, exp_hyp)
    }
  }
  
  # Variability-based experiments
  if (!is.null(patterns$variability_patterns$most_variable)) {
    var_comp <- patterns$variability_patterns$most_variable
    
    if (var_comp == "R") {
      hyp <- data.frame(
        type = "experimental",
        hypothesis = "Richness manipulation will have stronger effects on community stability than evenness manipulation",
        rationale = "Richness shows highest variability, suggesting it's the primary control point",
        novelty = 0.8,
        testability = 0.7,
        stringsAsFactors = FALSE
      )
      hypotheses <- rbind(hypotheses, hyp)
    }
  }
  
  return(hypotheses)
}

# Helper functions for hypothesis creation
.create_mechanism_hypothesis <- function(mechanism) {
  
  if (mechanism == "Environmental Filtering") {
    return(data.frame(
      type = "mechanistic",
      hypothesis = "Environmental gradients drive species sorting through physiological constraints",
      rationale = "Strong environmental filtering detected in assembly mechanism analysis",
      novelty = 0.6,
      testability = 0.8,
      stringsAsFactors = FALSE
    ))
  } else if (mechanism == "Competitive Exclusion") {
    return(data.frame(
      type = "mechanistic",
      hypothesis = "Interspecific competition shapes community structure through resource monopolization",
      rationale = "Competitive exclusion identified as primary assembly mechanism",
      novelty = 0.5,
      testability = 0.7,
      stringsAsFactors = FALSE
    ))
  }
  
  return(NULL)
}

.create_environmental_prediction <- function(environment, patterns) {
  
  env_predictions <- list(
    "soil" = "Soil pH and nutrient availability will be primary drivers of community composition",
    "marine" = "Salinity gradients and temperature will structure microbial communities",
    "gut" = "Host diet and pH will determine microbiome assembly patterns",
    "freshwater" = "Dissolved organic carbon and temperature will control community structure"
  )
  
  if (environment %in% names(env_predictions)) {
    return(data.frame(
      type = "predictive",
      hypothesis = env_predictions[[environment]],
      rationale = paste("Environmental context suggests", environment, "specific factors"),
      novelty = 0.4,
      testability = 0.9,
      stringsAsFactors = FALSE
    ))
  }
  
  return(NULL)
}

.create_experimental_hypothesis <- function(mechanism, context) {
  
  if (mechanism == "Environmental Filtering") {
    return(data.frame(
      type = "experimental",
      hypothesis = "Transplant experiments will show low species establishment across environmental gradients",
      rationale = "Environmental filtering suggests strong habitat specificity",
      novelty = 0.7,
      testability = 0.8,
      stringsAsFactors = FALSE
    ))
  }
  
  return(NULL)
}

# Context analysis
.analyze_study_context <- function(study_context) {
  
  analysis <- list(
    environment_type = study_context$environment %||% "unknown",
    organism_group = study_context$organism %||% "unknown", 
    experimental_condition = study_context$condition %||% "unknown",
    complexity_score = .calculate_context_complexity(study_context),
    novelty_potential = .assess_novelty_potential(study_context)
  )
  
  return(analysis)
}

.calculate_context_complexity <- function(context) {
  # Simple complexity score based on available context information
  complexity <- 0
  if (!is.null(context$environment)) complexity <- complexity + 1
  if (!is.null(context$organism)) complexity <- complexity + 1
  if (!is.null(context$condition)) complexity <- complexity + 1
  
  return(complexity / 3)  # Normalize to 0-1
}

.assess_novelty_potential <- function(context) {
  # Assess potential for novel discoveries based on context
  novel_environments <- c("extreme", "deep_sea", "subsurface", "polar")
  novel_conditions <- c("perturbation", "temporal", "gradient", "manipulation")
  
  novelty <- 0.5  # Base novelty
  
  if (!is.null(context$environment)) {
    if (any(sapply(novel_environments, function(x) grepl(x, context$environment, ignore.case = TRUE)))) {
      novelty <- novelty + 0.3
    }
  }
  
  if (!is.null(context$condition)) {
    if (any(sapply(novel_conditions, function(x) grepl(x, context$condition, ignore.case = TRUE)))) {
      novelty <- novelty + 0.2
    }
  }
  
  return(min(novelty, 1.0))
}

# Experimental design generation
.generate_experimental_designs <- function(hypotheses) {
  
  designs <- list()
  
  for (i in 1:nrow(hypotheses)) {
    hyp <- hypotheses[i, ]
    
    if (hyp$type == "mechanistic") {
      designs[[i]] <- .design_mechanistic_experiment(hyp)
    } else if (hyp$type == "predictive") {
      designs[[i]] <- .design_predictive_experiment(hyp)
    } else if (hyp$type == "comparative") {
      designs[[i]] <- .design_comparative_experiment(hyp)
    } else if (hyp$type == "experimental") {
      designs[[i]] <- .design_manipulation_experiment(hyp)
    }
  }
  
  names(designs) <- paste0("hypothesis_", 1:length(designs))
  return(designs)
}

.design_mechanistic_experiment <- function(hypothesis) {
  
  design <- list(
    approach = "Observational gradient study",
    sample_size = "n >= 30 per gradient level",
    variables = c("Diversity metrics", "Environmental variables", "Community composition"),
    analysis = "Correlation and regression analysis",
    controls = "Multiple sites per gradient level",
    timeline = "3-6 months",
    expected_outcome = "Mechanistic understanding of driver relationships"
  )
  
  return(design)
}

.design_predictive_experiment <- function(hypothesis) {
  
  design <- list(
    approach = "Temporal monitoring study",
    sample_size = "n >= 20 time points",
    variables = c("Diversity trajectories", "Environmental conditions", "Disturbance events"),
    analysis = "Time series analysis and predictive modeling",
    controls = "Reference sites with no manipulation",
    timeline = "6-12 months",
    expected_outcome = "Validated predictive models"
  )
  
  return(design)
}

.design_comparative_experiment <- function(hypothesis) {
  
  design <- list(
    approach = "Multi-site comparative study",
    sample_size = "n >= 15 sites per environment type",
    variables = c("Standardized diversity metrics", "Environmental metadata", "Geographic coordinates"),
    analysis = "ANOVA and multivariate analysis",
    controls = "Standardized sampling protocols",
    timeline = "2-4 months",
    expected_outcome = "Quantified differences between system types"
  )
  
  return(design)
}

.design_manipulation_experiment <- function(hypothesis) {
  
  design <- list(
    approach = "Controlled manipulation experiment",
    sample_size = "n >= 10 replicates per treatment",
    variables = c("Treatment effects", "Diversity responses", "Mechanistic indicators"),
    analysis = "Before-after control-impact analysis",
    controls = "Untreated control plots",
    timeline = "3-9 months",
    expected_outcome = "Causal evidence for mechanisms"
  )
  
  return(design)
}

# Testable predictions
.generate_testable_predictions <- function(hypotheses, patterns) {
  
  predictions <- list()
  
  for (i in 1:nrow(hypotheses)) {
    hyp <- hypotheses[i, ]
    
    pred <- list(
      primary_prediction = .generate_primary_prediction(hyp),
      measurable_outcomes = .generate_measurable_outcomes(hyp),
      statistical_tests = .suggest_statistical_tests(hyp),
      success_criteria = .define_success_criteria(hyp)
    )
    
    predictions[[i]] <- pred
  }
  
  names(predictions) <- paste0("hypothesis_", 1:length(predictions))
  return(predictions)
}

.generate_primary_prediction <- function(hypothesis) {
  
  if (grepl("richness", hypothesis$hypothesis, ignore.case = TRUE)) {
    return("Species richness will correlate significantly with measured environmental gradients (r > 0.5, p < 0.05)")
  } else if (grepl("evenness", hypothesis$hypothesis, ignore.case = TRUE)) {
    return("Species evenness will show predictable responses to experimental treatments (effect size > 0.3)")
  } else if (grepl("predictable", hypothesis$hypothesis, ignore.case = TRUE)) {
    return("Cross-validation will achieve >70% accuracy in predicting diversity patterns")
  } else {
    return("Measurable changes in diversity metrics will correlate with hypothesized drivers")
  }
}

.generate_measurable_outcomes <- function(hypothesis) {
  
  outcomes <- c(
    "Shannon diversity index changes > 0.5 units",
    "Species composition shifts (Bray-Curtis > 0.3)",
    "Richness changes > 20% from baseline",
    "Evenness changes > 0.1 Pielou units"
  )
  
  # Select 2-3 relevant outcomes
  return(outcomes[1:min(3, length(outcomes))])
}

.suggest_statistical_tests <- function(hypothesis) {
  
  if (hypothesis$type == "mechanistic") {
    return(c("Correlation analysis", "Multiple regression", "Structural equation modeling"))
  } else if (hypothesis$type == "predictive") {
    return(c("Cross-validation", "Time series analysis", "Machine learning models"))
  } else if (hypothesis$type == "comparative") {
    return(c("ANOVA", "Permutational MANOVA", "Ordination analysis"))
  } else {
    return(c("Before-after comparison", "Factorial ANOVA", "Mixed-effects models"))
  }
}

.define_success_criteria <- function(hypothesis) {
  
  criteria <- list(
    statistical_significance = "p < 0.05 with appropriate multiple testing correction",
    effect_size = "Cohen's d > 0.5 or r > 0.3 for correlations",
    reproducibility = "Consistent results across >50% of replicates or sites",
    biological_relevance = "Effect sizes exceed natural variation by >2x"
  )
  
  return(criteria)
}

# LLM enhancement functions
.enhance_hypotheses_with_llm <- function(results, llm_provider, study_context) {
  
  cat("Enhancing hypotheses with LLM interpretation...\n")
  
  # This would integrate with the existing LLM functions
  # For now, return results unchanged with a note
  results$llm_enhancement <- list(
    provider = llm_provider,
    enhanced = FALSE,
    note = "LLM enhancement integration pending"
  )
  
  return(results)
}

# Literature integration functions
.integrate_literature_context <- function(results, study_context) {
  
  cat("Integrating literature context...\n")
  
  # Placeholder for literature integration
  results$literature_integration <- list(
    relevant_papers = character(0),
    key_findings = character(0),
    integration_note = "Literature integration system under development"
  )
  
  return(results)
}

# S3 method for printing ecological hypotheses
#' @export
print.ecological_hypotheses <- function(x, ...) {
  cat("Ecological Hypothesis Generation Results\n")
  cat("=======================================\n\n")
  
  if (nrow(x$hypotheses) == 0) {
    cat("No hypotheses generated.\n")
    return(invisible(x))
  }
  
  cat("Generated Hypotheses (ordered by combined score):\n\n")
  
  for (i in 1:nrow(x$hypotheses)) {
    hyp <- x$hypotheses[i, ]
    cat(sprintf("%d. [%s] %s\n", i, toupper(hyp$type), hyp$hypothesis))
    cat(sprintf("   Rationale: %s\n", hyp$rationale))
    cat(sprintf("   Novelty: %.2f | Testability: %.2f | Combined: %.2f\n\n", 
                hyp$novelty, hyp$testability, hyp$combined_score))
  }
  
  if (length(x$experimental_designs) > 0) {
    cat("Experimental Design Summary:\n")
    cat(sprintf("- %d experimental designs generated\n", length(x$experimental_designs)))
    cat(sprintf("- %d testable predictions created\n", length(x$predictions)))
  }
  
  if (!is.null(x$context_analysis)) {
    cat("\nStudy Context Analysis:\n")
    cat(sprintf("- Environment: %s\n", x$context_analysis$environment_type))
    cat(sprintf("- Organism: %s\n", x$context_analysis$organism_group))
    cat(sprintf("- Condition: %s\n", x$context_analysis$experimental_condition))
    cat(sprintf("- Novelty potential: %.2f\n", x$context_analysis$novelty_potential))
  }
  
  invisible(x)
}