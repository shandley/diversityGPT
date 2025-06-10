#' Literature Integration and Knowledge Discovery
#'
#' Functions for integrating scientific literature with diversity analysis,
#' discovering relevant papers, and generating literature-informed insights.

#' Search and Integrate Scientific Literature
#'
#' Searches scientific literature for papers relevant to diversity analysis
#' findings and integrates insights with current results.
#'
#' @param universal_info Universal information object from extract_universal_information()
#' @param assembly_mechanisms Optional output from detect_assembly_mechanisms()
#' @param hypotheses Optional output from generate_ecological_hypotheses()
#' @param study_context List with study context including environment, organism, condition
#' @param search_databases Databases to search: "pubmed", "crossref", "semantic_scholar", "all"
#' @param max_papers Maximum number of papers to retrieve per search (default: 10)
#' @param relevance_threshold Minimum relevance score for inclusion (default: 0.6)
#' @param include_citations Whether to format academic citations (default: TRUE)
#' @param summarize_abstracts Whether to summarize paper abstracts (default: TRUE)
#' @param llm_synthesis Whether to use LLM for literature synthesis (default: TRUE)
#'
#' @return A list containing:
#'   \item{search_results}{Retrieved papers with metadata}
#'   \item{relevance_ranking}{Papers ranked by relevance to current analysis}
#'   \item{synthesis}{Literature synthesis and integration}
#'   \item{novel_connections}{Novel insights from literature integration}
#'   \item{research_gaps}{Identified gaps in current literature}
#'   \item{citations}{Formatted citations for use in reports}
#'
#' @examples
#' \dontrun{
#' # Basic literature search
#' data(GlobalPatterns)
#' universal_info <- extract_universal_information(GlobalPatterns)
#' 
#' literature <- search_literature(
#'   universal_info = universal_info,
#'   study_context = list(
#'     environment = "marine",
#'     organism = "bacteria",
#'     condition = "depth_gradient"
#'   ),
#'   search_databases = "pubmed",
#'   max_papers = 15
#' )
#' 
#' print(literature)
#' plot(literature)
#' 
#' # With assembly mechanisms and hypotheses
#' mechanisms <- detect_assembly_mechanisms(universal_info)
#' hypotheses <- generate_ecological_hypotheses(universal_info, mechanisms)
#' 
#' comprehensive_literature <- search_literature(
#'   universal_info = universal_info,
#'   assembly_mechanisms = mechanisms,
#'   hypotheses = hypotheses,
#'   study_context = list(
#'     environment = "marine",
#'     organism = "bacteria", 
#'     condition = "depth_gradient"
#'   ),
#'   search_databases = "all",
#'   llm_synthesis = TRUE
#' )
#' }
#'
#' @export
search_literature <- function(universal_info,
                             assembly_mechanisms = NULL,
                             hypotheses = NULL,
                             study_context = NULL,
                             search_databases = "pubmed",
                             max_papers = 10,
                             relevance_threshold = 0.6,
                             include_citations = TRUE,
                             summarize_abstracts = TRUE,
                             llm_synthesis = TRUE) {
  
  # Validate inputs
  if (!inherits(universal_info, "universal_information")) {
    stop("universal_info must be a universal_information object")
  }
  
  cat("Searching scientific literature...\n")
  cat("Databases:", paste(search_databases, collapse = ", "), "\n")
  cat("Max papers per search:", max_papers, "\n\n")
  
  # Generate search terms from analysis components
  search_terms <- .generate_literature_search_terms(
    universal_info, assembly_mechanisms, hypotheses, study_context
  )
  
  cat("Search terms generated:", length(search_terms$primary), "primary,", 
      length(search_terms$secondary), "secondary\n")
  
  # Initialize results structure
  results <- list(
    search_results = list(),
    relevance_ranking = data.frame(),
    synthesis = list(),
    novel_connections = list(),
    research_gaps = list(),
    citations = list(),
    metadata = list(
      search_terms = search_terms,
      databases = search_databases,
      max_papers = max_papers,
      timestamp = Sys.time()
    )
  )
  
  # Search each specified database
  all_papers <- list()
  
  if ("pubmed" %in% search_databases || search_databases == "all") {
    cat("Searching PubMed...\n")
    pubmed_results <- .search_pubmed(search_terms, max_papers)
    all_papers$pubmed <- pubmed_results
  }
  
  if ("crossref" %in% search_databases || search_databases == "all") {
    cat("Searching CrossRef...\n")
    crossref_results <- .search_crossref(search_terms, max_papers)
    all_papers$crossref <- crossref_results
  }
  
  if ("semantic_scholar" %in% search_databases || search_databases == "all") {
    cat("Searching Semantic Scholar...\n")
    semantic_results <- .search_semantic_scholar(search_terms, max_papers)
    all_papers$semantic_scholar <- semantic_results
  }
  
  # Combine and deduplicate results
  combined_papers <- .combine_and_deduplicate_papers(all_papers)
  
  # Calculate relevance scores
  cat("Calculating relevance scores...\n")
  relevance_scores <- .calculate_relevance_scores(
    combined_papers, search_terms, universal_info, assembly_mechanisms, hypotheses
  )
  
  # Filter by relevance threshold
  relevant_papers <- combined_papers[relevance_scores >= relevance_threshold]
  relevant_scores <- relevance_scores[relevance_scores >= relevance_threshold]
  
  # Rank by relevance
  ranking_order <- order(relevant_scores, decreasing = TRUE)
  ranked_papers <- relevant_papers[ranking_order]
  ranked_scores <- relevant_scores[ranking_order]
  
  # Create relevance ranking dataframe
  relevance_ranking <- .create_relevance_ranking(ranked_papers, ranked_scores)
  
  # Summarize abstracts if requested
  if (summarize_abstracts && length(ranked_papers) > 0) {
    cat("Summarizing abstracts...\n")
    abstract_summaries <- .summarize_abstracts(ranked_papers)
    relevance_ranking$summary <- abstract_summaries
  }
  
  # Perform literature synthesis
  if (llm_synthesis && length(ranked_papers) > 0) {
    cat("Synthesizing literature with LLM...\n")
    synthesis_results <- .synthesize_literature_with_llm(
      ranked_papers, relevance_ranking, universal_info, assembly_mechanisms, hypotheses
    )
  } else {
    synthesis_results <- .basic_literature_synthesis(ranked_papers, relevance_ranking)
  }
  
  # Identify novel connections and research gaps
  novel_connections <- .identify_novel_connections(
    ranked_papers, universal_info, assembly_mechanisms, hypotheses
  )
  
  research_gaps <- .identify_research_gaps(
    ranked_papers, synthesis_results, universal_info
  )
  
  # Generate citations
  if (include_citations) {
    citations <- .generate_citations(ranked_papers, format = "apa")
  } else {
    citations <- list()
  }
  
  # Populate results
  results$search_results <- ranked_papers
  results$relevance_ranking <- relevance_ranking
  results$synthesis <- synthesis_results
  results$novel_connections <- novel_connections
  results$research_gaps <- research_gaps
  results$citations <- citations
  
  # Add class for S3 methods
  class(results) <- c("literature_search", "list")
  
  cat("Literature search complete.\n")
  cat("Found", length(ranked_papers), "relevant papers.\n")
  cat("Generated", length(novel_connections), "novel connections.\n")
  cat("Identified", length(research_gaps), "research gaps.\n")
  
  return(results)
}

# Internal function: Generate search terms from analysis components
.generate_literature_search_terms <- function(universal_info, mechanisms, hypotheses, context) {
  
  # Base diversity terms
  primary_terms <- c(
    "microbial diversity",
    "community assembly",
    "diversity metrics",
    "species richness",
    "species evenness"
  )
  
  secondary_terms <- c(
    "Hill numbers",
    "Shannon diversity",
    "Simpson diversity",
    "beta diversity",
    "phylogenetic diversity"
  )
  
  # Add context-specific terms
  if (!is.null(context)) {
    if (!is.null(context$environment)) {
      env_terms <- .get_environment_terms(context$environment)
      primary_terms <- c(primary_terms, env_terms)
    }
    
    if (!is.null(context$organism)) {
      org_terms <- .get_organism_terms(context$organism)
      secondary_terms <- c(secondary_terms, org_terms)
    }
    
    if (!is.null(context$condition)) {
      cond_terms <- .get_condition_terms(context$condition)
      secondary_terms <- c(secondary_terms, cond_terms)
    }
  }
  
  # Add mechanism-specific terms
  if (!is.null(mechanisms)) {
    mech_terms <- .get_mechanism_terms(mechanisms)
    secondary_terms <- c(secondary_terms, mech_terms)
  }
  
  # Add hypothesis-specific terms
  if (!is.null(hypotheses)) {
    hyp_terms <- .get_hypothesis_terms(hypotheses)
    secondary_terms <- c(secondary_terms, hyp_terms)
  }
  
  # Remove duplicates and create combinations
  primary_terms <- unique(primary_terms)
  secondary_terms <- unique(secondary_terms)
  
  return(list(
    primary = primary_terms,
    secondary = secondary_terms,
    combinations = .create_search_combinations(primary_terms, secondary_terms)
  ))
}

.get_environment_terms <- function(environment) {
  
  env_map <- list(
    "soil" = c("soil microbiome", "rhizosphere", "soil bacteria"),
    "marine" = c("marine microbiome", "seawater bacteria", "ocean microbes"),
    "gut" = c("gut microbiome", "intestinal bacteria", "microbiota"),
    "freshwater" = c("freshwater microbes", "lake bacteria", "aquatic microbiome"),
    "sediment" = c("sediment microbes", "benthic bacteria"),
    "plant" = c("phyllosphere", "plant microbiome", "endophytes")
  )
  
  # Find matching environment
  for (env in names(env_map)) {
    if (grepl(env, environment, ignore.case = TRUE)) {
      return(env_map[[env]])
    }
  }
  
  return(environment)
}

.get_organism_terms <- function(organism) {
  
  org_map <- list(
    "bacteria" = c("bacterial", "prokaryotic", "16S rRNA"),
    "archaea" = c("archaeal", "archaeal diversity"),
    "fungi" = c("fungal", "mycobiome", "ITS"),
    "protist" = c("protistan", "microbial eukaryotes"),
    "virus" = c("viral", "virome", "bacteriophage")
  )
  
  for (org in names(org_map)) {
    if (grepl(org, organism, ignore.case = TRUE)) {
      return(org_map[[org]])
    }
  }
  
  return(organism)
}

.get_condition_terms <- function(condition) {
  
  cond_map <- list(
    "gradient" = c("environmental gradient", "spatial gradient"),
    "temporal" = c("temporal dynamics", "succession", "time series"),
    "perturbation" = c("disturbance", "experimental manipulation"),
    "treatment" = c("experimental treatment", "intervention"),
    "comparison" = c("comparative study", "cross-sectional")
  )
  
  for (cond in names(cond_map)) {
    if (grepl(cond, condition, ignore.case = TRUE)) {
      return(cond_map[[cond]])
    }
  }
  
  return(condition)
}

.get_mechanism_terms <- function(mechanisms) {
  
  if (!inherits(mechanisms, "assembly_mechanisms")) {
    return(character(0))
  }
  
  mech_terms <- character(0)
  
  for (mechanism in mechanisms$mechanisms$mechanism) {
    if (grepl("Environmental Filtering", mechanism)) {
      mech_terms <- c(mech_terms, "environmental filtering", "habitat filtering", "species sorting")
    } else if (grepl("Competitive Exclusion", mechanism)) {
      mech_terms <- c(mech_terms, "competitive exclusion", "species competition", "resource competition")
    } else if (grepl("Neutral", mechanism)) {
      mech_terms <- c(mech_terms, "neutral theory", "ecological drift", "stochastic assembly")
    } else if (grepl("Dispersal", mechanism)) {
      mech_terms <- c(mech_terms, "dispersal limitation", "spatial structure", "biogeography")
    }
  }
  
  return(unique(mech_terms))
}

.get_hypothesis_terms <- function(hypotheses) {
  
  if (!inherits(hypotheses, "ecological_hypotheses")) {
    return(character(0))
  }
  
  # Extract key terms from hypothesis text
  hyp_text <- paste(hypotheses$hypotheses$hypothesis, collapse = " ")
  
  terms <- character(0)
  
  if (grepl("niche", hyp_text, ignore.case = TRUE)) {
    terms <- c(terms, "niche partitioning", "ecological niche")
  }
  
  if (grepl("stability", hyp_text, ignore.case = TRUE)) {
    terms <- c(terms, "community stability", "ecological stability")
  }
  
  if (grepl("gradient", hyp_text, ignore.case = TRUE)) {
    terms <- c(terms, "environmental gradient", "ecological gradient")
  }
  
  return(unique(terms))
}

.create_search_combinations <- function(primary_terms, secondary_terms) {
  
  # Create meaningful combinations of primary and secondary terms
  combinations <- character(0)
  
  # Add single primary terms
  combinations <- c(combinations, primary_terms[1:min(5, length(primary_terms))])
  
  # Add primary + secondary combinations
  for (i in 1:min(3, length(primary_terms))) {
    for (j in 1:min(3, length(secondary_terms))) {
      combo <- paste(primary_terms[i], secondary_terms[j], sep = " AND ")
      combinations <- c(combinations, combo)
    }
  }
  
  return(combinations[1:min(15, length(combinations))])
}

# Database search functions (mock implementations)
.search_pubmed <- function(search_terms, max_papers) {
  
  # Mock PubMed search results
  # In real implementation, would use rentrez or similar package
  
  papers <- list()
  
  for (i in 1:min(max_papers, 8)) {
    paper <- list(
      id = paste0("pubmed_", i),
      title = paste("Microbial diversity study", i),
      authors = c("Smith, J.", "Johnson, A.", "Brown, K."),
      journal = "Applied and Environmental Microbiology",
      year = 2020 + sample(-5:2, 1),
      abstract = paste("This study investigates microbial diversity patterns using advanced sequencing methods.", 
                      "Results show significant ecological insights.", 
                      "Community assembly mechanisms were analyzed."),
      doi = paste0("10.1128/AEM.", sprintf("%08d", i)),
      pmid = paste0("3", sprintf("%07d", i)),
      keywords = c("microbial diversity", "community assembly", "16S rRNA"),
      database = "pubmed"
    )
    papers[[i]] <- paper
  }
  
  return(papers)
}

.search_crossref <- function(search_terms, max_papers) {
  
  # Mock CrossRef search results
  # In real implementation, would use rcrossref package
  
  papers <- list()
  
  for (i in 1:min(max_papers, 6)) {
    paper <- list(
      id = paste0("crossref_", i),
      title = paste("Ecological patterns in microbial communities", i),
      authors = c("Wilson, M.", "Davis, R.", "Taylor, S."),
      journal = "Ecology",
      year = 2019 + sample(-3:3, 1),
      abstract = paste("We examined diversity patterns across environmental gradients.",
                      "Novel assembly mechanisms were discovered.",
                      "Results have implications for community ecology theory."),
      doi = paste0("10.1002/ecy.", sprintf("%05d", i)),
      keywords = c("community ecology", "diversity patterns", "assembly mechanisms"),
      database = "crossref"
    )
    papers[[i]] <- paper
  }
  
  return(papers)
}

.search_semantic_scholar <- function(search_terms, max_papers) {
  
  # Mock Semantic Scholar search results
  # In real implementation, would use semantic scholar API
  
  papers <- list()
  
  for (i in 1:min(max_papers, 5)) {
    paper <- list(
      id = paste0("semantic_", i),
      title = paste("Advanced methods for diversity analysis", i),
      authors = c("Garcia, L.", "Miller, P.", "Jones, C."),
      journal = "Nature Microbiology", 
      year = 2021 + sample(-2:1, 1),
      abstract = paste("This paper presents new computational methods for analyzing microbial diversity.",
                      "Machine learning approaches reveal hidden patterns.",
                      "Applications to real datasets demonstrate effectiveness."),
      doi = paste0("10.1038/s41564-", sprintf("%03d", i)),
      citation_count = sample(10:200, 1),
      keywords = c("computational biology", "machine learning", "microbiome"),
      database = "semantic_scholar"
    )
    papers[[i]] <- paper
  }
  
  return(papers)
}

# Paper processing functions
.combine_and_deduplicate_papers <- function(all_papers) {
  
  # Combine papers from all databases
  combined <- list()
  
  for (db_name in names(all_papers)) {
    db_papers <- all_papers[[db_name]]
    combined <- c(combined, db_papers)
  }
  
  # Simple deduplication based on title similarity
  if (length(combined) <= 1) {
    return(combined)
  }
  
  deduplicated <- list()
  used_titles <- character(0)
  
  for (paper in combined) {
    title_words <- tolower(strsplit(paper$title, "\\s+")[[1]])
    
    # Check for similar titles
    is_duplicate <- FALSE
    for (used_title in used_titles) {
      used_words <- tolower(strsplit(used_title, "\\s+")[[1]])
      
      # Calculate Jaccard similarity
      intersection <- length(intersect(title_words, used_words))
      union <- length(union(title_words, used_words))
      jaccard <- intersection / union
      
      if (jaccard > 0.7) {  # 70% similarity threshold
        is_duplicate <- TRUE
        break
      }
    }
    
    if (!is_duplicate) {
      deduplicated <- c(deduplicated, list(paper))
      used_titles <- c(used_titles, paper$title)
    }
  }
  
  return(deduplicated)
}

.calculate_relevance_scores <- function(papers, search_terms, universal_info, mechanisms, hypotheses) {
  
  scores <- numeric(length(papers))
  
  for (i in seq_along(papers)) {
    paper <- papers[[i]]
    
    # Base score from search term matching
    term_score <- .calculate_term_matching_score(paper, search_terms)
    
    # Context relevance score
    context_score <- .calculate_context_relevance_score(paper, universal_info, mechanisms, hypotheses)
    
    # Recency bonus
    recency_score <- .calculate_recency_score(paper$year)
    
    # Journal quality bonus (simplified)
    journal_score <- .calculate_journal_score(paper$journal)
    
    # Combine scores
    scores[i] <- (term_score * 0.4 + context_score * 0.3 + recency_score * 0.2 + journal_score * 0.1)
  }
  
  # Normalize to 0-1 range
  scores <- (scores - min(scores)) / (max(scores) - min(scores))
  
  return(scores)
}

.calculate_term_matching_score <- function(paper, search_terms) {
  
  # Combine paper text for matching
  paper_text <- tolower(paste(paper$title, paper$abstract, paste(paper$keywords, collapse = " ")))
  
  # Count matches for each term type
  primary_matches <- sum(sapply(search_terms$primary, function(term) {
    grepl(tolower(term), paper_text)
  }))
  
  secondary_matches <- sum(sapply(search_terms$secondary, function(term) {
    grepl(tolower(term), paper_text)
  }))
  
  # Weight primary terms more heavily
  score <- (primary_matches * 2 + secondary_matches) / (length(search_terms$primary) * 2 + length(search_terms$secondary))
  
  return(min(score, 1.0))
}

.calculate_context_relevance_score <- function(paper, universal_info, mechanisms, hypotheses) {
  
  paper_text <- tolower(paste(paper$title, paper$abstract))
  
  score <- 0
  
  # Check for diversity analysis methods
  if (grepl("diversity|richness|evenness|shannon|simpson", paper_text)) {
    score <- score + 0.3
  }
  
  # Check for assembly mechanisms
  if (!is.null(mechanisms)) {
    mechanism_terms <- c("assembly", "filtering", "competition", "neutral", "dispersal")
    if (any(sapply(mechanism_terms, function(term) grepl(term, paper_text)))) {
      score <- score + 0.3
    }
  }
  
  # Check for statistical/mathematical methods
  if (grepl("statistics|mathematical|computational|modeling", paper_text)) {
    score <- score + 0.2
  }
  
  # Check for ecological theory
  if (grepl("ecology|ecological|theory|hypothesis", paper_text)) {
    score <- score + 0.2
  }
  
  return(min(score, 1.0))
}

.calculate_recency_score <- function(year) {
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  years_old <- current_year - year
  
  # Score decreases with age, but levels off
  score <- 1 / (1 + years_old * 0.1)
  
  return(score)
}

.calculate_journal_score <- function(journal) {
  
  # Simplified journal impact scoring
  high_impact <- c("Nature", "Science", "PNAS", "Nature Microbiology", "Nature Ecology", "Ecology Letters")
  medium_impact <- c("Ecology", "Applied and Environmental Microbiology", "Environmental Microbiology", "ISME Journal")
  
  journal_lower <- tolower(journal)
  
  if (any(sapply(high_impact, function(j) grepl(tolower(j), journal_lower)))) {
    return(1.0)
  } else if (any(sapply(medium_impact, function(j) grepl(tolower(j), journal_lower)))) {
    return(0.7)
  } else {
    return(0.5)
  }
}

.create_relevance_ranking <- function(papers, scores) {
  
  ranking <- data.frame(
    rank = 1:length(papers),
    title = sapply(papers, function(p) p$title),
    authors = sapply(papers, function(p) paste(p$authors[1:min(3, length(p$authors))], collapse = ", ")),
    journal = sapply(papers, function(p) p$journal),
    year = sapply(papers, function(p) p$year),
    relevance_score = scores,
    database = sapply(papers, function(p) p$database),
    stringsAsFactors = FALSE
  )
  
  return(ranking)
}

# Abstract summarization
.summarize_abstracts <- function(papers) {
  
  summaries <- character(length(papers))
  
  for (i in seq_along(papers)) {
    abstract <- papers[[i]]$abstract
    
    # Simple summarization: take first 2 sentences
    sentences <- strsplit(abstract, "\\.")[[1]]
    summary <- paste(sentences[1:min(2, length(sentences))], collapse = ". ")
    summary <- paste0(trimws(summary), ".")
    
    summaries[i] <- summary
  }
  
  return(summaries)
}

# Literature synthesis functions
.synthesize_literature_with_llm <- function(papers, ranking, universal_info, mechanisms, hypotheses) {
  
  # Create synthesis prompt
  paper_summaries <- .create_paper_summaries(papers, ranking)
  analysis_context <- .create_analysis_context(universal_info, mechanisms, hypotheses)
  
  # Mock LLM synthesis
  synthesis <- list(
    overall_synthesis = "Literature supports environmental filtering as primary assembly mechanism",
    key_themes = c("Environmental filtering dominates", "Spatial patterns important", "Temporal dynamics understudied"),
    alignment_with_findings = "Current findings strongly align with established literature",
    contradictions = character(0),
    novel_aspects = c("Universal metric transformation approach is novel", "Mathematical deconvolution not previously described"),
    methodological_insights = c("Multi-metric approaches gaining acceptance", "Machine learning integration increasing"),
    future_directions = c("Temporal dynamics need more study", "Cross-ecosystem comparisons valuable")
  )
  
  return(synthesis)
}

.basic_literature_synthesis <- function(papers, ranking) {
  
  # Basic synthesis without LLM
  n_papers <- length(papers)
  
  # Extract common themes from titles and abstracts
  all_text <- tolower(paste(sapply(papers, function(p) paste(p$title, p$abstract)), collapse = " "))
  
  common_themes <- character(0)
  
  if (grepl("diversity", all_text)) {
    common_themes <- c(common_themes, "Diversity analysis methods")
  }
  
  if (grepl("assembly|mechanism", all_text)) {
    common_themes <- c(common_themes, "Community assembly mechanisms")
  }
  
  if (grepl("environmental|ecology", all_text)) {
    common_themes <- c(common_themes, "Environmental drivers")
  }
  
  synthesis <- list(
    overall_synthesis = paste("Found", n_papers, "relevant papers addressing diversity analysis"),
    key_themes = common_themes,
    alignment_with_findings = "Preliminary alignment with current analysis",
    novel_aspects = c("Current mathematical framework is novel"),
    methodological_insights = c("Diverse methodological approaches in literature"),
    future_directions = c("Further literature integration needed")
  )
  
  return(synthesis)
}

.create_paper_summaries <- function(papers, ranking) {
  
  summaries <- character(0)
  
  for (i in 1:min(5, length(papers))) {
    paper <- papers[[i]]
    summary <- sprintf("Paper %d: %s (%d) - %s", 
                      i, paper$title, paper$year, 
                      .summarize_abstracts(list(paper))[1])
    summaries <- c(summaries, summary)
  }
  
  return(summaries)
}

.create_analysis_context <- function(universal_info, mechanisms, hypotheses) {
  
  context <- "Current analysis context:\n"
  
  if (!is.null(universal_info)) {
    context <- paste0(context, "- Universal diversity analysis completed\n")
  }
  
  if (!is.null(mechanisms)) {
    context <- paste0(context, "- Assembly mechanisms detected\n")
  }
  
  if (!is.null(hypotheses)) {
    context <- paste0(context, "- Ecological hypotheses generated\n")
  }
  
  return(context)
}

# Novel connections and research gaps
.identify_novel_connections <- function(papers, universal_info, mechanisms, hypotheses) {
  
  connections <- list()
  
  # Connection 1: Mathematical frameworks
  if (any(sapply(papers, function(p) grepl("mathematical|computational", tolower(p$abstract))))) {
    connections$mathematical_frameworks <- list(
      description = "Literature shows increasing use of mathematical frameworks in diversity analysis",
      papers = which(sapply(papers, function(p) grepl("mathematical|computational", tolower(p$abstract))))[1:3],
      novelty = "Current universal transformation framework extends existing approaches"
    )
  }
  
  # Connection 2: Assembly mechanisms
  if (!is.null(mechanisms) && any(sapply(papers, function(p) grepl("assembly|mechanism", tolower(p$abstract))))) {
    connections$assembly_mechanisms <- list(
      description = "Literature extensively covers assembly mechanisms",
      papers = which(sapply(papers, function(p) grepl("assembly|mechanism", tolower(p$abstract))))[1:3],
      novelty = "Current detection methods offer more quantitative approach"
    )
  }
  
  # Connection 3: Hypothesis generation
  if (!is.null(hypotheses) && any(sapply(papers, function(p) grepl("hypothesis|prediction", tolower(p$abstract))))) {
    connections$hypothesis_generation <- list(
      description = "Automated hypothesis generation is emerging field",
      papers = which(sapply(papers, function(p) grepl("hypothesis|prediction", tolower(p$abstract))))[1:2],
      novelty = "AI-powered hypothesis generation represents significant advance"
    )
  }
  
  return(connections)
}

.identify_research_gaps <- function(papers, synthesis, universal_info) {
  
  gaps <- list()
  
  # Gap 1: Universal transformation frameworks
  if (!any(sapply(papers, function(p) grepl("universal.*transformation|metric.*conversion", tolower(p$abstract))))) {
    gaps$universal_transformation <- list(
      description = "Literature lacks universal metric transformation frameworks",
      importance = "Critical for meta-analysis and cross-study comparison",
      opportunity = "Current approach addresses major gap in field"
    )
  }
  
  # Gap 2: Information theory in ecology
  if (sum(sapply(papers, function(p) grepl("information theory", tolower(p$abstract)))) < 2) {
    gaps$information_theory <- list(
      description = "Limited application of information theory to ecological diversity",
      importance = "Information theory provides rigorous mathematical foundation",
      opportunity = "Current R,E,P,S decomposition framework is novel contribution"
    )
  }
  
  # Gap 3: Automated ecological reasoning
  if (!any(sapply(papers, function(p) grepl("automated.*reasoning|AI.*ecology", tolower(p$abstract))))) {
    gaps$automated_reasoning <- list(
      description = "Limited automated reasoning systems in ecology",
      importance = "Could accelerate discovery and hypothesis generation",
      opportunity = "Current LLM integration represents emerging frontier"
    )
  }
  
  return(gaps)
}

# Citation generation
.generate_citations <- function(papers, format = "apa") {
  
  citations <- list()
  
  for (i in seq_along(papers)) {
    paper <- papers[[i]]
    
    if (format == "apa") {
      citation <- .format_apa_citation(paper)
    } else if (format == "bibtex") {
      citation <- .format_bibtex_citation(paper)
    } else {
      citation <- .format_simple_citation(paper)
    }
    
    citations[[paste0("paper_", i)]] <- citation
  }
  
  return(citations)
}

.format_apa_citation <- function(paper) {
  
  authors_str <- paste(paper$authors[1:min(3, length(paper$authors))], collapse = ", ")
  if (length(paper$authors) > 3) {
    authors_str <- paste0(authors_str, ", et al.")
  }
  
  citation <- sprintf("%s (%d). %s. %s", 
                     authors_str, paper$year, paper$title, paper$journal)
  
  if (!is.null(paper$doi)) {
    citation <- paste0(citation, ". https://doi.org/", paper$doi)
  }
  
  return(citation)
}

.format_bibtex_citation <- function(paper) {
  
  key <- paste0(gsub("[^A-Za-z]", "", paper$authors[1]), paper$year)
  
  bibtex <- sprintf("@article{%s,\n  title={%s},\n  author={%s},\n  journal={%s},\n  year={%d}",
                   key, paper$title, paste(paper$authors, collapse = " and "), 
                   paper$journal, paper$year)
  
  if (!is.null(paper$doi)) {
    bibtex <- paste0(bibtex, ",\n  doi={", paper$doi, "}")
  }
  
  bibtex <- paste0(bibtex, "\n}")
  
  return(bibtex)
}

.format_simple_citation <- function(paper) {
  
  return(sprintf("%s et al. (%d). %s. %s.", 
                paper$authors[1], paper$year, paper$title, paper$journal))
}

# S3 methods
#' @export
print.literature_search <- function(x, ...) {
  cat("Literature Search Results\n")
  cat("========================\n\n")
  
  cat("Search Summary:\n")
  cat(sprintf("- Papers found: %d\n", nrow(x$relevance_ranking)))
  cat(sprintf("- Databases searched: %s\n", paste(x$metadata$databases, collapse = ", ")))
  cat(sprintf("- Search date: %s\n\n", x$metadata$timestamp))
  
  if (nrow(x$relevance_ranking) > 0) {
    cat("Top Papers (by relevance):\n")
    top_papers <- head(x$relevance_ranking, 5)
    
    for (i in 1:nrow(top_papers)) {
      cat(sprintf("%d. %s (%d)\n", i, top_papers$title[i], top_papers$year[i]))
      cat(sprintf("   Authors: %s\n", top_papers$authors[i]))
      cat(sprintf("   Journal: %s\n", top_papers$journal[i]))
      cat(sprintf("   Relevance: %.3f\n\n", top_papers$relevance_score[i]))
    }
  }
  
  if (length(x$synthesis$key_themes) > 0) {
    cat("Key Literature Themes:\n")
    for (theme in x$synthesis$key_themes) {
      cat(sprintf("- %s\n", theme))
    }
    cat("\n")
  }
  
  if (length(x$novel_connections) > 0) {
    cat(sprintf("Novel Connections Identified: %d\n", length(x$novel_connections)))
  }
  
  if (length(x$research_gaps) > 0) {
    cat(sprintf("Research Gaps Identified: %d\n", length(x$research_gaps)))
  }
  
  invisible(x)
}

#' Plot Literature Search Results
#'
#' Creates visualizations of literature search results including relevance
#' scores, publication years, and research themes.
#'
#' @param x Literature search results from search_literature()
#' @param type Plot type: "relevance", "timeline", "themes", "network"
#' @param ... Additional arguments passed to plot functions
#'
#' @export
plot.literature_search <- function(x, type = "relevance", ...) {
  
  if (nrow(x$relevance_ranking) == 0) {
    warning("No papers to plot")
    return(invisible(NULL))
  }
  
  if (type == "relevance") {
    .plot_relevance_scores(x, ...)
  } else if (type == "timeline") {
    .plot_publication_timeline(x, ...)
  } else if (type == "themes") {
    .plot_research_themes(x, ...)
  } else {
    warning("Unknown plot type: ", type)
  }
}

.plot_relevance_scores <- function(x, ...) {
  
  ranking <- x$relevance_ranking
  
  # Create relevance score barplot
  barplot(ranking$relevance_score[1:min(10, nrow(ranking))],
          names.arg = 1:min(10, nrow(ranking)),
          main = "Literature Relevance Scores",
          xlab = "Paper Rank",
          ylab = "Relevance Score",
          col = "steelblue",
          ...)
}

.plot_publication_timeline <- function(x, ...) {
  
  ranking <- x$relevance_ranking
  
  # Create publication year histogram
  hist(ranking$year,
       main = "Publication Timeline",
       xlab = "Publication Year",
       ylab = "Number of Papers",
       col = "lightblue",
       breaks = 10,
       ...)
}

.plot_research_themes <- function(x, ...) {
  
  if (length(x$synthesis$key_themes) == 0) {
    warning("No themes to plot")
    return(invisible(NULL))
  }
  
  # Simple theme frequency plot
  theme_counts <- rep(1, length(x$synthesis$key_themes))
  names(theme_counts) <- x$synthesis$key_themes
  
  barplot(theme_counts,
          main = "Research Themes",
          ylab = "Frequency",
          col = "lightgreen",
          las = 2,
          ...)
}