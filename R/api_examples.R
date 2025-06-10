#' diversityGPT API Examples
#'
#' Example usage patterns for the diversityGPT API framework

#' @examples
#' \dontrun{
#' # ===================================
#' # Basic API Usage Examples
#' # ===================================
#' 
#' library(diversityGPT)
#' data(GlobalPatterns)
#' 
#' # Example 1: Simple analysis
#' # --------------------------
#' result <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   components = c("universal", "mechanisms")
#' )
#' 
#' # Check if successful
#' if (is_api_success(result)) {
#'   # Extract data
#'   universal_info <- get_api_data(result, "universal")
#'   mechanisms <- get_api_data(result, "mechanisms")
#'   
#'   # Use the results
#'   print(universal_info)
#'   plot(universal_info)
#' } else {
#'   # Handle error
#'   error <- get_api_error(result)
#'   cat("Error:", error$message, "\n")
#' }
#' 
#' # Example 2: Metric transformation
#' # --------------------------------
#' # Transform Shannon diversity to other metrics
#' source_data <- data.frame(
#'   shannon = c(2.1, 2.5, 1.8, 2.3),
#'   row.names = c("Sample1", "Sample2", "Sample3", "Sample4")
#' )
#' 
#' result <- diversityGPT_api(
#'   "transform",
#'   source_metrics = source_data,
#'   target_metrics = c("simpson", "invsimpson", "chao1"),
#'   phyloseq_reference = GlobalPatterns
#' )
#' 
#' if (is_api_success(result)) {
#'   predictions <- get_api_data(result)
#'   print(predictions$predictions)
#'   print(predictions$quality)
#' }
#' 
#' # Example 3: Async batch processing
#' # ---------------------------------
#' # Create multiple datasets
#' datasets <- list(
#'   marine = subset_samples(GlobalPatterns, SampleType == "Ocean"),
#'   soil = subset_samples(GlobalPatterns, SampleType == "Soil"),
#'   feces = subset_samples(GlobalPatterns, SampleType == "Feces")
#' )
#' 
#' # Submit async batch job
#' result <- diversityGPT_api(
#'   "batch_analyze",
#'   dataset_list = datasets,
#'   dataset_names = names(datasets),
#'   async = TRUE
#' )
#' 
#' if (result$status == "accepted") {
#'   job_id <- result$data$job_id
#'   cat("Batch job submitted:", job_id, "\n")
#'   
#'   # Check status periodically
#'   repeat {
#'     Sys.sleep(2)  # Wait 2 seconds
#'     status <- diversityGPT_api("status", job_id = job_id)
#'     
#'     cat("Job status:", status$data$status, "\n")
#'     
#'     if (status$data$status %in% c("completed", "failed", "timeout")) {
#'       break
#'     }
#'   }
#'   
#'   # Get results
#'   if (status$data$status == "completed") {
#'     results <- diversityGPT_api("get_results", job_id = job_id)
#'     
#'     # Process batch results
#'     batch_data <- get_api_data(results)
#'     for (dataset_name in names(batch_data$results)) {
#'       cat("\nDataset:", dataset_name, "\n")
#'       dataset_results <- batch_data$results[[dataset_name]]
#'       print(dataset_results$universal$deconvolution_quality)
#'     }
#'   }
#' }
#' 
#' # Example 4: Meta-analysis
#' # ------------------------
#' # Prepare multiple studies
#' studies <- list(
#'   study1 = subset_samples(GlobalPatterns, X.SampleID %in% sample_names(GlobalPatterns)[1:10]),
#'   study2 = subset_samples(GlobalPatterns, X.SampleID %in% sample_names(GlobalPatterns)[11:20]),
#'   study3 = subset_samples(GlobalPatterns, X.SampleID %in% sample_names(GlobalPatterns)[21:30])
#' )
#' 
#' result <- diversityGPT_api(
#'   "meta_analyze",
#'   study_list = studies,
#'   study_names = paste("Study", 1:3),
#'   meta_method = "random_effects"
#' )
#' 
#' if (is_api_success(result)) {
#'   meta_data <- get_api_data(result)
#'   
#'   # View meta-analysis results
#'   print(meta_data)
#'   plot(meta_data, type = "forest")
#' }
#' 
#' # Example 5: Custom configuration
#' # -------------------------------
#' # Create custom configuration
#' config <- create_api_config(
#'   cache = TRUE,
#'   cache_dir = "~/diversityGPT_cache",
#'   parallel = TRUE,
#'   n_cores = 4,
#'   llm_provider = "anthropic",
#'   api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#'   output_dir = "~/diversityGPT_results"
#' )
#' 
#' # Use configuration for analysis with LLM interpretation
#' result <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   components = c("universal", "mechanisms", "hypotheses", "literature"),
#'   config = config
#' )
#' 
#' # Example 6: Report generation
#' # ----------------------------
#' # First, run complete analysis
#' analysis <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   components = c("universal", "mechanisms", "hypotheses", "validation")
#' )
#' 
#' if (is_api_success(analysis)) {
#'   # Generate comprehensive report
#'   report_result <- diversityGPT_api(
#'     "generate_report",
#'     analysis_results = get_api_data(analysis),
#'     phyloseq_obj = GlobalPatterns,
#'     output_format = "both",  # HTML and PDF
#'     template = "research"
#'   )
#'   
#'   if (is_api_success(report_result)) {
#'     report_paths <- get_api_data(report_result)$report_path
#'     cat("Reports generated:\n")
#'     for (path in report_paths) {
#'       cat("-", path, "\n")
#'     }
#'   }
#' }
#' 
#' # Example 7: Error handling and retry
#' # -----------------------------------
#' # API automatically retries failed operations
#' result <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   retry = 3,  # Retry up to 3 times
#'   timeout = 60,  # 60 second timeout
#'   verbose = TRUE
#' )
#' 
#' # Manual error handling
#' tryCatch({
#'   result <- diversityGPT_api(
#'     "transform",
#'     source_metrics = invalid_data,  # This will fail
#'     target_metrics = c("shannon")
#'   )
#' }, error = function(e) {
#'   cat("Caught error:", e$message, "\n")
#' })
#' 
#' # ===================================
#' # REST API Server Examples
#' # ===================================
#' 
#' # Example 8: Create and run API server
#' # ------------------------------------
#' # Create API with custom configuration
#' api_config <- create_api_config(
#'   cache = TRUE,
#'   llm_provider = "openai",
#'   api_key = Sys.getenv("OPENAI_API_KEY")
#' )
#' 
#' # Create API server
#' api <- create_diversityGPT_api(
#'   config = api_config,
#'   port = 8080,
#'   cors = TRUE  # Enable CORS for web clients
#' )
#' 
#' # Run the server (blocking)
#' # api$run(port = 8080)
#' 
#' # Or use convenience function
#' # run_diversityGPT_api(config = api_config, port = 8080)
#' 
#' # Example 9: API client usage
#' # ---------------------------
#' # Create client for remote API
#' client <- create_api_client(
#'   base_url = "http://localhost:8080",
#'   api_key = "your-api-key"
#' )
#' 
#' # Use client to analyze data
#' result <- client$analyze(
#'   GlobalPatterns,
#'   components = c("universal", "mechanisms"),
#'   async = FALSE
#' )
#' 
#' print(result)
#' 
#' # Example 10: Curl examples for REST API
#' # --------------------------------------
#' # These examples show how to use the API from command line or other languages
#' 
#' # Health check
#' # curl http://localhost:8080/health
#' 
#' # Analysis (with base64 encoded phyloseq data)
#' # curl -X POST http://localhost:8080/analyze \
#' #   -H "Content-Type: application/json" \
#' #   -d '{"data": "<base64-encoded-phyloseq>", "components": ["universal"]}'
#' 
#' # List jobs
#' # curl http://localhost:8080/jobs?status=running
#' 
#' # Get job results
#' # curl http://localhost:8080/jobs/job_20240109120000_1234/results
#' 
#' # ===================================
#' # Advanced Usage Patterns
#' # ===================================
#' 
#' # Example 11: Pipeline with caching
#' # ---------------------------------
#' # Enable caching for repeated analyses
#' cache_config <- create_api_config(cache = TRUE)
#' 
#' # First run - calculates and caches
#' result1 <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   config = cache_config
#' )
#' 
#' # Second run - uses cache (much faster)
#' result2 <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   config = cache_config
#' )
#' 
#' # Clean old cache files
#' diversityGPT_api("clean_cache", age_days = 7, config = cache_config)
#' 
#' # Example 12: Custom analysis pipeline
#' # ------------------------------------
#' run_custom_pipeline <- function(phyloseq_obj, study_name) {
#'   # Step 1: Universal analysis
#'   universal_result <- diversityGPT_api(
#'     "analyze",
#'     phyloseq_obj = phyloseq_obj,
#'     components = "universal"
#'   )
#'   
#'   if (!is_api_success(universal_result)) {
#'     stop("Universal analysis failed: ", get_api_error(universal_result)$message)
#'   }
#'   
#'   universal_info <- get_api_data(universal_result, "universal")
#'   
#'   # Step 2: Validate results
#'   validation_result <- diversityGPT_api(
#'     "validate",
#'     universal_info = universal_info,
#'     phyloseq_obj = phyloseq_obj,
#'     validation_type = "quick"
#'   )
#'   
#'   validation <- get_api_data(validation_result)
#'   
#'   # Step 3: Only proceed if validation passes
#'   if (validation$overall_assessment$quality_level %in% c("Excellent", "Good")) {
#'     # Run advanced analyses
#'     advanced_result <- diversityGPT_api(
#'       "analyze",
#'       phyloseq_obj = phyloseq_obj,
#'       components = c("mechanisms", "hypotheses", "literature")
#'     )
#'     
#'     # Generate report
#'     all_results <- c(
#'       get_api_data(universal_result),
#'       get_api_data(advanced_result)
#'     )
#'     
#'     report_result <- diversityGPT_api(
#'       "generate_report",
#'       analysis_results = all_results,
#'       phyloseq_obj = phyloseq_obj,
#'       output_format = "html",
#'       template = "summary"
#'     )
#'     
#'     return(list(
#'       study_name = study_name,
#'       results = all_results,
#'       report = get_api_data(report_result)$report_path
#'     ))
#'   } else {
#'     warning("Validation failed for ", study_name, ": ", 
#'             validation$overall_assessment$quality_level)
#'     return(NULL)
#'   }
#' }
#' 
#' # Run pipeline
#' pipeline_result <- run_custom_pipeline(GlobalPatterns, "Global Patterns Study")
#' 
#' # Example 13: Progress monitoring for long operations
#' # --------------------------------------------------
#' monitor_async_job <- function(job_id, check_interval = 5) {
#'   cat("Monitoring job:", job_id, "\n")
#'   pb <- txtProgressBar(min = 0, max = 100, style = 3)
#'   
#'   last_status <- ""
#'   start_time <- Sys.time()
#'   
#'   while (TRUE) {
#'     status_result <- diversityGPT_api("status", job_id = job_id, verbose = FALSE)
#'     status_data <- get_api_data(status_result)
#'     
#'     if (status_data$status != last_status) {
#'       cat("\nStatus changed to:", status_data$status, "\n")
#'       last_status <- status_data$status
#'     }
#'     
#'     # Update progress bar (mock progress)
#'     elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#'     progress <- min(elapsed / 60 * 100, 99)  # Assume ~1 minute typical
#'     setTxtProgressBar(pb, progress)
#'     
#'     if (status_data$status %in% c("completed", "failed", "timeout", "cancelled")) {
#'       setTxtProgressBar(pb, 100)
#'       close(pb)
#'       
#'       if (status_data$status == "completed") {
#'         cat("\nJob completed successfully!\n")
#'         return(diversityGPT_api("get_results", job_id = job_id))
#'       } else {
#'         cat("\nJob ended with status:", status_data$status, "\n")
#'         return(status_result)
#'       }
#'     }
#'     
#'     Sys.sleep(check_interval)
#'   }
#' }
#' 
#' # Submit async job and monitor
#' async_result <- diversityGPT_api(
#'   "analyze",
#'   phyloseq_obj = GlobalPatterns,
#'   components = c("universal", "mechanisms", "hypotheses", "validation"),
#'   async = TRUE
#' )
#' 
#' if (async_result$status == "accepted") {
#'   final_result <- monitor_async_job(async_result$data$job_id)
#' }
#' 
#' # Example 14: Programmatic API exploration
#' # ----------------------------------------
#' # List all available operations
#' api_info <- diversityGPT_api("help")  # Would need to implement this
#' 
#' # Get configuration details
#' current_config <- create_api_config()
#' print(current_config)
#' 
#' # Check system resources
#' system_info <- list(
#'   cores_available = parallel::detectCores(),
#'   memory_limit_mb = as.numeric(memory.limit()),
#'   temp_space_gb = as.numeric(system("df -h /tmp | tail -1 | awk '{print $4}'", 
#'                                     intern = TRUE))
#' )
#' print(system_info)
#' 
#' }