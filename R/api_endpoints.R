#' REST API Endpoints for diversityGPT
#'
#' Functions to create REST API endpoints using plumber for web service access

#' Create diversityGPT REST API
#'
#' Creates a plumber API with endpoints for all diversityGPT functionality
#'
#' @param config API configuration object
#' @param port Port number for API server
#' @param host Host address
#' @param docs Whether to enable API documentation
#' @param cors Whether to enable CORS
#'
#' @return Plumber API object
#'
#' @examples
#' \dontrun{
#' # Create and run API
#' api <- create_diversityGPT_api()
#' api$run(port = 8000)
#' 
#' # With custom configuration
#' config <- create_api_config(
#'   cache = TRUE,
#'   llm_provider = "anthropic"
#' )
#' 
#' api <- create_diversityGPT_api(config = config)
#' api$run(port = 8080, host = "0.0.0.0")
#' }
#'
#' @export
create_diversityGPT_api <- function(config = NULL,
                                   port = 8000,
                                   host = "127.0.0.1",
                                   docs = TRUE,
                                   cors = TRUE) {
  
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required for REST API functionality")
  }
  
  # Load configuration
  config <- config %||% create_api_config()
  
  # Create plumber instance
  pr <- plumber::plumber$new()
  
  # Set API info
  pr$handle("GET", "/", function() {
    list(
      name = "diversityGPT API",
      version = as.character(packageVersion("diversityGPT")),
      description = "REST API for universal diversity analysis",
      endpoints = c(
        "/health", "/analyze", "/transform", "/validate",
        "/batch", "/meta-analysis", "/report", "/jobs"
      )
    )
  }, serializer = plumber::serializer_json())
  
  # Health check endpoint
  pr$handle("GET", "/health", function() {
    list(
      status = "healthy",
      timestamp = Sys.time(),
      version = as.character(packageVersion("diversityGPT")),
      r_version = as.character(getRversion())
    )
  }, serializer = plumber::serializer_json())
  
  # Main analysis endpoint
  pr$handle("POST", "/analyze", function(req, res) {
    tryCatch({
      # Parse request body
      body <- req$body
      
      # Validate required fields
      if (is.null(body$data)) {
        res$status <- 400
        return(list(
          error = "Missing required field: data",
          details = "Request must include phyloseq data"
        ))
      }
      
      # Convert data to phyloseq object
      phyloseq_obj <- .deserialize_phyloseq(body$data)
      
      # Get analysis components
      components <- body$components %||% c("universal", "mechanisms", "hypotheses")
      
      # Run analysis
      result <- diversityGPT_api(
        "analyze",
        phyloseq_obj = phyloseq_obj,
        components = components,
        config = config,
        async = body$async %||% FALSE
      )
      
      # Set response status
      if (result$status == "error") {
        res$status <- 400
      } else if (result$status == "accepted") {
        res$status <- 202
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(
        error = "Internal server error",
        message = e$message
      ))
    })
  }, serializer = plumber::serializer_json())
  
  # Transform endpoint
  pr$handle("POST", "/transform", function(req, res) {
    tryCatch({
      body <- req$body
      
      # Validate inputs
      if (is.null(body$source_metrics) || is.null(body$target_metrics)) {
        res$status <- 400
        return(list(
          error = "Missing required fields",
          details = "Request must include source_metrics and target_metrics"
        ))
      }
      
      # Run transformation
      result <- diversityGPT_api(
        "transform",
        source_metrics = body$source_metrics,
        target_metrics = body$target_metrics,
        transformation_matrix = body$transformation_matrix,
        config = config
      )
      
      if (result$status == "error") {
        res$status <- 400
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(error = "Internal server error", message = e$message))
    })
  }, serializer = plumber::serializer_json())
  
  # Validation endpoint
  pr$handle("POST", "/validate", function(req, res) {
    tryCatch({
      body <- req$body
      
      # Validate inputs
      if (is.null(body$universal_info) || is.null(body$phyloseq_data)) {
        res$status <- 400
        return(list(
          error = "Missing required fields",
          details = "Request must include universal_info and phyloseq_data"
        ))
      }
      
      # Deserialize objects
      universal_info <- .deserialize_universal_info(body$universal_info)
      phyloseq_obj <- .deserialize_phyloseq(body$phyloseq_data)
      
      # Run validation
      result <- diversityGPT_api(
        "validate",
        universal_info = universal_info,
        phyloseq_obj = phyloseq_obj,
        validation_type = body$validation_type %||% "full",
        config = config
      )
      
      if (result$status == "error") {
        res$status <- 400
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(error = "Internal server error", message = e$message))
    })
  }, serializer = plumber::serializer_json())
  
  # Batch analysis endpoint
  pr$handle("POST", "/batch", function(req, res) {
    tryCatch({
      body <- req$body
      
      if (is.null(body$datasets)) {
        res$status <- 400
        return(list(
          error = "Missing required field: datasets",
          details = "Request must include array of dataset objects"
        ))
      }
      
      # Deserialize datasets
      dataset_list <- lapply(body$datasets, .deserialize_phyloseq)
      
      # Run batch analysis
      result <- diversityGPT_api(
        "batch_analyze",
        dataset_list = dataset_list,
        dataset_names = body$dataset_names,
        analysis_steps = body$analysis_steps %||% c("universal", "mechanisms"),
        config = config,
        async = TRUE  # Always async for batch
      )
      
      if (result$status == "error") {
        res$status <- 400
      } else {
        res$status <- 202  # Accepted
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(error = "Internal server error", message = e$message))
    })
  }, serializer = plumber::serializer_json())
  
  # Meta-analysis endpoint
  pr$handle("POST", "/meta-analysis", function(req, res) {
    tryCatch({
      body <- req$body
      
      if (is.null(body$studies)) {
        res$status <- 400
        return(list(
          error = "Missing required field: studies",
          details = "Request must include array of study objects"
        ))
      }
      
      # Deserialize studies
      study_list <- lapply(body$studies, function(study) {
        if ("phyloseq_data" %in% names(study)) {
          .deserialize_phyloseq(study$phyloseq_data)
        } else if ("universal_info" %in% names(study)) {
          .deserialize_universal_info(study$universal_info)
        } else {
          stop("Study must contain either phyloseq_data or universal_info")
        }
      })
      
      # Run meta-analysis
      result <- diversityGPT_api(
        "meta_analyze",
        study_list = study_list,
        study_names = body$study_names,
        meta_method = body$method %||% "random_effects",
        config = config
      )
      
      if (result$status == "error") {
        res$status <- 400
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(error = "Internal server error", message = e$message))
    })
  }, serializer = plumber::serializer_json())
  
  # Report generation endpoint
  pr$handle("POST", "/report", function(req, res) {
    tryCatch({
      body <- req$body
      
      if (is.null(body$analysis_results)) {
        res$status <- 400
        return(list(
          error = "Missing required field: analysis_results",
          details = "Request must include analysis results"
        ))
      }
      
      # Generate report
      result <- diversityGPT_api(
        "generate_report",
        analysis_results = body$analysis_results,
        output_format = body$format %||% "html",
        template = body$template %||% "research",
        config = config
      )
      
      if (result$status == "error") {
        res$status <- 400
      }
      
      # If successful, optionally return file
      if (result$status == "success" && body$return_file %||% FALSE) {
        report_path <- result$data$report_path
        if (file.exists(report_path)) {
          res$setHeader("Content-Type", ifelse(
            grepl("\\.pdf$", report_path),
            "application/pdf",
            "text/html"
          ))
          res$setHeader("Content-Disposition", paste0(
            "attachment; filename=\"", basename(report_path), "\""
          ))
          return(readBin(report_path, "raw", file.info(report_path)$size))
        }
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(error = "Internal server error", message = e$message))
    })
  }, serializer = plumber::serializer_json())
  
  # Job management endpoints
  pr$handle("GET", "/jobs", function(req) {
    status_filter <- req$args$status
    
    result <- diversityGPT_api(
      "list_jobs",
      status_filter = status_filter,
      verbose = FALSE
    )
    
    return(result)
  }, serializer = plumber::serializer_json())
  
  pr$handle("GET", "/jobs/:job_id", function(job_id) {
    result <- diversityGPT_api(
      "status",
      job_id = job_id,
      verbose = FALSE
    )
    
    return(result)
  }, serializer = plumber::serializer_json())
  
  pr$handle("GET", "/jobs/:job_id/results", function(job_id, res) {
    result <- diversityGPT_api(
      "get_results",
      job_id = job_id,
      clean = FALSE,
      verbose = FALSE
    )
    
    if (result$status == "error") {
      res$status <- 404
    }
    
    return(result)
  }, serializer = plumber::serializer_json())
  
  pr$handle("DELETE", "/jobs/:job_id", function(job_id, res) {
    result <- diversityGPT_api(
      "cancel",
      job_id = job_id,
      verbose = FALSE
    )
    
    if (result$status == "error") {
      res$status <- 404
    }
    
    return(result)
  }, serializer = plumber::serializer_json())
  
  # Literature search endpoint
  pr$handle("POST", "/literature", function(req, res) {
    tryCatch({
      body <- req$body
      
      result <- diversityGPT_api(
        "search_literature",
        universal_info = if (!is.null(body$universal_info)) {
          .deserialize_universal_info(body$universal_info)
        } else NULL,
        query = body$query,
        databases = body$databases %||% c("biorxiv"),
        max_papers = body$max_papers %||% 10,
        config = config
      )
      
      if (result$status == "error") {
        res$status <- 400
      }
      
      return(result)
      
    }, error = function(e) {
      res$status <- 500
      return(list(error = "Internal server error", message = e$message))
    })
  }, serializer = plumber::serializer_json())
  
  # Enable CORS if requested
  if (cors) {
    pr$filter("cors", function(req, res) {
      res$setHeader("Access-Control-Allow-Origin", "*")
      res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
      res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
      
      if (req$REQUEST_METHOD == "OPTIONS") {
        res$status <- 200
        return(list(cors = "preflight"))
      } else {
        plumber::forward()
      }
    })
  }
  
  # Add OpenAPI documentation
  if (docs) {
    pr$setDocs(TRUE)
    pr$setApiSpec(function() {
      .create_api_spec()
    })
  }
  
  return(pr)
}

# Helper functions for serialization/deserialization

.deserialize_phyloseq <- function(data) {
  # Handle different input formats
  if (inherits(data, "phyloseq")) {
    return(data)
  }
  
  if (is.character(data) && length(data) == 1) {
    # Base64 encoded RDS
    return(unserialize(base64enc::base64decode(data)))
  }
  
  if (is.list(data)) {
    # JSON representation
    # This is a simplified version - full implementation would need
    # proper reconstruction of phyloseq components
    
    required_components <- c("otu_table", "tax_table", "sample_data")
    
    if (!any(required_components %in% names(data))) {
      stop("Invalid phyloseq data format")
    }
    
    # Reconstruct phyloseq object
    components <- list()
    
    if (!is.null(data$otu_table)) {
      otu_mat <- as.matrix(data$otu_table$data)
      components$otu_table <- phyloseq::otu_table(
        otu_mat,
        taxa_are_rows = data$otu_table$taxa_are_rows %||% TRUE
      )
    }
    
    if (!is.null(data$tax_table)) {
      tax_mat <- as.matrix(data$tax_table)
      components$tax_table <- phyloseq::tax_table(tax_mat)
    }
    
    if (!is.null(data$sample_data)) {
      samp_df <- as.data.frame(data$sample_data)
      components$sample_data <- phyloseq::sample_data(samp_df)
    }
    
    if (!is.null(data$phy_tree)) {
      # Tree reconstruction would require ape package
      warning("Phylogenetic tree reconstruction not implemented")
    }
    
    return(do.call(phyloseq::phyloseq, components))
  }
  
  stop("Unable to deserialize phyloseq data")
}

.deserialize_universal_info <- function(data) {
  if (inherits(data, "universal_information")) {
    return(data)
  }
  
  if (is.character(data) && length(data) == 1) {
    # Base64 encoded RDS
    return(unserialize(base64enc::base64decode(data)))
  }
  
  if (is.list(data)) {
    # Reconstruct universal_information object
    if (!all(c("information_components", "transformation_matrix") %in% names(data))) {
      stop("Invalid universal_information format")
    }
    
    # Convert components to proper format
    if (is.list(data$information_components)) {
      data$information_components <- as.data.frame(data$information_components)
    }
    
    class(data) <- c("universal_information", "list")
    return(data)
  }
  
  stop("Unable to deserialize universal_information data")
}

.create_api_spec <- function() {
  # OpenAPI specification for the API
  list(
    openapi = "3.0.0",
    info = list(
      title = "diversityGPT API",
      version = as.character(packageVersion("diversityGPT")),
      description = "REST API for universal diversity analysis using diversityGPT"
    ),
    servers = list(
      list(url = "http://localhost:8000", description = "Local server")
    ),
    paths = list(
      "/analyze" = list(
        post = list(
          summary = "Analyze phyloseq data",
          requestBody = list(
            required = TRUE,
            content = list(
              "application/json" = list(
                schema = list(
                  type = "object",
                  properties = list(
                    data = list(type = "object", description = "Phyloseq data"),
                    components = list(
                      type = "array",
                      items = list(type = "string"),
                      default = c("universal", "mechanisms", "hypotheses")
                    ),
                    async = list(type = "boolean", default = FALSE)
                  ),
                  required = list("data")
                )
              )
            )
          ),
          responses = list(
            "200" = list(description = "Successful analysis"),
            "202" = list(description = "Async job accepted"),
            "400" = list(description = "Invalid input"),
            "500" = list(description = "Server error")
          )
        )
      ),
      "/transform" = list(
        post = list(
          summary = "Transform diversity metrics",
          requestBody = list(
            required = TRUE,
            content = list(
              "application/json" = list(
                schema = list(
                  type = "object",
                  properties = list(
                    source_metrics = list(type = "object"),
                    target_metrics = list(type = "array", items = list(type = "string")),
                    transformation_matrix = list(type = "object", nullable = TRUE)
                  ),
                  required = list("source_metrics", "target_metrics")
                )
              )
            )
          ),
          responses = list(
            "200" = list(description = "Successful transformation"),
            "400" = list(description = "Invalid input"),
            "500" = list(description = "Server error")
          )
        )
      )
    )
  )
}

#' Run diversityGPT API Server
#'
#' Convenience function to create and run the API server
#'
#' @param config API configuration
#' @param port Port number
#' @param host Host address
#' @param ... Additional arguments passed to plumber::run
#'
#' @export
run_diversityGPT_api <- function(config = NULL,
                                port = 8000,
                                host = "127.0.0.1",
                                ...) {
  
  api <- create_diversityGPT_api(config = config, port = port, host = host)
  
  cat("Starting diversityGPT API server...\n")
  cat("API endpoint: http://", host, ":", port, "\n", sep = "")
  cat("API documentation: http://", host, ":", port, "/__docs__/\n", sep = "")
  cat("Press Ctrl+C to stop the server\n\n")
  
  api$run(port = port, host = host, ...)
}

#' Create API Client
#'
#' Creates a client for accessing diversityGPT API
#'
#' @param base_url Base URL of the API server
#' @param api_key Optional API key for authentication
#'
#' @return API client object
#' @export
create_api_client <- function(base_url = "http://localhost:8000",
                             api_key = NULL) {
  
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for API client functionality")
  }
  
  client <- list(
    base_url = base_url,
    api_key = api_key,
    
    # Analysis method
    analyze = function(phyloseq_obj, components = NULL, async = FALSE) {
      # Serialize phyloseq object
      serialized_data <- base64enc::base64encode(serialize(phyloseq_obj, NULL))
      
      # Build request
      req <- httr2::request(paste0(base_url, "/analyze"))
      req <- httr2::req_body_json(req, list(
        data = serialized_data,
        components = components,
        async = async
      ))
      
      if (!is.null(api_key)) {
        req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
      }
      
      # Send request
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)
    },
    
    # Transform method
    transform = function(source_metrics, target_metrics, transformation_matrix = NULL) {
      req <- httr2::request(paste0(base_url, "/transform"))
      req <- httr2::req_body_json(req, list(
        source_metrics = source_metrics,
        target_metrics = target_metrics,
        transformation_matrix = transformation_matrix
      ))
      
      if (!is.null(api_key)) {
        req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
      }
      
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)
    },
    
    # Job status method
    job_status = function(job_id) {
      req <- httr2::request(paste0(base_url, "/jobs/", job_id))
      
      if (!is.null(api_key)) {
        req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
      }
      
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)
    },
    
    # Get results method
    get_results = function(job_id) {
      req <- httr2::request(paste0(base_url, "/jobs/", job_id, "/results"))
      
      if (!is.null(api_key)) {
        req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
      }
      
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)
    }
  )
  
  class(client) <- c("diversityGPT_client", "list")
  return(client)
}

#' @export
print.diversityGPT_client <- function(x, ...) {
  cat("diversityGPT API Client\n")
  cat("======================\n")
  cat("Base URL:", x$base_url, "\n")
  cat("Authentication:", ifelse(is.null(x$api_key), "None", "API Key"), "\n")
  cat("\nAvailable methods:\n")
  cat("- analyze(): Analyze phyloseq data\n")
  cat("- transform(): Transform diversity metrics\n")
  cat("- job_status(): Check async job status\n")
  cat("- get_results(): Get job results\n")
  invisible(x)
}