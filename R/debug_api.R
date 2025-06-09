#' Debug API connection
#'
#' @description
#' Test API connection with minimal request to debug issues
#'
#' @param provider Character: "anthropic" or "openai"
#' @export
debug_api <- function(provider = "anthropic") {
  
  api_key <- get_api_key(provider)
  if (is.null(api_key)) {
    cli::cli_abort("No {provider} API key found")
  }
  
  cli::cli_alert_info("Testing {provider} API connection...")
  cli::cli_alert_info("API key found: {substr(api_key, 1, 10)}...")
  
  if (provider == "anthropic") {
    url <- "https://api.anthropic.com/v1/messages"
    
    headers <- list(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    )
    
    # Try different models
    models_to_try <- c("claude-3-5-sonnet-20240620", "claude-3-sonnet-20240229", "claude-3-haiku-20240307")
    
    for (model in models_to_try) {
      cli::cli_alert_info("Trying model: {model}")
      
      body <- list(
        model = model,
        max_tokens = 10,
        messages = list(
          list(
            role = "user",
            content = "Say hello"
          )
        )
      )
      
      # Test this model
      if (test_single_request(url, body, headers, provider, model)) {
        cli::cli_alert_success("Found working model: {model}")
        return(model)
      }
    }
    
    return(FALSE)
    
  } else if (provider == "openai") {
    url <- "https://api.openai.com/v1/chat/completions"
    
    headers <- list(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    )
    
    # Try different models
    models_to_try <- c("gpt-4o-mini", "gpt-3.5-turbo", "gpt-4o")
    
    for (model in models_to_try) {
      cli::cli_alert_info("Trying model: {model}")
      
      body <- list(
        model = model,
        max_tokens = 10,
        messages = list(
          list(
            role = "user",
            content = "Say hello"
          )
        )
      )
      
      # Test this model
      if (test_single_request(url, body, headers, provider, model)) {
        cli::cli_alert_success("Found working model: {model}")
        return(model)
      }
      
      # Wait between attempts to avoid rate limiting
      Sys.sleep(2)
    }
    
    return(FALSE)
  }
}

#' Test a single API request
#' @keywords internal
test_single_request <- function(url, body, headers, provider, model) {
  
  cli::cli_alert_info("URL: {url}")
  cli::cli_alert_info("Model: {model}")
  
  # Make request with detailed error reporting
  req <- httr2::request(url) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30)
  
  tryCatch({
    cli::cli_alert_info("Sending request...")
    resp <- httr2::req_perform(req)
    
    status <- httr2::resp_status(resp)
    cli::cli_alert_info("Response status: {status}")
    
    if (status == 200) {
      result <- httr2::resp_body_json(resp)
      cli::cli_alert_success("✓ Model {model} is working!")
      
      if (provider == "anthropic") {
        content <- result$content[[1]]$text
        cli::cli_alert_info("Response: {content}")
      } else {
        content <- result$choices[[1]]$message$content
        cli::cli_alert_info("Response: {content}")
      }
      
      return(TRUE)
    } else {
      cli::cli_alert_warning("✗ Model {model} failed with status {status}")
      body_text <- httr2::resp_body_string(resp)
      cli::cli_alert_info("Response: {substr(body_text, 1, 200)}...")
      return(FALSE)
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("✗ Model {model} failed: {e$message}")
    return(FALSE)
  })
}