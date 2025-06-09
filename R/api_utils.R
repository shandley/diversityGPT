#' Check and retrieve API keys
#'
#' @param provider Character string, either "anthropic" or "openai"
#' @return API key string or NULL if not found
#' @keywords internal
get_api_key <- function(provider = c("anthropic", "openai")) {
  provider <- match.arg(provider)
  
  key_name <- switch(provider,
    anthropic = "ANTHROPIC_API_KEY",
    openai = "OPENAI_API_KEY"
  )
  
  key <- Sys.getenv(key_name)
  
  if (key == "") {
    return(NULL)
  }
  
  return(key)
}

#' Check if API keys are configured
#'
#' @return Logical indicating if at least one API key is configured
#' @export
#' @examples
#' check_api_setup()
check_api_setup <- function() {
  anthropic_key <- get_api_key("anthropic")
  openai_key <- get_api_key("openai")
  
  if (is.null(anthropic_key) && is.null(openai_key)) {
    cli::cli_alert_danger("No API keys found")
    cli::cli_alert_info("Add to your .Renviron file:")
    cli::cli_alert_info("  ANTHROPIC_API_KEY=your-key-here")
    cli::cli_alert_info("  OPENAI_API_KEY=your-key-here")
    cli::cli_alert_info("Then restart your R session")
    return(FALSE)
  }
  
  if (!is.null(anthropic_key)) {
    cli::cli_alert_success("Anthropic API key configured")
  }
  
  if (!is.null(openai_key)) {
    cli::cli_alert_success("OpenAI API key configured")
  }
  
  return(TRUE)
}

#' Make API request with error handling
#'
#' @param url API endpoint URL
#' @param body Request body (list)
#' @param api_key API key
#' @param provider API provider ("anthropic" or "openai")
#' @return Parsed response or error
#' @keywords internal
make_api_request <- function(url, body, api_key, provider = "anthropic") {
  
  headers <- switch(provider,
    anthropic = list(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ),
    openai = list(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    )
  )
  
  req <- httr2::request(url) |>
    httr2::req_headers(!!!headers) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3)
  
  tryCatch({
    resp <- httr2::req_perform(req)
    
    # Check response status
    if (httr2::resp_status(resp) != 200) {
      cli::cli_alert_danger("API returned status {httr2::resp_status(resp)}")
      cli::cli_alert_info("Response body: {httr2::resp_body_string(resp)}")
      return(NULL)
    }
    
    httr2::resp_body_json(resp)
  }, error = function(e) {
    cli::cli_alert_danger("API request failed: {e$message}")
    cli::cli_alert_info("URL: {url}")
    cli::cli_alert_info("Provider: {provider}")
    NULL
  })
}