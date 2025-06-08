test_that("API key retrieval works", {
  # Test with mock environment variable
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "test-key-123"),
    {
      expect_equal(get_api_key("anthropic"), "test-key-123")
    }
  )
  
  # Test with missing key
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = ""),
    {
      expect_null(get_api_key("anthropic"))
    }
  )
})

test_that("check_api_setup provides correct feedback", {
  # Test with no keys
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "", OPENAI_API_KEY = ""),
    {
      expect_false(check_api_setup())
    }
  )
  
  # Test with Anthropic key only
  withr::with_envvar(
    c(ANTHROPIC_API_KEY = "test-key", OPENAI_API_KEY = ""),
    {
      expect_true(check_api_setup())
    }
  )
})