test_that("progress tracker creation works", {
  # Console progress
  progress <- create_progress_tracker(
    total = 100,
    message = "Test progress"
  )
  
  expect_type(progress, "list")
  expect_equal(progress$total, 100)
  expect_equal(progress$type, "console")
  expect_equal(progress$message, "Test progress")
  
  # Finish to clean up
  finish_progress(progress)
})

test_that("with_progress executes code correctly", {
  # Simple computation
  result <- with_progress({
    sum(1:100)
  }, message = "Computing sum")
  
  expect_equal(result, 5050)
  
  # Multiple steps
  result2 <- with_progress({
    step1 <- mean(1:10)
    step2 <- sd(1:10)
    list(mean = step1, sd = step2)
  }, message = "Multi-step computation")
  
  expect_type(result2, "list")
  expect_equal(result2$mean, 5.5)
})

test_that("progress updates work", {
  progress <- create_progress_tracker(
    total = 10,
    message = "Test updates"
  )
  
  # Update progress
  for (i in 1:5) {
    update_progress(progress, i, detail = paste("Step", i))
  }
  
  expect_equal(progress$current, 5)
  
  finish_progress(progress)
})

test_that("create_console_progress handles cli availability", {
  # Should work whether cli is available or not
  progress <- create_console_progress(50, "Test")
  
  expect_type(progress, "list")
  expect_true("update" %in% names(progress))
  expect_true("finish" %in% names(progress))
  
  # Update and finish should not error
  progress$update(25)
  progress$finish()
})

test_that("create_shiny_progress returns appropriate structure", {
  # Mock shiny environment
  mock_session <- list(ns = function(x) paste0("ns-", x))
  
  progress <- create_shiny_progress(100, "Shiny test", session = mock_session)
  
  expect_type(progress, "list")
  expect_true("update" %in% names(progress))
  expect_true("finish" %in% names(progress))
  
  # Should handle updates without error
  progress$update(50, detail = "Halfway")
  progress$finish()
})

test_that("progress tracking handles errors gracefully", {
  # Error during computation shouldn't break progress
  expect_error({
    with_progress({
      x <- 1:10
      stop("Intentional error")
      x
    }, message = "Error test")
  }, "Intentional error")
  
  # Progress should be cleaned up even on error
  # (Hard to test directly, but shouldn't leave artifacts)
})

test_that("nested progress tracking works", {
  # Outer progress
  result <- with_progress({
    outer_sum <- 0
    
    # Inner progress operations
    for (i in 1:3) {
      inner_result <- with_progress({
        sum(1:10) * i
      }, message = paste("Inner loop", i))
      
      outer_sum <- outer_sum + inner_result
    }
    
    outer_sum
  }, message = "Outer computation")
  
  expect_equal(result, sum(1:10) * (1 + 2 + 3))
})

test_that("progress tracker handles zero total", {
  # Zero total should not cause errors
  progress <- create_progress_tracker(
    total = 0,
    message = "Empty task"
  )
  
  expect_equal(progress$total, 0)
  
  # Updates should handle gracefully
  update_progress(progress, 0)
  finish_progress(progress)
})

test_that("update functions dispatch correctly", {
  # Console progress
  console_prog <- create_progress_tracker(10, "Console test", type = "console")
  update_console_progress(console_prog, 5, "Halfway")
  expect_equal(console_prog$current, 5)
  
  # Generic update
  update_progress(console_prog, 7)
  expect_equal(console_prog$current, 7)
  
  finish_progress(console_prog)
})