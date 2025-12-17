test_that("goose_save and goose_load work correctly", {
  skip_on_cran()
  
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  temp_memory <- file.path(temp_dir, "test_goose_memory")
  dir.create(temp_memory, showWarnings = FALSE, recursive = TRUE)
  
  # Mock the memory path
  with_mocked_bindings(
    get_memory_path = function(global = TRUE) temp_memory,
    {
      # Test saving a simple object
      test_data <- data.frame(x = 1:5, y = letters[1:5])
      
      expect_message(
        goose_save(test_data, "test_df", category = "test_category", 
                   tags = c("test", "example")),
        "Saved"
      )
      
      # Test loading the object
      loaded_data <- goose_load("test_df", category = "test_category")
      expect_equal(loaded_data, test_data)
      
      # Test that overwrite = FALSE prevents overwriting
      expect_error(
        goose_save(test_data, "test_df", category = "test_category"),
        "already exists"
      )
      
      # Test that overwrite = TRUE allows overwriting
      test_data2 <- data.frame(x = 6:10, y = letters[6:10])
      expect_message(
        goose_save(test_data2, "test_df", category = "test_category", 
                   overwrite = TRUE),
        "Saved"
      )
      
      loaded_data2 <- goose_load("test_df", category = "test_category")
      expect_equal(loaded_data2, test_data2)
    }
  )
  
  # Cleanup
  unlink(temp_memory, recursive = TRUE)
})

test_that("goose_list works correctly", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  temp_memory <- file.path(temp_dir, "test_goose_memory2")
  dir.create(temp_memory, showWarnings = FALSE, recursive = TRUE)
  
  with_mocked_bindings(
    get_memory_path = function(global = TRUE) temp_memory,
    {
      # Save multiple objects
      goose_save(mtcars, "mtcars_data", category = "datasets", tags = "example")
      goose_save(iris, "iris_data", category = "datasets", tags = "example")
      goose_save(lm(mpg ~ wt, data = mtcars), "model1", category = "models", 
                 tags = c("regression", "mtcars"))
      
      # Test listing all objects
      all_objects <- goose_list()
      expect_s3_class(all_objects, "data.frame")
      expect_equal(nrow(all_objects), 3)
      
      # Test filtering by category
      datasets <- goose_list(category = "datasets")
      expect_equal(nrow(datasets), 2)
      
      # Test filtering by tags
      mtcars_objects <- goose_list(tags = "mtcars")
      expect_equal(nrow(mtcars_objects), 1)
    }
  )
  
  unlink(temp_memory, recursive = TRUE)
})

test_that("goose_delete works correctly", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  temp_memory <- file.path(temp_dir, "test_goose_memory3")
  dir.create(temp_memory, showWarnings = FALSE, recursive = TRUE)
  
  with_mocked_bindings(
    get_memory_path = function(global = TRUE) temp_memory,
    {
      # Save an object
      test_data <- data.frame(x = 1:5)
      goose_save(test_data, "temp_data", category = "temp")
      
      # Verify it exists
      expect_true(file.exists(file.path(temp_memory, "r_objects", "temp_data.rds")))
      
      # Delete it (without confirmation)
      goose_delete("temp_data", category = "temp", confirm = FALSE)
      
      # Verify it's gone
      expect_false(file.exists(file.path(temp_memory, "r_objects", "temp_data.rds")))
      
      # Try to load deleted object
      expect_error(
        goose_load("temp_data", category = "temp"),
        "not found"
      )
    }
  )
  
  unlink(temp_memory, recursive = TRUE)
})

test_that("special characters in names are handled", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  temp_memory <- file.path(temp_dir, "test_goose_memory4")
  dir.create(temp_memory, showWarnings = FALSE, recursive = TRUE)
  
  with_mocked_bindings(
    get_memory_path = function(global = TRUE) temp_memory,
    {
      test_data <- data.frame(x = 1:3)
      
      # Names with special characters should be cleaned
      goose_save(test_data, "my model (v2)!", category = "test")
      
      # Should be saved with cleaned name
      expect_true(file.exists(file.path(temp_memory, "r_objects", "my_model__v2__.rds")))
      
      # Should be loadable with cleaned name
      loaded <- goose_load("my_model__v2__", category = "test")
      expect_equal(loaded, test_data)
    }
  )
  
  unlink(temp_memory, recursive = TRUE)
})
