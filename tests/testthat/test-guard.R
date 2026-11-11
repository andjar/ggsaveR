test_that("guard = TRUE ignores the ggsaveR.formats option", {
  local_test_env(ggsaveR.formats = c("png", "pdf"))

  save_quietly("guard.jpeg", p, guard = TRUE)

  expect_true(file.exists("guard.jpeg"))
  expect_false(file.exists("guard.png"))
  expect_false(file.exists("guard.pdf"))
})

test_that("guard = TRUE ignores overwrite_action = 'unique'", {
  local_test_env(ggsaveR.overwrite_action = "unique")

  save_quietly("guard.png", p, guard = TRUE)
  save_quietly("guard.png", p, guard = TRUE)

  expect_equal(list.files(), "guard.png")
})

test_that("guard = TRUE ignores overwrite_action = 'stop'", {
  local_test_env(ggsaveR.overwrite_action = "stop")

  save_quietly("guard.png", p, guard = TRUE)
  expect_no_error(save_quietly("guard.png", p, guard = TRUE))
})

test_that("guard = TRUE skips metadata and data embedding", {
  local_test_env(ggsaveR.embed_data = TRUE, ggsaveR.creator = "Test Author")

  save_quietly("guard.png", p, guard = TRUE)

  meta <- png_metadata("guard.png")
  expect_null(meta$Author)
  expect_null(meta$ggsaveR_data)
})

test_that("guard = TRUE evaluates arguments in the caller's environment", {
  local_test_env()
  f <- function() {
    local_plot <- p
    local_name <- "from_function.png"
    save_quietly(local_name, local_plot, guard = TRUE)
  }
  f()
  expect_true(file.exists("from_function.png"))
})
