test_that("formats with the same device get the size in their names", {
  local_test_env(ggsaveR.formats = list(
    list(device = "png", width = 60, height = 40, units = "mm", dpi = 50),
    list(device = "png", width = 120, height = 80, units = "mm", dpi = 25),
    list(device = "pdf", width = 60, height = 40, units = "mm")
  ))
  saved <- save_quietly("fig.png", p)
  expect_equal(saved, c("fig_60x40mm_50dpi.png", "fig_120x80mm_25dpi.png", "fig.pdf"))
  expect_true(all(file.exists(saved)))
})

test_that("size_suffix = TRUE names every file, without dpi for vector formats", {
  local_test_env(ggsaveR.size_suffix = TRUE, ggsaveR.formats = c("png", "pdf"))
  expect_equal(save_quietly("fig.png", p), c("fig_3x2in_50dpi.png", "fig_3x2in.pdf"))
})

test_that("the size suffix reflects scale, string dpi, pixels and defaults", {
  local_test_env(ggsaveR.size_suffix = TRUE)
  out <- suppressMessages(ggsave("a.png", p, width = 2.5, height = 2, scale = 2, dpi = "screen"))
  expect_equal(out, "a_5x4in_72dpi.png")

  out <- suppressMessages(ggsave("b.png", p, width = 300, height = 200, units = "px"))
  expect_equal(out, "b_300x200px.png")

  grDevices::pdf(NULL, width = 5, height = 4)
  withr::defer(grDevices::dev.off())
  out <- suppressMessages(ggsave("c.pdf", p))
  expect_equal(out, "c_5x4in.pdf")
})

test_that("an explicit suffix wins over the size suffix", {
  local_test_env(ggsaveR.formats = list(
    list(device = "png", suffix = "_slide"),
    list(device = "png", suffix = "_paper", width = 4)
  ))
  expect_equal(save_quietly("fig.png", p), c("fig_slide.png", "fig_paper.png"))
})

test_that("size_suffix = FALSE leaves clashing names as an error", {
  local_test_env(ggsaveR.size_suffix = FALSE, ggsaveR.formats = list(
    list(device = "png", dpi = 50), list(device = "png", dpi = 100)
  ))
  expect_error(save_quietly("fig.png", p), "Several formats would write to 'fig.png'")
  expect_length(list.files(), 0)
})

test_that("identical formats are an error even with size suffixes", {
  local_test_env(ggsaveR.formats = c("png", "png"))
  expect_error(save_quietly("fig.png", p), "Several formats would write")
})

test_that("invalid units and dpi are reported when a suffix is needed", {
  local_test_env(ggsaveR.size_suffix = TRUE)
  expect_error(save_quietly("a.png", p, units = "ft"), "`units`")
  expect_error(suppressMessages(ggsave("a.png", p, width = 2, height = 2, dpi = "high")), "`dpi`")
})

test_that("ggsaveR.dir puts relative file names in a directory it creates", {
  local_test_env(ggsaveR.dir = "figures/main")
  expect_equal(save_quietly("fig.png", p), file.path("figures/main", "fig.png"))
  expect_true(file.exists("figures/main/fig.png"))

  abs <- file.path(getwd(), "abs.png")
  expect_equal(save_quietly(abs, p), abs)
  expect_true(file.exists(abs))
})

test_that("ggsaveR.dir works together with metadata and unique names", {
  local_test_env(ggsaveR.dir = "out", ggsaveR.creator = "Me", ggsaveR.overwrite_action = "unique")
  save_quietly("fig.png", p)
  expect_equal(save_quietly("fig.png", p), file.path("out", "fig-1.png"))
  expect_equal(png_metadata("out/fig-1.png")$Author, "Me")
})
