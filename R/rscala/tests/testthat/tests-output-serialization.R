context("output-serialization")

# skip("output-serialization")

test_that("serialization captures output", {
  expect_silent(s2 * 'print("Hi")')
  expect_output(s * 'print("Hi")', "Hi")
})
