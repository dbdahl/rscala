context("serialization")

# skip("serialization")

test_that("serialization captures output", {
  expect_silent(s %~% 'print("Hi")')
  expect_output(s2 %~% 'print("Hi")', "Hi")
})
