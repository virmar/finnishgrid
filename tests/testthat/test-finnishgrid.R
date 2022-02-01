test_that("API call without parameters", {
  expect_error(get_data(), regexp = NULL)
})

test_that("API call without start, end, key", {
  expect_error(get_data(api_number = 124), regexp = NULL)
})

test_that("API call without end, key", {
  expect_error(get_data(api_number = 124,
                        start_time = "2021-01-01T00:00:00+0200"), regexp = NULL)
})

#test_that("API call with bad key, HTTP ERROR 403", {
#  expect_error(get_data(api_number = 124,
#                        start_time = "2021-01-01T00:00:00+0200",
#                        end_time = "2021-01-03T00:00:00+0200",
#                        user_key = "BAD_KEY_TEST"), regexp = NULL)
#})

# working call, key from .Renviron
#test_that("API call with working key returning data frame", {
#  tmp <- get_data(api_number = 124,
#                  start_time = "2021-01-01T00:00:00+0000",
#                  end_time = "2021-01-03T00:00:00+0000",
#                  user_key = NA)
#  expect_type(tmp, type = "list")  # df
#  expect_gt(nrow(tmp), 0)          # len > 0
#})

