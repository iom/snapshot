test_that("$data gives a tibble", {
  expect_output(str(get_emp()$data), "tibble")
  expect_output(str(get_empeduc()$data), "tibble")
})

test_that("$range gives integer vector of length 2", {
  expect_vector(get_emp()$range, ptype = integer(), size = 2)
  expect_vector(get_empeduc()$range, ptype = integer(), size = 2)
})

test_that('iso="DZA" (Algeria) gives null', {
  expect_null(get_emp("DZA")$data)
  expect_null(get_emp("DZA")$range)
  expect_null(get_empeduc("DZA")$data)
  expect_null(get_empeduc("DZA")$range)
})

test_that('iso="OOO" (Others) gives null', {
  expect_null(get_emp("OOO")$data)
  expect_null(get_emp("OOO")$range)
  expect_null(get_empeduc("OOO")$data)
  expect_null(get_empeduc("OOO")$range)
})
