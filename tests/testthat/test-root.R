
x=odataR::odataR_set_root()
 
test_that("default root is correctly set", {
  expect_identical(x, "https://opendata.cbs.nl")
})

x=odataR::odataR_get_root()

test_that("default root is correctly get", {
  expect_identical(x, "https://opendata.cbs.nl")
})

x=odataR::odataR_set_root("http://dataderden.cbs.nl")

test_that("specific root is correctly set", {
  expect_identical(x, "http://dataderden.cbs.nl")
})

x=odataR::odataR_get_root()

test_that("specific root is correctly get", {
  expect_identical(x, "http://dataderden.cbs.nl")
})

test_that("message given for non OData url ", {
  expect_error(odataR::odataR_set_root('xx'), regexp = 'invalid value for root')
})

x=odataR::odataR_get_root()

test_that("specific root after incorrect set", {
  expect_identical(x, "http://dataderden.cbs.nl")
})

