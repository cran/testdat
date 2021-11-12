## ---- echo = FALSE, warning = FALSE, message = FALSE--------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(testdat)
library(dplyr)

## -----------------------------------------------------------------------------
test_that("multi-variable identifier is unique", {
  expect_unique(c(name, year, month, day, hour), data = storms)
})

## -----------------------------------------------------------------------------
test_that("hour values are valid", {
  expect_base(ts_diameter, year >= 2004)
})

## -----------------------------------------------------------------------------
test_that("iris range checks", {
  expect_range(Petal.Width, 0, 1, data = iris)
})

test_that("iris range checks filtered", {
  # Test passes for setosa rows
  expect_range(Petal.Width, 0, 1, flt = Species == "setosa", data = iris)
  # Failures will provide the filter
  expect_range(Petal.Width, 0, 0.5, flt = Species == "setosa", data = iris)
})

## -----------------------------------------------------------------------------
set_testdata(iris)
identical(get_testdata(), iris)

test_that("Versicolor has sepal length greater than 5 - will fail", {
  expect_cond(Species %in% "versicolor", Sepal.Length >= 5)
})

## -----------------------------------------------------------------------------
set_testdata(mtcars)
identical(get_testdata(), mtcars)

with_testdata(iris, {
  test_that("Versicolor has sepal length greater than 5 - will fail", {
    expect_cond(Species %in% "versicolor", Sepal.Length >= 5)
  })
})

identical(get_testdata(), mtcars)

## -----------------------------------------------------------------------------
test_that("Versicolor has sepal length greater than 5 - will fail", {
  expect_cond(Species %in% "versicolor", Sepal.Length >= 5, data = iris)
})

## -----------------------------------------------------------------------------
tmp_data <- tibble(x = c(1, 0), y = c(1, NA))

set_testdata(tmp_data)
print(get_testdata())
expect_base(y, x == 1)

tmp_data$y <- 1
print(get_testdata())
expect_base(y, x == 1)

## -----------------------------------------------------------------------------
library(dplyr)

x <- tribble(
  ~id, ~pcode, ~state, ~nsw_only,
  1,   2000,   "NSW",  1,
  2,   3123,   "VIC",  NA,
  3,   2123,   "NSW",  3,
  4,   12345,  "VIC",  3
)

# check id is unique
x %>% filter(duplicated(id))

# check values
x %>% filter(!pcode %in% 2000:3999)
x %>% count(state)
x %>% count(nsw_only)

# check base for nsw_only variable
x %>% filter(state != "NSW") %>% count(nsw_only)

x <- x %>% mutate(market = case_when(pcode %in% 2000:2999 ~ 1,
                                     pcode %in% 3000:3999 ~ 2))

x %>% count(market)

## -----------------------------------------------------------------------------
library(testdat)
library(dplyr)

x <- tribble(
  ~id, ~pcode, ~state, ~nsw_only,
  1,   2000,   "NSW",  1,
  2,   3123,   "VIC",  NA,
  3,   2123,   "NSW",  3,
  4,   12345,  "VIC",  3
)

with_testdata(x, {
  test_that("id is unique", {
    expect_unique(id)
  })
  
  test_that("variable values are correct", {
    expect_values(pcode, 2000:2999, 3000:3999)
    expect_values(state, c("NSW", "VIC"))
    expect_values(nsw_only, 1:3) # by default expect_values allows NAs
  })
  
  test_that("filters applied correctly", {
    expect_base(nsw_only, state == "NSW")
  })
})

x <- x %>% mutate(market = case_when(pcode %in% 2000:2999 ~ 1,
                                     pcode %in% 3000:3999 ~ 2))

with_testdata(x, {
  test_that("market derived correctly", {
    expect_values(market, 1:2, miss = NULL) # miss = NULL excludes NAs from valid values
  })
})

## -----------------------------------------------------------------------------
set_testdata(iris)

test_that("Variable format checks", {
  expect_regex(Species, "^[a-z]+$")
})

test_that("Versicolor has sepal length greater than 5 - will fail", {
  expect_cond(Species %in% "versicolor", Sepal.Length >= 5)
})

