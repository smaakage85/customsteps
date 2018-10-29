library(testthat)
library(recipes)
library(dplyr)
library(purrr)
library(tibble)
library(tidyselect)

context("Testing custom filter")

# generate data.
df <- tibble(a = c(1, -999, 3,NA,NA),
             b = c(1,3, NA,NA,NA),
             c = c(1,-999,3,4,5),
             d = rep(1, 5),
             e = c(-999, -999, -999, -999, NA),
             f = rep(NA, 5))

# Create custom function to identify variables with a proportion of missing
# values above some threshold. The function treats
# values provided with the 'other_values' argument as missing.

filter_missings <- function(x, threshold = 0.5, other_values = NULL) {
  
  # identify problematic variables.
  if (is.null(other_values)) {
    
    problematic_lgl <- map_lgl(x, ~ mean(is.na(.)) >= threshold)
    
  } else {
    
    problematic_lgl <- map_lgl(x, ~ mean(is.na(.) | . %in% other_values) >= threshold)
    
  }
  
  # return names of problematic variables.
  names(x)[problematic_lgl]
  
}

rec <- recipe(df)

test_that('end-to-end results for custom filter', {
  
  # create recipe.
  rec_custom <- rec %>%
    step_custom_filter(everything(),
                       filter_function = filter_missings,
                       options = list(threshold = 0.5, other_values = -999))
  
  # prep recipe.
  rec_prep <- prep(rec_custom, retain = TRUE)
  
  # identify variables.
  filter <- filter_missings(df, threshold = 0.5, other_values = -999)
  baseline <- df %>% select(-filter)
  
  expect_identical(juice(rec_prep), baseline)
  
})

test_that('expected output when no variables are removed', {
  
  # variable not found.
  rec_custom <- rec %>%
    step_custom_filter(everything(),
                       filter_function = function (x) {
                         NULL
                       })
  
  expect_identical(juice(prep(rec_custom, retain = TRUE)), df)
  
})

test_that('expected errors for incorrect filter function output', {
  
  # numeric output.
  rec_custom <- rec %>%
    step_custom_filter(everything(),
                       filter_function = function (x) {
                         2
                       })
  
  expect_error(prep(rec_custom, retain = TRUE))
  
  # variable not found.
  rec_custom <- rec %>%
    step_custom_filter(everything(),
                       filter_function = function (x) {
                         c("c", "x")
                       })
  
  expect_error(prep(rec_custom, retain = TRUE))
  
  
})

test_that('printing', {
  
  # create recipe.
  rec_custom <- rec %>%
    step_custom_filter(everything(),
                       filter_function = function (x) {"c"})
  
  
  # prep recipe.
  rec_prep <- prep(rec_custom, retain = TRUE)
  
  expect_output(print(rec_custom))
  expect_output(print(prep(rec_custom)))
})