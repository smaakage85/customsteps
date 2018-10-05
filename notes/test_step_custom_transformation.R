library(magrittr)
library(tidyselect)

# generate data.
df <- tibble::tibble(a = rnorm(100),
                     b = rnorm(100),
                     c = rnorm(100))

# custom step: prep and bake functions ------------------------------------


# define prep helper function, that computes means and standard deviations
# for all variables in a data set.
compute_means_sd <- function(x, na.rm = FALSE, trim = 0) {
  
  purrr::map(x, ~ list(mean = mean(.x, na.rm = na.rm, trim = trim), 
                       sd = sd(.x)))

  }

# define prep function, that subtracts k means from the variable, and 
# divides by the standard deviation.
center_scale <- function(x, prep_output, k) {
  
  newdata <- dplyr::select(x, names(prep_output))

  purrr::map2(.x = newdata,
              .y = prep_output,
              ~ (.x - k * .y$mean) / .y$sd) 
  }

# create recipe.
rec <- recipes::recipe(df) %>%
  step_custom_transformation(b, c,
                             prep_fct = compute_means_sd,
                             prep_fct_args = list(na.rm = TRUE, trim = 0.05),
                             bake_fct = center_scale,
                             bake_fct_args = list(k = 2),
                             bake_how = "bind_cols")

# prep recipe.
rec_prep <- recipes::prep(rec)

# bake recipe.
rec_baked <- recipes::bake(rec_prep, df)
rec_baked

# # inspect output.
# rec
# rec_baked
# broom::tidy(rec)
# broom::tidy(rec, 1)
# broom::tidy(rec_prep)
# broom::tidy(rec_prep, 1)

# custom step: bake only --------------------------------------------------


# create custom bake function.
simple_calculation <- function(x) {
  
  x %>%
    dplyr::transmute(d = a + b - c)
  
}

# create recipe.
rec <- recipes::recipe(df) %>%
  step_custom_transformation(everything(),
                             bake_fct = simple_calculation,
                             bake_how = "bind_cols")

# prep recipe.
rec_prep <- recipes::prep(rec)

# bake recipe.
rec_baked <- recipes::bake(rec_prep, df)

# # inspect output.
# rec
# rec_baked
# broom::tidy(rec)
# broom::tidy(rec, 1)
# broom::tidy(rec_prep)
# broom::tidy(rec_prep, 1)