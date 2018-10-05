library(recipes)
library(magrittr)
library(tidyselect)

# function to identify variables with missing proportion above some threshold.
missing_proportion <- function(x, threshold = 0.5) {
  # identify problematic variables
  problematic_lgl <- purrr::map_lgl(x, ~ mean(is.na(.)) >= threshold)
  # return names of problematic variables
  names(x)[problematic_lgl]
}

# # function to identify variables with missing proportion above some threshold.
# missing_proportion <- function(x, threshold = 0.5, other_values = NULL) {
#   # identify problematic variables
#   if (is.null(other_values)) {
#     problematic_lgl <- purrr::map_lgl(x, ~ mean(is.na(.)) >= threshold)
#   } else {
#     problematic_lgl <- purrr::map_lgl(x, ~ mean(is.na(.) | . %in% other_values) >= threshold)
#   }
#   # return names of problematic variables
#   names(x)[problematic_lgl]
# }

# constructor function.
step_missing_proportion_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           options = NULL,
           removals = NULL,
           skip = FALSE) {
    recipes::step(
      subclass = "missing_proportion",
      terms = terms,
      role = role,
      trained = trained,
      options = options,
      removals = removals,
      skip = skip
    )
  }

# add step to recipe.
#' @export
step_missing_proportion <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           options = list(threshold = 0.95),
           removals = NULL,
           skip = FALSE) {
    recipes::add_step(
      recipe,
      step_missing_proportion_new(
        terms = recipes::ellipse_check(...),
        role = role,
        trained = trained,
        options = options,
        removals = removals,
        skip = skip
      )
    )
  }

# prepare step.
#' @export
prep.step_missing_proportion <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(x$terms, info = info)
  filter <- missing_proportion(
    x = training[, col_names],
    threshold = x$options$threshold
  )

  step_missing_proportion_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    options = x$options,
    removals = filter,
    skip = x$skip
  )
}

# bake step.
#' @export
bake.step_missing_proportion <- function(object, newdata, ...) {
  if (length(object$removals) > 0)
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  tibble::as_tibble(newdata)
}

# print step.
print.step_missing_proportion <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Variables with high missing ratio removed ")
        cat(recipes:::format_ch_vec(x$removals, width = width))
      } else
        cat("Variables with high missing ratio removed no terms")
    } else {
      cat("High missing ratio filter on ", sep = "")
      cat(recipes:::format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_missing_proportion
#' @param x A `step_missing_proportion` object.
tidy.step_missing_proportion <- recipes:::tidy_filter
