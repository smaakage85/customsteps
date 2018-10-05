library(recipes)
library(magrittr)
library(tidyselect)

# constructor function.
step_external_function_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           options = NULL,
           external_function = NULL,
           function_output = NULL,
           skip = FALSE) {
    recipes::step(
      subclass = "external_function",
      terms = terms,
      role = role,
      trained = trained,
      options = options,
      external_function = external_function,
      function_output = function_output,
      skip = skip
    )
  }

# add step to recipe.
#' @export
step_external_function <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           external_function = NULL,
           function_output = NULL,
           skip = FALSE) {
    recipes::add_step(
      recipe,
      step_external_function_new(
        terms = recipes::ellipse_check(...),
        role = role,
        trained = trained,
        external_function = external_function,
        function_output = function_output,
        skip = skip
      )
    )
  }

# prepare step.
#' @export
prep.step_external_function <- function(x, training, info = NULL, ...) {

  col_names <- recipes::terms_select(x$terms, info = info)

  output <- x$external_function(x = training[, col_names])

  step_external_function_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    external_function = x$external_function,
    function_output = output,
    skip = x$skip
  )

}

# bake step.
#' @export
bake.step_external_function <- function(object, newdata, ...) {
  tibble::as_tibble(object$function_output)
}

# print step.
print.step_external_function <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
    cat("External function applied ")
    invisible(x)
    }
  }

#' @rdname step_custom_filter
#' @param x A `step_external_function` object.
tidy.step_external_function <- recipes:::tidy_filter
