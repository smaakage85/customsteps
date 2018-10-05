#' Custom Filter
#'
#' `step_custom_filter` creates a *specification* of a recipe step
#'  that will potentially remove variables using a custom filter
#'  function.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables that will be evaluated by the filtering.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param filter_function A custom filter function, that will
#' diagnose problematic variables (see Details below).
#' @param options A list of options that will be provided to the
#'  filter function as arguments (see Details below).
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until `prep.recipe()` is called.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be removed.
#' @keywords datagen
#' @concept preprocessing variable_filters
#' @export
#'
#' @details This step diagnoses problematic variables according to
#'  a custom filter function. The filter function must meet the
#'  following requirements:
#' \enumerate{
#'   \item the function must take an argument `x` as input. `x`
#'   must belong to the data.frame/tbl_dfmatrix class. The function
#'    should identify problematic variables.
#'   \item the function should return a vector with the names of
#'   the variables diagnosed as problematic.
#' }
#'
#' The custom filter function is called with the `x` argument set
#'  to a data.frame with the selected terms. All other arguments to
#'  the custom filter function must be provided through the
#'  'options' argument.
#'
#' @examples
#' library(magrittr)
#' library(tidyselect)
#'
#' # generate data.
#' df <- tibble::tibble(a = c(1, -999, 3,NA,NA),
#'                      b = c(1,3, NA,NA,NA),
#'                      c = c(1,-999,3,4,5),
#'                      d = rep(1, 5),
#'                      e = c(-999, -999, -999, -999, NA),
#'                      f = rep(NA, 5))
#'
#' # Create custom function to identify variables with missing
#' # proportion above some threshold. The function treats
#' # values provided with the 'other_values' argument as missing.
#'
#' filter_missings <- function(x, threshold = 0.5, other_values = NULL) {
#'
#'   # identify problematic variables
#'   if (is.null(other_values)) {
#'     problematic_lgl <- purrr::map_lgl(x, ~ mean(is.na(.)) >= threshold)
#'   } else {
#'     problematic_lgl <- purrr::map_lgl(x, ~ mean(is.na(.) | . %in% other_values) >= threshold)
#'   }
#'   # return names of problematic variables
#'   names(x)[problematic_lgl]
#'
#' }
#'
#' # create recipe.
#' rec <- recipes::recipe(df) %>%
#'   step_custom_filter(everything(),
#'                      filter_function = filter_missings,
#'                      options = list(threshold = 0.5, other_values = -999))
#' # prep recipe.
#' rec_prep <- recipes::prep(rec)
#'
#' # bake recipe.
#' rec_baked <- recipes::bake(rec_prep, df)
#'
#' ## inspect output.
#' # broom::tidy(rec)
#' # broom::tidy(rec, number = 1)
#' # broom::tidy(rec_prep)
#' # broom::tidy(rec_prep, number = 1)
#' # rec_baked
#'
#' @seealso [recipe()]
#'   [prep.recipe()] [bake.recipe()]
#'
step_custom_filter <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           filter_function = NULL,
           options = NULL,
           removals = NULL,
           skip = FALSE) {

    # check inputs.
    if (is.null(filter_function)) {
      stop("'filter_function' must be specified.")
    }

    if (!is.null(options) && !inherits(options, "list")) {
      stop("'options' must belong to the 'list' class.")
    }

    recipes::add_step(
      recipe,
      step_custom_filter_new(
        terms = recipes::ellipse_check(...),
        role = role,
        trained = trained,
        filter_function = filter_function,
        options = options,
        removals = removals,
        skip = skip
      )
    )
  }

# constructor function.
step_custom_filter_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           filter_function = NULL,
           options = NULL,
           removals = NULL,
           skip = FALSE) {
    recipes::step(
      subclass = "custom_filter",
      terms = terms,
      role = role,
      trained = trained,
      filter_function = filter_function,
      options = options,
      removals = removals,
      skip = skip
    )
  }

# prepare step.
#' @export
prep.step_custom_filter <- function(x, training, info = NULL, ...) {

  # column names as character vector.
  col_names <- recipes::terms_select(x$terms, info = info)

  # set arguments for filter function call.
  if (is.null(x$options)) {
    filter_args <-  list(x = training[, col_names])
  } else {
    filter_args <- append(list(x = training[, col_names]), x$options)
  }

  # identify problematic variables.
  filter <- do.call(x$filter_function, filter_args)

  # return "trained" step.
  step_custom_filter_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    filter_function = x$filter_function,
    options = x$options,
    removals = filter,
    skip = x$skip
  )

}

# bake step.
#' @export
bake.step_custom_filter <- function(object, newdata, ...) {

  # remove problematic variables.
  if (length(object$removals) > 0) {
    newdata <- newdata[, !(colnames(newdata) %in% object$removals)]
  }

  # return data set after filtering.
  tibble::as_tibble(newdata)

}

# print step.
print.step_custom_filter <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Filter removed the following variables ")
        cat(recipes:::format_ch_vec(x$removals, width = width))
      } else
        cat("Filter removed no terms")
    } else {
      cat("Filter applied on ", sep = "")
      cat(recipes:::format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_custom_filter
#' @param x A `step_custom_filter` object.
tidy.step_custom_filter <- recipes:::tidy_filter
