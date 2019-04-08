#' Custom Filter
#'
#' `step_custom_filter` creates a *specification* of a (higher order) recipe
#' step that will potentially remove variables using a custom filter function.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables that will evaluated by the filtering. See
#'  [recipes::selections()] for more details.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param filter_function A custom filter function, that will
#' diagnose problematic variables (see Details below).
#' @param options A list of options that will be provided to the
#'  filter function as arguments (see Details below).
#' @param removals A character string that contains the names of
#'  the columns that should be removed. These values are not determined
#'  until [recipes::prep.recipe()] is called.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be removed.
#' @keywords datagen
#' @concept preprocessing variable_filters
#' 
#' @importFrom recipes add_step rand_id ellipse_check
#'
#' @export
#'
#' @details This step diagnoses problematic variables according to
#'  a custom filter function. The filter function must meet the
#'  following requirements:
#' \enumerate{
#'   \item the function must at least take one argument `x`:
#'   input data with the selected variables.
#'   \item the function must return a vector with the names of
#'   the variables diagnosed as problematic.
#' }
#'
#' All additional arguments to the custom filter function must be provided
#' through the 'options' argument.
#'
#' @examples
#' library(magrittr)
#' library(tidyselect)
#' library(generics)
#' library(tibble)
#' library(purrr)
#' library(recipes)
#'
#' # generate data.
#' df <- tibble(a = c(1, -999, 3,NA,NA),
#'              b = c(1,3, NA,NA,NA),
#'              c = c(1,-999,3,4,5),
#'              d = rep(1, 5),
#'              e = c(-999, -999, -999, -999, NA),
#'              f = rep(NA, 5))
#'
#' # Create custom function to identify variables with a proportion of missing
#' # values above some threshold. The function treats
#' # values provided with the 'other_values' argument as missings.
#'
#' filter_missings <- function(x, threshold = 0.5, other_values = NULL) {
#'
#'   # identify problematic variables.
#'   if (is.null(other_values)) {
#'
#'     problematic_lgl <- map_lgl(x, ~ mean(is.na(.)) >= threshold)
#'
#'   } else {
#'
#'     problematic_lgl <- map_lgl(x, ~ mean(is.na(.) | 
#'     . %in% other_values) >= threshold)
#'
#'   }
#'
#'   # return names of problematic variables.
#'   names(x)[problematic_lgl]
#'
#' }
#'
#' # create recipe.
#' rec <- recipe(df) %>%
#'   step_custom_filter(everything(),
#'                      filter_function = filter_missings,
#'                      options = list(threshold = 0.5, other_values = -999))
#'
#' # prep recipe.
#' rec_prep <- prep(rec)
#'
#' # bake recipe.
#' rec_baked <- bake(rec_prep, df)
#'
#' # inspect output.
#' tidy(rec)
#' tidy(rec, number = 1)
#' tidy(rec_prep)
#' tidy(rec_prep, number = 1)
#' rec_baked
#'
#' @seealso [recipes::recipe()]
#'   [recipes::prep.recipe()] [recipes::bake.recipe()]
step_custom_filter <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           filter_function = NULL,
           options = NULL,
           removals = NULL,
           skip = FALSE,
           id = rand_id("custom_filter")) {
    
    # check inputs.
    if (is.null(filter_function)) {
      stop("'filter_function' must be specified.")
    }
    
    # inputs for 'bake.recipe()'.
    if (!is.function(filter_function)) {
      stop("'filter_function' must be a function.")
    }
    
    if (!is.null(options) && !inherits(options, "list")) {
      stop("'options' must belong to the 'list' class.")
    }
    
    if (!("x" %in% formalArgs(filter_function))) {
      stop("The filter function - 'filter_function' - must take an 'x'
           argument, which should be a data set with the selected variables
           from the input data.")
    }
    
    add_step(
      recipe,
      step_custom_filter_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        filter_function = filter_function,
        options = options,
        removals = removals,
        skip = skip,
        id = id
      )
    )
    }

# constructor function.
#' @importFrom recipes step
step_custom_filter_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           filter_function = NULL,
           options = NULL,
           removals = NULL,
           skip = FALSE,
           id = id) {
    step(
      subclass = "custom_filter",
      terms = terms,
      role = role,
      trained = trained,
      filter_function = filter_function,
      options = options,
      removals = removals,
      skip = skip,
      id = id
    )
  }

# prepare step (detect problematic variables on training data).
#' @export
#' @importFrom recipes prep terms_select
#' @importFrom dplyr setdiff
prep.step_custom_filter <- function(x, training, info = NULL, ...) {
  
  # column names as character vector.
  col_names <- terms_select(x$terms, info = info)
  
  # set arguments for call to the filter function.
  if (is.null(x$options)) {
    filter_args <-  list(x = training[, col_names])
  } else {
    filter_args <- append(list(x = training[, col_names]), x$options)
  }
  
  # identify problematic variables using the filter function.
  filter <- tryCatch({do.call(x$filter_function, filter_args)},
                     error = function(e) {
                       stop("Error when invoking the filter function. ",
                            "See details below: \n",
                            e)
                     })
  
  # check output from filter function.
  if (!is.null(filter) && !is.character(filter)) {
    stop("Output from filter function is not a character.")
  }
  
  if (length(setdiff(filter, colnames(training[, col_names]))) > 0) {
    stop("Output from the filter function must be one or more names of ",
         "the selected variables.")
  }
  
  # return "trained" step.
  step_custom_filter_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    filter_function = x$filter_function,
    options = x$options,
    removals = filter,
    skip = x$skip,
    id = x$id
  )
  
}

# bake step (/apply step on new data).
#' @export
#' @importFrom tibble as_tibble
bake.step_custom_filter <- function(object, new_data, ...) {
  
  # remove problematic variables.
  if (length(object$removals) > 0) {
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  }
  
  # return data set after filtering.
  as_tibble(new_data)
  
}

# print step.
#' @export
print.step_custom_filter <-
  function(x, width = max(20, options()$width - 38), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Filter removed the following variables ")
        cat(format_ch_vec(x$removals, width = width))
      } else
        cat("Filter removed no terms")
    } else {
      cat("Filter applied on ", sep = "")
      cat(format_selectors(x$terms, wdth = width))
    }
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }

#' @rdname step_custom_filter
#' @param x A `step_custom_filter` object.
#' @export
#' @importFrom recipes sel2char
#' @importFrom generics tidy
#' @importFrom tibble tibble frame_data
tidy.step_custom_filter <- function(x, ...) {
  
  if (!x$trained) {
    res <- tibble(terms = sel2char(x$terms))
  } else {
    if (is.null(x$removals)) {
      res <- frame_data(~removals)
    } else {
      res <- tibble(removals = x$removals)
    }
  }
  
  res$id <- x$id
  res
  
}