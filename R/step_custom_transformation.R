#' Custom Transformation
#'
#' `step_custom_transformation` creates a *specification* of a recipe step that
#' will make a user-defined transformation of the input data from custom `prep`
#' and `bake` helper functions.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the function assumes that the new columns
#'   will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   `bake.recipe()`? While all operations are baked when `prep.recipe()` is
#'   run, some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using `skip
#'   = TRUE` as it may affect the computations for subsequent operations.
#' @param prep_fct A function. This is a helper function for the `prep` method.
#'   It will be invoked, when the recipe is 'prepped' (/trained) with
#'   `prep.recipe()`. The function MUST satisfy the following conditions: (1)
#'   the function must take an argument `x`: input data with only the selected
#'   variables, (2) the function MUST return the relevant statistics that should
#'   be learned on the train set and used for preparation of new new data sets.
#'   This output can be of any appropriate type and shape. Leave `prep_fct` as
#'   NULL, if the preparation of new data sets does not depend on
#'   statistics/computations learned on the train set.
#' @param prep_fct_args A list with (any) additional arguments for the prep
#'   helper function call EXCEPT for the `x` argument. Leave as NULL, if no
#'   `prep_fct` is given.
#' @param prep_output Output from prep helper (`prep_fct`) function call. The
#'   results are not computed until `prep.recipe()` is called.
#' @param bake_fct A function. This is a helper function for the 'bake' method.
#'   It will be invoked, when the recipe is 'baked' (/transforms a new data set)
#'   with `bake.recipe()`. The function MUST satisfy the following conditions:
#'   (1) the function must take an argument `x`: data set with the selected
#'   variables from the new data set, (2) if the preparation of new data sets
#'   depends on statistics learned on the train set, the function must take the
#'   argument `prep_output`: the output from the prep helper fct (`prep_fct`),
#'   (3) the output from from the function should be the transformed variables.
#'   The output must be of a type and shape, that allows it to be binded column
#'   wise to the new data set after converting it to a `tibble`.
#' @param bake_fct_args A list with (any) arguments for the bake helper function
#'   call EXCEPT for the `x` and `prep_output` arguments.
#' @param bake_how A character. How should the transformed variables be appended
#'   to the new data set? Choose from options (1) `bind_cols`: simply bind the
#'   transformed variables to the new data set or (2) `replace`: replace the
#'   selected variables (`selected vars`) from the new data set with the
#'   transformed variables.
#' @param selected_vars A character string that contains the names of the
#'   selected variables. These values are not determined until prep.recipe() is
#'   called.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a `tibble` with
#'   columns `terms` (the selectors or variables selected).
#'
#' @keywords datagen
#' @concept preprocessing
#'
#' @importFrom recipes bake
#' @export
#'
#' @examples
#' library(magrittr)
#' library(tidyselect)
#'
#' # generate data.
#' df <- tibble::tibble(a = rnorm(100),
#'                      b = rnorm(100),
#'                      c = rnorm(100))
#'
#' # custom step: prep and bake functions ------------------------------------
#'
#'
#' # define prep helper function, that computes means and standard deviations
#' # for all variables in a data set.
#' compute_means_sd <- function(x, na.rm = FALSE, trim = 0) {
#'
#'   purrr::map(x, ~ list(mean = mean(.x, na.rm = na.rm, trim = trim),
#'                        sd = sd(.x)))
#'
#' }
#'
#' # define prep function, that subtracts k means from the variable, and
#' # divides by the standard deviation.
#' center_scale <- function(x, prep_output, k) {
#'
#'   newdata <- dplyr::select(x, names(prep_output))
#'
#'   purrr::map2(.x = newdata,
#'               .y = prep_output,
#'               ~ (.x - k * .y$mean) / .y$sd)
#' }
#'
#' # create recipe.
#' rec <- recipes::recipe(df) %>%
#'   step_custom_transformation(b, c,
#'                              prep_fct = compute_means_sd,
#'                              prep_fct_args = list(na.rm = TRUE, trim = 0.05),
#'                              bake_fct = center_scale,
#'                              bake_fct_args = list(k = 2),
#'                              bake_how = "bind_cols")
#'
#' # prep recipe.
#' rec_prep <- recipes::prep(rec)
#'
#' # bake recipe.
#' rec_baked <- recipes::bake(rec_prep, df)
#' rec_baked
#'
#' # # inspect output.
#' # rec
#' # rec_baked
#' # broom::tidy(rec)
#' # broom::tidy(rec, 1)
#' # broom::tidy(rec_prep)
#' # broom::tidy(rec_prep, 1)
#'
#' # custom step: bake only --------------------------------------------------
#'
#'
#' # create custom bake function.
#' simple_calculation <- function(x) {
#'
#'   x %>%
#'     dplyr::transmute(d = a + b - c)
#'
#' }
#'
#' # create recipe.
#' rec <- recipes::recipe(df) %>%
#'   step_custom_transformation(everything(),
#'                              bake_fct = simple_calculation,
#'                              bake_how = "bind_cols")
#'
#' # prep recipe.
#' rec_prep <- recipes::prep(rec)
#'
#' # bake recipe.
#' rec_baked <- recipes::bake(rec_prep, df)
#'
#' # # inspect output.
#' # rec
#' # rec_baked
#' # broom::tidy(rec)
#' # broom::tidy(rec, 1)
#' # broom::tidy(rec_prep)
#' # broom::tidy(rec_prep, 1)
step_custom_transformation <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           prep_fct = NULL,
           prep_fct_args = NULL,
           prep_output = NULL,
           bake_fct = NULL,
           bake_fct_args = NULL,
           bake_how = "bind_cols",
           selected_vars = NULL,
           skip = FALSE) {

    ####  check inputs.
    if (is.null(bake_fct)) {
      stop("No bake helper function ('bake_fct') has been set.")
    }

    # inputs for 'prep.recipe()'.
    if (!is.null(prep_fct) && !is.function(prep_fct)) {
      stop("'prep_fct' must be a function.")
    }

    if (!is.null(prep_fct_args) && !is.list(prep_fct_args)) {
      stop("'prep_fct_args' must be a list.")
    }

    if (!is.null(prep_fct) && !("x" %in% methods::formalArgs(prep_fct))) {
      stop("The prep helper function - 'prep_fct' - must have an 'x'
           argument, that should correspond to the input data.")
    }

    # inputs for 'bake.recipe()'.
    if (!is.function(bake_fct)) {
      stop("'bake_fct' must be a function.")
    }

    if (!is.null(bake_fct_args) && !is.list(bake_fct_args)) {
      stop("'bake_fct_args' must be a list.")
    }

    if (!isTRUE(bake_how %in% c("bind_cols", "replace"))) {
      stop("Set 'bake_how' to either 'bind_cols' or 'replace'.")
    }

    if (!("x" %in% methods::formalArgs(bake_fct))) {
      stop("The bake helper function - 'bake_fct' - must have an 'x'
           argument, that should correspond to the new data set
           on which the recipe step will be applied.")
    }

    # add step.
    recipes::add_step(
      recipe,
      step_custom_transformation_new(
        terms = recipes::ellipse_check(...),
        trained = trained,
        role = role,
        prep_fct = prep_fct,
        prep_fct_args = prep_fct_args,
        prep_output = prep_output,
        bake_fct = bake_fct,
        bake_fct_args = bake_fct_args,
        bake_how = bake_how,
        selected_vars = selected_vars,
        skip = skip
      )
    )
    }

# constructor function.
step_custom_transformation_new <-
  function(terms = NULL,
           role = "predictor",
           trained = FALSE,
           prep_fct = NULL,
           prep_fct_args = NULL,
           prep_output = prep_output,
           bake_fct = NULL,
           bake_fct_args = NULL,
           bake_how = "bind_cols",
           selected_vars = NULL,
           skip = FALSE) {
    recipes::step(
      subclass = "custom_transformation",
      terms = terms,
      role = role,
      trained = trained,
      prep_fct = prep_fct,
      prep_fct_args = prep_fct_args,
      prep_output = prep_output,
      bake_fct = bake_fct,
      bake_fct_args = bake_fct_args,
      bake_how = bake_how,
      selected_vars = selected_vars,
      skip = skip
    )
  }

# prepare step.
#' @export
prep.step_custom_transformation <- function(x, training, info = NULL, ...) {

  # check inputs.
  if (is.null(x$prep_fct) && !is.null(x$prep_fct_args) && length(list(...)) == 0) {
    stop("Arguments for the prep helper function have been provided, but
         no prep helper function has been set.")
  }

  # selected vars as character vector.
  selected_vars <- recipes::terms_select(x$terms, info = info)

  # if no prep helper function has been specified, do nothing. Execute the
  # prep helper function otherwise.
  if (!is.null(x$prep_fct)) {

    #### prepare all arguments before calling the prep helper function.

    # add mandatory argument 'x'.
    args <- list(x = training[, selected_vars])

    # add additional arguments (if any).
    if (!is.null(x$prep_fct_args)) {
      args <- append(args, x$prep_fct_args)
    }

    # add arbitrary arguments (if any).
    if (length(list(...)) > 0) {
      args <- append(args, list(...))
    }

    # compute intermediate output from prep helper function.
    prep_output <- do.call(purrr::safely(x$prep_fct), args)

    # handle errors (if any).
    if (!is.null(prep_output$error)) {
      cat("An error occured in the call to the prep helper function",
          "('prep_fct'). See details below: ")
      stop(prep_output$error)
    }

    # otherwise continue with output from the function call.
    prep_output <- prep_output$result

  } else {

    # set output to NULL otherwise.
    prep_output <- NULL

  }

  step_custom_transformation_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    prep_fct = x$prep_fct,
    prep_fct_args = x$prep_fct_args,
    prep_output = prep_output,
    bake_fct = x$bake_fct,
    bake_fct_args = x$bake_fct_args,
    bake_how = x$bake_how,
    selected_vars = selected_vars,
    skip = x$skip
  )

  }

# bake step.
#' @export
bake.step_custom_transformation <- function(object, newdata, info = NULL, ...) {

  #### prepare arguments before calling the bake helper function.

  # add mandatory argument for 'x' - set to newdata.
  args <- list(x = newdata)

  # add intermediate output from the prep helper function.
  if (!is.null(object$prep_output)) {
    args <- append(args, list(prep_output = object$prep_output))
  }

  # add additional arguments (if any).
  if (!is.null(object$bake_fct_args)) {
    args <- append(args, object$bake_fct_args)
  }

  # add arbitrary arguments (if any).
  if (length(list(...)) > 0) {
    args <- append(args, list(...))
  }

  # invoke the bake helper function.
  bake_fct_output <- do.call(object$bake_fct, args) %>%
    # convert output to tibble.
    tibble::as_tibble(.)

  # append to input data the output from the bake helper function.
  output <- switch(object$bake_how,

                   # append output to input by binding columns.
                   "bind_cols" = {

                     # check_inputs.
                     if (nrow(bake_fct_output) != nrow(newdata)) {
                       stop("There was a mismatch between the number of rows in ",
                            "the output from the bake helper function (",
                            nrow(bake_fct_output), ") and the number of rows of ",
                            "the input data (", nrow(newdata), ").")
                     }

                     # bind output columns to input data.frame.
                     newdata %>%
                       tibble::as.tibble(.) %>%
                       dplyr::bind_cols(., bake_fct_output)
                   },

                   # replace selected variables with output.
                   "replace" = {

                     # check_inputs.
                     if (!all(names(newdata) %in% object$selected_vars) &&
                         nrow(bake_fct_output) != nrow(newdata)) {
                       stop("There was a mismatch between the ",
                            "number of rows in the output from ",
                            "the bake helper function (", nrow(bake_fct_output),
                            ") and the number of rows of the input data ",
                            "(", nrow(newdata), ").")
                     }

                     newdata %>%
                       tibble::as.tibble(.) %>%
                       # drop selected vars.
                       dplyr::select(-c(object$selected_vars)) %>%
                       # bind output columns to input data.frame.
                       dplyr::bind_cols(., bake_fct_output)

                   })

  # return output.
  output

}

print.step_custom_transformation <-
  function(x, width = max(20, options()$width - 30), ...) {

    cat("The following variables are used for computing" ,
        " transformations", ifelse(x$bake_how == "replace",
                                   "\n and are removed from the data set afterwards:\n ", ":\n "), sep = "")
    cat(recipes:::format_selectors(x$terms, wdth = width))
    invisible(x)

  }

#' @rdname step_custom_transformation
#' @param x A `step_custom_transformation` object.
#' @export
tidy.step_custom_transformation <- function(x, ...) {

  res <- tibble::tibble(terms = recipes::sel2char(x$terms))

}
