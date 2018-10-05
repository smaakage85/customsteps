library(magrittr)
library(tidyselect)

# generér data
df <- tibble::tibble(a = c(1, -999, 3,NA,NA),
                     b = c(1,3, NA,NA,NA),
                     c = c(1,-999,3,4,5),
                     d = rep(1, 5),
                     e = c(-999, -999, -999, -999, NA),
                     f = rep(NA, 5))


rec <- recipes::recipe(df) %>%
  step_custom_filter(everything(),
                     filter_function = custom_filter,
                     options = list(threshold = 0.5, other_values = -999))


%>%
  recipes::prep(.)

baked <- bake(rec, df)

tidy(rec)

# #### step_missing_proportion
# missing_proportion(df, 0.5)
#
# rec <- recipes::recipe(df) %>%
#   step_missing_proportion(everything(), options = list(threshold = 0.5)) %>%
#   recipes::prep(.)
#
# bake(rec, df)
#
# rec
# tidy(rec)
# tidy(rec, 1)
#
# #### step_missing_other_proportion
# missing_other_proportion(df, 0.5, other_values = NULL)
# missing_other_proportion(df, 0.5, other_values = c(1,2))
#
# rec <- recipes::recipe(df) %>%
#   step_missing_other_proportion(everything(), options = list(threshold = 0.5, other_values = c(1,2))) %>%
#   recipes::prep(.)
#
# bake(rec, df)
#
# rec
# tidy(rec)
# tidy(rec, 1)
#
# #### step_missing_other_proportion
# fn <- function (x) {
#  dplyr::mutate(x, gg = a + b)
# }
#
# fn(df)
