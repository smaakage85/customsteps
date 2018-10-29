## ancillary step functions

## 9 is to keep space for "[trained]"
format_ch_vec <-
  function(x,
           sep = ", ",
           width = options()$width - 9) {
    widths <- nchar(x)
    sep_wd <- nchar(sep)
    adj_wd <- widths + sep_wd
    if (sum(adj_wd) >= width) {
      keepers <- max(which(cumsum(adj_wd) < width)) - 1
      if (length(keepers) == 0 || keepers < 1) {
        x <- paste(length(x), "items")
      } else {
        x <- c(x[1:keepers], "...")
      }
    }
    paste0(x, collapse = sep)
  }

format_selectors <- function(x, wdth = options()$width - 9, ...) {
  ## convert to character without the leading ~
  x_items <- lapply(x, function(x)
    as.character(x[-1]))
  x_items <- unlist(x_items)
  format_ch_vec(x_items, width = wdth, sep = ", ")
}