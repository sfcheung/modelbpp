
#' @noRd
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

catwrap <- function(x,
                    width = 0.9 * getOption("width"),
                    indent = 0,
                    exdent = 0,
                    prefix = "",
                    simplify = TRUE,
                    initial = prefix,
                    sep = "\n",
                    fill = FALSE,
                    labels = NULL,
                    append = FALSE) {
    out <- strwrap(x,
                   width = width,
                   indent = indent,
                   exdent = exdent,
                   prefix = prefix,
                   simplify = simplify,
                   initial = initial)
    cat(out,
        sep = sep,
        fill = fill,
        labels = labels,
        append = append)
  }

#' @noRd
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

auto_tab <- function(x,
                     y,
                     between = ": ") {
    x_max_length <- max(nchar(x))
    y_max_length <- max(nchar(y))
    y2 <- pad_whitespace(y,
                         align = "right",
                         common_length = y_max_length)
    x2 <- pad_whitespace(x,
                         align = "left",
                         common_length = x_max_length)
    y2 <- unname(y2)
    x2 <- unname(x2)
    out <- paste0(x2, between, y2)
    out
  }

#' @noRd
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

pad_whitespace <- function(object,
                           align = c("left", "right"),
                           common_length = NA) {
    align <- match.arg(align)
    out <- sapply(object, function(yy) {
            tmp <- common_length - nchar(yy)
            if (tmp > 0) {
                tmp2 <- paste0(rep(" ", tmp),
                              collapse = "")
                tmp3 <- switch(align,
                          right = paste0(tmp2, yy),
                          left = paste0(yy, tmp2))
                return(tmp3)
              } else {
                return(yy)
              }
          })
    out
  }
