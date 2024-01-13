#' @title Convert a 'model_set'-Object
#' to an Adjacency Matrix
#'
#' @description Convert a
#' 'model_set'-Object to an adjacency
#' matrix for 'igraph'.
#'
#' @param object A `model_set`-class
#' object or a `partables`-class
#' object.
#'
#' @return
#' A numeric matrix of zeros and ones,
#' to indicate the relations between
#' models.
#'
#' @noRd

models_network <- function(object) {
    if (inherits(object, "model_set")) {
        models <- object$models
      } else {
        if (!inherits(object, "partables")) {
            stop("The object must be either a",
                 "model_set object or a",
                 "partables object.")
          }
      }
    m_added <- added(models)
    m_dropped <- dropped(models)
    chk <- mapply(c, m_added, m_dropped)

    p <- length(models)
    i <- which(names(models) == "original")
    net_out <- matrix(0, p, p)
    colnames(net_out) <- rownames(net_out) <- names(models)

    tmp <- sapply(m_added, length) == 1
    net_out[i, tmp] <- 1

    tmp <- sapply(m_dropped, length) == 1
    net_out[tmp, i] <- 1

    net_out <- mark_added(net_out,
                          m_added = m_added)
    net_out <- mark_dropped(net_out,
                            m_dropped = m_dropped)
    net_out
  }

#' @title Convert a 'model_set'-Object
#' to an Adjacency Matrix Using 'semTools::net()' Method
#'
#' @description Convert a
#' 'model_set'-Object to an adjacency
#' matrix for 'igraph'.
#'
#' @details
#' Work-in-progress. Does not work for now.
#'
#' @param object A `model_set`-class
#' object.
#'
#' @return
#' A numeric matrix of zeros and ones,
#' to indicate the relations between
#' models.
#'
#' @noRd

models_network2 <- function(object,
                            one_df_only = TRUE) {
    if (inherits(object, "model_set")) {
        models <- object$fit
      } else {
        models <- object
      }
    if (!all(sapply(models, inherits, what = "lavaan"))) {
        stop("The object must be either a",
              "model_set object or a",
              "list of lavaan objects")
      }
    p <- length(models)
    i <- which(names(models) == "original")
    net_out <- matrix(0, p, p)
    colnames(net_out) <- rownames(net_out) <- names(models)
    for (i in seq_len(p)) {
        for (j in seq_len(p)) {
            df_i <- unname(lavaan::fitMeasures(models[[i]],
                                        fit.measures = "df"))
            df_j <- unname(lavaan::fitMeasures(models[[j]],
                                        fit.measures = "df"))
            net_chk <- x_net_y(models[[i]],
                               models[[j]])
            if (net_chk == "x_within_y") {
                net_out[i, j] <- df_i - df_j
              }
          }
      }
    if (one_df_only) {
        net_out[net_out > 1] <- 0
      }
    net_out
  }

#' @title Net Without Call
#'
#' @description Based on semTools:::x.within.y().
#'
#' @noRd

x_net_y <- function(x,
                    y,
                    crit = 1e-4,
                    check_x_y = TRUE) {
    # Based on semTools:::x.within.y().
    if (check_x_y) {
        chk <- check_x_net_y(x, y,
                             ignore_fixed_x = TRUE)
      }
    x_df <- lavaan::fitMeasures(x, fit.measures = "df")
    y_df <- lavaan::fitMeasures(y, fit.measures = "df")
    # Reorder the models f1 >= f2 on df
    if (x_df < y_df) {
        f1 <- y
        f2 <- x
        out <- "y_within_x"
      } else {
        f1 <- x
        f2 <- y
        out <- "x_within_y"
      }

    # Refit f1 with fixed.x = FALSE if necessary

    f1_fixed <- (lavaan::lavInspect(f1, "options")$fixed.x &&
                 length(lavaan::lavNames(f1, "ov.x")) > 0)
    if (f1_fixed) {
        f1 <- lavaan_fast_update(f1)
      }

    slot_opt <- f2@Options
    slot_pat <- f2@ParTable
    slot_mod <- f2@Model

    slot_opt2 <- slot_opt
    slot_opt2$se <- "none"
    slot_opt2$fixed.x <- FALSE
    slot_opt2$baseline <- FALSE
    slot_opt2$test <- "standard"

    f1_nobs <- lavaan::lavInspect(f1, "nobs")

    implied_sigma <- lavaan::lavInspect(f1, "cov.ov")
    implied_mean <- lavaan::lavInspect(f1, "mean.ov")
    if (is.list(implied_mean)) {
        if (all(sapply(implied_mean, length) == 0)) {
            implied_mean <- NULL
          }
      } else {
        if (length(implied_mean) == 0) {
            implied_mean <- NULL
          }
      }
    implied_threshold <- lavaan::lavInspect(f1, "thresholds")
    if (is.list(implied_threshold)) {
        if (all(sapply(implied_threshold, length) == 0)) {
            implied_threshold <- NULL
          }
      } else {
        if (length(implied_threshold) == 0) {
            implied_threshold <- NULL
          }
      }

    f1_estimator <- lavaan::lavInspect(f1, "options")$estimator
    if (f1_estimator == "DWLS") {
        f1_WLS.V <- lavaan::lavInspect(f1, "WLS.V")
        f1_NACOV <- lavaan::lavInspect(f1, "gamma")
      } else {
        f1_WLS.V <- NULL
        f1_NACOV <- NULL
      }

    f2_1 <- lavaan::lavaan(slotOptions = slot_opt2,
                           slotParTable = slot_pat,
                           slotModel = slot_mod,
                           data = NULL,
                           sample.cov = implied_sigma,
                           sample.mean = implied_mean,
                           sample.nobs = f1_nobs,
                           sample.th = implied_threshold,
                           WLS.V = f1_WLS.V,
                           NACOV = f1_NACOV)

    if (!lavaan::lavInspect(f2_1, "converged")) {
        return(NA)
      }
    f2_1_chisq <- unname(lavaan::fitMeasures(f2_1, fit.measures = "chisq"))
    chisq_eq <- f2_1_chisq < crit
    if (chisq_eq) {
        if (x_df == y_df) {
            out <- "equivalent"
          }
      } else {
        out <- "not_nested"
      }
    return(out)
  }

#' @noRd

check_x_net_y <- function(x,
                          y,
                          ignore_fixed_x = FALSE) {
    # Based on semTools::x.within.y()
    # lavaan objects?
    if (!inherits(x, what = "lavaan") ||
        !inherits(y, what = "lavaan")) {
        stop("One or both objects are not lavaan objects.")
      }
    # Same variables?
    vnamesx <- lavaan::lavNames(x, type = "ov")
    vnamesy <- lavaan::lavNames(y, type = "ov")
    if (!setequal(vnamesx, vnamesy)) {
        stop("The two models do not have the same observed variables.")
      }
    # Clustered?
    if ((lavaan::lavInspect(x, "nclusters") != 1) ||
        (lavaan::lavInspect(y, "nclusters") != 1)) {
        stop("Models with clustered data not supported.")
      }
    # Check sample sizes
    if (!identical(lavaan::lavInspect(x, "nobs"),
                   lavaan::lavInspect(y, "nobs"))) {
        stop("Sample sizes not identical.")
      }
    # fixed.x?
    if (!ignore_fixed_x) {
        x_fixed <- (lavaan::lavInspect(x, "options")$fixed.x &&
                    length(lavaan::lavNames(x, "ov.x")) > 0)
        y_fixed <- (lavaan::lavInspect(y, "options")$fixed.x &&
                    length(lavaan::lavNames(y, "ov.x")) > 0)
        if (x_fixed || y_fixed) {
            stop("Does not support models with 'fixed.x' = TRUE.")
          }
      }
    # Meanstructure
    if (!identical(lavaan::lavInspect(x, "meanstructure"),
                   lavaan::lavInspect(y, "meanstructure"))) {
        stop("One model has a mean structure but the other does not.")
      }
    # Same dataset?
    x_sp <- lavaan::lavTech(x, "sampstat",
                            drop.list.single.group = FALSE,
                            add.labels = TRUE,
                            list.by.group = TRUE)
    y_sp <- lavaan::lavTech(y, "sampstat",
                            drop.list.single.group = FALSE,
                            add.labels = TRUE,
                            list.by.group = TRUE)
    x_g <- length(x_sp)
    y_g <- length(y_sp)
    x_names <- colnames(x_sp[[1]]$cov)
    for (j in seq_len(x_g)) {
        if (!identical(x_sp[[1]]$cov,
                       y_sp[[1]]$cov[x_names, x_names])) {
            stop("The sample statistics are not identical.")
          }
        # Identical if both mean vectors are NULL
        if (!identical(x_sp[[1]]$mean,
                       y_sp[[1]]$mean[x_names])) {
            stop("The sample statistics are not identical.")
          }
      }
    return(TRUE)
  }

#' @title Internal Fast Update
#'
#' @noRd

lavaan_fast_update <- function(x) {
    slot_opt <- x@Options
    slot_pat <- x@ParTable
    slot_mod <- x@Model
    slot_smp <- x@SampleStats
    slot_dat <- x@Data

    slot_opt2 <- slot_opt
    slot_opt2$se <- "none"
    slot_opt2$fixed.x <- FALSE
    slot_opt2$baseline <- FALSE
    slot_opt2$test <- "none"

    fit2 <- lavaan::lavaan(slotOptions = slot_opt2,
                           slotParTable = slot_pat,
                           slotModel = slot_mod,
                           slotSampleStats = slot_smp,
                           slotData = slot_dat)
    fit2
  }

#' @title Mark Models Formed by Adding Free Parameters
#'
#' @noRd

mark_added <- function(net_out,
                       m_added) {
    for (i in seq_len(length(m_added))) {
        for (j in seq_len(length(m_added))) {
            x <- m_added[[i]]
            y <- m_added[[j]]
            if (identical(x, y)) next
            if (is.null(x)) next
            if (is.null(y)) next
            tmp <- union(x, y)
            tmp1 <- setequal(tmp, x)
            tmp2 <- setequal(tmp, y)
            if (tmp1 && !tmp2) {
                z <- length(setdiff(x, y))
                if (z == 1) {
                    net_out[j, i] <- z
                  }
              }
          }
      }
    net_out
  }

#' @title Mark Models Formed by Dropping Free Parameters
#'
#' @noRd

mark_dropped <- function(net_out,
                       m_dropped) {
    for (i in seq_len(length(m_dropped))) {
        for (j in seq_len(length(m_dropped))) {
            x <- m_dropped[[i]]
            y <- m_dropped[[j]]
            if (identical(x, y)) next
            if (is.null(x)) next
            if (is.null(y)) next
            tmp <- union(x, y)
            tmp1 <- setequal(tmp, x)
            tmp2 <- setequal(tmp, y)
            if (tmp1 && !tmp2) {
                z <- length(setdiff(x, y))
                if (z == 1) {
                    net_out[i, j] <- z
                  }
              }
          }
      }
    net_out
  }

#' @title Add Weights to an Adjacency Matrix
#'
#' @noRd

add_weight <- function(net_out,
                       bpp) {
    for (i in seq_len(nrow(net_out))) {
        for (j in seq_len(ncol(net_out))) {
            if (net_out[i, j] != 0) {
                net_out[i, j] <- bpp[i] - bpp[j]
              }
          }
      }
    net_out
  }

#' @title Normalize Node Size
#'
#' @noRd

normalize_size <- function(bpp,
                           min_size = 1,
                           max_size = 30) {
    bpp_min <- min(bpp)
    bpp_max <- max(bpp)
    bpp_range <- bpp_max - bpp_min
    tmp_size <- max_size * (bpp - bpp_min)/bpp_range +
                min_size
    tmp_size
  }

#' @title Labels for Vertices
#'
#' @noRd

v_labels <- function(x) {
    out0 <- gsub(": ", ":\n", x, fixed = TRUE)
    out0 <- gsub(";", "\n", out0, fixed = TRUE)
    out0
  }