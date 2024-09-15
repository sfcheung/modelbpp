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
#' The function [semTools::net()] cannot
#' be used here because the models are
#' not fitted by function calls, but by
#' setting slots directly.
#'
#' @param object A `model_set`-class
#' object.
#'
#' @references
#' Bentler, P. M., & Satorra, A. (2010).
#' Testing model nesting and equivalence.
#' *Psychological Methods, 15*(2),
#' 111--123. \doi{10.1037/a0019625}
#' Asparouhov, T., & Muthén, B. (2019).
#' Nesting and Equivalence Testing for
#' Structural Equation Models.
#' *Structural Equation Modeling: A Multidisciplinary Journal, 26*(2),
#' 302--309. \doi{10.1080/10705511.2018.1513795}
#'
#' @return
#' A numeric matrix of zeros and ones,
#' to indicate the relations between
#' models.
#'
#' @noRd

models_network2 <- function(object,
                            one_df_only = TRUE,
                            progress = FALSE,
                            mark_equivalent = FALSE) {
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
    converged <- sapply(models,
                        lavaan::lavInspect,
                        what = "converged")
    if (!all(converged)) {
        stop("One or more models did not converge.")
      }
    if (inherits(object, "model_set")) {
        if (!is.null(object$model_df)) {
            model_dfs <- object$model_df
          } else {
            model_dfs <- sapply(models, function(x) {
                  unname(lavaan::fitMeasures(x,
                                             fit.measures = "df"))
                })
          }
      } else {
        model_dfs <- sapply(models, function(x) {
              unname(lavaan::fitMeasures(x,
                                         fit.measures = "df"))
            })
      }
    p <- length(models)
    i <- which(names(models) == "original")
    net_out <- matrix(0, p, p)
    colnames(net_out) <- rownames(net_out) <- names(models)
    if (p == 1) {
        return(net_out)
      }
    if (progress) {
        pstar <- p * (p - 1) / 2
        cat("\nCheck for nested models (",
             pstar,
             " pair[s] of models to check):\n",
             sep = "")
        pb <- utils::txtProgressBar(min = 0,
                                    max = pstar,
                                    width = 50,
                                    char = "+",
                                    style = 3)
      }
    k <- 0
    for (i in seq_len(p - 1)) {
        for (j in seq(from = i + 1,
                      to = p)) {
            k <- k + 1
            if (progress) {
                utils::setTxtProgressBar(pb,
                                         value = k)
              }
            df_i <- model_dfs[i]
            df_j <- model_dfs[j]
            net_chk <- x_net_y(x = models[[i]],
                               y = models[[j]],
                               x_df = df_i,
                               y_df = df_j)
            if (is.na(net_chk)) next
            if (net_chk == "x_within_y") {
                net_out[i, j] <- df_i - df_j
              } else if (net_chk == "y_within_x") {
                net_out[j, i] <- df_j - df_i
              }
            # TODO: Decide what to do with equivalent models
            if (net_chk == "equivalent" && mark_equivalent) {
                net_out[i, j] <- net_out[j, i] <- NA
              }
          }
      }
    if (progress) {
        close(pb)
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
                    x_df = NULL,
                    y_df = NULL,
                    crit = 1e-4,
                    check_x_y = TRUE) {
    # Based on semTools:::x.within.y().
    if (check_x_y) {
        chk <- check_x_net_y(x, y,
                             ignore_fixed_x = FALSE)
      }
    if (is.null(x_df)) {
        x_df <- lavaan::fitMeasures(x, fit.measures = "df")
      }
    if (is.null(y_df)) {
        y_df <- lavaan::fitMeasures(y, fit.measures = "df")
      }
    if ((x_df == y_df) &&
        (abs(lavaan::fitMeasures(x, fit.measures = "chisq") -
             lavaan::fitMeasures(y, fit.measures = "chisq")) < crit)) {
        return("equivalent")
      }
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

    f1_fixed <- (lavaan::lavInspect(f1, "options")$fixed.x &&
                 length(lavaan::lavNames(f1, "ov.x")) > 0)
    if (f1_fixed) {
        stop("The net-method does not support models with fixed.x = TRUE. ",
             "Please fit the models with fixed.x = FALSE.")
        # f1 <- lavaan_full_update(f1,
        #                          new_options = list(se = "none",
        #                                             baseline = FALSE,
        #                                             test = "standard",
        #                                             fixed.x = FALSE))
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
                           NACOV = f1_NACOV,
                           warn = FALSE)
    if (!lavaan::lavInspect(f2_1, "converged")) {
        # If estimation failed to converged,
        # then do not assume a nested relation.
        # Should not be a major problem because this only
        # affect the graph.
        return(NA)
      }
    f2_1_chisq <- unname(lavaan::fitMeasures(f2_1, fit.measures = "chisq"))
    chisq_eq <- f2_1_chisq < crit
    if (chisq_eq) {
        # Equivalence should not be determined this way
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
    if (any(lavaan::lavInspect(x, "nclusters") != 1) ||
        any(lavaan::lavInspect(y, "nclusters") != 1)) {
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
            stop("Does not support models with fixed.x = TRUE. ",
                 "Please refit models with fixed.x = FALSE.")
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

lavaan_fast_update <- function(x,
                               new_options = list()) {
    stop("This function should not be called. It is a work-in-progress.")
    # slot_opt <- x@Options
    # slot_pat <- x@ParTable
    # slot_mod <- x@Model
    # slot_smp <- x@SampleStats
    # slot_dat <- x@Data

    # slot_opt2 <- slot_opt
    # slot_opt2$se <- "none"
    # slot_opt2$fixed.x <- FALSE
    # slot_opt2$baseline <- FALSE
    # slot_opt2$test <- "none"
    # slot_opt2 <- utils::modifyList(slot_opt2,
    #                                new_options,
    #                                keep.null = TRUE)

    # fit2 <- lavaan::lavaan(slotOptions = slot_opt2,
    #                        slotParTable = slot_pat,
    #                        slotModel = slot_mod,
    #                        slotSampleStats = slot_smp,
    #                        slotData = slot_dat)
    # fit2
  }

#' @title Internal Full Update
#'
#' @noRd

lavaan_full_update <- function(x,
                               new_options = list()) {
    stop("This function should not be called. It is a work-in-progress.")
#     x_model <- lavaan::parameterTable(x)
#     slot_opt <- x@Options
#     slot_opt2 <- utils::modifyList(slot_opt,
#                                   new_options,
#                                   keep.null = TRUE)

#     raw_data <- tryCatch(lavaan::lavInspect(x, "data"),
#                         error = function(e) e)
#     if (inherits(raw_data, "error")) {
#         raw_data <- NULL
#         has_data <- FALSE
#       } else {
#         colnames(raw_data) <- lavaan::lavNames(x)
#         rownames(raw_data) <- lavaan::lavInspect(x, "case.idx")
#         has_data <- TRUE
#       }

#     if (has_data) {
#         # Placeholder
#       } else {
#         x_nobs <- lavaan::lavInspect(x, "nobs")
#         x_sp <- lavaan::lavInspect(x, "sampstat")
#         ng <- lavaan::lavInspect(x, "ngroups")
#         if (ng > 1) {
#             x_cov <- lapply(x_sp, function(x) x$cov)
#           } else {
#             x_cov <- x_sp$cov
#           }
#         if (ng > 1) {
#             x_mean <- lapply(x_sp, function(x) x$mean)
#           } else {
#             x_mean <- x_sp$mean
#           }
#         x_thresholds <- lavaan::lavInspect(x, "thresholds")
#         if (is.list(x_thresholds)) {
#             if (all(sapply(x_thresholds, length) == 0)) {
#                 x_thresholds <- NULL
#               }
#           } else {
#             if (length(x_thresholds) == 0) {
#                 x_thresholds <- NULL
#               }
#           }
#         x_estimator <- lavaan::lavInspect(x, "options")$estimator
#         if (x_estimator == "DWLS") {
#             x_WLS.V <- lavaan::lavInspect(x, "WLS.V")
#             x_NACOV <- lavaan::lavInspect(x, "gamma")
#           } else {
#             x_WLS.V <- NULL
#             x_NACOV <- NULL
#           }
#       }

#     if (has_data) {
#         out <- lavaan::lavaan(model = x_model,
#                               slotOptions = slot_opt2,
#                               data = raw_data)
#       } else {
#         out <- lavaan::lavaan(model = x,
#                               slotOptions = slot_opt2,
#                               sample.cov = x_cov,
#                               sample.mean = x_mean,
#                               sample.nobs = x_nobs,
#                               semple.th = x_thresholds,
#                               WLS.V = x_WLS.V,
#                               NACOV = x_NACOV)
#       }
#     return(out)
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

#' @title Identify Clusters of Equivalent Models
#'
#' @noRd

equivalent_clusters <- function(fits,
                                name_cluster = TRUE,
                                progress = FALSE) {
    net_eq <- models_network2(fits,
                              mark_equivalent = TRUE,
                              progress = progress,
                              one_df_only = TRUE)
    if (!any(is.na(net_eq))) {
        return(NULL)
      }
    net_eq[!is.na(net_eq)] <- 0
    net_eq[is.na(net_eq)] <- 1
    p_eq <- igraph::graph_from_adjacency_matrix(net_eq,
                                                mode = "undirected")
    p_eq_group <- igraph::max_cliques(p_eq)
    i <- sapply(p_eq_group, length)
    p_eq_group <- p_eq_group[i > 1]
    p_eq_group_names <- lapply(p_eq_group, function(x) x$name)
    if (name_cluster) {
        tmp <- sapply(p_eq_group_names, function(x) x[1])
        names(p_eq_group_names) <- tmp
      }
    p_eq_group_names
  }