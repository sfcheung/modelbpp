#' @title Fit a List of Models
#'
#' @description Fit a list of models
#' to a dataset.
#'
#' @details It receives a list of
#' models, defined by `lavaan` parameter
#' tables (usually generated by
#' [model_set()], [get_add()] or
#' [get_drop()]),
#' and fit them to a dataset stored
#' in a `lavaan`-class object.
#'
#' This function is called by
#' [model_set()] and usually users do
#' not need to call it. It is exported
#' for advanced users.
#'
#' @param model_list A list of parameter
#' tables to be used by
#' [lavaan::lavaan()] or [update()].
#' Usually generated by [get_add()] or
#' [get_drop()].
#'
#' @param sem_out The output from an
#' structural equation modeling
#' function. It currently supports
#' [lavaan::lavaan-class] objects
#' only. Usually
#' the one used in [model_set()],
#' [get_add()] or
#' [get_drop()] to generate the list of
#' models.
#'
#' @param original String. If provided,
#' it should be a name of a model
#' in `model_list`, with which
#' differences in model degrees of
#' freedom will be computed for other
#' models. If `NULL`, the default,
#' then the model in `sem_out` will
#' be used to computed the differences
#' in model degrees of freedom. If `NA`,
#' then differences in model *df* will
#' not be computed.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to fit the
#' models. Default is `FALSE`.
#'
#' @param ncores Numeric. The number of
#' CPU cores to be used if `parallel`
#' is `TRUE`.
#'
#' @param make_cluster_args A list of
#' named arguments to be passed to
#' `parallel::makeCluster()`. Used by
#' advanced users to configure the
#' cluster if `parallel` is `TRUE`.
#' Default is `list()`.
#'
#' @param progress Whether a progress
#' bar will be displayed, implemented
#' by the `pbapply` package. Default
#' is `TRUE`.
#'
#' @param verbose Whether additional
#' messages will be displayed, such
#' as the expected processing time.
#' Default is `TRUE`.
#'
#' @return An object of the class
#' `sem_outs`, a list with the
#' following major elements:
#'
#' * `fit`: A named list of
#'    [lavaan::lavaan()] output objects or
#'    [update()] for fitting a model with
#'    the added parameters.
#'
#' * `change`: A numeric vector, of the
#'    same length as `fit`. The change
#'    in model *df* for each fit compared
#'    to the original model. A
#'    positive number denotes one less
#'    free parameter. A negative number
#'    denotes one more free parameter or
#'    one less constraint.
#'
#' * `converged`: A named vector of
#'    boolean values, of the same length
#'    as `fit`. Indicates whether each
#'    fit converged or not.
#'
#' * `post_check`: A named vector of
#'    boolean values, of the same length
#'    as `fit`. Indicates whether the
#'    solution of each fit is
#'    admissible or not. Checked by
#'    [lavaan::lavInspect()] with
#'    the `what` argument set to
#'    `"post.check"`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- dat_path_model
#' mod <-
#' "
#' x3 ~ a*x1 + b*x2
#' x4 ~ a*x1
#' ab := a*b
#' "
#' fit <- sem(mod, dat_path_model, fixed.x = TRUE)
#' mod_to_add <- get_add(fit)
#' fit_many(mod_to_add, fit)
#'
#' @export

fit_many <- function(model_list,
                     sem_out,
                     original = NULL,
                     parallel = FALSE,
                     ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                     make_cluster_args = list(),
                     progress = TRUE,
                     verbose = TRUE) {
  if (missing(model_list)) stop("model_list is not supplied.")
  if (missing(sem_out)) stop("sem_out is not supplied.")
  if (!inherits(sem_out, "lavaan")) {
      stop("sem_out is not a lavaan-class object.")
    }

  p_models <- length(model_list)

  slot_opt <- sem_out@Options
  # slot_smp <- sem_out@SampleStats
  # slot_dat <- sem_out@Data
  slot_opt$se <- "none"
  slot_opt$baseline <- FALSE
  slot_opt$verbose <- FALSE
  has_groups <- lavaan::lavTech(sem_out, "ngroups") > 1
  if (has_groups) {
      group_var <- lavaan::lavTech(sem_out, "group")
    }
  raw_data <- tryCatch(lavaan::lavInspect(sem_out, "data",
                                          drop.list.single.group = FALSE),
                       error = function(e) e)
  if (inherits(raw_data, "error")) {
      raw_data <- NULL
      has_data <- FALSE
    } else {
      for (i in seq_along(raw_data)) {
          idx <- lavaan::lavInspect(sem_out, "case.idx",
                                    drop.list.single.group = FALSE)
          colnames(raw_data[[i]]) <- lavaan::lavNames(sem_out)
          rownames(raw_data[[i]]) <- idx[[i]]
        }
      has_data <- TRUE
    }

  if (has_data) {
      # Placeholder
    } else {
      # This block works with ngroups > 1
      sem_out_nobs <- lavaan::lavInspect(sem_out, "nobs")
      sem_out_sp <- lavaan::lavInspect(sem_out, "sampstat")
      ng <- lavaan::lavInspect(sem_out, "ngroups")
      if (ng > 1) {
          sem_out_cov <- lapply(sem_out_sp, function(x) x$cov)
        } else {
          sem_out_cov <- sem_out_sp$cov
        }
      if (ng > 1) {
          sem_out_mean <- lapply(sem_out_sp, function(x) x$mean)
        } else {
          sem_out_mean <- sem_out_sp$mean
        }
      sem_thresholds <- lavaan::lavInspect(sem_out, "thresholds")
      if (is.list(sem_thresholds)) {
          if (all(sapply(sem_thresholds, length) == 0)) {
              sem_thresholds <- NULL
            }
        } else {
          if (length(sem_thresholds) == 0) {
              sem_thresholds <- NULL
            }
        }
      sem_out_estimator <- lavaan::lavInspect(sem_out, "options")$estimator
      if (sem_out_estimator == "DWLS") {
          sem_out_WLS.V <- lavaan::lavInspect(sem_out, "WLS.V")
          sem_out_NACOV <- lavaan::lavInspect(sem_out, "gamma")
        } else {
          sem_out_WLS.V <- NULL
          sem_out_NACOV <- NULL
        }
    }

  # fit_i <- function(x) {
  #     lavaan::lavaan(model = x,
  #                    slotOptions = slot_opt,
  #                    slotSampleStats = slot_smp,
  #                    slotData = slot_dat)
  #   }

  if (has_data) {
      # Merge into one data frame
      if (length(raw_data) == 1) {
          raw_data <- as.data.frame(raw_data[[1]])
        } else {
          for (i in seq_along(raw_data)) {
              raw_data[[i]] <- as.data.frame(raw_data[[i]])
              raw_data[[i]][, group_var] <- names(raw_data)[i]
            }
          raw_data <- do.call(rbind, raw_data)
        }
    }

  if (has_data) {
      if (has_groups) {
          fit_i <- function(x,
                            opt_args = list()) {
              slot_opt1 <- utils::modifyList(slot_opt,
                                             opt_args)
              # We need the raw data because the order
              # of variables may change
              lavaan::lavaan(model = x,
                             slotOptions = slot_opt1,
                             group = group_var,
                             data = raw_data)
            }
        } else {
          fit_i <- function(x,
                            opt_args = list()) {
              slot_opt1 <- utils::modifyList(slot_opt,
                                             opt_args)
              # We need the raw data because the order
              # of variables may change
              lavaan::lavaan(model = x,
                            slotOptions = slot_opt1,
                            data = raw_data)
            }
        }
    } else {
      fit_i <- function(x,
                        opt_args = list()) {
          slot_opt1 <- utils::modifyList(slot_opt,
                                         opt_args)
          lavaan::lavaan(model = x,
                         slotOptions = slot_opt1,
                         sample.cov = sem_out_cov,
                         sample.mean = sem_out_mean,
                         sample.nobs = sem_out_nobs,
                         semple.th = sem_thresholds,
                         WLS.V = sem_out_WLS.V,
                         NACOV = sem_out_NACOV)
        }
    }

  # Check ncores
  # Adapted from manymome::fit2boot_out_do_boot()
  ft <- as.numeric(lavaan::lavInspect(sem_out, "timing")$total)
  if (parallel) {
        if (is.numeric(ncores)) {
            ncores0 <- parallel::detectCores()
            if (ncores == ncores0) {
                warning(paste0("'ncores' >= The number of detected cores (",
                                ncores0,"). The computer may not be responsive",
                                " when models are estimated."),
                        immediate. = TRUE)
                utils::flush.console()
              }
            if (ncores > ncores0) {
                ncores <- max(ncores0 - 1, 1L)
              }
          } else {
            ncores <- 1L
          }
    } else {
      ncores <- 1L
    }
  # Set has_cl
  # Adapted from manymome::fit2boot_out_do_boot()
  if (ncores > 1L) {
      make_cluster_args <- utils::modifyList(make_cluster_args,
                                      list(spec = ncores))
      tmp <- tryCatch({cl <- do.call(parallel::makeCluster,
                                    make_cluster_args)},
                      error = function(e) e)
      has_cl <- !inherits(tmp, "error")
    } else {
      has_cl <- FALSE
    }
  # Do the analysis.
  # Adapted from manymome::fit2boot_out_do_boot()
  if (has_cl) {
      texp <-  1.2 * p_models * ft / length(cl)
      if (verbose) {
          message(paste0(length(cl), " processes started to run model fitting."))
          message(paste0("The expected CPU time is about ",
                          round(texp, 2),
                          " second(s)."))
          utils::flush.console()
        }
      pkgs <- .packages()
      pkgs <- rev(pkgs)
      parallel::clusterExport(cl, "pkgs", envir = environment())
      parallel::clusterEvalQ(cl, {
                      sapply(pkgs,
                      function(x) {
                          # Packages temporarily loaded are excluded
                          try(library(x, character.only = TRUE),
                                      silent = TRUE)
                        })
                    })
      if (progress) {
          op_old <- pbapply::pboptions(type = "timer")
          cat("\nFit the", length(model_list), "models:\n")
          tmp <- tryCatch({rt <- system.time(fit_list <- suppressWarnings(
                            pbapply::pblapply(model_list,
                                              fit_i,
                                              cl = cl)))},
                            error = function(e) e)
          pbapply::pboptions(op_old)
        } else {
          tmp <- tryCatch({rt <- system.time(fit_list <- suppressWarnings(
                            parallel::parLapplyLB(cl,
                                                  model_list,
                                                  fit_i)))},
                            error = function(e) e)
        }
      if (inherits(tmp, "error")) {
          try(parallel::stopCluster(cl), silent = TRUE)
          stop("Running in parallel failed. Please set 'parallel' to FALSE.")
        }
      parallel::stopCluster(cl)
    } else {
      if (progress) {
          cat("\nFit the", length(model_list), "model(s) (duplicated models removed):\n")
          rt <- system.time(fit_list <- suppressWarnings(pbapply::pblapply(model_list, fit_i)))
        } else {
          rt <- system.time(fit_list <- suppressWarnings(lapply(model_list, fit_i)))
        }
    }
  df_list <- mapply(fit_many_get_df,
                    fit = fit_list,
                    model = model_list,
                    MoreArgs = list(fit_i = fit_i),
                    SIMPLIFY = TRUE)
  if (is.null(original)) {
      sem_out_df <- as.numeric(lavaan::fitMeasures(sem_out, "df"))
      # change_list <- sapply(fit_list,
      #     function(x) sem_out_df - as.numeric(lavaan::fitMeasures(x, fit.measures = "df")))
      change_list <- sem_out_df - df_list
    } else {
      if (original %in% names(model_list)) {
          i_original <- match(original, names(model_list))
          # change_list <- sapply(fit_list,
          #     function(x) as.numeric(lavaan::fitMeasures(x, fit.measures = "df")))
          df_original <- df_list[i_original]
          change_list <- df_original - df_list
        } else {
          change_list <- rep(NA, p_models)
        }
    }
  converged_list <- sapply(fit_list,
      function(x) lavaan::lavInspect(x, "converged"))
  post_check_list <- sapply(fit_list,
      function(x) lavaan::lavInspect(x, "post.check"))
  out <- list(fit = fit_list,
              change = change_list,
              converged = converged_list,
              post_check = post_check_list,
              model_df = df_list,
              call = match.call())
  class(out) <- c("sem_outs", class(out))
  out
}

#' @noRd

lavaan_to_sem_outs <- function(x,
                               original = NULL) {
    p_models <- length(x)
    if (is.null(original)) {
        change_list <- rep(NA, x)
      } else {
        if (original %in% names(x)) {
            i_original <- match(original, names(x))
            change_list <- sapply(x,
                function(x) as.numeric(lavaan::fitMeasures(x, fit.measures = "df")))
            df_original <- change_list[i_original]
            change_list <- df_original - change_list
          } else {
            change_list <- rep(NA, p_models)
          }
      }
    converged_list <- sapply(x,
        function(x) lavaan::lavInspect(x, "converged"))
    post_check_list <- sapply(x,
        function(x) lavaan::lavInspect(x, "post.check"))
    out <- list(fit = x,
                change = change_list,
                converged = converged_list,
                post_check = post_check_list,
                call = match.call())
    class(out) <- c("sem_outs", class(out))
    out
  }

#' @noRd

fit_many_get_df <- function(fit,
                            model,
                            fit_i) {
    out <- tryCatch(lavaan::fitMeasures(fit, fit.measures = "df"),
                    error = function(e) e)
    if (!inherits(out, "error")) {
        return(as.numeric(out))
      }
    fit1 <- suppressWarnings(fit_i(model,
                                   opt_args = list(optim.force.converged = TRUE,
                                                   do.fit = FALSE,
                                                   warn = FALSE)))
    out <- tryCatch(lavaan::fitMeasures(fit1, fit.measures = "df"),
                    error = function(e) e)
    if (!inherits(out, "error")) {
        return(as.numeric(out))
      }
    return(NA)
  }
