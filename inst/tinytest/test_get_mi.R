# WIP

if (FALSE) {

suppressMessages(library(lavaan))

dat <- dat_sem
set.seed(1234)
dat_sem$group <- sample(c("gp1", "gp2", "gp3"), size = nrow(dat_sem), replace = TRUE)

mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
"

fit_metric <- cfa(mod, dat_sem, group = "group",
                  group.equal = "loadings")
fit_scalar <- cfa(mod, dat_sem, group = "group",
                  group.equal = c("intercepts", "loadings"))

measurement_invariance_models <- function(cfa_out,
                                          max_free = 1) {
    fit_metric <- lavaan::update(cfa_out,
                                  group.equal = "loadings")
    fit_scalar <- lavaan::update(cfa_out,
                                  group.equal = c("loadings", "intercepts"))
    fit_pi_metric <- partial_invariance(fit_metric,
                                        pars = "loadings",
                                        max_free = max_free)
    fit_pi_scalar <- partial_invariance(fit_scalar,
                                        pars = "intercepts",
                                        max_free = max_free)
    out <- c(list(config = cfa_out),
              fit_pi_metric,
              fit_pi_scalar)
  }

partial_invariance <- function(cfa_out,
                                pars = c("loadings", "intercepts"),
                                max_free = 1,
                                keep_original = TRUE) {
    pars <- match.arg(pars)
    opt <- lavaan::lavInspect(cfa_out, "options")
    if ((pars == "loadings") &&
        opt$group.equal != "loadings") {
        stop("To use pars = 'loadings', group.equal must be 'loadings'.")
      }
    chk <- opt$group.equal
    if ((pars == "intercepts") &&
        !setequal(opt$group.equal, c("loadings", "intercepts"))) {
        stop("To use pars = 'intercepts', group.equal must be c('loadings', 'intecepts').")
      }
    free_pars <- switch(pars,
                        loadings = get_free_loadings(cfa_out),
                        intercepts = get_free_intercepts(cfa_out))
    to_release <- lapply(seq_len(max_free), function(xx) {
          combn(free_pars, m = xx, simplify = FALSE)
        })
    to_release <- unlist(to_release, recursive = FALSE)
    gp_eq <- switch(pars,
                    loadings = c("loadings"),
                    intercepts = c("loadings", "intercepts"))
    pt_out <- lapply(to_release, function(x) {
        sem_out_update <- lavaan::update(cfa_out,
                                          group.equal = gp_eq,
                                          group.partial = x)
        sem_out_update
      })
    pt_out_names <- sapply(to_release, paste, collapse = ";")
    names(pt_out) <- pt_out_names
    if (keep_original) {
        if (pars == "loadings") {
            pt_out$metric <- cfa_out
          }
        if (pars == "intercepts") {
            pt_out$scalar <- cfa_out
          }
      }
    pt_out
  }

get_free_loadings <- function(cfa_out) {
    pt <- lavaan::parameterTable(cfa_out)
    loadings <- pt[(pt$op == "=~") & (pt$free != 0) & (pt$group == 1), ]
    out <- sapply(split(loadings, seq_len(nrow(loadings))), function(xx) {
                paste0(xx$lhs, xx$op, xx$rhs)
              })
    out
  }

get_free_intercepts <- function(cfa_out) {
    pt <- lavaan::parameterTable(cfa_out)
    intercepts <- pt[(pt$op == "~1") & (pt$free != 0) & (pt$group == 1), ]
    out <- sapply(split(intercepts, seq_len(nrow(intercepts))), function(xx) {
                paste0(xx$lhs, xx$op)
              })
    out
  }

fit_partial_metric <- partial_invariance(fit_metric, max_free = 2, pars = "loadings")
fit_partial_scalar <- partial_invariance(fit_scalar, max_free = 2, pars = "intercepts")
names(fit_partial_metric)
names(fit_partial_scalar)
# models <- do.call(to_partables, c(list(metric = fit), fit_partial))
out <- model_set(sem_out = c(fit_partial_metric,
                              fit_partial_scalar),
                  skip_check_sem_out = TRUE,
                  progress = FALSE,
                  parallel = FALSE)
out

HSmod <-
"
spatial =~ x1 + x2 + x3
verbal =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
"
fit_config <- cfa(model = mod,
                  data = HolzingerSwineford1939,
                  group = "school")
fit_mi <- measurement_invariance_models(fit_config, max_free = 2)
out <- model_set(sem_out = fit_mi,
                  skip_check_sem_out = TRUE,
                  progress = FALSE,
                  parallel = FALSE)
print(out, more_fit_measures = c("cfi", "rmsea"), max_models = NA)
p <- model_graph(out)
plot(p)

}

