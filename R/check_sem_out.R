#' @noRd

check_sem_out <- function(sem_out) {
    # Estimator: Must be ML
    # No. of groups: 1
    # No. of levels: 1
    if (!("ML" %in% sem_out@Options$estimator)) {
        stop("The estimator of 'sem_out' is not 'ML'.")
      }
    if (lavaan::lavInspect(sem_out, "ngroups") != 1) {
        stop("The number of groups in 'sem_out' is not 1.")
      }
    if (lavaan::lavInspect(sem_out, "nlevels") != 1) {
        stop("The number of levels in 'sem_out' is not 1.")
      }
    return(TRUE)
  }
