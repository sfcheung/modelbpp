suppressMessages(library(lavaan))

xy_lav <- function(sem_out) {
  # Find latent variables with an x-y relation,
  # either directly or indirectly
  tmp1 <- lavaan::lavInspect(
            sem_out,
            what = "free",
            drop.list.single.group = FALSE
          )
  tmp2 <- lavaan::lavInspect(
            sem_out,
            what = "est",
            drop.list.single.group = FALSE
          )
  tmp1 <- lapply(tmp1, function(x) x$beta > 0)
  tmp2 <- lapply(tmp2, function(x) x$beta != 0)
  f <- function(x1, x2) {
          a <- x1 | x2
          a[] <- as.numeric(a)
          a
        }
  beta <- mapply(
            f,
            x1 = tmp1,
            x2 = tmp2,
          SIMPLIFY = FALSE
        )
  adj <- lapply(
            beta,
            t
          )
  graph_adj <- lapply(
              adj,
              igraph::graph_from_adjacency_matrix,
              mode = "directed",
            )
  lav_x <- lavaan::lavNames(
              sem_out,
              type = "lv.x"
            )
  lav_nox <- lavaan::lavNames(
              sem_out,
              type = "lv.nox"
            )
  xy_pairs <- expand.grid(y = lav_nox,
                          x = c(lav_x, lav_nox),
                          stringsAsFactors = FALSE)
  # TODO:
  # - Support multigroup models
  out <- mapply(igraph::all_simple_paths,
                from = xy_pairs$x,
                to = xy_pairs$y,
                MoreArgs = list(graph = graph_adj[[1]]),
                SIMPLIFY = FALSE)
  out <- out[sapply(out, length) > 0]
  out <- unlist(out, recursive = FALSE)
  out2 <- unname(lapply(out, names))

  to_x_y <- function(x) {
    out <- data.frame(x = x[1],
                y = x[length(x)])
    out
  }

  out3 <- lapply(
            out2,
            to_x_y
          )
  out4 <- do.call(
            rbind,
            out3
          )
  out4 <- out4[!duplicated(out4), ]
  rownames(out4) <- NULL
  out4
}

lav_y_on_x <- function(
  x,
  y,
  yx_table) {
  out <- (yx_table$x %in% x) &
         (yx_table$y %in% y)
  any(out)
}

all_possible_loadings <- function(
  sem_out
) {
  all_ins <- lavaan::lavNames(
                sem_out,
                type = "ov.ind"
              )
  all_lv <- lavaan::lavNames(
                sem_out,
                type = "lv"
              )
  out <- expand.grid(
            lhs = all_lv,
            op = "=~",
            rhs = all_ins,
            stringsAsFactors = FALSE
          )
  out
}

all_nonxy_loadings <- function(
  sem_out
) {
  pt <- lavaan::parameterTable(sem_out)
  lambdas0 <- all_possible_loadings(sem_out)
  ind <- unique(lambdas0$rhs)
  # lambdas0 <- lapply(
  #               seq_len(nrow(lambdas0)),
  #               function(x) {
  #                 c(lhs = lambdas0[x, "lhs"],
  #                   rhs = lambdas0[x, "rhs"])
  #               })
  i1 <- pt$op == "=~"
  i2 <- (pt$free > 0) |
        ((pt$free == 0) & (pt$est != 0))
  lambdas1 <- pt[i1 & i2, c("lhs", "op", "rhs")]
  # lambdas1 <- lapply(
  #               seq_len(nrow(lambdas1)),
  #               function(x) {
  #                 c(lhs = lambdas1[x, "lhs"],
  #                   rhs = lambdas1[x, "rhs"])
  #               })
  fxy <- xy_lav(sem_out)
  # fxy <- lapply(
  #               seq_len(nrow(fxy)),
  #               function(x) {
  #                 c(x = fxy[x, "x"],
  #                   y = fxy[x, "y"])
  #               })
  out <- numeric()
  for (x in ind) {
    a2 <- lambdas1$lhs[lambdas1$rhs == x]
    b1 <- fxy$y[fxy$x == a2]
    b2 <- fxy$x[fxy$y == a2]
    b <- union(b1, b2)
    i1 <- which((lambdas0$lhs %in% b) &
                (lambdas0$rhs == x))
    i2 <- which((lambdas0$lhs %in% a2) &
                (lambdas0$rhs == x))
    out <- c(out, i1, i2)
  }
  out <- unique(out)
  lambdas2 <- lambdas0[-out, ]
  lambdas2 <- lambdas2[
                order(lambdas2$lhs,
                      lambdas2$rhs),
              ]
  rownames(lambdas2) <- NULL
  lambdas2
}


mod <-
"
fx1 =~ x1 + x2 + x3
fx2 =~ x4 + x5 + x6
fy1 =~ x7 + x8 + x9
fy2 =~ x10 + x11 + x12
fy3 =~ x13 + x14 + x15
fy1 ~ fx1 + fx2
fy2 ~ fx2
fy3 ~ fy1 + fy2
"

fit <- sem(mod,
           dat_sem,
           warn = FALSE)

out1 <- xy_lav(fit)

expect_true(nrow(out1) == 7)
expect_true(lav_y_on_x("fy1", "fy3", out1))
expect_true(lav_y_on_x("fx2", "fy3", out1))
expect_false(lav_y_on_x("fx2", "fx1", out1))

# TOOD:
# - Need to exclude `fx1 =~ x10`.
all_nonxy_loadings(fit)
