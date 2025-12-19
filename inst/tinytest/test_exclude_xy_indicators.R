suppressMessages(library(lavaan))

lav_pure_x <- function(sem_out) {
  # Find the names of pure-x latent variables
  a <- lavaan::lavNames(
          sem_out,
          type = "lv.x"
        )
  a
}

lav_pure_y <- function(sem_out) {
  # Find the names of pure-y latent variables
  a <- lavaan::lavNames(
          sem_out,
          type = "lv.y"
        )
  a
}

lav_indicators <- function(
  sem_out,
  lav
) {
  # Find the indicators of lav,
  # a vector of names of latent variables
  pt <- lavaan::parameterTable(sem_out)
  i1 <- pt$lhs %in% lav
  i2 <- pt$op == "=~"
  pt$rhs[i1 & i2]
}

lav_pure_x_indicators <- function(sem_out) {
  # Find the indicators of
  # all pure-x latent variables
  a <- lav_pure_x(sem_out)
  lav_indicators(sem_out,
                 lav = a)
}

lav_pure_y_indicators <- function(sem_out) {
  # Find the indicators of
  # all pure-y latent variables
  a <- lav_pure_y(sem_out)
  lav_indicators(sem_out,
                 lav = a)
}

lav_loadings <- function(
  sem_out,
  lav
) {
  pt <- lavaan::parameterTable(sem_out)
  i1 <- (pt$lhs %in% lav) &
        (pt$op == "=~")
  pt_lav <- pt[i1, c("lhs", "op", "rhs", "block", "group")]
  pt_lav
}

lav_pure_x_loadings <- function(sem_out) {
  x <- lav_pure_x(sem_out)
  lav_loadings(sem_out,
               lav = x)
}

lav_pure_y_loadings <- function(sem_out) {
  y <- lav_pure_y(sem_out)
  lav_loadings(sem_out,
               lav = y)
}

lav_cross_set <- function(
  sem_out,
  type = c("pure_x", "pure_y", "user"),
  user_lav = character(0)
) {
  # Find loadings between
  #   pure x latent variables and current x indicators
  #   pure y latent variables and current y indicators
  # which are
  #   not yet in the model.
  # That is:
  # Find all possible new cross loadings among
  # pure x/y latent factors
  type <- match.arg(type)
  x_inds <- switch(
              type,
              pure_x = lav_pure_x_indicators(sem_out),
              pure_y = lav_pure_y_indicators(sem_out),
              user = lav_indicators(sem_out, user_lav)
            )
  x <- switch(
          type,
          pure_x = lav_pure_x(sem_out),
          pure_y = lav_pure_y(sem_out),
          user = user_lav
        )
  out0 <- all_possible_loadings(sem_out)
  a1 <- out0[(out0$lhs %in% x) &
             (out0$rhs %in% x_inds), ]
  # TODO:
  # - Support multigroup model
  a2 <- switch(
          type,
          pure_x = lav_pure_x_loadings(sem_out)[, c("lhs", "op", "rhs")],
          pure_y = lav_pure_y_loadings(sem_out)[, c("lhs", "op", "rhs")],
          user = lav_loadings(sem_out, lav = user_lav)
        )
  a1$id <- seq_len(nrow(a1))
  tmp <- merge(
        x = a2,
        y = a1,
        all.y = FALSE
      )
  # Loadings not yet in the model
  a3 <- a1[!(a1$id %in% tmp$id), ]
  a3$id <- NULL
  rownames(a3) <- NULL
  if (nrow(a3) > 0) {
    a3 <- a3[order(a3$lhs, a3$rhs), ]
  }
  a3
}

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

lav_cross_set(fit, type = "pure_x")
lav_cross_set(fit, type = "pure_y")
lav_cross_set(fit, type = "user", user_lav = c("fx1", "fy1", "fy3"))

lav_loadings(fit, lav = c("fx1", "fy3", "fy1"))
