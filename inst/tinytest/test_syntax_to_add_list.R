suppressMessages(library(lavaan))

out1 <- syntax_to_add_list(c("m3 ~ m4", "x4 ~ x3", "f0 =~ x1"))
out2 <- syntax_to_add_list(list("m3 ~ m4", "x4 ~ x3", "f0 =~ x1"))
out3 <- syntax_to_add_list("m3 ~ m4\nx4 ~ x3\nf0 =~ x1")

out1_check <- list(c("m3", "~", "m4"),
                   c("x4", "~", "x3"),
                   c("f0", "=~", "x1"))

out1_check <- lapply(out1_check,
                      function(x) {
                          names(x) <- c("lhs", "op", "rhs")
                          x
                        })

expect_equal(out1,
              out1_check,
              info = "syntax_to_add_list")
expect_equal(out2,
              out1_check,
              info = "syntax_to_add_list")
expect_equal(out3,
              out1_check,
              info = "syntax_to_add_list")
