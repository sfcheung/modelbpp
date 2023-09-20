if (requireNamespace("tinytest", quietly = TRUE)){
    home <- length(unclass(packageVersion("modelbpp"))[[1]]) == 4
    tinytest::test_package("modelbpp", at_home = home)
  }

