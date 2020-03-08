good_fun <- function(x) {
    x <- x[x[, "mpg"] > 15, ]
    return(x)
}

bad_fun <- function(x) {
    x[, "mpg"] <- x[, "mpg"] * 2
    return(x)
}

test_that("with functions that do more than filter", {
  expect_error(filter_data(mtcars, bad_fun))
})

test_that("when objects not of class 'function' or 'list' supplied to '...'", {
    expect_error(filter_data(mtcars, "kjlaskjl"))
    expect_error(filter_data(mtcars, list(good_fun, "kjlaskjl")))
})

test_that("when 'list' supplied to '...' is too deeply nested", {
    expect_error(filter_data(mtcars, list(list(good_fun))))
    expect_error(filter_data(mtcars, list(good_fun, list(good_fun))))
})
