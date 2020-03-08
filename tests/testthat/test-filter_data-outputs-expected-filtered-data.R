context("filter_data-outputs-expected-filtered-data")

data <- data.frame(A = 1:10, B = LETTERS[1:10],
                   stringsAsFactors = FALSE)

remove_A_equals_2 <- function(x) x[x[, 1] != 2, ]
remove_A_equals_8 <- function(x) x[x[, 1] != 8, ]
remove_B_equals_E <- function(x) x[x[, 2] != "E", ]
remove_B_equals_D <- function(x) x[x[, 2] != "D", ]

run_all_filters <- function(data, funs) {
    for (i in 1:length(funs)) {
        data <- funs[[i]](data)
    }
    return(data)
}

totes_filtered <-
    run_all_filters(data,
                    list(remove_A_equals_2, remove_A_equals_8,
                         remove_B_equals_D, remove_B_equals_E))

test_that("with two balanced phases", {

    expect_equal(
        filter_data(data,
                    phase_1 = list(remove_A_equals_2 = remove_A_equals_2,
                                   remove_A_equals_8 = remove_A_equals_8),
                    phase_2 = list(remove_B_equals_E = remove_B_equals_E,
                                   remove_B_equals_D = remove_B_equals_D))[[1]],
        totes_filtered)

})

test_that("with single list", {

    expect_equal(
        filter_data(data,
                    list(remove_A_equals_2 = remove_A_equals_2,
                         remove_A_equals_8 = remove_A_equals_8,
                         remove_B_equals_E = remove_B_equals_E,
                         remove_B_equals_D = remove_B_equals_D))[[1]],
        totes_filtered)

})

test_that("with list then function", {

    expect_equal(
        filter_data(data,
                    list(remove_A_equals_2 = remove_A_equals_2,
                         remove_A_equals_8 = remove_A_equals_8,
                         remove_B_equals_E = remove_B_equals_E),
                    remove_B_equals_D = remove_B_equals_D)[[1]],
        totes_filtered)


})

test_that("with function then list", {

    expect_equal(
        filter_data(data,
                    remove_B_equals_D = remove_B_equals_D,
                    list(remove_A_equals_2 = remove_A_equals_2,
                         remove_A_equals_8 = remove_A_equals_8,
                         remove_B_equals_E = remove_B_equals_E))[[1]],
        totes_filtered)

})

test_that("with no lists", {

    expect_equal(
        filter_data(data,
                    remove_A_equals_2 = remove_A_equals_2,
                    remove_A_equals_8 = remove_A_equals_8,
                    remove_B_equals_E = remove_B_equals_E,
                    remove_B_equals_D = remove_B_equals_D)[[1]],
        totes_filtered)

})
