context("filter_data-outputs-expected-criteria-table")

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

test_that("with two balanced phases", {
    out <- filter_data(data,
                       phase_1 = list(remove_A_equals_2 = remove_A_equals_2,
                                      remove_A_equals_8 = remove_A_equals_8),
                       phase_2 = list(remove_B_equals_E = remove_B_equals_E,
                                      remove_B_equals_D = remove_B_equals_D))
    expect_equal(out[[2]][, 1], c("phase_1", "phase_1", "phase_2", "phase_2"))
    expect_equal(out[[2]][, 2],
                 c("remove_A_equals_2", "remove_A_equals_8",
                   "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[2]][, 3], rep("1", 4))
    expect_equal(out[[2]][, 4], rep("1", 4))
})

test_that("with a single phase", {

    out <- filter_data(data,
                       list(remove_A_equals_2 = remove_A_equals_2,
                            remove_A_equals_8 = remove_A_equals_8,
                            remove_B_equals_E = remove_B_equals_E,
                            remove_B_equals_D = remove_B_equals_D))
    expect_equal(out[[2]][, 1], rep("1", 4))
    expect_equal(out[[2]][, 2],
                 c("remove_A_equals_2", "remove_A_equals_8",
                           "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[2]][, 3], rep("1", 4))
    expect_equal(out[[2]][, 4], rep("1", 4))

})

test_that("with a list, then function", {

    out <- filter_data(data,
                       list(remove_A_equals_2 = remove_A_equals_2,
                            remove_A_equals_8 = remove_A_equals_8,
                            remove_B_equals_E = remove_B_equals_E),
                       remove_B_equals_D = remove_B_equals_D)
    expect_equal(out[[2]][, 1], as.character(c(1,1,1,2)))
    expect_equal(out[[2]][, 2],
                 c("remove_A_equals_2", "remove_A_equals_8",
                   "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[2]][, 3], rep("1", 4))
    expect_equal(out[[2]][, 4], rep("1", 4))

})

test_that("with no lists", {

    out <- filter_data(data,
                       remove_A_equals_2 = remove_A_equals_2,
                            remove_A_equals_8 = remove_A_equals_8,
                            remove_B_equals_E = remove_B_equals_E,
                            remove_B_equals_D = remove_B_equals_D)
    expect_equal(out[[2]][, 1],
                 c("remove_A_equals_2", "remove_A_equals_8",
                   "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[2]][, 2],
                 c("remove_A_equals_2", "remove_A_equals_8",
                         "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[2]][, 3], rep("1", 4))
    expect_equal(out[[2]][, 4], rep("1", 4))
})

test_that("with no function names", {
    criteria_names_out <- c("remove_A_equals_2", "remove_A_equals_8",
                            "remove_B_equals_E", "remove_B_equals_D")

    expect_equal(filter_data(data,  remove_A_equals_2,
                             remove_A_equals_8, remove_B_equals_E,
                             remove_B_equals_D)[[2]][, 2],
                 criteria_names_out)

    expect_equal(filter_data(data,
                             phase_1 = list(remove_A_equals_2,
                                            remove_A_equals_8),
                             phase_2 = list(remove_B_equals_E,
                                            remove_B_equals_D))[[2]][, 2],
                 criteria_names_out)

    expect_equal(filter_data(data,
                             list(remove_A_equals_2,
                                  remove_A_equals_8,
                                  remove_B_equals_E),
                             remove_B_equals_D)[[2]][, 2],
                 criteria_names_out)
})


