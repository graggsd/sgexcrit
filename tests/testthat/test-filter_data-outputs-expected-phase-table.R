context("filter_data-outputs-expected-phase-table")

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

    expect_equal(out[[3]][, 1], c("phase_1", "phase_2"))
    expect_equal(out[[3]][, 2],
                 c(paste("remove_A_equals_2", "remove_A_equals_8", sep = "; "),
                   paste("remove_B_equals_E", "remove_B_equals_D", sep = "; ")))
    expect_equal(out[[3]][, 3], c("10", "8"))
    expect_equal(out[[3]][, 4], c("8", "6"))
    expect_equal(out[[3]][, 5], c("2", "2"))
})

test_that("with a single phase", {

    out <- filter_data(data,
                       list(remove_A_equals_2 = remove_A_equals_2,
                            remove_A_equals_8 = remove_A_equals_8,
                            remove_B_equals_E = remove_B_equals_E,
                            remove_B_equals_D = remove_B_equals_D))
    expect_equal(out[[3]][, 1], "1")
    expect_equal(out[[3]][, 2],
                 paste(c("remove_A_equals_2", "remove_A_equals_8",
                           "remove_B_equals_E", "remove_B_equals_D"),
                         collapse = "; "))
    expect_equal(out[[3]][, 3], as.character(10))
    expect_equal(out[[3]][, 4], as.character(6))
    expect_equal(out[[3]][, 5], as.character(4))

})

test_that("with a list, then function", {

    out <- filter_data(data,
                       list(remove_A_equals_2 = remove_A_equals_2,
                            remove_A_equals_8 = remove_A_equals_8,
                            remove_B_equals_E = remove_B_equals_E),
                       remove_B_equals_D = remove_B_equals_D)
    expect_equal(out[[3]][, 1], c("1", "2"))
    expect_equal(out[[3]][, 2],
                 c(paste(c("remove_A_equals_2", "remove_A_equals_8",
                         "remove_B_equals_E"),
                       collapse = "; "),
                   "remove_B_equals_D"))
    expect_equal(out[[3]][, 3], as.character(c(10, 7)))
    expect_equal(out[[3]][, 4], as.character(c(7, 6)))
    expect_equal(out[[3]][, 5], as.character(c(3, 1)))

})

test_that("with no lists", {

    out <- filter_data(data,
                       remove_A_equals_2 = remove_A_equals_2,
                            remove_A_equals_8 = remove_A_equals_8,
                            remove_B_equals_E = remove_B_equals_E,
                            remove_B_equals_D = remove_B_equals_D)
    expect_equal(out[[3]][, 1], c("remove_A_equals_2", "remove_A_equals_8",
                                  "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[3]][, 2],
                 c("remove_A_equals_2", "remove_A_equals_8",
                         "remove_B_equals_E", "remove_B_equals_D"))
    expect_equal(out[[3]][, 3], as.character(10:7))
    expect_equal(out[[3]][, 4], as.character(9:6))
    expect_equal(out[[3]][, 5], as.character(rep(1, 4)))
})

