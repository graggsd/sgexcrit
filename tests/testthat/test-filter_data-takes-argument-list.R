


test_that("argument list works", {
    data <- data.frame(A = 1:10, B = LETTERS[1:10],
                       stringsAsFactors = FALSE)

    remove_A_equals_2 <- function(x, b) x[x[, 1] != b, ]


    out_1 <- filter_data(data,
                         remove_A_equals_2,
                         args_list = list(b = 2))

    remove_A_equals_2 <- function(x) x[x[, 1] != 2, ]


    out_2 <- filter_data(data,
                         remove_A_equals_2)

    expect_equal(out_1, out_2)
})
