data <- data.frame(a = 1:4)
test_subsets <- function(subsets, length) {
  x <- qsubset(data, subsets)
  expect_equal(nrow(x), length)
}


test_that("it works with ==", {
  qexpression(data, "a", comparitors$GREATER_THAN, 1) %>%
    test_subsets(3)
})

test_that("it groups subsets with &", {
  list(
    qexpression(data, "a", comparitors$GREATER_THAN, 1),
    qexpression(data, "a", comparitors$GREATER_THAN, 2)
  ) %>%
    test_subsets(2)
})

test_that("it groups subsets with |", {
  list(
    qexpression(data, "a", comparitors$GREATER_THAN, 1),
    qexpression(data, "a", comparitors$GREATER_THAN, 2, operator = "|")
  ) %>% test_subsets(3)
})

test_that("is.qexpression works", {
  is.qexpression(qexpression(data, "a", comparitors$GREATER_THAN, 1)) %>%
    expect_true()

  is.qexpression(3) %>% expect_false()
})