data(mtcars)
expr <- qexpression(mtcars, "mpg", comparitors$GREATER_THAN, 20)
qsubset(mtcars, expr)