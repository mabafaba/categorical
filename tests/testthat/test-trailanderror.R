context("test-trailanderror")
#
test_that('categorical()',{


  # length in == length out

  expect_equal(length(categorical(c())),0)
  expect_equal(length(categorical(c(1))),1)
  expect_equal(length(categorical(list(list(1,2,3)))),1)
  expect_equal(length(categorical(NULL)),0)
  expect_equal(length(categorical(categorical(c(1,2,3)))),3)
  expect_true(is.categorical(categorical(NULL)))

  # na in input = NA in output
  expect_equal(is.na(categorical(c(NA,'A',NA))),c(T,F,T))

  # '==' works (or not if not wished)

  expect_error(categorical(c(1,2,3))==c(1,2,3))
  expect_true(all(categorical(c(1,2,3))==categorical(c(1,2,3))))
  expect_true(all(categorical(c(1,0,1))==categorical(c(1,0,1))))
  expect_false(all(categorical(c(1,0,1))==categorical(c(2,2,2))))



  expect_true(all(categorical(factor(c(1,0,1)))==categorical(c("1","0","1"))))

})

