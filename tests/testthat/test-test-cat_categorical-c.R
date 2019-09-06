
context("c()")


context('c() - create')
empty<-categorical()
simple1<-categorical(x = c(1,2,1,3),levels = 1:5,letters = letters[1:5], LETTERS = LETTERS[1:5], messy = c(T,1,"", NA, 'NA'))
simple2<-categorical(x=letters[c(1,1,6,6,5,5,9,9,9,10)],levels=letters[1:10],numbers = 1:10,lgl=rep(c(TRUE,FALSE),5))
simple2.2<-categorical(x=letters[c(1,1,10,5,5,5,9,9,9,10)],levels=letters[c(1:5,7:11)],numbers = 10:1,lgl=rep(c(TRUE,FALSE),5))


context('c() - use c()')
empty_empty <- c(empty,empty)
empty_simple1<-c(empty,simple1)
empty_simple2<-c(empty,simple2)
simple1_simple1<-c(simple1,simple1)
simple2_simple2<-c(simple2,simple2)
simple1_simple2<-c(simple1,simple2)
simple2_simple1<-c(simple2,simple1)
simple2_simple2.2<-c(simple2,simple2.2)
context('c() - levels')


test_that("c() -  levels", {

  expect_equal(levels(empty_empty),levels(empty))
  expect_equal(levels(empty_simple1),levels(simple1))
  expect_equal(levels(empty_simple2),levels(simple2))
  expect_equal(levels(simple1_simple1),levels(simple1))
  expect_equal(levels(simple2_simple2),levels(simple2))
  expect_equal(levels(simple2_simple2.2),unique(c(levels(simple2),levels(simple2.2))))

  expect_equal(levels(simple1_simple2),(c(levels(simple1),levels(simple2))))
  expect_equal(levels(simple2_simple1),(c(levels(simple2),levels(simple1))))
  expect_equal(levels(simple2_simple1),(c(levels(simple2),levels(simple1))))


  })

context('c() - alternatives')

test_that("alternative names after c()", {

expect_equal(list_alternatives(empty_empty)$public,character())
expect_equal(list_alternatives(empty_simple1)$public,list_alternatives(simple1)$public)
expect_equal(list_alternatives(empty_simple2)$public,list_alternatives(simple2)$public)
expect_equal(list_alternatives(simple1_simple1)$public,list_alternatives(simple1)$public)
expect_equal(list_alternatives(simple1_simple2)$public,unique(c(list_alternatives(simple1)$public,list_alternatives(simple2)$public)))
expect_equal(list_alternatives(simple2_simple1)$public,unique(c(list_alternatives(simple2)$public,list_alternatives(simple1)$public)))

})

test_that("correct alternative names", {

  expect_equal(list_alternatives(empty_empty)$public,character())
  expect_equal(list_alternatives(empty_simple1)$public,list_alternatives(simple1)$public)
  expect_equal(list_alternatives(empty_simple2)$public,list_alternatives(simple2)$public)
  expect_equal(list_alternatives(simple1_simple1)$public,list_alternatives(simple1)$public)
  expect_equal(list_alternatives(simple1_simple2)$public,unique(c(list_alternatives(simple1)$public,list_alternatives(simple2)$public)))
  expect_equal(list_alternatives(simple2_simple1)$public,unique(c(list_alternatives(simple2)$public,list_alternatives(simple1)$public)))

})

test_that("valid alternative rows", {

  expect_true({!any(duplicated(levels(empty_empty)))})
  expect_true({!any(duplicated(levels(empty_simple1)))})
  expect_true({!any(duplicated(levels(simple1_simple2)))})

  expect_equal(length(levels(empty_empty)),nrow(attributes(empty_empty)$alternatives))
  expect_equal(length(levels(empty_simple1)),nrow(attributes(empty_simple1)$alternatives))
  expect_equal(length(levels(simple1_simple1)),nrow(attributes(simple1_simple1)$alternatives))
  expect_equal(length(levels(simple1_simple2)),nrow(attributes(simple1_simple2)$alternatives))
  expect_equal(length(levels(simple2_simple1)),nrow(attributes(simple2_simple1)$alternatives))

  expect_equal(list_alternatives(empty_empty)$public,character())
  expect_equal(list_alternatives(empty_simple1)$public,list_alternatives(simple1)$public)
  expect_equal(list_alternatives(empty_simple2)$public,list_alternatives(simple2)$public)
  expect_equal(list_alternatives(simple1_simple1)$public,list_alternatives(simple1)$public)
  expect_equal(list_alternatives(simple1_simple2)$public,unique(c(list_alternatives(simple1)$public,list_alternatives(simple2)$public)))
  expect_equal(list_alternatives(simple2_simple1)$public,unique(c(list_alternatives(simple2)$public,list_alternatives(simple1)$public)))

})








