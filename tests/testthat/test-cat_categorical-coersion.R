context("casting / coersion")

testthat::test_that("coersion - character --> categorical",{

  mydays <- categorical(c(1,2,1,5,6,3,7),
                        levels=1:7,
                        labels_english = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                        labels_german = c('Montag','Dienstag','Mittwoch','Donnerstag','Freitag','Samstag','Sonntag')
  )
  mydays_en<-alternate(mydays,'labels_english')


  monday_tuesday_char<-c('Monday','Tuesday')
  coerced <- vec_cast.cat_categorical.character(monday_tuesday_char,mydays_en)

  expect_equal(as.character(levels(coerced)),as.character(1:7))


  expect_true(all(
    c(mydays_en[c(1,2)],monday_tuesday_char)==c(mydays_en[c(1,2)],monday_tuesday_char))
    )

  expect_true(all(
    c(mydays_en[c(1,2)],monday_tuesday_char)==c(mydays_en[c(1,2)],monday_tuesday_char))
  )

  rhs<-c(monday_tuesday_char,monday_tuesday_char)
  lhs<-mydays_en[c(1,2,1,2)]
  expect_true(all(
    lhs == rhs
  ))
  expect_true(all(
    rhs == lhs
  )
)


})
