context("casting / coersion")

testthat::test_that("coersion - character --> categorical",{

  mydays <- categorical(c(1,2,1,5,6,3,7),
                        levels=1:7,
                        alternatives = list(
                          labels_english = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                          labels_german = c('Montag','Dienstag','Mittwoch','Donnerstag','Freitag','Samstag','Sonntag'))
  )
  mydays_en<-alternate(mydays,'labels_english')


  x<-c('Monday','Tuesday')

  # for now, casting alternated vectors is not supported:
  expect_error(categorical:::vec_cast.cat_categorical.character(x,mydays_en))
  # maybe later:
  # expect_equal(as.character(levels(coerced)),as.character(1:7))
})
