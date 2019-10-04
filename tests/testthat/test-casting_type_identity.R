context("casting type identity")

# there's been some issue in equality checks down in the deep C when after casting, types are not exactly identical
# examples would be levels not having the same order, or maybe an attribute being NULL vs. not existing
# so here's some tests to make sure that casting produces strictly consistent prototypes


is_vec_cc_identical<-function(common_cast){
  # cast to common type (returns list with the two new vectors), get their prototype, check if identical:
  do.call(identical,
          lapply(
            common_cast,
            vec_ptype)
          )
}


test_that("vec_cast_common categorical & categorical", {


  expect_true({vec_cast_common(
    categorical(c('a')),
    categorical(c('a'))
    ) %>% is_vec_cc_identical})

  expect_true({vec_cast_common(
    categorical(c('a')),
    categorical(c('b'))
  ) %>% is_vec_cc_identical})


  expect_true({vec_cast_common(
    categorical(c('b')),
    categorical(c('a'))
  ) %>% is_vec_cc_identical})


  expect_true({vec_cast_common(
    categorical(c(1)),
    categorical(c("1"))
  ) %>% is_vec_cc_identical})

  expect_true({vec_cast_common(
    categorical(c('a','b')),
    categorical(c('a'))
  ) %>% is_vec_cc_identical})


  expect_true({vec_cast_common(
    categorical(c('a','b')),
    categorical(c('b'))
  ) %>% is_vec_cc_identical})


  expect_true({vec_cast_common(
    categorical(c('b','a')),
    categorical(c('a'))
  ) %>% is_vec_cc_identical})


  expect_true({vec_cast_common(
    categorical(c('b','a')),
    categorical(c('X'))
  ) %>% is_vec_cc_identical})

  expect_true({vec_cast_common(
    categorical(c('b','X','a')),
    categorical(c('X'))
  ) %>% is_vec_cc_identical})

  expect_true({vec_cast_common(
    categorical(c('b','X','a',NA)),
    categorical(c('X'))
  ) %>% is_vec_cc_identical})

  expect_true({vec_cast_common(
    categorical(c('b','X','a',NA)),
    categorical(c('X',NA))
  ) %>% is_vec_cc_identical})

  expect_true({
    vec_cast_common(
    categorical(c('b','X','a',NA)),
    categorical(c(NA),levels='Y')
  ) %>% is_vec_cc_identical})

})
