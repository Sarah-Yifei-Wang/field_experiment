source('./ps3.R')

library(testthat)

question_1 <- NULL
question_2 <- NULL

test_that(
  'Question 1 does not have any testing suite associated with it; this is a concept based question.',
  expect_null(question_1)
)

test_that(
  'Question 2 does not have any testing suite associted with it; this is a concept based question.',
  expect_null(question_2)
)

test_that(
  'Model 1 is a lm model object',
  expect_s3_class(mod_1, 'lm')
)

test_that(
  'Model 2 is a lm model object',
  expect_s3_class(mod_2, 'lm')
)

test_that(
  'Model 2 has more features than model 1',
  expect_true(length(coef(model_2)) > length(coef(model_1)))
)

test_that(
  'Model 3 is a lm model object',
  expect_s3_class(mod_3, 'lm')
)

test_that(
  'Model 3 has many parameters',
  expect_true(length(coef(mod_3)) > 180)
)

test_that(
  'Model 3 has factor variables for the streets',
  expect_true(sum(grepl('factor', names(coef(mod_3)))) > 100)
)

test_that(
  'F-test for the street coefficients',
  expect_s3_class(test_fixed_effects, 'anova')
)

test_that(
  'Model 5 is a regression',
  expect_s3_class(mod_5, 'lm')
)

test_that(
  'Model 5 is a regression',
  expect_s3_class(mod_6, 'lm')
)


## ZMAPP

test_that(
  'Model 1 is a lm object',
  expect_s3_class(zmapp_1, 'lm')
)

test_that(
  'treatment effect is negative',
  expect_lt(coef(zmapp_1)['treat_zmapp'], 0)
)

test_that(
  'treatment effect is significant',
  expect_lt(summary(zmapp_1)$coefficients['treat_zmapp', 'Pr(>|t|)'], 0.02))

test_that(
  'Model 2 is a lm object',
  expect_s3_class(zmapp_2, 'lm')
)

test_that(
  'Model 2 has covariates',
  expect_gte(length(coef(model_2)), 4)
)

test_that(
  'Model 3 is a lm object',
  expect_s3_class(zmapp_3, 'lm')
)

test_that(
  'Model 4 is a lm object',
  expect_s3_class(zmapp_4, 'lm')
)

test_that(
  'Model 3 has an interaction',
  expect_gte(sum(grepl(':', names(coef(zmapp_4)))), 1)
)
