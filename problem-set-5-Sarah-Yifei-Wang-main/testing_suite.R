library(testthat)

test_that(
  'The data has loaded', 
  expect_s3_class(d, 'data.table')
)

test_that(
  'You have estimated a model of the observational data', 
  expect_s3_class(model_observational, 'lm')
)

test_that(
  'You have estimated the correct (incorrect...) observational model', 
  expect_equal(as.numeric(coef(model_observational)['years_education']), 5750.5, tolerance = 0.2)
)

test_that(
  'You have built a high_draft variable', 
  expect_named(d, c('draft_number', 'years_education', 'income', 'high_draft'))
)

test_that(
  'You have the correct number of high_draft individuals', 
  expect_equal(d[high_draft == TRUE, .N], 3896)
)

test_that(
  'There exists a model that was estimated for education', 
  expect_s3_class(model_education, 'lm')
)

test_that(
  'Estimate for the education model is correctly specified', 
  expect_equal(as.numeric(coef(model_education)['high_draftTRUE']), 2.12, tolerance = 0.2)
)

test_that(
  'There exists a model that was estiamted for income', 
  expect_s3_class(model_income, 'lm')
)

test_that(
  'Estimate for the income model is correctly specified', 
  expect_equal(as.numeric(coef(model_income)['high_draftTRUE']), 6638, tolerance = 0.2)
)

test_that(
  'There is a model for the instrumental variables regression', 
  expect_s3_class(model_iv, 'ivreg')
)

test_that(
  'The IV reg model is producing the correct results', 
  expect_equal(as.numeric(coef(model_iv)['years_education']), 3122, tolerance = 0.2)
)

## We're going to leave you to solve the question about differential attrition;
## a testing suite can't be written comprehensively enough to catch this 
## without giving away the puzzle that you've got to solve. 