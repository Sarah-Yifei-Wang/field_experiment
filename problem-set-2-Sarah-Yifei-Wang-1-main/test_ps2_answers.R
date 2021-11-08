source('./ps2_answers.R')

## Question 1 

context('Question 1: Hajj and Views of')
test_that(
  'hajj ate object is a data.table', {
    expect_s3_class(hajj_group_mean, 'data.table')
  })

test_that(
  'hajj ate object has the right shape', {
    expect_equal(nrow(hajj_group_mean), 2)
    expect_equal(ncol(hajj_group_mean), 2)
    })

test_that(
  'computed ate is correct', {
  expect_equal(hajj_ate, expected = 0.4748, tolerance = .01)
  })

test_that(
  'ri distribution is correct class', {
    expect_vector(hajj_ri_distribution)
  })

test_that(
  'number larger is sensible', {
    expect_lt(hajj_count_larger, 40)
    expect_gt(hajj_count_larger, 10)
  })

test_that(
  'number larger is correct', { 
    expect_equal(
      object    = hajj_count_larger,
      expected  = sum(hajj_ri_distribution > hajj_ate), 
      tolerance = 0.01)
    })

test_that(
  'p-value is derived from number larger', { 
    expect_equal(
      object    = hajj_one_tailed_p_value, 
      expected  = mean(hajj_ri_distribution > hajj_ate), 
      tolerange = 0.01)
  })

## Question 2

test_that(
  'the cards auction is a t.test', 
  expect_s3_class(t_test_cards, 'htest')
)

test_that(
  'cards auction p-value is correct', {
  expect_equal(
    object    = t_test_cards$p.value, 
    expected  = 0.00642, 
    tolerance = 0.001)
  })

test_that(
  'cards ate is correct', { 
    expect_equal(
      object    = abs(cards_ate), 
      expected  = 12.2, 
      tolerance = 0.1)}
)

test_that(
  'cards ri distribution is reasonable', { 
    expect_vector(cards_ri_distribution)
    expect_equal(length(cards_ri_distribution), 1000, .001)
    expect_lt(
      object    = abs(quantile(cards_ri_distribution, 0.025)), 
      expected  = 9.25)
    expect_gt(
      object    = abs(quantile(cards_ri_distribution, 0.025)), 
      expected = 8.75)
    })

test_that(
  'estimated a lm for the model', {
    expect_s3_class(
      object = mod, 
      class = 'lm')
  })  

test_that(
  'robust ci is correct', {
    expect_equal(
      object = cards_robust_ci['uniform_price_auction', '2.5 %'], 
      expected = -20.97, 
      tolerance = 0.01)
    expect_equal(
      object = cards_robust_ci['uniform_price_auction', '97.5 %'], 
      expected = -3.44,
      tolerance = 0.01)
      })

## Problem 3

## There are no tests for problem three. You'll have to work the problem 
## yourself without our guide-rails.