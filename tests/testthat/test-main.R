context("Main functionality")

test_that("problem should not be built for incorrect margValueFuncShapes", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3);
  margValueFuncShapes = c("GAIN", "INCORRECT");
  M = 10000;
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M),
               "Argument 'margValueFuncShapes' must be either.*")
})

test_that("problem should not be built for alternatives number less than 3", {
  alternatives = matrix(c(100, 5, 120, 3), nrow = 2);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M),
                 "Alternatives number must be greater or equals 3");
})

test_that("problem should not built be for M equals nonpositive value", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3);
  margValueFuncShapes = c("GAIN", "COST");
  M1 = 0;
  M2 = -1;
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M1),
               "Argument 'M' must be greater than 0")
  expect_error(buildProblem (alternatives, margValueFuncShapes, M2),
               "Argument 'M' must be greater than 0")
})

test_that("problem should be built proper arguments", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  
  problem = buildProblem (alternatives, margValueFuncShapes, M)
  
  expect_equal(problem$alternatives, alternatives);
  expect_equal(problem$criteriaNumber, ncol(alternatives));
  expect_equal(problem$alternativesNumber, nrow(alternatives));
  expect_equal(problem$margValueFuncShapes, margValueFuncShapes);
  expect_equal(problem$M, M);
})

