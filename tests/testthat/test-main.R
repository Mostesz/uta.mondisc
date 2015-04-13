context("Main functionality")

test_that("problem should not be built for alternatives number less than 3", {
  alternatives = matrix(c(100, 5, 120, 3), nrow = 2, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Alternatives number must be greater or equals 3");
})

test_that("problem should not be built for null alternative", {
  alternatives = NULL;
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'alternatives' must be non-null matrix of criteria.*");
})

test_that("problem should not be built for null alternative", {
  alternatives = c(1,2,3,4);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'alternatives' must be non-null matrix of criteria.*");
})

test_that("problem should not be built for null margValueFuncShapes", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = NULL;
  M = 1000;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'margValueFuncShapes' must be non-null vector")
})

test_that("problem should not be built in case that margValueFuncShapes size is not equal to criteria number", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c('GAIN', 'COST', 'NOT-PREDEFINED');
  M = 1000;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'margValueFuncShapes' must correspond to criteria number")
})

test_that("problem should not be built for incorrect value of margValueFuncShapes", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "INCORRECT");
  M = 1000;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'margValueFuncShapes' must be a vector of either.*")
})

test_that("problem should not be built for null M", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = NULL;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'M' must be greater than 0");
})

test_that("problem should not be built for null eps", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = NULL;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences),
               "Argument 'eps' must be greater than 0 and less than 1");
})

test_that("problem should not be built for M equals nonpositive value", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M1 = 0;
  M2 = -1;
  eps = 0.0001;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M1, strictPreferences=strictPreferences),
               "Argument 'M' must be greater than 0")
  expect_error(buildProblem (alternatives, margValueFuncShapes, M2, strictPreferences=strictPreferences),
               "Argument 'M' must be greater than 0")
})

test_that("problem should not be built for eps less or equals 0 and greater or equals 1", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps1 = -1;
  eps2 = 0;
  eps3 = 1;
  eps4 = 2;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps1, strictPreferences=strictPreferences),
               "Argument 'eps' must be greater than 0 and less than 1");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps2, strictPreferences=strictPreferences),
               "Argument 'eps' must be greater than 0 and less than 1");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps3, strictPreferences=strictPreferences),
               "Argument 'eps' must be greater than 0 and less than 1");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps4, strictPreferences=strictPreferences),
               "Argument 'eps' must be greater than 0 and less than 1");
})

test_that("problem should not be built for no preference given", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps),
               "Did not specified any preferences of DM")
})

test_that("problem should not be built in case that any not null preference is not 2-column matrix", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  
  sp3ColMatrix = matrix(c(1,2,3,4,5,6), ncol=3, byrow=TRUE);
  spVector = c(1,2,3,4);
  
  wp4ColMatrix = matrix(c(1,2,3,4), ncol=4, byrow=TRUE);
  wpVector = c(1,2,3,4);
  
  in1ColMatrix = matrix(c(1,2,3,4,5,6), ncol=1, byrow=TRUE);
  inVector = c(1,2,3,4);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = sp3ColMatrix),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = spVector),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, weakPreferences = wp4ColMatrix),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, weakPreferences = wpVector),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = in1ColMatrix),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = inVector),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
})

test_that("problem should not be built for incorrect indexes of preferences", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  pref1 = matrix(c(0, 1), ncol=2, byrow=TRUE);
  pref2 = matrix(c(4, 1), ncol=2, byrow=TRUE);
  pref3 = matrix(c(1, 4), ncol=2, byrow=TRUE);
  pref4 = matrix(c(1, 0), ncol=2, byrow=TRUE);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = pref1),
               "Argument 'indifferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = pref2),
               "Argument 'indifferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = pref3),
               "Argument 'indifferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = pref4),
               "Argument 'indifferences' must consist of pairs.*");
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, weakPreferences = pref1),
               "Argument 'weakPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, weakPreferences = pref2),
               "Argument 'weakPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, weakPreferences = pref3),
               "Argument 'weakPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, weakPreferences = pref4),
               "Argument 'weakPreferences' must consist of pairs.*");
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = pref1),
               "Argument 'strictPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = pref2),
               "Argument 'strictPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = pref3),
               "Argument 'strictPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = pref4),
               "Argument 'strictPreferences' must consist of pairs.*");
})

test_that("problem should not be built for inconsistency weak preferences", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  inpref1 = matrix(c(1, 2), ncol=2, byrow=TRUE);
  inpref2 = matrix(c(2, 1), ncol=2, byrow=TRUE);
  weakpref = matrix(c(1, 2), ncol=2, byrow=TRUE);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = inpref1, weakPreferences = weakpref),
               "Inconsistency of DM preference information. 'indifferences' and 'weakPreferences' contain.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, indifferences = inpref2, weakPreferences = weakpref),
               "Inconsistency of DM preference information. 'indifferences' and 'weakPreferences' contain.*");
})

test_that("problem should not be built for inconsistency weak preferences", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  inpref1 = matrix(c(1, 2), ncol=2, byrow=TRUE);
  inpref2 = matrix(c(2, 1), ncol=2, byrow=TRUE);
  weakPref = matrix(c(1, 2), ncol=2, byrow=TRUE);
  weakPref = matrix(c(2, 1), ncol=2, byrow=TRUE);
  strictPref = matrix(c(1, 2), ncol=2, byrow=TRUE);
  strictPrefIncorrect = matrix(c(1, 2, 3, 1, 2, 1), ncol=2, byrow=TRUE);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences = strictPrefIncorrect),
               "Inconsistency of DM preference information. 'strictPreferences' contains.*");
})

test_that("problem should be built for proper arguments", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
  weakPreferences = matrix(c(2,3), ncol=2, byrow=TRUE);
  
  problem = buildProblem (alternatives, margValueFuncShapes, M, eps, strictPreferences=strictPreferences,
                          weakPreferences=weakPreferences)
  
  expect_equal(problem$alternatives, alternatives);
  expect_equal(problem$criteriaNumber, ncol(alternatives));
  expect_equal(problem$alternativesNumber, nrow(alternatives));
  expect_equal(problem$margValueFuncShapes, margValueFuncShapes);
  expect_equal(problem$M, M);
})

test_that("lpmodel should be generated for full example problem", {
  alternatives = matrix(c(100, 1000,  9,   110,  300,  800,
                          90,  1100,  11,  120,  310,  801,
                          100, 1500,  12,  100,  340,  803,
                          120, 800,   13,  105,  360,  809), ncol = 6, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST", "NOT_PREDEFINED", "A_TYPE", "V_TYPE", "NON_MON");
  M = 1000;
  eps = 0.0001;
  
  strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
  weakPreferences = matrix(c(2,3), ncol=2, byrow=TRUE);
  indifferences = matrix(c(3,4), ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    eps,
    strictPreferences = strictPreferences,
    weakPreferences = weakPreferences,
    indifferences = indifferences
  )
  
  lpmodel = initLpModel(problem);
  lpmodel = addProblemConstraintsToLpModel(problem, lpmodel);
})

test_that("should returns growing additive value function for gain case", {
  alternatives = matrix(c(0, 50, 100), ncol = 1, byrow=TRUE);
  margValueFuncShapes = c("GAIN");
  M = 1000;
  eps = 0.0001;
  
  strictPreferences = matrix(
    c(3,2,
      3,1,
      2,1),
    ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    eps,
    strictPreferences = strictPreferences
  )
  
  solution = calcSolution(problem);
  expect_that(solution$solutionsNumber, equals(1)); 
  expect_true(solution$additiveValueFunctions[1,1] < solution$additiveValueFunctions[1,2]); 
  expect_true(solution$additiveValueFunctions[1,2] < solution$additiveValueFunctions[1,3]); 
})

test_that("should returns decreasing additive value function for cost case", {
  alternatives = matrix(c(0, 50, 100), ncol = 1, byrow=TRUE);
  margValueFuncShapes = c("COST");
  M = 1000;
  eps = 0.0001;
  
  strictPreferences = matrix(
    c(1,2,
      1,3,
      2,3),
    ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    eps,
    strictPreferences = strictPreferences
  )
  
  solution = calcSolution(problem);
  expect_that(solution$solutionsNumber, equals(1)); 
  expect_true(solution$additiveValueFunctions[1,1] > solution$additiveValueFunctions[1,2]); 
  expect_true(solution$additiveValueFunctions[1,2] > solution$additiveValueFunctions[1,3]); 
})

test_that("should returns growing additive value function for not predefined monotonicity case", {
  alternatives = matrix(c(0, 50, 100), ncol = 1, byrow=TRUE);
  margValueFuncShapes = c("NOT_PREDEFINED");
  M = 1000;
  eps = 0.0001;
  
  strictPreferences = matrix(
    c(3,2,
      3,1,
      2,1),
    ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    eps,
    strictPreferences = strictPreferences
  )
  
  solution = calcSolution(problem);
  expect_that(solution$solutionsNumber, equals(1)); 
  expect_true(solution$additiveValueFunctions[1,1] < solution$additiveValueFunctions[1,2]); 
  expect_true(solution$additiveValueFunctions[1,2] < solution$additiveValueFunctions[1,3]); 
})

test_that("should returns decreasing additive value function for not predefined monotonicity case", {
  alternatives = matrix(c(0, 50, 100), ncol = 1, byrow=TRUE);
  margValueFuncShapes = c("NOT_PREDEFINED");
  M = 1000;
  eps = 0.0001;
  
  strictPreferences = matrix(
    c(1,2,
      1,3,
      2,3),
    ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    eps,
    strictPreferences = strictPreferences
  )
  
  solution = calcSolution(problem);
  expect_that(solution$solutionsNumber, equals(1)); 
  expect_true(solution$additiveValueFunctions[1,1] > solution$additiveValueFunctions[1,2]); 
  expect_true(solution$additiveValueFunctions[1,2] > solution$additiveValueFunctions[1,3]); 
})

test_that("simple example", {
  alternatives = matrix(c(100, 0,
                          0, 50,
                          0, 51), ncol = 2, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 1000;
  eps = 0.0001;
  
  strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    eps,
    strictPreferences = strictPreferences
  )
})

