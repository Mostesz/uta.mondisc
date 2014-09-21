context("Main functionality")

test_that("problem should not be built for alternatives number less than 3", {
  alternatives = matrix(c(100, 5, 120, 3), nrow = 2, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Alternatives number must be greater or equals 3");
})

test_that("problem should not be built for null alternative", {
  alternatives = NULL;
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Argument 'alternatives' must be non-null matrix of criteria.*");
})

test_that("problem should not be built for null alternative", {
  alternatives = c(1,2,3,4);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Argument 'alternatives' must be non-null matrix of criteria.*");
})

test_that("problem should not be built for null margValueFuncShapes", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = NULL;
  M = 10000;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Argument 'margValueFuncShapes' must be non-null vector")
})

test_that("problem should not be built in case that margValueFuncShapes size is not equal to criteria number", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c('GAIN', 'COST', 'NOT-PREDEFINED');
  M = 10000;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Argument 'margValueFuncShapes' must correspond to criteria number")
})

test_that("problem should not be built for incorrect value of margValueFuncShapes", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "INCORRECT");
  M = 10000;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Argument 'margValueFuncShapes' must be a vector of either.*")
})

test_that("problem should not be built for null M", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = NULL;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences),
               "Argument 'M' must be greater than 0");
})

test_that("problem should not be built for M equals nonpositive value", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M1 = 0;
  M2 = -1;
  strictPreferences = c(1,2);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M1, strictPreferences=strictPreferences),
               "Argument 'M' must be greater than 0")
  expect_error(buildProblem (alternatives, margValueFuncShapes, M2, strictPreferences=strictPreferences),
               "Argument 'M' must be greater than 0")
})

test_that("problem should not be built for no preference given", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M),
               "Did not specified any preferences of DM")
})

test_that("problem should not be built in case that any not null preference is not 2-column matrix", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  
  sp3ColMatrix = matrix(c(1,2,3,4,5,6), ncol=3, byrow=TRUE);
  spVector = c(1,2,3,4);
  
  wp4ColMatrix = matrix(c(1,2,3,4), ncol=4, byrow=TRUE);
  wpVector = c(1,2,3,4);
  
  in1ColMatrix = matrix(c(1,2,3,4,5,6), ncol=1, byrow=TRUE);
  inVector = c(1,2,3,4);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = sp3ColMatrix),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = spVector),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, weakPreferences = wp4ColMatrix),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, weakPreferences = wpVector),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = in1ColMatrix),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = inVector),
               "Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
})

test_that("problem should not be built for incorrect indexes of preferences", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  pref1 = matrix(c(0, 1), ncol=2, byrow=TRUE);
  pref2 = matrix(c(4, 1), ncol=2, byrow=TRUE);
  pref3 = matrix(c(1, 4), ncol=2, byrow=TRUE);
  pref4 = matrix(c(1, 0), ncol=2, byrow=TRUE);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = pref1),
               "Argument 'indifferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = pref2),
               "Argument 'indifferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = pref3),
               "Argument 'indifferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = pref4),
               "Argument 'indifferences' must consist of pairs.*");
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, weakPreferences = pref1),
               "Argument 'weakPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, weakPreferences = pref2),
               "Argument 'weakPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, weakPreferences = pref3),
               "Argument 'weakPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, weakPreferences = pref4),
               "Argument 'weakPreferences' must consist of pairs.*");
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = pref1),
               "Argument 'strictPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = pref2),
               "Argument 'strictPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = pref3),
               "Argument 'strictPreferences' must consist of pairs.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = pref4),
               "Argument 'strictPreferences' must consist of pairs.*");
})

test_that("problem should not be built for inconsistency weak preferences", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  inpref1 = matrix(c(1, 2), ncol=2, byrow=TRUE);
  inpref2 = matrix(c(2, 1), ncol=2, byrow=TRUE);
  weakpref = matrix(c(1, 2), ncol=2, byrow=TRUE);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = inpref1, weakPreferences = weakpref),
               "Inconsistency of DM preference information. 'indifferences' and 'weakPreferences' contain.*");
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, indifferences = inpref2, weakPreferences = weakpref),
               "Inconsistency of DM preference information. 'indifferences' and 'weakPreferences' contain.*");
})

test_that("problem should not be built for inconsistency weak preferences", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  inpref1 = matrix(c(1, 2), ncol=2, byrow=TRUE);
  inpref2 = matrix(c(2, 1), ncol=2, byrow=TRUE);
  weakPref = matrix(c(1, 2), ncol=2, byrow=TRUE);
  weakPref = matrix(c(2, 1), ncol=2, byrow=TRUE);
  strictPref = matrix(c(1, 2), ncol=2, byrow=TRUE);
  strictPrefIncorrect = matrix(c(1, 2, 3, 1, 2, 1), ncol=2, byrow=TRUE);
  
  expect_error(buildProblem (alternatives, margValueFuncShapes, M, strictPreferences = strictPrefIncorrect),
               "Inconsistency of DM preference information. 'strictPreferences' contains.*");
})

test_that("problem should be built for proper arguments", {
  alternatives = matrix(c(100, 5, 120, 3, 98, 4), nrow = 3, byrow=TRUE);
  margValueFuncShapes = c("GAIN", "COST");
  M = 10000;
  strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
  weakPreferences = matrix(c(2,3), ncol=2, byrow=TRUE);
  
  problem = buildProblem (alternatives, margValueFuncShapes, M, strictPreferences=strictPreferences,
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
  M = 1000000;
  
  strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
  weakPreferences = matrix(c(2,3), ncol=2, byrow=TRUE);
  indifferences = matrix(c(3,4), ncol=2, byrow=TRUE);
  
  problem = buildProblem (
    alternatives,
    margValueFuncShapes,
    M,
    strictPreferences = strictPreferences,
    weakPreferences = weakPreferences,
    indifferences = indifferences
  )
  
  lpmodel = initLpModel(problem);
  lpmodel = addProblemConstraintsToLpModel(problem, lpmodel);
  
#   for (i in 1:length(lpmodel$matDataTypes)) {
#     currType = lpmodel$matDataTypes[i];
#     startIdx = matDataTypesStartIndexes[[currType]];
#     size = matDataTypesValues[[currType]]$size;
#     print("lol");
#     print(startIdx);
#     print(size);
#     
#     write.csv(lpmodel$mat[,startIdx:(startIdx + size-1)], file = currType)
#   }
})

