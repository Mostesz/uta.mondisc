#' @param alternatives Matrix of n alternatives. Each alternative represents row of m evaluation criteria.
#' @param margValueFuncShapes Represents informations about shapes (monotonicity) of marginal value functions.
#' Each element must be one of "GAIN", "COST", "NOT-PREDEFINED", "A-TYPE", "V-TYPE" or "NON-MON".
buildProblem = function(alternatives, margValueFuncShapes, M) {
  criteriaNumber = ncol(alternatives);
  alternativesNumber = nrow(alternatives)
  marginalFunctionsMons = match(margValueFuncShapes, c("GAIN", "COST", "NOT-PREDEFINED", "A-TYPE", "V-TYPE", "NON-MON"));
  if (any(is.na(marginalFunctionsMons))) {
    stop("Argument 'margValueFuncShapes' must be either 'GAIN', 'COST', 'NOT-PREDEFINED', 'A-TYPE', 'V-TYPE' or 'NON-MON'.")
  }
  if (!(alternativesNumber >= 3)) stop("Alternatives number must be greater or equals 3");
  if (!(M > 0)) stop("Argument 'M' must be greater than 0");
  
  return (list(alternatives = alternatives,
               criteriaNumber = criteriaNumber,
               alternativesNumber = alternativesNumber,
               margValueFuncShapes = margValueFuncShapes,
               alternativesValuesForCriteria = NULL,
               strictPreferences = NULL,
               weakPreferences = NULL,
               indifferences = NULL,
               M = M))
}

addStrictPreferences = function(problem, ...) {
  if ()
  
  problem$strictPreferences = addElementsToMat(problem$strictPreferences, 2, ..., DISTINCT = TRUE);
  return(problem);
}

deleteStrictPreferences = function(problem, ...) {
  problem$strictPreferences = deleteElementsFromMat(problem$strictPreferences, 2, ..., DISTINCT = TRUE);
  return(problem); 
}

addWeakPreferences = function(problem, ...) {
  problem$weakPreferences = addElementsToMat(problem$weakPreferences, 2, ..., DISTINCT = TRUE);
  return(problem);
}

deleteWeakPreferences = function(problem, ...) {
  problem$weakPreferences = deleteElementsFromMat(problem$weakPreferences, 2, ..., DISTINCT = TRUE);
  return(problem);
}

addIndifferences = function(problem, ...) {
  problem$indifferences = addElementsToMat(problem$indifferences, 2, ..., DISTINCT = TRUE);
  return(problem);
}

deleteIndifferences = function(problem, ...) {
  problem$indifferences = deleteElementsFromMat(problem$indifferences, 2, ..., DISTINCT = TRUE);
  return(problem);
}

calcSolution = function(problem) {
  if (strictPreferences == NULL && weakPreferences == NULL && indifferences == NULL
      && length(strictPreferences) == 0 && length(weakPreferences) == 0 && length(indifferences) == 0) {
    stop('DM preference information was not provided.')
  }
  
  lpmodel = initLpModel(problem);
  
  #creating increasingly ordered values of Xj
  problem$alternativesValuesForCriteria = list();
  criteriaNumber = problem$criteriaNumber;
  alternativesNumber = problem$alternativesNumber;
  
  for (j in 1:criteriaNumber) {
    currCriterionAltValues = data.frame(value = numeric(alternativesNumber), index = numeric(alternativesNumber), stringsAsFactors = FALSE);
    for (i in 1:alternativesNumber) {
      currCriterionAltValues$value[i] = problem$alternatives[i, j];
      currCriterionAltValues$index[i] = i;
    }
    currCriterionAltValues = currCriterionAltValues[with(currCriterionAltValues, order(value)), ]
    problem$alternativesValuesForCriteria = append(problem$alternativesValuesForCriteria, list(currCriterionAltValues));
  }
  
  #TODO add validation for strictPreferences, weakPreferences, indifferences (inconsistency)
  
  lpmodel = addProblemConstraintsToLpModel(problem, lpmodel);
  
  lpresult = Rglpk_solve_LP(lpmodel$obj, lpmodel$mat, lpmodel$dir, lpmodel$rhs, types = lpmodel$types, max = lpmodel$max);
  
  return(lpresult);
}

###

initLpModel = function() {
  matDataTypes = initMatDataTypes();
  matDataTypesValues = initMatDataTypesValues(problem);
  matDataTypesStartIndexes = initMatDataTypesStartIndexes(matDataTypes, matDataTypesValues);
  lpmodel = list(
    obj = NULL,
    mat = matrix(),
    dir = c(),
    rhs = c(),
    types = c(),
    max = FALSE,
    matDataTypes = matDataTypes,
    matDataTypesValues = matDataTypesValues,
    matDataTypesStartIndexes = matDataTypesStartIndexes
  );
  lpmodel$obj = initLpModelObj(lpmodel);
  
  return(lpmodel);
}

initMatDataTypes = function() {
  return(c(
    "CHARACT_POINTS",
    "NOT_PREDEFINED_MON_CHARACT_POINTS_GAIN_CASE",
    "NOT_PREDEFINED_MON_CHARACT_POINTS_COST_CASE",
    "NOT_PREDEFINED_MON_COST_BINARY_VARIABLES",
    "A_AND_V_TYPE_BINARY_VARIABLES",
    "MON_DIRECTION_BINARY_VARIABLES",
    "CHANGE_MON_BINARY_VARIABLES",
    "A_TYPE_NORMALIZATION_ZERO_BINARY_VARIABLES",
    "NON_MON_NORMALIZATION_ZERO_BINARY_VARIABLES",
    "BEST_EVALUATIONS_ON_CRITERIA",
    "V_TYPE_NORMALIZATION_ONE_BINARY_VARIABLES",
    "NON_MON_NORMALIZATION_ONE_BINARY_VARIABLES"
  ));
}

initMatDataTypesValues = function(problem) {
  return(list(
    CHARACT_POINTS = list(
      size = problem$alternativesNumber * problem$criteriaNumber,
      type = 'I'
    ),
    NOT_PREDEFINED_MON_CHARACT_POINTS_GAIN_CASE = list(
      size = problem$alternativesNumber * problem$criteriaNumber,
      type = 'I'
    ),
    NOT_PREDEFINED_MON_CHARACT_POINTS_COST_CASE = list(
      size = problem$alternativesNumber * problem$criteriaNumber,
      type = 'I'
    ),
    NOT_PREDEFINED_MON_COST_BINARY_VARIABLES = list(
      size = problem$criteriaNumber,
      type = 'B'
    ),
    A_AND_V_TYPE_BINARY_VARIABLES = list(
      size = (problem$alternativesNumber - 1)  * problem$criteriaNumber,
      type = 'B'
    ),
    MON_DIRECTION_BINARY_VARIABLES = list(
      size = (problem$alternativesNumber - 1)  * problem$criteriaNumber,
      type = 'B'
    ),
    CHANGE_MON_BINARY_VARIABLES = list(
      size = (problem$alternativesNumber - 2)  * problem$criteriaNumber,
      type = 'B'
    ),
    A_TYPE_NORMALIZATION_ZERO_BINARY_VARIABLES = list(
      size = problem$criteriaNumber,
      type = 'B'
    ),
    NON_MON_NORMALIZATION_ZERO_BINARY_VARIABLES = list(
      size = problem$alternativesNumber * problem$criteriaNumber,
      type = 'B'
    ),
    BEST_EVALUATIONS_ON_CRITERIA = list(
      size = problem$criteriaNumber,
      type = 'I'
    ),
    V_TYPE_NORMALIZATION_ONE_BINARY_VARIABLES = list(
      size = problem$criteriaNumber,
      type = 'B'
    ),
    NON_MON_NORMALIZATION_ONE_BINARY_VARIABLES = list(
      size = problem$alternativesNumber * problem$criteriaNumber,
      type = 'B'
    )
  ));
}

initMatDataTypesStartIndexes = function(matDataTypes, matDataTypesValues) {
  matDataTypes = c(matDataTypes, 'END'); # END value will be store total size of all structures
  indexes = list();
  index = 1;
  for(i in 1:length(matDataTypes)) {
    currType = matDataTypes[i];
    indexes[currType] = index;
    if (currType == 'END') {
      break;
    }
    index = index + matDataTypesValues[currType]$size;
  }
  return(indexes);
}