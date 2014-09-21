#' Build problem
#'
#' This function allows to build instance of the problem.
#' 
#' @param alternatives Matrix of alternatives values for criteria.
#' Criteria arranged in columns, alternatives in rows.
#' Number of alternatives (rows) must be greater than 3.
#' Number of criteria (columns) must be greater than 1.
#' @param margValueFuncShapes Vector of criterion types, which correspond to each criterion.
#' Each value of vector must be one of either "GAIN", "COST", "NOT_PREDEFINED",
#' "A_TYPE", "V_TYPE", "NON_MON", which represent knowledge about the monotonicity for criteria, 
#' respectively criterion is of predefined gain type (GAIN), pre-defined cost type (COST), monotonic,
#' but the type of the monotonicity is not pre-defined (NOT_PREDEFINED), criterion for which the most
#' preferred evaluation is possibly not in the extreme point of evaluation scale (A_TYPE),
#' criterion for which the least preferred evaluation is possibly not in the extreme point
#' of evaluation scale (V_TYPE) and criterion with no prior information on the monotonicity (NON_MON). 
#' Index of each type must correspond to index of column of the criterion in alternatives matrix.
#' @param M Big positive numeric value which is much greater that any value of each alternative.
#' @param strictPreferences Matrix of the strict preferences provided by DM. Each row represents
#' comparision between two alternatives and it means that alternative provided in the first 
#' column is strictly preferred over alternative provided in the second column. Value of each matrix 
#' cell must correspond to index of alternative defined in alternatives matrix.
#' @param weakPreferences Matrix of the weak preferences provided by DM. Each row represents
#' comparision between two alternatives and it means that alternative provided in the first 
#' column is weakly preferred over alternative provided in the second column. Value of each matrix 
#' cell must correspond to index of alternative defined in alternatives matrix.
#' @param indifferences Matrix of the indifferences provided by DM. Each row represents
#' comparision between two alternatives and it means that alternative provided in the first 
#' column is indifferent for alternative provided in the second column. Value of each matrix 
#' cell must correspond to index of alternative defined in alternatives matrix.
#' @return Instance of the problem
#' @examples
#' alternatives = matrix(c(100, 1000,  9,   110,  300,  800,
#'                         90,  1100,  11,  120,  310,  801,
#'                         100, 1500,  12,  100,  340,  803,
#'                         120, 800,   13,  105,  360,  809), ncol = 6, byrow=TRUE);
#' margValueFuncShapes = c("GAIN", "COST", "NOT_PREDEFINED", "A_TYPE", "V_TYPE", "NON_MON");
#' M = 1000000;
#' 
#' strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
#' weakPreferences = matrix(c(2,3), ncol=2, byrow=TRUE);
#' indifferences = matrix(c(3,4), ncol=2, byrow=TRUE);
#' 
#' problem = buildProblem (
#'   alternatives,
#'   margValueFuncShapes,
#'   M,
#'   strictPreferences = strictPreferences,
#'   weakPreferences = weakPreferences,
#'   indifferences = indifferences
#' )
#' @export
buildProblem = function(alternatives, margValueFuncShapes, M, strictPreferences = NULL, weakPreferences = NULL, indifferences = NULL) {
  validateAlternatives(alternatives);
  
  criteriaNumber = ncol(alternatives);
  alternativesNumber = nrow(alternatives);
  
  validateMargValueFuncShapes(margValueFuncShapes, criteriaNumber);
  validateM(M);
  dmPreferences = validateDMPreferences(strictPreferences, weakPreferences, indifferences, alternativesNumber);
  
  problem = list(alternatives = alternatives,
               criteriaNumber = criteriaNumber,
               alternativesNumber = alternativesNumber,
               margValueFuncShapes = margValueFuncShapes,
               alternativesValuesForCriteria = NULL,
               strictPreferences = dmPreferences$strictPreferences,
               weakPreferences = dmPreferences$weakPreferences,
               indifferences = dmPreferences$indifferences,
               M = M);
  problem = initIncreasinglyOrderedValuesForAlternatives(problem);

  return(problem);
}

#' Calculate solution
#'
#' This function allows to calculate solution of the problem
#' 
#' @param problem Instance of the problem
#' @return Result of the LP calculation
#' @examples
#' alternatives = matrix(c(100, 1000,  9,   110,  300,  800,
#'                         90,  1100,  11,  120,  310,  801,
#'                         100, 1500,  12,  100,  340,  803,
#'                         120, 800,   13,  105,  360,  809), ncol = 6, byrow=TRUE);
#' margValueFuncShapes = c("GAIN", "COST", "NOT_PREDEFINED", "A_TYPE", "V_TYPE", "NON_MON");
#' M = 1000000;
#' 
#' strictPreferences = matrix(c(1,2), ncol=2, byrow=TRUE);
#' weakPreferences = matrix(c(2,3), ncol=2, byrow=TRUE);
#' indifferences = matrix(c(3,4), ncol=2, byrow=TRUE);
#' 
#' problem = buildProblem (
#'   alternatives,
#'   margValueFuncShapes,
#'   M,
#'   strictPreferences = strictPreferences,
#'   weakPreferences = weakPreferences,
#'   indifferences = indifferences
#' )
#' lpresult = calcSolution(problem);
#' @export
calcSolution = function(problem) {
  lpmodel = initLpModel(problem);  
  lpmodel = addProblemConstraintsToLpModel(problem, lpmodel);
  
  lpresult = Rglpk_solve_LP(lpmodel$obj, lpmodel$mat, lpmodel$dir, lpmodel$rhs, types = lpmodel$types, max = lpmodel$max);
  
  return(lpresult);
}

### INITIALIZATION

initLpModel = function(problem) {
  matDataTypes = initMatDataTypes();
  matDataTypesValues = initMatDataTypesValues(problem);
  matDataTypesStartIndexes = initMatDataTypesStartIndexes(matDataTypes, matDataTypesValues);
  lpmodel = list(
    obj = NULL,
    mat = NULL,
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
      size = problem$alternativesNumber  * problem$criteriaNumber,
      type = 'B'
    ),
    MON_DIRECTION_BINARY_VARIABLES = list(
      size = problem$alternativesNumber  * problem$criteriaNumber,
      type = 'B'
    ),
    CHANGE_MON_BINARY_VARIABLES = list(
      size = problem$alternativesNumber  * problem$criteriaNumber,
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

initIncreasinglyOrderedValuesForAlternatives = function(problem) {
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
  
  return(problem);
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
    index = index + matDataTypesValues[[currType]]$size;
  }
  return(indexes);
}

### VALIDATION

validateAlternatives = function(alternatives) {
  if (is.null(alternatives) || !is.matrix(alternatives)) {
    stop("Argument 'alternatives' must be non-null matrix of criteria (arranged in columns) and alternatives values for criteria (in rows)")
  }
  if (!(nrow(alternatives) >= 3)) {
    stop("Alternatives number must be greater or equals 3");
  }
}

validateMargValueFuncShapes = function(margValueFuncShapes, criteriaNumber) {
  criteriaTypes = c("GAIN", "COST", "NOT_PREDEFINED", "A_TYPE", "V_TYPE", "NON_MON");
  if (is.null(margValueFuncShapes) || !is.vector(margValueFuncShapes)) {
    stop("Argument 'margValueFuncShapes' must be non-null vector");
  }
  if (length(margValueFuncShapes) != criteriaNumber) {
    stop("Argument 'margValueFuncShapes' must correspond to criteria number");
  }
  if (any(is.na(match(margValueFuncShapes, criteriaTypes)))) {
    stop(paste("Argument 'margValueFuncShapes' must be a vector of either ", paste(criteriaTypes, collapse=", ")," for each criterion."), collapse="");
  }
}

validateM = function(M) {
  if (is.null(M) || !(M > 0)) {
    stop("Argument 'M' must be greater than 0");
  }
}

validateDMPreferences = function(strictPreferences, weakPreferences, indifferences, alternativesNumber) {
  if (is.null(strictPreferences) && is.null(weakPreferences) && is.null(indifferences)) {
    stop("Did not specified any preferences of DM");
  }
  
  if (!((is.null(strictPreferences) || (is.matrix(strictPreferences) && ncol(strictPreferences) == 2))
        && (is.null(weakPreferences) || (is.matrix(weakPreferences) && ncol(weakPreferences) == 2))
        && (is.null(indifferences) || (is.matrix(indifferences) && ncol(indifferences) == 2)))) {
    stop("Arguments 'strictPreferences', 'weakPreferences' and 'indifferences' must be NULL or two-column matrix");
  }
  
  indifferentPairs = set();
  if (!is.null(indifferences)) {
    for (i in nrow(indifferences):1) {
      if (indifferences[i,1] == indifferences[i,2]
          || indifferences[i,1] < 1 || indifferences[i,1] > alternativesNumber
          || indifferences[i,2] < 1 || indifferences[i,2] > alternativesNumber) {
        stop("Argument 'indifferences' must consist of pairs of indexes corresponding to the two alternatives from a given set of alternatives");
      } else if (set_contains_element(indifferentPairs, tuple(indifferences[i,1], indifferences[i,2]))
                 || set_contains_element(indifferentPairs, tuple(indifferences[i,2], indifferences[i,1]))) {
        indifferences = indifferences[-i:-i,];
      } else {
        indifferentPairs = set_union(indifferentPairs, tuple(indifferences[i,1], indifferences[i,2]))
      }
    }
  }
  
  weakPreferencesPairs = set();
  if (!is.null(weakPreferences)) {
    for (i in nrow(weakPreferences):1) {
      if (weakPreferences[i,1] == weakPreferences[i,2]
          || weakPreferences[i,1] < 1 || weakPreferences[i,1] > alternativesNumber
          || weakPreferences[i,2] < 1 || weakPreferences[i,2] > alternativesNumber) {
        stop("Argument 'weakPreferences' must consist of pairs of indexes corresponding to the two alternatives from a given set of alternatives");
      } else if (set_contains_element(weakPreferencesPairs, tuple(weakPreferences[i,1], weakPreferences[i,2]))) {
        weakPreferences = weakPreferences[-i:-i,];
      } else if (set_contains_element(indifferentPairs, tuple(weakPreferences[i,1], weakPreferences[i,2]))
                 || set_contains_element(indifferentPairs, tuple(weakPreferences[i,2], weakPreferences[i,1]))) {
        stop(paste("Inconsistency of DM preference information. 'indifferences' and 'weakPreferences' contain (",
                   weakPreferences[i,1], ", ", weakPreferences[i,2], ") pair"), collapse="");
      } else {
        weakPreferencesPairs = set_union(weakPreferencesPairs, tuple(weakPreferences[i,1], weakPreferences[i,2]))
      }
    }
  }
  strictPreferencesPairs = set();
  if (!is.null(strictPreferences)) {
    for (i in nrow(strictPreferences):1) {
      if (strictPreferences[i,1] == strictPreferences[i,2]
          || strictPreferences[i,1] < 1 || strictPreferences[i,1] > alternativesNumber
          || strictPreferences[i,2] < 1 || strictPreferences[i,2] > alternativesNumber) {
        stop("Argument 'strictPreferences' must consist of pairs of indexes corresponding to the two alternatives from a given set of alternatives");
      } else if (set_contains_element(strictPreferencesPairs, tuple(strictPreferences[i,1], strictPreferences[i,2]))) {
        strictPreferences = strictPreferences[-i:-i,];
      } else if (set_contains_element(strictPreferencesPairs, tuple(strictPreferences[i,2], strictPreferences[i,1]))) {
        stop(paste("Inconsistency of DM preference information. 'strictPreferences' contains (", 
                   strictPreferences[i,1], ", ", strictPreferences[i,2], ") and (",
                   strictPreferences[i,2], ", ", strictPreferences[i,1], ") pairs"), collapse="");
      } else if (set_contains_element(indifferentPairs, tuple(strictPreferences[i,1], strictPreferences[i,2]))
                 || set_contains_element(indifferentPairs, tuple(strictPreferences[i,2], strictPreferences[i,1]))
                 || set_contains_element(weakPreferencesPairs, tuple(strictPreferences[i,1], strictPreferences[i,2]))
                 || set_contains_element(weakPreferencesPairs, tuple(strictPreferences[i,2], strictPreferences[i,1]))) {
        stop(paste("Inconsistency of DM preference information. 'indifferences' or 'weakPreferences' contain (",
                   strictPreferences[i,1], ", ", strictPreferences[i,2], ") pair of 'strictPreferences'"), collapse="");
      } else {
        strictPreferencesPairs = set_union(strictPreferencesPairs, tuple(strictPreferences[i,1], strictPreferences[i,2]))
      }
    }
  }
  
  return (list(strictPreferences = strictPreferences,
               weakPreferences = weakPreferences,
               indifferences = indifferences))
}