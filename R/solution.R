buildSolution = function(finalCriteriaTypes = NULL, solutionsMat = NULL, additiveValueFunctions = NULL, preferenceRelations = NULL) {
  solution = list(
    finalCriteriaTypes = finalCriteriaTypes,
    solutionsMat = solutionsMat,
    additiveValueFunctions = additiveValueFunctions
  );

  if (!is.null(solutionsMat)) {
    solution$solutionsNumber = nrow(solutionsMat);
  } else {
    solution$solutionsNumber = 0;
  }
  
  if (!is.null(preferenceRelations)) {
    solution$possibleRelations = preferenceRelations$possible;
    solution$necessaryRelations = preferenceRelations$necessary;
  } else {
    solution$possibleRelations = NULL;
    solution$necessaryRelations = NULL;
  }
  
  return(solution);
}

getFinalCriteriaTypes = function(problem, lpmodel, solutionsMat) {
  result = matrix(data=NA, nrow=nrow(solutionsMat), ncol=problem$criteriaNumber);
  for (solutionIdx in 1:nrow(solutionsMat)) {
    for (critIdx in 1:problem$criteriaNumber) {
      if (problem$margValueFuncShapes[critIdx] %in% c('GAIN', 'COST')) {
        result[solutionIdx, critIdx] = problem$margValueFuncShapes[critIdx]
      } else if (problem$margValueFuncShapes[critIdx] == 'NOT_PREDEFINED') {
        result[solutionIdx, critIdx] = getFinalCriteriaTypeForNotPredefinedCase(problem, lpmodel, solutionsMat, solutionIdx, critIdx);
      } else if (problem$margValueFuncShapes[critIdx] == 'A_TYPE') {
        result[solutionIdx, critIdx] = getFinalCriteriaTypeForAAndVTypeCase(problem, lpmodel, solutionsMat, solutionIdx, critIdx, TRUE);
      } else if (problem$margValueFuncShapes[critIdx] == 'V_TYPE') {
        result[solutionIdx, critIdx] = getFinalCriteriaTypeForAAndVTypeCase(problem, lpmodel, solutionsMat, solutionIdx, critIdx, FALSE);
      } else if (problem$margValueFuncShapes[critIdx] == 'NON_MON') {
        result[solutionIdx, critIdx] = 'NON_MON'
      }
    }
  }
  return(result);
}

getFinalCriteriaTypeForNotPredefinedCase = function(problem, lpmodel, solutionsMat, solutionIdx, critIdx) {
  binaryValue = getNotPredefinedMonCostBinaryVarFromConstraintRow(problem, lpmodel, solutionsMat[solutionIdx,], critIdx);
  if (binaryValue == 1) {
    return('COST');
  } else if (binaryValue == 0) {
    return('GAIN');
  }
}

getFinalCriteriaTypeForAAndVTypeCase = function(problem, lpmodel, solutionsMat, solutionIdx, critIdx, isAType) {
  for (altIdx in 2:problem$alternativesNumber) {
    if (getAAndVTypeMonBinaryVarFromConstraintRow(problem, lpmodel, solutionsMat[solutionIdx,], altIdx, critIdx) == 1) {
      if (altIdx == 2) {
        return(ifelse(isAType, 'COST', 'GAIN'));
      } else {
        return(ifelse(isAType, 'A_TYPE', 'V_TYPE'));
      }
    }
  }
  return(ifelse(isAType, 'GAIN', 'COST'));
}

solveLP = function(problem, lpmodel) {
  #for (constraintIdx in 1:length(lpmodel$dir)) {
  #  if (lpmodel$dir[constraintIdx] == ">") {
  #    lpmodel$dir[constraintIdx] = ">=";
  #    lpmodel$rhs[constraintIdx] = lpmodel$rhs[constraintIdx] + problem$eps;
  #  } else if (lpmodel$dir[constraintIdx] == "<") {
  #    lpmodel$dir[constraintIdx] = "<=";
  #    lpmodel$rhs[constraintIdx] = lpmodel$rhs[constraintIdx] - problem$eps;
  #  }
  #}
  lpresult = Rglpk_solve_LP(lpmodel$obj, lpmodel$mat, lpmodel$dir, lpmodel$rhs, types = lpmodel$types, max = lpmodel$max);
  return(lpresult);
}

getPossibleAndNecessaryRelations = function(additiveValueFunctions) {
  possibleRelations = list();
  necessaryRelations = list();
  
  for (i in 1:ncol(additiveValueFunctions)) {
    for (j in 1:ncol(additiveValueFunctions)) {
      if (i != j) {
        foundPossibleRelation = FALSE;
        foundNecessaryRelation = TRUE;
        
        for (solIdx in 1:nrow(additiveValueFunctions)) {
          utilityA = additiveValueFunctions[solIdx, i];
          utilityB = additiveValueFunctions[solIdx, j];
          if(utilityA >= utilityB) {
            foundPossibleRelation = TRUE;
          }
          if (utilityB > utilityA) {
            foundNecessaryRelation = FALSE;
          }
        }
        
        if (foundPossibleRelation) {
          possibleRelations = append(possibleRelations, list(c(i, j)));
        }
        if (foundNecessaryRelation) {
          necessaryRelations = append(necessaryRelations, list(c(i, j)));
        }
      }
    }
  }
  
  return(list("possible" = possibleRelations, "necessary" = necessaryRelations));
}

calcAdditiveValueFunctions = function(problem, lpmodel, solutionsMat) {
  additiveValueFunctions = matrix(nrow = nrow(solutionsMat), ncol = problem$alternativesNumber);
  for (solutionIdx in 1:nrow(solutionsMat)) {
    for (altIdx in 1:problem$alternativesNumber) {
      alternativeValue = 0;
      for (critIdx in 1:problem$criteriaNumber) {
        altValueForCrit = getAlternativeValueFromConstraintRow(problem, lpmodel, solutionsMat[solutionIdx,], altIdx, critIdx)
        alternativeValue = alternativeValue + altValueForCrit
      }
      additiveValueFunctions[solutionIdx, altIdx] = alternativeValue;
    }
  }
  return(additiveValueFunctions);
}

printSolution = function(lpmodel, solution) {
  print("optimum:");
  print(solution);
  for (dataType in lpmodel$matDataTypes) {
    startIdx = lpmodel$matDataTypesStartIndexes[[dataType]];
    endIdx = lpmodel$matDataTypesValues[[dataType]]$size + startIdx - 1;
    print(dataType);
    print(solution[startIdx:endIdx]);
    print("");
  }
  print("");
}