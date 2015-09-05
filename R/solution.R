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

solveLpRglpk = function(problem, lpmodel) {
  lpresult = Rglpk_solve_LP(lpmodel$obj, lpmodel$mat, lpmodel$dir, lpmodel$rhs, types = lpmodel$types, max = lpmodel$max);
  return(lpresult);
}

solveLpRoi = function(problem, lpmodel) {
  obj <- L_objective(lpmodel$obj)
  roiConst <- L_constraint(L=lpmodel$mat, dir=lpmodel$dir, rhs=lpmodel$rhs)
  lp <- OP(objective=obj, constraints=roiConst, maximum=lpmodel$max, types=lpmodel$types)
  res <- ROI_solve(lp, .solver)
  return(list(solution=res$solution, status=res$status$code))
}

getPossibleAndNecessaryRelations = function(problem, lpmodel, lpresult) {
  lpmodel = forbidSolution(lpmodel, lpresult$solution);
  lpmodel$obj = NULL
  
  necessaryRelations = NULL
  possibleRelations = NULL
  for (i in 1:problem$alternativesNumber) {
    for(j in 1:problem$alternativesNumber) {
      if (checkRelation(problem, lpmodel, i, j, TRUE)) {
        necessaryRelations = addElementsToMatrix(necessaryRelations, 2, c(i, j))
      }
      if (checkRelation(problem, lpmodel, i, j, FALSE)) {
        possibleRelations = addElementsToMatrix(possibleRelations, 2, c(i, j))
      }
    }
  }
  
  return(list("possible" = possibleRelations, "necessary" = necessaryRelations));
}

checkRelation = function(problem, lpmodel, left.alternative.idx, right.alternative.idx, necessary) {
  if (left.alternative.idx == right.alternative.idx) {
    return(FALSE)
  }
  
  obj = initLpModelMatrixRow(lpmodel);
  obj = setEpsValueOnConstraintRow(problem, lpmodel, obj, 1)
  lpmodel$obj = obj
  lpmodel$max = TRUE
  
  constraintRow = initLpModelMatrixRow(lpmodel)
  if (necessary) {
    constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, right.alternative.idx, 1)
    constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, left.alternative.idx, -1)
    constraintRow = setEpsValueOnConstraintRow(problem, lpmodel, constraintRow, -1)
  } else {
    constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, left.alternative.idx, 1)
    constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, right.alternative.idx, -1)
    constraintRow = setEpsValueOnConstraintRow(problem, lpmodel, constraintRow, -1)
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
  
  lpresult = solveLpRglpk(problem, lpmodel)
  
  epsValue = getEpsValueFromConstraintRow(problem, lpmodel, lpresult$solution)
  necessaryCondition = necessary == TRUE && (lpresult$status != 0 || epsValue <= 0)
  possibleCondition = necessary == FALSE && lpresult$status == 0 && epsValue >= 0
  if (necessaryCondition || possibleCondition) {
    return(TRUE)
  }
  return(FALSE)
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