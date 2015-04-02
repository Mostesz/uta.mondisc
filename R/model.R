###

initLpModelMatrixRow = function(lpmodel) {  
  result = rep(0, getLpModelMatrixRowSize(lpmodel));
  return(result);
}

initLpModelObj = function(lpmodel) {
  return(initLpModelMatrixRow(lpmodel));
}

addTypesToLpModel = function(lpmodel) {
  for (dataIdx in 1:length(lpmodel$matDataTypesValues)) {
    dataNumber = lpmodel$matDataTypesValues[[dataIdx]]$size;
    dataType = lpmodel$matDataTypesValues[[dataIdx]]$type;
    
    lpmodel$types = c(lpmodel$types, rep(dataType, dataNumber));
  }
  
  return(lpmodel);
}

forbidSolution = function(lpmodel, solution) {
  constraint = vector(mode = 'numeric', length = length(solution));
  for (dataType in c('A_AND_V_TYPE_BINARY_VARIABLES', 'CHANGE_MON_BINARY_VARIABLES')) {
    startIdx = getLpModelMatrixRowStartIdx(lpmodel, dataType);
    endIdx = getLpModelMatrixSizeForDataType(lpmodel, dataType) + startIdx - 1;
    for (i in startIdx:endIdx) {
      constraint[i] = solution[i];
    }
  }
    
  lpmodel = addConstraintToLpModel(lpmodel, constraint, '<=', sum(constraint) - 1);
  
  return(lpmodel);
}

addConstraintToLpModel = function(lpmodel, constraintRow, dir, rhs) {
  lpmodel$mat = addElementsToMatrix(lpmodel$mat, length(constraintRow), constraintRow);
  lpmodel$dir = c(lpmodel$dir, dir);
  lpmodel$rhs = c(lpmodel$rhs, rhs);
  
  return(lpmodel);
}

addProblemConstraintsToLpModel = function(problem, lpmodel) {
  addHolisticJudgmentsConstraintsToLpModel(problem, lpmodel, problem$strictPreferences, '>');
  addHolisticJudgmentsConstraintsToLpModel(problem, lpmodel, problem$weakPreferences, '>=');
  addHolisticJudgmentsConstraintsToLpModel(problem, lpmodel, problem$indifferences, '==');
  
  normToOneConstraintRow = initLpModelMatrixRow(lpmodel);
  for (critIdx in 1:problem$criteriaNumber) {
    normToOneConstraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, normToOneConstraintRow, critIdx, 1);
    
    switch(problem$margValueFuncShapes[critIdx],
           GAIN={
             lpmodel = addPredefinedMonConstraintsToLpModel(problem, lpmodel, TRUE, critIdx)
             lpmodel = addPredefinedMonNormalizationToLpModel(problem, lpmodel, TRUE, critIdx)
           },
           COST={
             lpmodel = addPredefinedMonConstraintsToLpModel(problem, lpmodel, FALSE, critIdx)
             lpmodel = addPredefinedMonNormalizationToLpModel(problem, lpmodel, FALSE, critIdx)
           },
           NOT_PREDEFINED={
             lpmodel = addNotPredefinedMonConstraintsToLpModel(problem, lpmodel, critIdx)
             lpmodel = addNotPredefinedMonNormalizationToLpModel(problem, lpmodel, critIdx)
           },
           A_TYPE={
             lpmodel = addAAndVTypeMonConstraintsToLpModel(problem, lpmodel, TRUE, critIdx)
             lpmodel = addATypeMonNormalizationToLpModel(problem, lpmodel, critIdx)
             lpmodel = addObjForAAndVTypeToLpModel(problem, lpmodel, critIdx)
           },
           V_TYPE={
             lpmodel = addAAndVTypeMonConstraintsToLpModel(problem, lpmodel, FALSE, critIdx)
             lpmodel = addVTypeMonNormalizationToLpModel(problem, lpmodel, critIdx)
             lpmodel = addObjForAAndVTypeToLpModel(problem, lpmodel, critIdx)
           },
           NON_MON={
             lpmodel = addNonMonConstraintsToLpModel(problem, lpmodel, critIdx)
             lpmodel = addNonMonNormalizationToLpModel(problem, lpmodel, critIdx)
             lpmodel = addObjForNonMonTypeToLpModel(problem, lpmodel, critIdx)
           });
  }
  lpmodel = addConstraintToLpModel(lpmodel, normToOneConstraintRow, '==', 1);
  
  lpmodel = addTypesToLpModel(lpmodel);
  
  return(lpmodel);
}

getIndexForDataTypeByCritAndAltIdx = function(problem, lpmodel, dataType, altIdx, critIdx,
                                            startAltIdx = 1, endAltIdx = problem$alternativesNumber,
                                            startCritIdx = 1, endCritIdx = problem$criteriaNumber) {
  stopifnot(critIdx >= startCritIdx);
  stopifnot(critIdx <= endCritIdx);
  stopifnot(altIdx  >= startAltIdx);
  stopifnot(altIdx  <= endAltIdx);
  
  startIdx = getLpModelMatrixRowStartIdx(lpmodel, dataType);
  i = problem$criteriaNumber * (altIdx - startAltIdx) + critIdx;
  
  return(startIdx + i - 1);
}

getIndexForDataTypeByCritIdx = function(problem, lpmodel, dataType, critIdx,
                                              startCritIdx = 1, endCritIdx = problem$criteriaNumber) {
  stopifnot(critIdx >= startCritIdx);
  stopifnot(critIdx <= endCritIdx);
  
  startIdx = getLpModelMatrixRowStartIdx(lpmodel, dataType);
  i = critIdx;
  
  return(startIdx + i - 1);
}

getIndexForDataTypeByAltIdx = function(problem, lpmodel, dataType, altIdx, critIdx,
                                              startAltIdx = 1, endAltIdx = problem$alternativesNumber) {
  stopifnot(altIdx  >= startAltIdx);
  stopifnot(altIdx  <= endAltIdx);
  
  startIdx = getLpModelMatrixRowStartIdx(lpmodel, dataType);
  i = altIdx;
  
  return(startIdx + i - 1);
}

getLpModelMatrixSizeForDataType = function(lpmodel, dataType = 'END') {
  validateDataType(lpmodel, dataType);
  return(lpmodel$matDataTypesValues[[dataType]]$size);
}

getLpModelMatrixRowStartIdx = function(lpmodel, dataType = 'END') {
  validateDataType(lpmodel, dataType);
  return(lpmodel$matDataTypesStartIndexes[[dataType]]);
}

getLpModelMatrixRowSize = function(lpmodel) {
  return(getLpModelMatrixRowStartIdx(lpmodel)-1);
}

validateDataType = function(lpmodel, dataType) {
  if (dataType != 'END') {
    type = match(dataType, lpmodel$matDataTypes);
    if (any(is.na(type))) {
      stop(paste("Argument 'dataType' must be either ", paste(lpmodel$matDataTypes,collapse=", ")));
    }
  }
}