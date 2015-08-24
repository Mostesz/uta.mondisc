addObjForAAndVTypeToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  for (p in 2:levelsNumber) {
    lpmodel$obj = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, lpmodel$obj, p, critIdx, 1);
  }
  return(lpmodel);
}

addAAndVTypeMonConstraintsToLpModel = function(problem, lpmodel, isAType, critIdx) {
  levelsNumber = problem$levelNoForCriteria[critIdx]
  for (chPointIdx in 2:levelsNumber) {
    binAndRhsValue = if (isAType) problem$M else -problem$M;
    dir = if (isAType) '>=' else '<=';
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    for (p in 2:chPointIdx) {
      constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, binAndRhsValue);
    }
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
    
    dir = if (isAType) '<=' else '>=';
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    for (p in 2:chPointIdx) {
      constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, binAndRhsValue);
    }
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, binAndRhsValue);
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  for (p in 2:levelsNumber) {
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, 1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 1);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  return(lpmodel);
}

addATypeMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  
  # Normalization to zero
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
  constraintRow = setEpsValueOnConstraintRow(problem, lpmodel, constraintRow, 1)
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, 1);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  # Normalization to one
  constraintRowNo3 = initLpModelMatrixRow(lpmodel);
  constraintRowNo3 = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRowNo3, critIdx, 1);
  constraintRowNo3 = setCharacPointOnConstraintRow(problem, lpmodel, constraintRowNo3, levelsNumber, critIdx, -1);
  
  constraintRowNo4 = initLpModelMatrixRow(lpmodel);
  constraintRowNo4 = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRowNo4, critIdx, 1);
  constraintRowNo4 = setCharacPointOnConstraintRow(problem, lpmodel, constraintRowNo4, levelsNumber, critIdx, -1);
  for (chPointIdx in 2:levelsNumber) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, -problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
    
    constraintRowNo3 = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRowNo3, chPointIdx, critIdx, 1);
    
    constraintRowNo4 = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRowNo4, chPointIdx, critIdx, -1);    
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRowNo3, '>=', 0);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRowNo4, '<=', 0);
  
  return(lpmodel);
}

addVTypeMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  
  # Normalization to zero
  for (chPointIdx in 2:levelsNumber) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, 1);
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 1);
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, 1);
  for (p in 2:levelsNumber) {
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, -1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  # Normalization to one
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', problem$M);
  
  return(lpmodel);
}

setAAndVTypeMonBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'A_AND_V_TYPE_BINARY_VARIABLES', chPointIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getAAndVTypeMonBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'A_AND_V_TYPE_BINARY_VARIABLES', chPointIdx, critIdx);
  
  return(constraintRow[i]);
}

setATypeMonNormalizationZeroBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, critIdx, value) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'A_TYPE_NORMALIZATION_ZERO_BINARY_VARIABLES', critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getATypeMonNormalizationZeroBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, critIdx) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'A_TYPE_NORMALIZATION_ZERO_BINARY_VARIABLES', critIdx);
  
  return(constraintRow[i]);
}

setVTypeMonNormalizationOneBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, critIdx, value) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'V_TYPE_NORMALIZATION_ONE_BINARY_VARIABLES', critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getVTypeMonNormalizationOneBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, critIdx) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'V_TYPE_NORMALIZATION_ONE_BINARY_VARIABLES', critIdx);
  
  return(constraintRow[i]);
}