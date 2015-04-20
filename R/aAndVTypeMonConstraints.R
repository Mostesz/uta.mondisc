addObjForAAndVTypeToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  for (p in 2:problem$alternativesNumber) {
    lpmodel$obj = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, lpmodel$obj, p, critIdx, 1);
  }
  return(lpmodel);
}

addAAndVTypeMonConstraintsToLpModel = function(problem, lpmodel, isAType, critIdx) {
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  for (i in 2:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues[i, 'index'];
    prevAltIdx = currCriterionAlternativesValues[i - 1, 'index'];
    binAndRhsValue = if (isAType) problem$M else -problem$M;
    
    dir = if (isAType) '>=' else '<=';
    constraintRow = initLpModelMatrixRow(lpmodel);
    for (p in 2:i) {
      constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, binAndRhsValue);
    }
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
    
    dir = if (isAType) '<=' else '>=';
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    for (p in 2:i) {
      constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, binAndRhsValue);
    }
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, binAndRhsValue);
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  for (p in 2:problem$alternativesNumber) {
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, 1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 1);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  return(lpmodel);
}

setAAndVTypeMonBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'A_AND_V_TYPE_BINARY_VARIABLES', altIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getAAndVTypeMonBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'A_AND_V_TYPE_BINARY_VARIABLES', altIdx, critIdx);
  
  return(constraintRow[i]);
}

addATypeMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  lastAltIdx = currCriterionAlternativesValues[problem$alternativesNumber, 'index'];
  firstAltIdx = currCriterionAlternativesValues[1, 'index'];
  
  # Normalization to zero
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, -1);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, -1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, 1);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, 1);
  constraintRow = setATypeMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  # Normalization to one
  constraintRowNo3 = initLpModelMatrixRow(lpmodel);
  constraintRowNo3 = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRowNo3, critIdx, 1);
  constraintRowNo3 = setCharacPointOnConstraintRow(problem, lpmodel, constraintRowNo3, lastAltIdx, critIdx, -1);
  
  constraintRowNo4 = initLpModelMatrixRow(lpmodel);
  constraintRowNo4 = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRowNo4, critIdx, 1);
  constraintRowNo4 = setCharacPointOnConstraintRow(problem, lpmodel, constraintRowNo4, lastAltIdx, critIdx, -1);
  for (i in 2:(problem$alternativesNumber)) {
    altIdx = currCriterionAlternativesValues[i, 'index'];
    prevAltIdx = currCriterionAlternativesValues[i - 1, 'index'];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, -problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
    
    constraintRowNo3 = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRowNo3, altIdx, critIdx, 1);
    
    constraintRowNo4 = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRowNo4, altIdx, critIdx, -1);    
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRowNo3, '>=', 0);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRowNo4, '<=', 0);
  
  return(lpmodel);
}

addVTypeMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  firstAltIdx = currCriterionAlternativesValues[1, 'index'];
  lastAltIdx = currCriterionAlternativesValues[problem$alternativesNumber, 'index'];
  
  # Normalization to zero
  for (i in 2:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues[i, 'index'];
    prevAltIdx = currCriterionAlternativesValues[i - 1, 'index'];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, 1);
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 1);
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, 1);
  for (p in 2:problem$alternativesNumber) {
    constraintRow = setAAndVTypeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, p, critIdx, -1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  # Normalization to one
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, -1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, lastAltIdx, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, firstAltIdx, critIdx, -1);
  constraintRow = setVTypeMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', problem$M);
  
  return(lpmodel);
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