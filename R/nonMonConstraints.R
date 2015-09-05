addObjForNonMonTypeToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  for (chPointIdx in 3:levelsNumber) {
    lpmodel$obj = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, lpmodel$obj, chPointIdx, critIdx, 1)
  }
  return(lpmodel);
}

addNonMonConstraintsToLpModel = function(problem, lpmodel, critIdx) {
  levelsNumber = problem$levelNoForCriteria[critIdx]
  for (chPointIdx in 2:levelsNumber) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, -problem$M);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, -problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    
    if (chPointIdx != 2) {
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
      constraintRow = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, problem$M);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
      
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
      constraintRow = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, -problem$M);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    }
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, 1, critIdx, 1);
  constraintRow = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, 2, critIdx, 1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  return(lpmodel);
}

setMonDirectionBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'MON_DIRECTION_BINARY_VARIABLES', chPointIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getMonDirectionBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'MON_DIRECTION_BINARY_VARIABLES', chPointIdx, critIdx);
  return(constraintRow[i]);
}

setChangeMonBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'CHANGE_MON_BINARY_VARIABLES', chPointIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getChangeMonBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'CHANGE_MON_BINARY_VARIABLES', chPointIdx, critIdx);
  return(constraintRow[i]);
}

addNonMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  
  # Normalization to zero
  binVariableConstraintRow = initLpModelMatrixRow(lpmodel);
  for (chPointIdx in 1:levelsNumber) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setNonMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
    
    binVariableConstraintRow = setNonMonNormalizationZeroBinaryVarOnConstraintRow(problem, lpmodel, binVariableConstraintRow, chPointIdx, critIdx, 1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, binVariableConstraintRow, '>=', 1);
  
  # Normalization to one
  constraintRowVarsSumGreaterThanOne = initLpModelMatrixRow(lpmodel);
  for (k in 1:levelsNumber) {
    for (i in 1:levelsNumber) {
      if(i != k) {
        constraintRow = initLpModelMatrixRow(lpmodel);
        constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, k, critIdx, 1);
        constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, i, critIdx, -1);
        constraintRow = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, k, critIdx, -problem$M);
        lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
      }
    }
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
    constraintRow = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, k, critIdx, -problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, levelsNumber, critIdx, -1);
    constraintRow = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, k, critIdx, problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
    
    constraintRowVarsSumGreaterThanOne = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, lpmodel,
                                                                                           constraintRowVarsSumGreaterThanOne, k, critIdx, 1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRowVarsSumGreaterThanOne, '>=', 1);
  
  return(lpmodel);
}

setNonMonNormalizationZeroBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ZERO_BINARY_VARIABLES', chPointIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getNonMonNormalizationZeroBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ZERO_BINARY_VARIABLES', chPointIdx, critIdx);
  
  return(constraintRow[i]);
}

setNonMonNormalizationOneBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ONE_BINARY_VARIABLES', chPointIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

setNonMonNormalizationOneBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ONE_BINARY_VARIABLES', chPointIdx, critIdx);
  
  return(constraintRow[i]);
}