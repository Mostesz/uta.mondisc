addObjForNonMonTypeToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(criterionIdx>0);
  stopifnot(criterionIdx<=problem$criteriaNumber);
  
  for (k in 3:problem$alternativesNumber) {
    lpmodel$obj = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, lpmodel$obj, k, critIdx, 1)
  }
  return(lpmodel);
}

addNonMonConstraintsToLpModel = function(problem, lpmodel, critIdx) {
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[critIdx];
  for (i in 2:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues$index[i];
    prevAltIdx = currCriterionAlternativesValues$index[i - 1];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, -problem$M);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, -problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    
    if (i != 2) {
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
      constraintRow = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, problem$M);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
      
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
      constraintRow = setMonDirectionBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
      constraintRow = setChangeMonBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, -problem$M);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    }
  }
  
  return(lpmodel);
}

setMonDirectionBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'MON_DIRECTION_BINARY_VARIABLES', altIdx, critIdx,
                                                startAltIdx = 2);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getMonDirectionBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'MON_DIRECTION_BINARY_VARIABLES', altIdx, critIdx,
                                                startAltIdx = 2);
  return(constraintRow[i]);
}

setChangeMonBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'CHANGE_MON_BINARY_VARIABLES', altIdx, critIdx,
                                         startAltIdx = 3);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getChangeMonBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'CHANGE_MON_BINARY_VARIABLES', altIdx, critIdx,
                                         startAltIdx = 3);
  return(constraintRow[i]);
}

addNonMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(criterionIdx>0);
  stopifnot(criterionIdx<=problem$criteriaNumber);
  
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[critIdx];
  lastAltIdx = currCriterionAlternativesValues$index[problem$alternativesNumber];
  
  # Normalization to zero
  binVariableConstraintRow = initLpModelMatrixRow(lpmodel);
  for (i in 2:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues$index[i];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, constraintRow, altIdx, critIdx, 1);
    constraintRow = setNonMonNormalizationZeroBinaryVarOnConstraintRow(problem, constraintRow, altIdx, critIdx, 1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
    
    binVariableConstraintRow = setNonMonNormalizationZeroBinaryVarOnConstraintRow(problem, binVariableConstraintRow, altIdx, critIdx, 1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, binVariableConstraintRow, '>=', 1);
  
  # Normalization to one
  constraintRowVarsSumGreaterThanOne = initLpModelMatrixRow(lpmodel);
  for (k in 1:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues$index[k];
    
    for (i in 1:problem$alternativesNumber) {
      if(i != k) {
        iAltIdx = currCriterionAlternativesValues$index[i];
        
        constraintRow = initLpModelMatrixRow(lpmodel);
        constraintRow = setCharacPointOnConstraintRow(problem, constraintRow, altIdx, critIdx, 1);
        constraintRow = setCharacPointOnConstraintRow(problem, constraintRow, iAltIdx, critIdx, -1);
        constraintRow = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, constraintRow, altIdx, critIdx, -problem$M);
        lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
      }
    }
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, constraintRow, lastAltIdx, critIdx, -1);
    constraintRow = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, constraintRow, altIdx, critIdx, -problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, constraintRow, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, constraintRow, lastAltIdx, critIdx, -1);
    constraintRow = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem, constraintRow, altIdx, critIdx, problem$M);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
    
    constraintRowVarsSumGreaterThanOne = setNonMonNormalizationOneBinaryVarOnConstraintRow(problem,
                                                                                           constraintRowVarsSumGreaterThanOne, altIdx, critIdx, 1);
  }
  lpmodel = addConstraintToLpModel(lpmodel, constraintRowVarsSumGreaterThanOne, '>=', 1);
  
  return(lpmodel);
}

setNonMonNormalizationZeroBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ZERO_BINARY_VARIABLES', altIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getNonMonNormalizationZeroBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ZERO_BINARY_VARIABLES', altIdx, critIdx);
  
  return(constraintRow[i]);
}

setNonMonNormalizationOneBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ONE_BINARY_VARIABLES', altIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

setNonMonNormalizationOneBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NON_MON_NORMALIZATION_ONE_BINARY_VARIABLES', altIdx, critIdx);
  
  return(constraintRow[i]);
}