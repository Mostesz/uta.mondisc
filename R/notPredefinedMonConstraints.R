addNotPredefinedMonConstraintsToLpModel = function(problem, lpmodel, critIdx) {
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  for (i in 1:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues[i, 'index'];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, altIdx, critIdx, -1);
    constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, altIdx, critIdx, -1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
    
    if (i != 1) {
      prevAltIdx = currCriterionAlternativesValues[i - 1, 'index'];
      
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, altIdx, critIdx, 1);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, prevAltIdx, critIdx, -1);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
      
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, altIdx, critIdx, 1);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, prevAltIdx, critIdx, -1);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    }
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, currCriterionAlternativesValues$index[problem$alternativesNumber], critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, currCriterionAlternativesValues$index[1], critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  return(lpmodel);
}

addNotPredefinedMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  
  lastAltIdx = currCriterionAlternativesValues[problem$alternativesNumber, 'index'];
  firstAltIdx = currCriterionAlternativesValues[1, 'index'];
  
  # Normalization to zero  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, lastAltIdx, critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, firstAltIdx, critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  # Normalization to one
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, firstAltIdx, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, firstAltIdx, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, lastAltIdx, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, lastAltIdx, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  return(lpmodel);
}

setNotPredefinedMonCharacPointOnConstraintRow = function(problem, lpmodel, constraintRow, isGainCase, altIdx, critIdx, value) {
  if (isGainCase) {
    i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_GAIN_CASE', altIdx, critIdx);
  } else {
    i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_COST_CASE', altIdx, critIdx);
  }
  constraintRow[i] = value;
  
  return(constraintRow);
}

getNotPredefinedMonCharacPointFromConstraintRow = function(problem, lpmodel, constraintRow, isGainCase, altIdx, critIdx) {
  if (isGainCase) {
    i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_GAIN_CASE', altIdx, critIdx);
  } else {
    i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_COST_CASE', altIdx, critIdx);
  }
  return(constraintRow[i]);
}

setNotPredefinedMonCostBinaryVarOnConstraintRow = function(problem, lpmodel, constraintRow, critIdx, value) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_COST_BINARY_VARIABLES', critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getNotPredefinedMonCostBinaryVarFromConstraintRow = function(problem, lpmodel, constraintRow, critIdx) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_COST_BINARY_VARIABLES', critIdx);
  return(constraintRow[i]);
}