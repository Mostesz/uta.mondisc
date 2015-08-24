addNotPredefinedMonConstraintsToLpModel = function(problem, lpmodel, critIdx) {
  levelsNumber = problem$levelNoForCriteria[critIdx]
  for (chPointIdx in 1:levelsNumber) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, chPointIdx, critIdx, -1);
    constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, chPointIdx, critIdx, -1);
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
    
    if (i != 1) {
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, chPointIdx, critIdx, 1);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, chPointIdx - 1, critIdx, -1);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
      
      constraintRow = initLpModelMatrixRow(lpmodel);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, chPointIdx, critIdx, 1);
      constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, chPointIdx - 1, critIdx, -1);
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
    }
  }
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, levelsNumber, critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, 1, critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  return(lpmodel);
}

addNotPredefinedMonNormalizationToLpModel = function(problem, lpmodel, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  
  # Normalization to zero  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, levelsNumber, critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, 1, critIdx, 1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  # Normalization to one
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, 1, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', -problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, FALSE, 1, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', problem$M);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, levelsNumber, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '>=', 0);
  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, 1);
  constraintRow = setNotPredefinedMonCharacPointOnConstraintRow(problem, lpmodel, constraintRow, TRUE, levelsNumber, critIdx, -1);
  constraintRow = setNotPredefinedMonCostBinaryVarOnConstraintRow(problem, lpmodel, constraintRow, critIdx, -problem$M);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '<=', 0);
  
  return(lpmodel);
}

setNotPredefinedMonCharacPointOnConstraintRow = function(problem, lpmodel, constraintRow, isGainCase, chPointIdx, critIdx, value) {
  if (isGainCase) {
    i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_GAIN_CASE', chPointIdx, critIdx);
  } else {
    i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_COST_CASE', chPointIdx, critIdx);
  }
  constraintRow[i] = value;
  
  return(constraintRow);
}

getNotPredefinedMonCharacPointFromConstraintRow = function(problem, lpmodel, constraintRow, isGainCase, chPointIdx, critIdx) {
  if (isGainCase) {
    i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_GAIN_CASE', chPointIdx, critIdx);
  } else {
    i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'NOT_PREDEFINED_MON_CHARACT_POINTS_COST_CASE', chPointIdx, critIdx);
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