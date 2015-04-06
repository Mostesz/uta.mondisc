addHolisticJudgmentsConstraintsToLpModel = function(problem, lpmodel, preferences, dir) {
  if (!is.null(preferences) && nrow(preferences) > 0) {
    for (prefIdx in 1:nrow(preferences)) {
      constraintRow = initLpModelMatrixRow(lpmodel);
      
      constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, preferences[prefIdx, 1], 1);
      constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, preferences[prefIdx, 2], -1);
  
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
    }
  }
  return(lpmodel);
}

setAlternativeOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, value) {
  for (critIdx in 1:problem$criteriaNumber) {
    i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'CHARACT_POINTS', altIdx, critIdx);
    constraintRow[i] = value;
  }
  
  return(constraintRow);
}

getAlternativeFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx) {
  i = getIndexForDataTypeByAltIdx(problem, lpmodel, 'CHARACT_POINTS', altIdx);
  
  return(constraintRow[i:(i+problem$criteriaNumber - 1)]);
}