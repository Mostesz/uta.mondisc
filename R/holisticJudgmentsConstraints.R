addHolisticJudgmentsConstraintsToLpModel = function(problem, lpmodel, preferences, dir) {
  for (prefIdx in nrow(preferences)) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    for (altIdx in 1:problem$alternativesNumber) {
      if (preferences[prefIdx, 1] == altIdx) {
        constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, altIdx, 1);
      } else if (preferences[prefIdx,2] == altIdx) {
        constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, altIdx, -1);
      } else {
        constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, altIdx, 0)
      }
    }
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0)
  }
  return(lpmodel);
}

setAlternativeOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, value) {
  startIdx = getIndexForDataTypeByAltIdx(problem, lpmodel, 'CHARACT_POINTS', altIdx);

  for (i in startIdx:(startIdx + problem$criteriaNumber - 1)) {
    constraintRow[i] = value;
  }
  
  return(constraintRow);
}

getAlternativeFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx) {
  i = getIndexForDataTypeByAltIdx(problem, lpmodel, 'CHARACT_POINTS', altIdx);
  
  return(constraintRow[i:(i+problem$criteriaNumber - 1)]);
}