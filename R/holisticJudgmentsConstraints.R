addHolisticJudgmentsConstraintsToLpModel = function(problem, lpmodel, preferences, prefType) {
  if (!is.null(preferences) && nrow(preferences) > 0) {
    for (prefIdx in 1:nrow(preferences)) {
      constraintRow = initLpModelMatrixRow(lpmodel);
      
      constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, preferences[prefIdx, 1], 1)
      constraintRow = setAlternativeOnConstraintRow(problem, lpmodel, constraintRow, preferences[prefIdx, 2], -1)
      dir = ">="
      if (prefType == 'STRICT') {
        constraintRow = setEpsValueOnConstraintRow(problem, lpmodel, constraintRow, -1)
      } else if (prefType == 'INDIFF') {
        dir = "=="
      }
  
      lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
    }
  }
  return(lpmodel);
}

setAlternativeOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, value) {
  for (critIdx in 1:problem$criteriaNumber) {
    chPointIdx = problem$alternativesIndexesForCriteria[[critIdx]][altIdx]
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, value)
  }
  
  return(constraintRow);
}

getAlternativeValueFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  chPointIdx = problem$alternativesIndexesForCriteria[[critIdx]][altIdx]
  return(getCharacPointFromConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx))
}