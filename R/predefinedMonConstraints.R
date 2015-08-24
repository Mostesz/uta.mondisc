addPredefinedMonConstraintsToLpModel = function(problem, lpmodel, isGainCase, critIdx) {
  levelsNumber = problem$levelNoForCriteria[critIdx]
  for (chPointIdx in 2:levelsNumber) {
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, chPointIdx - 1, critIdx, -1);
    
    dir = if(isGainCase) '>=' else '<=';
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
  }
  
  return(lpmodel);
}

addPredefinedMonNormalizationToLpModel = function(problem, lpmodel, isGainCase, critIdx) {
  stopifnot(critIdx > 0);
  stopifnot(critIdx <= problem$criteriaNumber);
  
  levelsNumber = problem$levelNoForCriteria[critIdx]
  
  theWorstChPointIdx = if (isGainCase) 1 else levelsNumber
  theBestChPointIdx = if (isGainCase) levelsNumber else 1
  
  # Normalization to zero  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, theWorstChPointIdx, critIdx, 1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  # Normalization to one
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, theBestChPointIdx, critIdx, 1);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, -1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  return(lpmodel);
}