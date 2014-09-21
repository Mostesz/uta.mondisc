addPredefinedMonConstraintsToLpModel = function(problem, lpmodel, isGainCase, critIdx, dir) {  
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  for (i in 2:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues[i, 'index'];
    prevAltIdx = currCriterionAlternativesValues[i - 1, 'index'];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    
    dir = if(isGainCase) '>=' else '<=';
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
  }
  
  return(lpmodel);
}

addPredefinedMonNormalizationToLpModel = function(problem, lpmodel, isGainCase, critIdx) {
  stopifnot(critIdx>0);
  stopifnot(critIdx<=problem$criteriaNumber);
  
  i = if (isGainCase) 1 else problem$alternativesNumber;
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[[critIdx]];
  altIdx = currCriterionAlternativesValues[i, 'index'];
  bestAltIdx = if (isGainCase) currCriterionAlternativesValues[problem$alternativesNumber, 'index']
               else currCriterionAlternativesValues[1, 'index'];
  
  # Normalization to zero  
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  # Normalization to one
  constraintRow = initLpModelMatrixRow(lpmodel);
  constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, bestAltIdx, critIdx, 1);
  constraintRow = setBestEvaluationOnCriteriaOnContraintRow(problem, lpmodel, constraintRow, critIdx, -1);
  lpmodel = addConstraintToLpModel(lpmodel, constraintRow, '==', 0);
  
  return(lpmodel);
}