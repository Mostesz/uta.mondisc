addPredefinedMonConstraintsToLpModel = function(problem, lpmodel, isGainCase, critIdx, dir) {  
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[critIdx];
  for (i in 2:problem$alternativesNumber) {
    altIdx = currCriterionAlternativesValues$index[i];
    prevAltIdx = currCriterionAlternativesValues$index[i - 1];
    
    constraintRow = initLpModelMatrixRow(lpmodel);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, altIdx, critIdx, 1);
    constraintRow = setCharacPointOnConstraintRow(problem, lpmodel, constraintRow, prevAltIdx, critIdx, -1);
    
    dir = if(isGainCase) '>=' else '<=';
    lpmodel = addConstraintToLpModel(lpmodel, constraintRow, dir, 0);
  }
  
  return(lpmodel);
}

addPredefinedMonNormalizationToLpModel = function(problem, lpmodel, isGainCase, critIdx) {
  stopifnot(criterionIdx>0);
  stopifnot(criterionIdx<=problem$criteriaNumber);
  
  i = if (isGainCase) 1 else problem$alternativesNumber;
  currCriterionAlternativesValues = problem$alternativesValuesForCriteria[critIdx];
  altIdx = currCriterionAlternativesValues$index[i];
  bestAltIdx = if (isGainCase) currCriterionAlternativesValues$index[problem$alternativesNumber]
               else currCriterionAlternativesValues$index[1];
  
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