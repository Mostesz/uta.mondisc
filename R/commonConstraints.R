setCharacPointOnConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'CHARACT_POINTS', altIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getCharacPointFromConstraintRow = function(problem, lpmodel, constraintRow, altIdx, critIdx) {
  i = getIndexForDataTypeByCritAndAltIdx(problem, lpmodel, 'CHARACT_POINTS', altIdx, critIdx);
  
  return(constraintRow[i]);
}

setBestEvaluationOnCriteriaOnContraintRow = function(problem, lpmodel, constraintRow, critIdx, value) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'BEST_EVALUATIONS_ON_CRITERIA', critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getBestEvaluationOnCriteriaFromContraintRow = function(problem, lpmodel, constraintRow, critIdx) {
  i = getIndexForDataTypeByCritIdx(problem, lpmodel, 'BEST_EVALUATIONS_ON_CRITERIA', critIdx);
  
  return(constraintRow[i]);
}