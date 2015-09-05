setCharacPointOnConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx, value) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'CHARACT_POINTS', chPointIdx, critIdx);
  constraintRow[i] = value;
  
  return(constraintRow);
}

getCharacPointFromConstraintRow = function(problem, lpmodel, constraintRow, chPointIdx, critIdx) {
  i = getIndexForDataTypeByCritAndChPointIdx(problem, lpmodel, 'CHARACT_POINTS', chPointIdx, critIdx);
  
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

setEpsValueOnConstraintRow = function(problem, lpmodel, constraintRow, sign) {
  if (abs(sign) != 1) {
    stop('Incorrect sign for eps')
  }
  i = getLpModelMatrixRowStartIdx(lpmodel, 'EPS')
  constraintRow[i] = sign
  
  return(constraintRow);
}

getEpsValueFromConstraintRow = function(problem, lpmodel, constraintRow) {
  i = getLpModelMatrixRowStartIdx(lpmodel, 'EPS')
  return(constraintRow[i])
}