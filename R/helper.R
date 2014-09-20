addElementsToMat = function(matrix, ncol, ..., DISTINCT = FALSE) {
  elementsToAdd = c(...);
  stopifnot(ncol>0);
  stopifnot(length(elementsToAdd)>0);
  stopifnot(length(elementsToAdd)%%ncol == 0);
  
  elements = matrix(elementsToAdd, ncol = ncol, byrow = TRUE);
  if (is.null(matrix)) {
    matrix = elements;
  } else if (DISTINCT) {
    matrix = unique(rbind(matrix, elements));
  } else {
    matrix = rbind(matrix, elements);
  }
  return (matrix);
}

deleteElementsFromMat = function(matrix, ncol, ..., DISTINCT = FALSE) {  
  elementsToDel = list(...);
  stopifnot(ncol>0);
  stopifnot(length(elementsToDel)>0);
  stopifnot(length(elementsToDel)%%ncol == 0);
  
  for (i in nrow(matrix):1) {
    for (element in elementsToDel) {
      if (matrix[i, 1] == element[1]
          && matrix[i, 2] == element[2]) {
        matrix = matrix[-i:-i, , drop=FALSE];
        if (DISTINCT)
          break;
      }
    }
  }
  return(matrix);
}