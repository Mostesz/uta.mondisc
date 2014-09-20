addElementsToMatrix = function(mat, ncol, ..., DISTINCT = FALSE) {
  elementsToAdd = c(...);
  stopifnot(ncol>0);
  stopifnot(length(elementsToAdd)>0);
  stopifnot(length(elementsToAdd)%%ncol == 0);
  
  elements = matrix(elementsToAdd, ncol = ncol, byrow = TRUE);
  if (is.null(mat)) {
    mat = elements;
  } else if (DISTINCT) {
    mat = unique(rbind(mat, elements));
  } else {
    mat = rbind(mat, elements);
  }
  return (mat);
}

deleteElementsFromMatrix = function(mat, ncol, ..., DISTINCT = FALSE) {  
  elementsToDel = list(...);
  stopifnot(ncol>0);
  stopifnot(length(elementsToDel)>0);
  stopifnot(length(elementsToDel)%%ncol == 0);
  
  for (i in nrow(mat):1) {
    for (element in elementsToDel) {
      if (mat[i, 1] == element[1]
          && mat[i, 2] == element[2]) {
        mat = mat[-i:-i, , drop=FALSE];
        if (DISTINCT)
          break;
      }
    }
  }
  return(mat);
}