# Funktion gibt eine Liste aller Elemente einer Spalte aus, die mit einem "," getrennt sind.
seperateByComma <- function(lis) {
  res = c()
  j = 1
  for (i in 1:length(lis)) {
    if (grepl(", ", lis[i])) {
      for (x in 1:length(strsplit(lis[i], ", ")[[1]])) {
        res[j] = strsplit(lis[i], ", ")[[1]][x]
        j = j + 1
      }
    }
    else{
      res[j] = lis[i]
      j = j + 1
    }
  }
  return(res)
}

# Funktion gibt eine Liste aller Elemente eines SpaltenELEMENTES aus, die mit einem "," getrennt sind.
giveElementByComma <- function(lis, index) {
  res = c()
  if (grepl(", ", lis[index])) {
    j = 1
    for (x in 1:length(strsplit(lis[index], ", ")[[1]])) {
      res[j] = strsplit(lis[index], ", ")[[1]][x]
      j = j + 1
    }
  }
  else{
    res[1] = lis[index]
  }
  return(res)
}
