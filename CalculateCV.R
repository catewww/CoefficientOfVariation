#function that calculates Coefficient of Variation for the dataset (Columns-obserwations, Rows-features)
#and returns df to global environment, saving data in .txt format

CalculateCV <- function (input, finame)
{
  #check if data is not empty
  if (nrow(input) <= 1)
    cat("No data passed --> check if the data has any features in passed data rows")
    
  #matrix storing estimates
  CV <- matrix(ncol = 1, nrow = nrow(input))
  rownames(CV) = rownames(input)
  colnames(CV) = c('CV')
  
  #calculate CV
  k=0 #number of features that cannot be computed
  for (i in 1 : nrow(CV))
  {
    CV[i,1] = (mean(as.numeric(input[i, ])) / sd(as.numeric(input[i, ])))
    #if std for the feature i equals 0 then
    if(is.na(CV[i, 1]))
    {
      CV[i, 1] = NaN
      k=k+1
    }
  }
  if (k > 0 & nrow(input) > 1)
    cat("Number of features that cannot be calculated due to 0 std: ", k, '\n')
  
  CV <- as.data.frame(CV)
  #save file in .txt format
  if (nchar(finame) == 0)
  {
    cat("finame is empty --> proceeding with default finame value: CoefOfVar")
    finame=c('CoefOfVar')
  }
  write.table(CV, paste(finame, '.txt', sep = ""), quote = F, col.names = F)
  
  return(CV)
}