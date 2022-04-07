## Prepare for grep by taking punctuation
grepprep<- function(x){
  
  var <- str_to_upper(x) #capital letters
  var = gsub("Á","A", var) #replacing all punctuations
  var = gsub("Â","A", var)
  var = gsub("É","E", var)
  var = gsub("Ê","E", var)
  var = gsub("Í","I", var)
  var = gsub("Ô","O", var)
  var = gsub("Ó","O", var)
  var = gsub("Ú","U", var)
  var = gsub("Ç","C", var)
  var = str_replace_all(var, "[[:punct:]]" , " ")  #taking out special characters
  return(var)
}