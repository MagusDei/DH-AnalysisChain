library(jsonlite)

collocator <- function(word, n) {
  print(word)
  jsondata <- fromJSON(paste("https://vm0824.kaj.pouta.csc.fi/octavo/ecco/collocations?query=",
                             word,"&limit=",n,"&maxDocs=-1&localScaling=ABSOLUTE&maxTotalTermFreq=5000000&minTotalTermFreq=5000&timeout=-1", sep = ""))
  jsondata <- jsondata$results$terms
  colnames(jsondata) <- c('term', 'score')
  return(jsondata)
}
update_master <- function(collocations_master, word, n) {
  collocations <- collocator(word, n)
  collocations_master <- merge(collocations_master, collocations, by.x = 'term', by.y = 'term', all = T)
  collocations_master$score <- rowSums(collocations_master[, c('score.x', 'score.y')], na.rm = T)
  collocations_master$score.x = NULL
  collocations_master$score.y = NULL
  return(collocations_master)
}

Create_collocated_wordlist <- function(originalwordlist,name,variable,n=21){
  Content <- originalwordlist
  collocations_master <- collocator(NULL, n)
  for(variable in originalwordlist){
    update_master(collocations_master=collocations_master,as.character(originalwordlist$variable,n))  
  }
  csv_file <- collocations_master[order(-collocations_master$score),]
  write.csv(csv_file,name,row.names = F)
  
}