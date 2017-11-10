library(jsonlite)

# Genre texts

estcid <- "T071036"
mydata <- fromJSON(paste("https://vm0824.kaj.pouta.csc.fi/octavo/ecco/search?query=%3CPARAGRAPH%C2%A7ESTCID:", estcid, "%C2%A7PARAGRAPH%3E&field=content&limit=-1", sep=""))
mydata <- mydata$results$docs
# Remove the first doc, it's always nonsense
mydata <- mydata[2:length(mydata[, 1]), ]
# Take only docs with >= 10 words
myfinaldata <- data.frame(x = character(), y = numeric(), stringsAsFactors = F)
i = 1
for (j in 1:length(mydata[, 1])) {
  row <- mydata[j, ]
  if (length(strsplit(row$content, " ")[[1]]) >= 10) {
    myfinaldata[i, 1] <- row$content
    myfinaldata[i, 2] <- 1
    i = i + 1
  }
}

# Control group

controldata <- fromJSON(paste("https://vm0824.kaj.pouta.csc.fi/octavo/ecco/search?query=%3CPARAGRAPH%C2%A7content:", "and", "%C2%A7PARAGRAPH%3E&field=content&limit=1300", sep=""))
controldata <- controldata$results$docs
# Remove the first doc, it's always nonsense
controldata <- controldata[2:length(controldata[, 1]), ]
# Take only docs with >= 10 words
controlfinaldata <- data.frame(x = character(), y = numeric(), stringsAsFactors = F)
i = 1
for (j in 1:length(controldata[, 1])) {
  row <- controldata[j, ]
  if (length(strsplit(row$content, " ")[[1]]) >= 10) {
    controlfinaldata[i, 1] <- row$content
    controlfinaldata[i, 2] <- 0
    i = i + 1
  }
}

# Remember to set working directory!
combineddata <- rbind(myfinaldata, controlfinaldata)
write.csv(combineddata, file = "trainingdata.csv", row.names = F)