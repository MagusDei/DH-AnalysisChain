# Linear predictor for the degree of "genreization" of a given paragraph.

# Load the stringr library for advanced string manipulations
library(stringr)

# A table with terms and their genreization values
terms_genre <- read.csv("medicalterms.csv", header = T)

# A table with paragraphs and their correct classifications
paragraphs <- read.csv("paragraphs.csv", header = F)

# Returns the genreization value for a given term, or 0 if not found
calculate_genreization <- function(word) {
  if (word %in% terms_genre[, 1]) {
    return(terms_genre[match(word, terms_genre[, 1]), 2])
  } else {
    return(0)
  }
}

# A function which calculates the mean genreization of a paragraph
mean_genreization <- function(paragraph) {
  # Change strsplit to some custom function later...
  return(mean(apply(strsplit(paragraph, " "), 1, calculate_genreization)))
}

# A function which classifies a paragraph p given a threshold c
classify_paragraph <- function(paragraph, threshold) {
  average_genreization <- mean_genreization(paragraph)
  if (average_genreization >= threshold) {
    return(1)
  } else {
    return(0)
  }
}

# Function to calculate prediction error given a list of paragraphs
# and their correct classifications.
classification_error <- function(threshold) {
  return(sum(abs(apply(paragraphs[, 1], 1, classify_paragraph, threshold) - paragraphs[, 2])))
}

# Find a threshold c which globally minimizes the classification error
# function
sorted_scores <- sort(apply(paragraphs, 1, mean_genreization))
sorted_scores1 <- sorted_scores[2:length(sorted_scores[, 1]), ]
sorted_scores2 <- sorted_scores[1:(length(sorted_scores[, 1])-1), ]
delta <- min(sorted_scores1-sorted_scores2)/2

c = 0
cmin = 0
cmax = sorted_scores[length(sorted_scores[, 1]), 2]
Emin = Inf
while (c < cmax) {
  if (classification_error(c) < Emin) {
    cmin = c
    Emin = classification_error(c)
  }
  c = c + delta
}

# For our dataset, cmin = ???

# Prediction statistics
print(paste("Globally optimal threshold (probably overfitted):", cmin))
print(paste("Prediction error for best threshold (%):", 100*classification_error(cmin)/length(paragraphs[, 1])))