library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
books <<- list("Pride and Prejudice" = "prejudice",
               "Alice's Adventures in Wonderland" = "alice",
               "Adventures of Tom Sawyer" = "tom",
               "Grimms' Fairy Tales" = "grimms",
               "The Picture of Dorian Gray" = "dorian",
               "Dracula" = "dracula",
               "The Raven" = "raven",
               "First Book of Adam and Eve" = "eve")

getTermMatrix <- memoise(function(book) {
 
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("C:/Users/home/Desktop/shinycloud/books./%s.txt", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})