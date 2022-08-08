#Loading all packages:
library(devtools)
library(Rcpp)
library(wikipediatrend)
library(AnomalyDetection)
library(dplyr)
library(ggplot2)
library(WikipediR)
library(tm)
library(stringi)
library(lattice)
library(udpipe)
library(wordcloud)
library(readr)
library(dplyr)
library(e1071)
library(mlbench)
library(tm)
library(SnowballC)
library(RColorBrewer)

#First we load all the CSV files in as datasets
Film_Animation_Data <- read.csv("Film_Animation.csv")
Automotive_Vehicle_Data <- read.csv("Automotive_Vehicle.csv")
Music_Data <- read.csv("Music.csv")
Pets_Animals_Data <- read.csv("Pets_Animals.csv")
Sports_Data <- read.csv("Sports.csv")
Travel_Events_Data <- read.csv("Travel_Events.csv")
Gaming_Data <- read.csv("Gaming.csv")
People_Blogs_Data <- read.csv("People_Blogs.csv")
Comedy_Data <- read.csv("Comedy.csv")
Entertainment_Data <- read.csv("Entertainment.csv")
News_Politics_Data <- read.csv("News_Politics.csv")
HowTo_Style_Data <- read.csv("HowTo_Style.csv")
Education_Data <- read.csv("Education.csv")
Science_Tech_Data <- read.csv("Science_Tech.csv")


#Now lets view the structure of each dataset:
str(Film_Animation_Data)
str(Automotive_Vehicle_Data)
str(Music_Data)
str(Pets_Animals_Data)
str(Sports_Data)
str(Travel_Events_Data)
str(Gaming_Data)
str(People_Blogs_Data)
str(Comedy_Data)
str(Entertainment_Data)
str(News_Politics_Data)
str(HowTo_Style_Data)
str(Education_Data)
str(Science_Tech_Data)

##Creating Section to Process the Film/Animations Category
Film_Animation_Titles <- Film_Animation_Data$title
str(Film_Animation_Titles)

# Create Corpus
Film_Animation_Corpus = Corpus(VectorSource(Film_Animation_Data$title))
# Look at Corpus
Film_Animation_Corpus[[1]][1]


#Conversion to Lowercase
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, PlainTextDocument)
Film_Animation_Corpus[[1]][1]
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, tolower)
Film_Animation_Corpus[[1]][1]

#Removing Punctuation
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removePunctuation)
Film_Animation_Corpus[[1]][1]

#Remove stopwords
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removeWords, c(stopwords("english")))
Film_Animation_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stripWhitespace)
Film_Animation_Corpus[[1]][1] 

inspect(Film_Animation_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Film_Animation_Corpus)
tdm<-TermDocumentMatrix(Film_Animation_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=20)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Film_Animation_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Film/Animation", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Film/Animation", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Film/Animation", xlab = "Freq. Count")


##Creating Section to Process the Automotive/Vehicle Category
Automotive_Vehicle_Titles <- Automotive_Vehicle_Data$title
str(Automotive_Vehicle_Titles)

# Create Corpus
Automotive_Vehicle_Corpus = Corpus(VectorSource(Automotive_Vehicle_Data$title))
# Look at Corpus
Automotive_Vehicle_Corpus[[1]][1]


#Conversion to Lowercase
Automotive_Vehicle_Corpus = tm_map(Automotive_Vehicle_Corpus, PlainTextDocument)
Automotive_Vehicle_Corpus[[1]][1]
Automotive_Vehicle_Corpus = tm_map(Automotive_Vehicle_Corpus, tolower)
Automotive_Vehicle_Corpus[[1]][1]

#Removing Punctuation
Automotive_Vehicle_Corpus = tm_map(Automotive_Vehicle_Corpus, removePunctuation)
Automotive_Vehicle_Corpus[[1]][1]

#Remove stopwords
Automotive_Vehicle_Corpus = tm_map(Automotive_Vehicle_Corpus, removeWords, c(stopwords("english")))
Automotive_Vehicle_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Automotive_Vehicle_Corpus= tm_map(Automotive_Vehicle_Corpus, stripWhitespace)
Automotive_Vehicle_Corpus[[1]][1] 

inspect(Automotive_Vehicle_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Automotive_Vehicle_Corpus)
tdm<-TermDocumentMatrix(Automotive_Vehicle_Corpus)

# this is showing that there are 384 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=10)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Automotive_Vehicle_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Blues", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Blues", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Blues", xlab = "Freq. Count")

##Creating Section to Process the Music Category
Music_Titles <- Music_Data$title
str(Music_Titles)

# Create Corpus
Music_Corpus = Corpus(VectorSource(Music_Data$title))
# Look at Corpus
Music_Corpus[[1]][1]


#Conversion to Lowercase
Music_Corpus = tm_map(Music_Corpus, PlainTextDocument)
Music_Corpus[[1]][1]
Music_Corpus = tm_map(Music_Corpus, tolower)
Music_Corpus[[1]][1]

#Removing Punctuation
Music_Corpus= tm_map(Music_Corpus, removePunctuation)
Music_Corpus[[1]][1]

#Remove stopwords
Music_Corpus = tm_map(Music_Corpus, removeWords, c(stopwords("english")))
Music_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Music_Corpus = tm_map(Music_Corpus, stripWhitespace)
Music_Corpus[[1]][1] 

inspect(Music_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Music_Corpus)
tdm<-TermDocumentMatrix(Music_Corpus)

# this is showing that there are 1824 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=50)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Music_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## PROPER NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Blues", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Blues", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Blues", xlab = "Freq. Count")


##Creating Section to Process the Jazz Genre:

##Creating Section to Process the Film/Animations Category
Film_Animation_Titles <- Film_Animation_Data$title
str(Film_Animation_Titles)

# Create Corpus
Film_Animation_Corpus = Corpus(VectorSource(Film_Animation_Data$title))
# Look at Corpus
Film_Animation_Corpus[[1]][1]


#Conversion to Lowercase
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, PlainTextDocument)
Film_Animation_Corpus[[1]][1]
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, tolower)
Film_Animation_Corpus[[1]][1]

#Removing Punctuation
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removePunctuation)
Film_Animation_Corpus[[1]][1]

#Remove stopwords
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removeWords, c(stopwords("english")))
Film_Animation_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stripWhitespace)
Film_Animation_Corpus[[1]][1] 

inspect(Film_Animation_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Film_Animation_Corpus)
tdm<-TermDocumentMatrix(Film_Animation_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=20)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Film_Animation_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Blues", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Blues", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Blues", xlab = "Freq. Count")

##Creating Section to Process the Pop Genre:

##Creating Section to Process the Film/Animations Category
Film_Animation_Titles <- Film_Animation_Data$title
str(Film_Animation_Titles)

# Create Corpus
Film_Animation_Corpus = Corpus(VectorSource(Film_Animation_Data$title))
# Look at Corpus
Film_Animation_Corpus[[1]][1]


#Conversion to Lowercase
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, PlainTextDocument)
Film_Animation_Corpus[[1]][1]
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, tolower)
Film_Animation_Corpus[[1]][1]

#Removing Punctuation
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removePunctuation)
Film_Animation_Corpus[[1]][1]

#Remove stopwords
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removeWords, c(stopwords("english")))
Film_Animation_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stripWhitespace)
Film_Animation_Corpus[[1]][1] 

inspect(Film_Animation_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Film_Animation_Corpus)
tdm<-TermDocumentMatrix(Film_Animation_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=20)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Film_Animation_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Blues", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Blues", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Blues", xlab = "Freq. Count")


##Creating Section to Process the Reggae Genre:
##Creating Section to Process the Film/Animations Category
Film_Animation_Titles <- Film_Animation_Data$title
str(Film_Animation_Titles)

# Create Corpus
Film_Animation_Corpus = Corpus(VectorSource(Film_Animation_Data$title))
# Look at Corpus
Film_Animation_Corpus[[1]][1]


#Conversion to Lowercase
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, PlainTextDocument)
Film_Animation_Corpus[[1]][1]
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, tolower)
Film_Animation_Corpus[[1]][1]

#Removing Punctuation
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removePunctuation)
Film_Animation_Corpus[[1]][1]

#Remove stopwords
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removeWords, c(stopwords("english")))
Film_Animation_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stripWhitespace)
Film_Animation_Corpus[[1]][1] 

inspect(Film_Animation_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Film_Animation_Corpus)
tdm<-TermDocumentMatrix(Film_Animation_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=20)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Film_Animation_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Blues", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Blues", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Blues", xlab = "Freq. Count")


##Creating Section to Process the Rock Genre:
##Creating Section to Process the Film/Animations Category
Film_Animation_Titles <- Film_Animation_Data$title
str(Film_Animation_Titles)

# Create Corpus
Film_Animation_Corpus = Corpus(VectorSource(Film_Animation_Data$title))
# Look at Corpus
Film_Animation_Corpus[[1]][1]


#Conversion to Lowercase
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, PlainTextDocument)
Film_Animation_Corpus[[1]][1]
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, tolower)
Film_Animation_Corpus[[1]][1]

#Removing Punctuation
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removePunctuation)
Film_Animation_Corpus[[1]][1]

#Remove stopwords
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, removeWords, c(stopwords("english")))
Film_Animation_Corpus[[1]][1]

# Stemming
#Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stemDocument)
#Film_Animation_Corpus[[1]][1]

# Eliminate white spaces
Film_Animation_Corpus = tm_map(Film_Animation_Corpus, stripWhitespace)
Film_Animation_Corpus[[1]][1] 

inspect(Film_Animation_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Film_Animation_Corpus)
tdm<-TermDocumentMatrix(Film_Animation_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=20)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Film_Animation_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Blues", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Blues", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Blues", xlab = "Freq. Count")

##Now lets start the ML Process:

Videos_raw <- read.csv("VideoCategory.csv", stringsAsFactors = FALSE, header=F)
Videos_raw$V1 <- factor(Videos_raw$V1) ## transform to factors for ML

str(Videos_raw$V1)
table(Videos_raw$V1)

### text mining and preparation    
### text mining and preparation
library(tm)

Videos_corpus <- VCorpus(VectorSource(Videos_raw$V2))
print(Videos_corpus)
inspect(Videos_corpus[1:2])
as.character(Videos_corpus[[1]])
lapply(Videos_corpus[1:2], as.character)

Videos_corpus_clean <- tm_map(Videos_corpus, content_transformer(tolower)) 
## transforms capital letters into lower case letters
as.character(Videos_corpus[[1]])
as.character(Videos_corpus_clean[[1]])
Videos_corpus_clean <- tm_map(Videos_corpus_clean, removeNumbers) 
## removes numbers

Videos_corpus_clean <- tm_map(Videos_corpus_clean, removeWords, stopwords()) 
## removes common words
Videos_corpus_clean <- tm_map(Videos_corpus_clean, removePunctuation) 
## removes punctuation

#Replace Punctuation
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x) }

library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
#Videos_corpus_clean <- tm_map(Videos_corpus_clean, stemDocument)
Videos_corpus_clean <- tm_map(Videos_corpus_clean, stripWhitespace)

Videos_dtm <- DocumentTermMatrix(Videos_corpus_clean)

###another way to create the document term matrix with the preprocessing inside the function
Videos_dtm2 <- DocumentTermMatrix(Videos_corpus, control = list( tolower = TRUE, 
              removeNumbers = TRUE, stopwords = TRUE, removePunctuation = TRUE, stemming = TRUE, stripWhitespace =T))

Videos_dtm
Videos_dtm2

## split the data (55%-45%)
Videos_dtm_train <- Videos_dtm[1:29880, ]
Videos_dtm_test <- Videos_dtm[29881:40835, ]

Videos_train_labels <- Videos_raw[1:29880, ]$V1
Videos_test_labels <- Videos_raw[29881:40835, ]$V1


## compare the proportion of songs in the training and test data frames:
prop.table(table(Videos_train_labels))
prop.table(table(Videos_test_labels))

findFreqTerms(Videos_dtm_train, 5)
Videos_freq_words <- findFreqTerms(Videos_dtm_train, 5)

## we want all the rows, but only the columns representing the words in the songs_freq_words vector
Videos_dtm_freq_train<- Videos_dtm_train[ , Videos_freq_words]
Videos_dtm_freq_test <- Videos_dtm_test[ , Videos_freq_words]

# The training and test datasets now include 1,116 features, which correspond to words appearing in at least five messages.
# The Naive Bayes classifier is typically trained on data with categorical features. This poses a problem, since the cells in the sparse matrix are numeric and measure the number of times a word appears in a message. 
# We need to change this to a categorical variable that simply indicates yes or no depending on whether the word appears at all.

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

Videos_train <- apply(Videos_dtm_freq_train, MARGIN = 2, convert_counts)
Videos_test <- apply(Videos_dtm_freq_test, MARGIN = 2, convert_counts)


library(e1071)
## check the ?naiveBayes() function in this package

#Classifier
Videos_classifier <- naiveBayes(Videos_train, Videos_train_labels)

#Predicter
Videos_test_pred <- predict(Videos_classifier, Videos_test)

library(gmodels)
CrossTable(Videos_test_pred, Videos_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
