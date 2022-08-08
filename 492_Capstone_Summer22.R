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

# Now let's use UDpipe to show PROPNs or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Film_Animation_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): PROPNs, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring nouns in Film/Animation", xlab = "Freq. Count")

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

# Now let's use UDpipe to show PROPNs or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Automotive_Vehicle_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): PROPNs, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring nouns in Automobiles_Vehicle", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Automobiles_Vehicle", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Automobiles_Vehicle", xlab = "Freq. Count")


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
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=30)

# Now let's use UDpipe to show PROPNs or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Music_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): PROPNs, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Music", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Music", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Music", xlab = "Freq. Count")


##Creating Section to Process the Sports Category
Sports_Titles <- Sports_Data$title
str(Sports_Titles)

# Create Corpus
Sports_Corpus = Corpus(VectorSource(Sports_Data$title))
# Look at Corpus
Sports_Corpus[[1]][1]


#Conversion to Lowercase
Sports_Corpus = tm_map(Sports_Corpus, PlainTextDocument)
Sports_Corpus[[1]][1]
Sports_Corpus = tm_map(Sports_Corpus, tolower)
Sports_Corpus[[1]][1]

#Removing Punctuation
Sports_Corpus = tm_map(Sports_Corpus, removePunctuation)
Sports_Corpus[[1]][1]

#Remove stopwords
Sports_Corpus = tm_map(Sports_Corpus, removeWords, c(stopwords("english")))
Sports_Corpus[[1]][1]

# Stemming
#Sports_Corpus = tm_map(Sports_Corpus, stemDocument)
#Sports_Corpus[[1]][1]

# Eliminate white spaces
Sports_Corpus = tm_map(Sports_Corpus, stripWhitespace)
Sports_Corpus[[1]][1] 

inspect(Sports_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Sports_Corpus)
tdm<-TermDocumentMatrix(Sports_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=20)

# Now let's use UDpipe to show PROPNs or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Sports_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): PROPNs, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring nouns in Sports", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Sports", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Sports", xlab = "Freq. Count")

##Creating Section to Process the Entertainment Category:
Entertainment_Titles <- Entertainment_Data$title
str(Entertainment_Titles)

# Create Corpus
Entertainment_Corpus = Corpus(VectorSource(Entertainment_Data$title))
# Look at Corpus
Entertainment_Corpus[[1]][1]


#Conversion to Lowercase
Entertainment_Corpus = tm_map(Entertainment_Corpus, PlainTextDocument)
Entertainment_Corpus[[1]][1]
Entertainment_Corpus = tm_map(Entertainment_Corpus, tolower)
Entertainment_Corpus[[1]][1]

#Removing Punctuation
Entertainment_Corpus = tm_map(Entertainment_Corpus, removePunctuation)
Entertainment_Corpus[[1]][1]

#Remove stopwords
Entertainment_Corpus = tm_map(Entertainment_Corpus, removeWords, c(stopwords("english")))
Entertainment_Corpus[[1]][1]

# Stemming
#Entertainment_Corpus = tm_map(Entertainment_Corpus, stemDocument)
#Entertainment_Corpus[[1]][1]

# Eliminate white spaces
Entertainment_Corpus = tm_map(Entertainment_Corpus, stripWhitespace)
Entertainment_Corpus[[1]][1] 

inspect(Entertainment_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Entertainment_Corpus)
tdm<-TermDocumentMatrix(Entertainment_Corpus)

# this is showing that there are 963 words in the document
dtm
tdm

# and let's create a word cloud of the words in the article
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing=TRUE)
myNames <- names(v)
dtmnew <- data.frame(word=myNames, freq=v)
wordcloud(dtmnew$word, colors=c(3,4), random.color=TRUE, dtmnew$freq, min.freq=30)

# Now let's use UDpipe to show nouns or  verbs in the text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
# this takes a few minutes to run, it is annotating the text
x <- udpipe_annotate(ud_model, x = Entertainment_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Entertainment", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Entertainment", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Entertainment", xlab = "Freq. Count")


##Creating Section to Process the Pets_Animals Category:

Pets_Animals_Titles <- Pets_Animals_Data$title
str(Pets_Animals_Titles)

# Create Corpus
Pets_Animals_Corpus = Corpus(VectorSource(Pets_Animals_Data$title))
# Look at Corpus
Pets_Animals_Corpus[[1]][1]


#Conversion to Lowercase
Pets_Animals_Corpus = tm_map(Pets_Animals_Corpus, PlainTextDocument)
Pets_Animals_Corpus[[1]][1]
Pets_Animals_Corpus = tm_map(Pets_Animals_Corpus, tolower)
Pets_Animals_Corpus[[1]][1]

#Removing Punctuation
Pets_Animals_Corpus = tm_map(Pets_Animals_Corpus, removePunctuation)
Pets_Animals_Corpus[[1]][1]

#Remove stopwords
Pets_Animals_Corpus = tm_map(Pets_Animals_Corpus, removeWords, c(stopwords("english")))
Pets_Animals_Corpus[[1]][1]

# Stemming
#Pets_Animals_Corpus = tm_map(Pets_Animals_Corpus, stemDocument)
#Pets_Animals_Corpus[[1]][1]

# Eliminate white spaces
Pets_Animals_Corpus = tm_map(Pets_Animals_Corpus, stripWhitespace)
Pets_Animals_Corpus[[1]][1] 

inspect(Pets_Animals_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Pets_Animals_Corpus)
tdm<-TermDocumentMatrix(Pets_Animals_Corpus)

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
x <- udpipe_annotate(ud_model, x = Pets_Animals_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Pets_Animals", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Pets_Animals", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Pets_Animals", xlab = "Freq. Count")

##Creating Section to Process the Travel_Events Category:

Travel_Events_Titles <- Travel_Events_Data$title
str(Travel_Events_Titles)

# Create Corpus
Travel_Events_Corpus = Corpus(VectorSource(Travel_Events_Data$title))
# Look at Corpus
Travel_Events_Corpus[[1]][1]


#Conversion to Lowercase
Travel_Events_Corpus = tm_map(Travel_Events_Corpus, PlainTextDocument)
Travel_Events_Corpus[[1]][1]
Travel_Events_Corpus = tm_map(Travel_Events_Corpus, tolower)
Travel_Events_Corpus[[1]][1]

#Removing Punctuation
Travel_Events_Corpus = tm_map(Travel_Events_Corpus, removePunctuation)
Travel_Events_Corpus[[1]][1]

#Remove stopwords
Travel_Events_Corpus = tm_map(Travel_Events_Corpus, removeWords, c(stopwords("english")))
Travel_Events_Corpus[[1]][1]

# Stemming
#Travel_Events_Corpus = tm_map(Travel_Events_Corpus, stemDocument)
#Travel_Events_Corpus[[1]][1]

# Eliminate white spaces
Travel_Events_Corpus = tm_map(Travel_Events_Corpus, stripWhitespace)
Travel_Events_Corpus[[1]][1] 

inspect(Travel_Events_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Travel_Events_Corpus)
tdm<-TermDocumentMatrix(Travel_Events_Corpus)

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
x <- udpipe_annotate(ud_model, x = Travel_Events_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Travel_Events", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Travel_Events", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Travel_Events", xlab = "Freq. Count")

##Creating Section to Process the Gaming Category:

Gaming_Titles <- Gaming_Data$title
str(Gaming_Titles)

# Create Corpus
Gaming_Corpus = Corpus(VectorSource(Gaming_Data$title))
# Look at Corpus
Gaming_Corpus[[1]][1]


#Conversion to Lowercase
Gaming_Corpus = tm_map(Gaming_Corpus, PlainTextDocument)
Gaming_Corpus[[1]][1]
Gaming_Corpus = tm_map(Gaming_Corpus, tolower)
Gaming_Corpus[[1]][1]

#Removing Punctuation
Gaming_Corpus = tm_map(Gaming_Corpus, removePunctuation)
Gaming_Corpus[[1]][1]

#Remove stopwords
Gaming_Corpus = tm_map(Gaming_Corpus, removeWords, c(stopwords("english")))
Gaming_Corpus[[1]][1]

# Stemming
#Gaming_Corpus = tm_map(Gaming_Corpus, stemDocument)
#Gaming_Corpus[[1]][1]

# Eliminate white spaces
Gaming_Corpus = tm_map(Gaming_Corpus, stripWhitespace)
Gaming_Corpus[[1]][1] 

inspect(Gaming_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Gaming_Corpus)
tdm<-TermDocumentMatrix(Gaming_Corpus)

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
x <- udpipe_annotate(ud_model, x = Gaming_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Gaming", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Gaming", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Gaming", xlab = "Freq. Count")



##Creating Section to Process the People_Blogs Category:

People_Blogs_Titles <- People_Blogs_Data$title
str(People_Blogs_Titles)

# Create Corpus
People_Blogs_Corpus = Corpus(VectorSource(People_Blogs_Data$title))
# Look at Corpus
People_Blogs_Corpus[[1]][1]


#Conversion to Lowercase
People_Blogs_Corpus = tm_map(People_Blogs_Corpus, PlainTextDocument)
People_Blogs_Corpus[[1]][1]
People_Blogs_Corpus = tm_map(People_Blogs_Corpus, tolower)
People_Blogs_Corpus[[1]][1]

#Removing Punctuation
People_Blogs_Corpus = tm_map(People_Blogs_Corpus, removePunctuation)
People_Blogs_Corpus[[1]][1]

#Remove stopwords
People_Blogs_Corpus = tm_map(People_Blogs_Corpus, removeWords, c(stopwords("english")))
People_Blogs_Corpus[[1]][1]

# Stemming
#People_Blogs_Corpus = tm_map(People_Blogs_Corpus, stemDocument)
#People_Blogs_Corpus[[1]][1]

# Eliminate white spaces
People_Blogs_Corpus = tm_map(People_Blogs_Corpus, stripWhitespace)
People_Blogs_Corpus[[1]][1] 

inspect(People_Blogs_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(People_Blogs_Corpus)
tdm<-TermDocumentMatrix(People_Blogs_Corpus)

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
x <- udpipe_annotate(ud_model, x = People_Blogs_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in People_Blogs", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in People_Blogs", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in People_Blogs", xlab = "Freq. Count")
         

##Creating Section to Process the Comedy Category:

Comedy_Titles <- Comedy_Data$title
str(Comedy_Titles)

# Create Corpus
Comedy_Corpus = Corpus(VectorSource(Comedy_Data$title))
# Look at Corpus
Comedy_Corpus[[1]][1]


#Conversion to Lowercase
Comedy_Corpus = tm_map(Comedy_Corpus, PlainTextDocument)
Comedy_Corpus[[1]][1]
Comedy_Corpus = tm_map(Comedy_Corpus, tolower)
Comedy_Corpus[[1]][1]

#Removing Punctuation
Comedy_Corpus = tm_map(Comedy_Corpus, removePunctuation)
Comedy_Corpus[[1]][1]

#Remove stopwords
Comedy_Corpus = tm_map(Comedy_Corpus, removeWords, c(stopwords("english")))
Comedy_Corpus[[1]][1]

# Stemming
#Comedy_Corpus = tm_map(Comedy_Corpus, stemDocument)
#Comedy_Corpus[[1]][1]

# Eliminate white spaces
Comedy_Corpus = tm_map(Comedy_Corpus, stripWhitespace)
Comedy_Corpus[[1]][1] 

inspect(Comedy_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Comedy_Corpus)
tdm<-TermDocumentMatrix(Comedy_Corpus)

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
x <- udpipe_annotate(ud_model, x = Comedy_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Comedy", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Comedy", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Comedy", xlab = "Freq. Count")

##Creating Section to Process the News_Politics Category:

News_Politics_Titles <- News_Politics_Data$title
str(News_Politics_Titles)

# Create Corpus
News_Politics_Corpus = Corpus(VectorSource(News_Politics_Data$title))
# Look at Corpus
News_Politics_Corpus[[1]][1]


#Conversion to Lowercase
News_Politics_Corpus = tm_map(News_Politics_Corpus, PlainTextDocument)
News_Politics_Corpus[[1]][1]
News_Politics_Corpus = tm_map(News_Politics_Corpus, tolower)
News_Politics_Corpus[[1]][1]

#Removing Punctuation
News_Politics_Corpus = tm_map(News_Politics_Corpus, removePunctuation)
News_Politics_Corpus[[1]][1]

#Remove stopwords
News_Politics_Corpus = tm_map(News_Politics_Corpus, removeWords, c(stopwords("english")))
News_Politics_Corpus[[1]][1]

# Stemming
#News_Politics_Corpus = tm_map(News_Politics_Corpus, stemDocument)
#News_Politics_Corpus[[1]][1]

# Eliminate white spaces
News_Politics_Corpus = tm_map(News_Politics_Corpus, stripWhitespace)
News_Politics_Corpus[[1]][1] 

inspect(News_Politics_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(News_Politics_Corpus)
tdm<-TermDocumentMatrix(News_Politics_Corpus)

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
x <- udpipe_annotate(ud_model, x = News_Politics_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in News_Politics", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in News_Politics", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in News_Politics", xlab = "Freq. Count")


##Creating Section to Process the HowTo_Style Category:

HowTo_Style_Titles <- HowTo_Style_Data$title
str(HowTo_Style_Titles)

# Create Corpus
HowTo_Style_Corpus = Corpus(VectorSource(HowTo_Style_Data$title))
# Look at Corpus
HowTo_Style_Corpus[[1]][1]


#Conversion to Lowercase
HowTo_Style_Corpus = tm_map(HowTo_Style_Corpus, PlainTextDocument)
HowTo_Style_Corpus[[1]][1]
HowTo_Style_Corpus = tm_map(HowTo_Style_Corpus, tolower)
HowTo_Style_Corpus[[1]][1]

#Removing Punctuation
HowTo_Style_Corpus = tm_map(HowTo_Style_Corpus, removePunctuation)
HowTo_Style_Corpus[[1]][1]

#Remove stopwords
HowTo_Style_Corpus = tm_map(HowTo_Style_Corpus, removeWords, c(stopwords("english")))
HowTo_Style_Corpus[[1]][1]

# Stemming
#HowTo_Style_Corpus = tm_map(HowTo_Style_Corpus, stemDocument)
#HowTo_Style_Corpus[[1]][1]

# Eliminate white spaces
HowTo_Style_Corpus = tm_map(HowTo_Style_Corpus, stripWhitespace)
HowTo_Style_Corpus[[1]][1] 

inspect(HowTo_Style_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(HowTo_Style_Corpus)
tdm<-TermDocumentMatrix(HowTo_Style_Corpus)

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
x <- udpipe_annotate(ud_model, x = HowTo_Style_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in HowTo_Style", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in HowTo_Style", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in HowTo_Style", xlab = "Freq. Count")


##Creating Section to Process the Education Category:

Education_Titles <- Education_Data$title
str(Education_Titles)

# Create Corpus
Education_Corpus = Corpus(VectorSource(Education_Data$title))
# Look at Corpus
Education_Corpus[[1]][1]


#Conversion to Lowercase
Education_Corpus = tm_map(Education_Corpus, PlainTextDocument)
Education_Corpus[[1]][1]
Education_Corpus = tm_map(Education_Corpus, tolower)
Education_Corpus[[1]][1]

#Removing Punctuation
Education_Corpus = tm_map(Education_Corpus, removePunctuation)
Education_Corpus[[1]][1]

#Remove stopwords
Education_Corpus = tm_map(Education_Corpus, removeWords, c(stopwords("english")))
Education_Corpus[[1]][1]

# Stemming
#Education_Corpus = tm_map(Education_Corpus, stemDocument)
#Education_Corpus[[1]][1]

# Eliminate white spaces
Education_Corpus = tm_map(Education_Corpus, stripWhitespace)
Education_Corpus[[1]][1] 

inspect(Education_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Education_Corpus)
tdm<-TermDocumentMatrix(Education_Corpus)

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
x <- udpipe_annotate(ud_model, x = Education_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Education", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Education", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Education", xlab = "Freq. Count")


##Creating Section to Process the Science_Tech Category:

Science_Tech_Titles <- Science_Tech_Data$title
str(Science_Tech_Titles)

# Create Corpus
Science_Tech_Corpus = Corpus(VectorSource(Science_Tech_Data$title))
# Look at Corpus
Science_Tech_Corpus[[1]][1]


#Conversion to Lowercase
Science_Tech_Corpus = tm_map(Science_Tech_Corpus, PlainTextDocument)
Science_Tech_Corpus[[1]][1]
Science_Tech_Corpus = tm_map(Science_Tech_Corpus, tolower)
Science_Tech_Corpus[[1]][1]

#Removing Punctuation
Science_Tech_Corpus = tm_map(Science_Tech_Corpus, removePunctuation)
Science_Tech_Corpus[[1]][1]

#Remove stopwords
Science_Tech_Corpus = tm_map(Science_Tech_Corpus, removeWords, c(stopwords("english")))
Science_Tech_Corpus[[1]][1]

# Stemming
#Science_Tech_Corpus = tm_map(Science_Tech_Corpus, stemDocument)
#Science_Tech_Corpus[[1]][1]

# Eliminate white spaces
Science_Tech_Corpus = tm_map(Science_Tech_Corpus, stripWhitespace)
Science_Tech_Corpus[[1]][1] 

inspect(Science_Tech_Corpus)

## create a document matrix with the terms identified in the texts, with the columns as the terms or the rows as the terms
dtm<-DocumentTermMatrix(Science_Tech_Corpus)
tdm<-TermDocumentMatrix(Science_Tech_Corpus)

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
x <- udpipe_annotate(ud_model, x = Science_Tech_Titles) 
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


## NOUNS
stats <- subset(x, upos %in% c("PROPN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Nouns in Science_Tech", xlab = "Freq. Count")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Adjectives in Science_Tech", xlab = "Freq. Count")

## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most Occurring Verbs in Science_Tech", xlab = "Freq. Count")


##Now lets start the ML Process:

Videos_raw <- read.csv("VideoCategory.csv", stringsAsFactors = FALSE, header=F)
Videos_raw$V1 <- factor(Videos_raw$V1) ## transform to factors for ML

str(Videos_raw$V1)
table(Videos_raw$V1)

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
Videos_corpus_clean <- tm_map(Videos_corpus_clean, stripWhitespace)

Videos_dtm <- DocumentTermMatrix(Videos_corpus_clean)
Videos_dtm


## split the data (73%-27%)
Videos_dtm_train <- Videos_dtm[1:29880, ]
Videos_dtm_test <- Videos_dtm[29881:40835, ]

Videos_train_labels <- Videos_raw[1:29880, ]$V1
Videos_test_labels <- Videos_raw[29881:40835, ]$V1


## compare the proportion of Videos in the training and test data frames:
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
## We will be utilizing the naiveBayes() function in this package

#Classifier
Videos_classifier <- naiveBayes(Videos_train, Videos_train_labels)

#Predicter
Videos_test_pred <- predict(Videos_classifier, Videos_test)

library(gmodels)
#Load the crossTab for the predicted labels.
CrossTable(Videos_test_pred, Videos_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
