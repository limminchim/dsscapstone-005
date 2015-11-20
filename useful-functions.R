###################################################################################

## ANNEX B: Useful Functions

#install.packages("devtools")
#require(devtools)
#library(devtools)

#install.packages("tm")
#install.packages("NLP")
#install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
#install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org")
#install.packages("ggplot2")
#install.packages("RColorBrewer")
#install.packages("wordcloud")
#install.packages("RWeka")
#install.packages("grid")

#install.packages("stringi")
#install.packages("SnowballC")
#install.packages("xtable")

library(ggplot2)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(tm.lexicon.GeneralInquirer)
library(slam)
library(grid)


################################################################################
# Function to compute net sentiment score by analyzing text (tweets, blogs, reviews)
#
# Input: Text objects
# - text: vector of text objects
#
# Calculate the positive and negative sentiment scores of text
#
# Output:
# - Data frame of 3 columns: positive score, negative score, net score
#   where net score = positive score - negative score)
#
# Reference: http://stackoverflow.com/questions/29918017/how-to-use-sentiment-package-in-r-3-2-0

getSentimentScore = function(text)
{
    require(tm.lexicon.GeneralInquirer)
    #require(tm.plugin.sentiment)
    require(tm)

    corpus <- Corpus(VectorSource(text))
    pos <- sum(sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Positiv")))
    neg <- sum(sapply(corpus, tm_term_score, terms_in_General_Inquirer_categories("Negativ")))
    pos.score <- tm_term_score(TermDocumentMatrix(corpus, 
                                                  control = list(removePunctuation = TRUE)),
                               terms_in_General_Inquirer_categories("Positiv")) 
    neg.score <- tm_term_score(TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE)), 
                               terms_in_General_Inquirer_categories("Negativ")) 
    total.df <- data.frame(positive = pos.score, negative = neg.score)
    total.df <- transform(total.df, score = positive - negative)
    total.df
}

################################################################################
# Generic function to clean up text for visualization with wordcloud
#
# Input: Text objects
# - x: vector of text objects
#
# - can be used for tweets, blogs and reviews
# - remove rt
# - remove at
# - remove punctuation
# - remove numbers
# - remove tabs
# - remove HTTP links
# - remove blank space at beginning and end
#
# Output:
# - Text objects
#
# Reference: https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud

# Clean up text prior using for wordcloud
cleanUpText = function(x)
{
    # tolower
    x = tolower(x)
    # remove rt
    x = gsub("rt", "", x)
    # remove at
    x = gsub("@\\w+", "", x)
    # remove punctuation
    x = gsub("[[:punct:]]", "", x)
    # remove numbers
    x = gsub("[[:digit:]]", "", x)
    # remove links http
    x = gsub("http\\w+", "", x)
    # remove tabs
    x = gsub("[ |\t]{2,}", "", x)
    # remove blank spaces at the beginning
    x = gsub("^ ", "", x)
    # remove blank spaces at the end
    x = gsub(" $", "", x)
    return(x)
}

# Cleans text and display top number of words (x) or Inf in a wordcloud

################################################################################
# Generic function to cleanup text and display in wordcloud
#
# Input: 
# - x: vector of text objects
# - max.words: Maximum number of words to be plotted. 
# - title: title of plot to be displayed  
#
# - cleans up text 
# - remove stopwords
# - dislay wordcloud
#
# Output:
# - Nothing
#
# Reference: https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud

renderCleanedWordCloud = function(x, max.words=Inf, title = "")
{
    cleanedText <- cleanUpText(x)
    vector <- paste(cleanedText, collapse=" ")
    remwords <- c("the", "just",
                  "will", "would", "shall", "should",
                  "got", "ive",
                  "they", "them",
                  "he", "his",
                  "she", "hers")
    vector <- removeWords(vector,c(stopwords("english"),remwords))
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, title)
    wordcloud(vector,  
              random.order=FALSE,
              max.words=max.words,
              rot.per=0.35,
              colors = brewer.pal(8,"Dark2"))
}

################################################################################
# Generic function to display words in wordcloud (without cleanup of words)
#
# Input: 
# - words: words to be displayed
# - freq: word frequencies
# - max.words: Maximum number of words to be plotted. 
# - title: title of plot to be displayed  
# - scale: vector of length 2 indicating the range of the size of the words
# - min.freq: words with frequency below min.freq will not be plotted
#
# Output:
# - Nothing

renderWordCloud = function(words, freq, max.words = Inf, title = "", scale = c(5,0.5), min.freq = 1)
{
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, title)
    
    wordcloud(words = words,
              freq = freq,
              random.order=FALSE,
              rot.per=0.35,
              min.freq = 1,
              scale = scale,
              use.r.layout=FALSE,
              colors=brewer.pal(8, "Dark2"))
}


################################################################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# Ref: https://rpubs.com/AsemRadhwi/capstone

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

################################################################################
# Functions to splits a string into an n-gram with min and max grams. 
# Specifically for 1-gram, 2-gram, 3-gram and 4-gram 
#
# Ref: https://rpubs.com/AsemRadhwi/capstone
require(RWeka)
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Useful Functions
###############################################################################

