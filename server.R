library(httr)
library(twitteR)
library(base64enc)
library(RCurl)
library(sentiment)
library(wordcloud)
library(ggplot2)
library(stringr)
 
 # load("twitteR_credentials")
# registerTwitterOAuth(twitCred)
 options(httr_oauth_cache=TRUE)
 setup_twitter_oauth("t1SdcitWjF447MH3h1uT9SfVr", "ZaRhXSTyLFXGB2sKxVVOuI4FC16Qm1NdsVEJYkQo3Vkyxco403", "34264715-ABcNyqqVRoGGwQv4DSNdH9BjrzvvZZi6YOdOQhvFJ", "oJZEbOyvritPAscu8ghxj2YlCcFu2FS7q8bD5xwbWNkVx")

getTweets<-function(txt,nooftweets,sDate,eDate,type){
  some_tweets = searchTwitter(txt, n=nooftweets, lang='en', since=sDate, until=eDate, resultType=type)
  some_txt = sapply(some_tweets, function(x) x$getText())
  # public_tweets = publicTimeline()
  # remove retweet entities
  some_txt=str_replace_all(some_txt,"[^[:graph:]]", " ") 
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  # remove at people
  some_txt = gsub("@\\w+", "", some_txt)
  # remove punctuation
  some_txt = gsub("[[:punct:]]", "", some_txt)
  # remove numbers
  some_txt = gsub("[[:digit:]]", "", some_txt)
  # remove html links
  some_txt = gsub("http\\w+", "", some_txt)
  # remove unnecessary spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
   
  # define "tolower error handling" function
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply
  some_txt = sapply(some_txt, try.error)
  
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  
  # Perform Sentiment Analysis
  # classify emotion
  class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # classify polarity
  class_pol = classify_polarity(some_txt, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  # Create data frame with the results and obtain some general statistics
  # data frame with results
  sent_df = data.frame(text=some_txt, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  return(sent_df)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  getTweets_df<-reactive({getTweets(input$text,input$slider,toString(input$StartDate),toString(input$EndDate),input$Type)})
  
  output$distPlot <- renderPlot({    
    # Let’s do some plots of the obtained results
    # plot distribution of emotions
    sent_df<-getTweets_df()
    ggplot(sent_df, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="emotion categories", y="number of tweets") +
      ggtitle(paste("Sentiment Analysis of Tweets about ",input$text, "\n(classification by emotion)")) +
      theme(plot.title = element_text(size=12, face="bold"))
    
  })
  
  output$polarityPlot <- renderPlot({
    # plot distribution of polarity
    sent_df<-getTweets_df()
    ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="polarity categories", y="number of tweets") +
      ggtitle(paste("Sentiment Analysis of Tweets about", input$text , "\n(classification by polarity)")) +
      theme(plot.title = element_text(size=12, face="bold"))
  })
  
  output$wordCloud <- renderPlot({
    sent_df<-getTweets_df()
    some_txt<-sent_df["text"]
    some_txt = some_txt[!is.na(some_txt)]
    names(some_txt) = NULL
    emos = levels(factor(sent_df$emotion))
    nemo = length(emos)
    emo.docs = rep("", nemo)
    for (i in 1:nemo)
    {
      tmp = some_txt[emotion == emos[i]]
      emo.docs[i] = paste(tmp, collapse=" ")
    }
    # remove stopwords
    emo.docs = removeWords(emo.docs, stopwords("english"))
    # create corpus
    corpus = Corpus(VectorSource(emo.docs))
    tdm = TermDocumentMatrix(corpus, control = list(
      wordLengths=c(0,Inf),
      removePunctuation = TRUE,
      removeNumbers = TRUE, tolower = TRUE))
    tdm = as.matrix(tdm)
    colnames(tdm) = emos
    # comparison word cloud
    comparison.cloud(tdm, max.words=input$slider, colors = brewer.pal(nemo, "Dark2"),
                     scale = c(5,.7), random.order = FALSE, title.size = 1.5)
   
  })
  
  output$tableMessages <- renderTable({
    
    sent_df<-getTweets_df()
    sent_df
  })
  
  output$helpText <- renderText({
    "SentiTwite is a shiny app that allows users to perform sentiment analysis of any keyword based on twitter messages.

    Enter a keyword or phrase of choice in the twitter text to analyze field
    Select the number of messages to analyze - More than a hundred messages can take a while - be patient
    Choose a date range to analyze messages
    Choose message type - 
            Mixed - Includes both most recent and popular messages
            Recent - Includes only the most recent 'x' messages
            Popular - Includes only the most popular 'x' messages in descending order of popularity
    
    The app uses the 'sentiment' R package for its analysis.
    The app displays a histogram of number of messages broken down by various emotions - 
        Joy, Sadness, Fear, Anger, Disgust, Surprise and Unknown
    It also displays a polarity bar graph of positive, negative and neutral messages
    A word cloud color-coded and clustered by emotions
    Finally a list of messages that were analyzed is displayed for users to better understand the results"
  })
  
})