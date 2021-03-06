SentiTwite
========================================================
author: Suma Krishnaprasad
date: 08/23/2015

Sentiment Analysis of Twitter messsages
========================================================

SentiTwite is a sentiment analysis app that allows users 
to enter a search keyword/phrase to analyze twitter
messages related to it.   
  
SentiTwite uses the "sentiment" package for its analysis

The classify_emotion and classify_polarity functions with 
Bayes algorithm is used in the analysis.

https://datadame.shinyapps.io/SentiTwite

Application Inputs
========================================================

User can enter a -  
  
* Keyword/Phrase  
* No of messages  
* Date range  
* Message Type -   Mixed, Recent or Popular

Application Outputs
========================================================

The app provides -   
  * A histogram of message count by emotion -  
      Joy, Sadness, Surprise, Fear, Anger, Disgust or Unknown  
  * A polarity graph of message count as positive, negative or neutral  
  * A wordcloud of all related words clustered and color coded by emotion  
  * Finally a list of all messages that were analyzed    

Sample Outputs
========================================================

For instance the polarity of a phrase - "The Stock Market lost 530 points on friday, it was a sad day for many investors"  
is classified as follows -

```
  BEST_FIT 
"negative" 
```
However the emotion of the same phrase is classified as -

```
 BEST_FIT 
"sadness" 
```

