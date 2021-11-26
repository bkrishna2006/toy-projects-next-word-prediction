library(shiny)
library(dplyr)
library(stringi)

dfAllselNew = read.csv("LookupFinal_01_99_modified.csv",header = TRUE)
#Test = read.csv("Test.csv",header = TRUE)
df <- dfAllselNew
df <- mutate(dfAllselNew,prob=round( (Freq/sum(Freq)*100),2))
df <- df[order(df$ngram, -df$prob),]

#df = readRDS(file = "df.RData")
#df = read.csv("LookupFinal_01_99_modified.csv")
#write.csv(dfOld,"df.csv")
#df = read.csv("df.csv")
#load("df.RData")
#setwd(myGenDataDir)
#load("allgram.RData")
#options(shiny.reactlog=TRUE)

shinyServer(function(input, output) {

  inputWordsReactive <- reactive({
    inputText <- input$my_terms
  })

  output$predictedWords = renderText({
    termsSimple()
  })

  termsSimple <- reactive({
    
    t <- stri_trans_tolower(inputWordsReactive())

# Need  to add a logic to replace I'd I'll etc with I would I will etc...
#    t <- stri_replace(t,replacement = )
    t <- stri_replace_all_regex(t,"[[:punct:]]", "")
    t <- stri_replace_all_regex(t,"[^[:alnum:]]", " ")
  #  str_replace_all(x, "[[:punct:]]", " ")
    
    if (t=="") {
      return(" ")
    }
    t <- unlist(strsplit(x = t,split = " "))
#    t <- unlist(strsplit(x = stri_trans_tolower(inputWordsReactive()),split = " "))
    last <- t[length(t)]
    lastBut1 <- t[length(t)-1]
    lastBut2 <- t[length(t)-2]
    lastBut3 <- t[length(t)-3]
    lastBut4 <- t[length(t)-4]
    t5 <- paste(lastBut4,lastBut3,lastBut2,lastBut1,last, sep = " ")
    t4 <- paste(lastBut3,lastBut2,lastBut1,last, sep = " ")
    t3 <- paste(lastBut2,lastBut1,last, sep = " ")
    t2 <- paste(lastBut1,last, sep = " ")
    t1 <- paste(last)
    
#    allgramTempt5 <- filter(.data = dfAllsel,ngram == t5)
#    allgramTempt4 <- filter(.data = dfAllsel,ngram == t4)
#    allgramTempt3 <- filter(.data = dfAllsel,ngram == t3)
#    allgramTempt2 <- filter(.data = dfAllsel,ngram == t2)
#    allgramTempt1 <- filter(.data = dfAllsel,ngram == t1)

     allgramTempt5 <- filter(.data = df,ngram == t5) 
     allgramTempt4 <- filter(.data = df,ngram == t4)
     allgramTempt3 <- filter(.data = df,ngram == t3)
     allgramTempt2 <- filter(.data = df,ngram == t2)
     allgramTempt1 <- filter(.data = df,ngram == t1)
 
# for testing, once Test.csv is loaded...       

#    allgramTempt5 <- filter(.data = Test,ngram == t5)   
#    allgramTempt4 <- filter(.data = Test,ngram == t4)     
#    allgramTempt3 <- filter(.data = Test,ngram == t3)
#    allgramTempt2 <- filter(.data = Test,ngram == t2)
#    allgramTempt1 <- filter(.data = Test,ngram == t1)
    
    
# to keep predicted word place-holder clean
    
#    blank <- " "
#    return(blank)
    
      if (length(allgramTempt5$ngram) > 0) {
          predTextTemp1 <- filter(allgramTempt5, prob == max(prob))
          predTextTemp2 <- select(predTextTemp1,PredNext)
          for(i in 1:length(predTextTemp2$PredNext)){
            predictedWord.i <- paste(predTextTemp2$PredNext,"\n")
            return(predictedWord.i)        
          }
      } else if (length(allgramTempt4$ngram) > 0) { 
          predTextTemp1 <- filter(allgramTempt4, prob == max(prob))
          predTextTemp2 <- select(predTextTemp1,PredNext)
          for(i in 1:length(predTextTemp2$PredNext)){
            predictedWord.i <- paste(predTextTemp2$PredNext,"\n")
            return(predictedWord.i)  
        }
      } else if (length(allgramTempt3$ngram) > 0) {
        predTextTemp1 <- filter(allgramTempt3, prob == max(prob))
        predTextTemp2 <- select(predTextTemp1,PredNext)
        for(i in 1:length(predTextTemp2$PredNext)){
          predictedWord.i <- paste(predTextTemp2$PredNext,"\n")
          return(predictedWord.i)
        }
      } else if (length(allgramTempt2$ngram) > 0) {
          predTextTemp1 <- filter(allgramTempt2, prob == max(prob))
          predTextTemp2 <- select(predTextTemp1,PredNext)
          for(i in 1:length(predTextTemp2$PredNext)){
            predictedWord.i <- paste(predTextTemp2$PredNext,"\n")
            return(predictedWord.i)
        }
      } else if (length(allgramTempt1$ngram) > 0) {
          predTextTemp1 <- filter(allgramTempt1, prob == max(prob))
          predTextTemp2 <- select(predTextTemp1,PredNext)
          for(i in 1:length(predTextTemp2$PredNext)){
            predictedWord.i <- paste(predTextTemp2$PredNext,"\n")
            return(predictedWord.i)
        }
    } else if (length(allgramTempt1$ngram) == 0) {
#      return(predictedWord.i)
      return("SORRY...Cannot be predicted...")      
    }  
  })
})
