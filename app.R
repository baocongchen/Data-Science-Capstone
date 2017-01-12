# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tm)
library(RWeka)
library(dplyr)

if (all(file.exists('freq_one.rds','freq_two.rds','freq_three.rds','freq_four.rds','freq_five.rds') == TRUE)) {
  print('objects already exist')
  freq_one <- readRDS('freq_one.rds')
  freq_two <- readRDS('freq_two.rds')
  freq_three <- readRDS('freq_three.rds')
  freq_four <- readRDS('freq_four.rds')
  freq_five <- readRDS('freq_five.rds')
} else {
  print('no objects')
  twitter_con <- file('en_US.twitter.txt', 'r')
  twitter <- readLines(twitter_con, skipNul = T)
  close(twitter_con)
  # summary(sapply(twitter,nchar))
  # loves <- sum(grepl('love', twitter))
  # hates <- sum(grepl('hate', twitter))
  # loves/hates
  # twitter[which(grepl('biostat', twitter))]
  # which(grepl('A computer once beat me at chess, but it was no match for me at kickboxing', twitter))
  
  news_con <- file('en_US.news.txt', 'r')
  news <- readLines(news_con, skipNul = T, n = 77258)
  close(news_con)
  # max(sapply(news,nchar))
  
  blogs_con <- file('en_US.blogs.txt', 'r')
  blogs <- readLines(blogs_con, skipNul = T)
  close(blogs_con)
  # max(sapply(blogs,nchar))
  
  profaneDoc <- file('Terms-to-Block.csv','r')
  profane <- readLines(profaneDoc, n = 722)
  close(profaneDoc)
  
  # Randomly select 80000 lines from the text data 
  set.seed(1000)
  data <- sample(c(twitter, news, blogs), 80000)
  
  # Perform text transformation for analytics using tm package
  corpus <- Corpus(VectorSource(data))
  
  # pre-process data
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) {
    gsub("[^[:alnum:][:space:]']", ' ', x)
  }))
  corpus <- tm_map(corpus, content_transformer(function(x) {
    gsub('\\S{25,} ', '', x)
  }))
  
  # inspect(profane)
  corpus <- tm_map(corpus, removeWords, profane)
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from = 'latin1', to = 'ASCII', sub = '')))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Create Term Document Matrix with strings split into n-grams 
  
  tdm_1 <- TermDocumentMatrix(corpus, control = list(tokenize =  function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))))
  tdm_datfr_1 <- data.frame(index = tdm_1$i, term = tdm_1$dimnames$Terms[tdm_1$i], score = tdm_1$v)
  tdm_2 <- TermDocumentMatrix(corpus, control = list(tokenize =  function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))))
  tdm_datfr_2 <- data.frame(index = tdm_2$i, term = tdm_2$dimnames$Terms[tdm_2$i], score = tdm_2$v)
  tdm_3 <- TermDocumentMatrix(corpus, control = list(tokenize =  function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))))
  tdm_datfr_3 <- data.frame(index = tdm_3$i, term = tdm_3$dimnames$Terms[tdm_3$i], score = tdm_3$v)
  tdm_4 <- TermDocumentMatrix(corpus, control = list(tokenize =  function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))))
  tdm_datfr_4 <- data.frame(index = tdm_4$i, term = tdm_4$dimnames$Terms[tdm_4$i], score = tdm_4$v)
  tdm_5 <- TermDocumentMatrix(corpus, control = list(tokenize =  function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))))
  tdm_datfr_5 <- data.frame(index = tdm_5$i, term = tdm_5$dimnames$Terms[tdm_5$i], score = tdm_5$v)
  
  # Let's try to predict which word appear next
  freq_one <- count(tdm_datfr_1, term, wt = score, sort = T)
  freq_two <- count(tdm_datfr_2, term, wt = score, sort = T)
  freq_three <- count(tdm_datfr_3, term, wt = score, sort = T)
  freq_four <- count(tdm_datfr_4, term, wt = score, sort = T)
  freq_five <- count(tdm_datfr_5, term, wt = score, sort = T)
  
  saveRDS(freq_one, file='freq_one.rds')
  saveRDS(freq_two, file='freq_two.rds')
  saveRDS(freq_three, file='freq_three.rds')
  saveRDS(freq_four, file='freq_four.rds')
  saveRDS(freq_five, file='freq_five.rds')
  rm(twitter, blogs, news, data, corpus, tdm_1, tdm_2, tdm_3, tdm_4, tdm_5, tdm_datfr_1,
     tdm_datfr_2, tdm_datfr_3, tdm_datfr_4, tdm_datfr_5, twitter_con, blogs_con, news_con)
}

ui <- shinyUI(navbarPage("LUX et VERITAS",
                         # Select a theme
                         theme = shinytheme('united'),
                         # Create tab panel
                         tabPanel('Text Wizard',
                                  tags$head(
                                    tags$link(rel = "stylesheet", type = "text/css", href = "modified.css")
                                  ),
                                  br(),
                                  fluidRow(
                                    column(6,
                                           tags$textarea(id = 'text', placeholder = 'Type here', rows = 8, ""),
                                           submitButton('Submit')
                                    ),
                                    br(),
                                    column(6,
                                           wellPanel(textOutput('recom'))
                                    )
                                  ),
                                  
                                  br(),
                                  fluidRow(
                                    tags$footer(
                                      tags$address(tags$a(href='mailto:peter_tran_cm@yahoo.com','Thong B. Tran')),
                                      tags$p(tags$small('Copyright (c) 2016 Text Wizard All rights reserved'))
                                    )
                                  )
                                  ),
                         tabPanel('Documentation',
                                  div(a(href='http://rpubs.com/Thong/text-wizard','Click Here'))                                     )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  raw_input <- reactive({input$text})
  processed <- reactive({
    raw_input() %>%
      tolower() %>%
      (function(x) { gsub("[^[:alnum:][:space:]']", ' ', x)}) %>%
      (function(x) { gsub('\\S{25,} ', '', x)}) %>%
      (function(x) { iconv(x, from = 'latin1', to = 'ASCII', sub = '')}) %>%
      removeNumbers() %>%
      stripWhitespace() %>%
      (function(x) { gsub(pattern = '^\\s|\\s$', '', x)})
  })
  
  output$recom <- renderText({
    if(processed() != '') {
      forty_char <- substring(processed(), nchar(processed()) - 40, last = 1000000L)
      last_four_words <- tail(unlist(strsplit(forty_char, ' ')), 4)
      recom <- c()
      # Check input length if it has 4 words or above
      if(length(last_four_words) >= 4) {
        four_words <- paste(last_four_words, sep = ' ', collapse = ' ') 
        recom_4x <- sapply(strsplit(as.character(freq_five$term[grep(paste0('^', four_words, ' '), freq_five$term)]), '\\s'), function(x) x[5])
        if ( length(recom_4x) < 8) {
          recom_4x <- sapply(strsplit(as.character(freq_five$term[grep(paste0('^', four_words), freq_five$term)]), '\\s'), function(x) x[4])
          recom <- unique(c(recom, recom_4x))
          if(length(recom) < 8) {
            three_words <- paste(last_four_words[-1], sep = ' ', collapse = ' ') 
            recom_3x <- sapply(strsplit(as.character(freq_four$term[grep(paste0('^', three_words, ' '), freq_four$term)]), '\\s'), function(x) x[4])
            if ( length(recom_3x) == 0) {
              recom_3x <- sapply(strsplit(as.character(freq_four$term[grep(paste0('^', three_words), freq_four$term)]), '\\s'), function(x) x[3])
            }
            recom <- unique(c(recom, recom_3x))
            if(length(recom) < 8) {
              two_words <- paste(last_four_words[-1:-2], sep = ' ', collapse = ' ') 
              recom_2x <- sapply(strsplit(as.character(freq_three$term[grep(paste0('^', two_words, ' '), freq_three$term)]), '\\s'), function(x) x[3])
              if ( length(recom_2x) == 0) {
                recom_2x <- sapply(strsplit(as.character(freq_three$term[grep(paste0('^', two_words), freq_three$term)]), '\\s'), function(x) x[2])
              }
              recom <- unique(c(recom, recom_2x))
              
              if(length(recom) < 8) {
                one_word <- paste(last_four_words[-1:-3], sep = ' ', collapse = ' ') 
                recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word, ' '), freq_two$term)]), '\\s'), function(x) x[2])
                if ( length(recom_1x) == 0) {
                  recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word), freq_two$term)]), '\\s'), function(x) x[1])
                }
                recom <- unique(c(recom, recom_1x))
                if(8 - length(recom) > 0) {
                  recom <- c(recom, as.character(freq_one$term[1:8]))
                }
              }
            }
          }
        }
      }
      
      # Check input length if it has 3 words
      if(length(last_four_words) == 3) {
        three_words <- paste(last_four_words, sep = ' ', collapse = ' ') 
        recom_3x <- sapply(strsplit(as.character(freq_four$term[grep(paste0('^', three_words, ' '), freq_four$term)]), '\\s'), function(x) x[4])
        if ( length(recom_3x) == 0) {
          recom_3x <- sapply(strsplit(as.character(freq_four$term[grep(paste0('^', three_words), freq_four$term)]), '\\s'), function(x) x[3])
        }
        
        recom <- unique(c(recom, recom_3x))
        if(length(recom) < 8) {
          two_words <- paste(last_four_words[-1], sep = ' ', collapse = ' ') 
          recom_2x <- sapply(strsplit(as.character(freq_three$term[grep(paste0('^', two_words, ' '), freq_three$term)]), '\\s'), function(x) x[3])
          if ( length(recom_2x) == 0) {
            recom_2x <- sapply(strsplit(as.character(freq_three$term[grep(paste0('^', two_words), freq_three$term)]), '\\s'), function(x) x[2])
          }
          recom <- unique(c(recom, recom_2x))
          
          if(length(recom) < 8) {
            one_word <- paste(last_four_words[-1:-2], sep = ' ', collapse = ' ') 
            recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word, ' '), freq_two$term)]), '\\s'), function(x) x[2])
            if ( length(recom_1x) == 0) {
              recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word), freq_two$term)]), '\\s'), function(x) x[1])
            }
            recom <- unique(c(recom, recom_1x))
            if(8 - length(recom) > 0) {
              recom <- c(recom, as.character(freq_one$term[1:8]))
            }
          }
        }
      }
      
      # Check input length if it has 2 words
      if(length(last_four_words) == 2) {
        two_words <- paste(last_four_words, sep = ' ', collapse = ' ') 
        recom_2x <- sapply(strsplit(as.character(freq_three$term[grep(paste0('^', two_words, ' '), freq_three$term)]), '\\s'), function(x) x[3])
        if (length(recom_2x) == 0) {
          recom_2x <- sapply(strsplit(as.character(freq_three$term[grep(paste0('^', two_words), freq_three$term)]), '\\s'), function(x) x[2])
        }
        recom <- unique(c(recom, recom_2x))
        
        if(length(recom) < 8) {
          one_word <- paste(last_four_words[-1], sep = ' ', collapse = ' ') 
          recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word, ' '), freq_two$term)]), '\\s'), function(x) x[2])
          if ( length(recom_1x) == 0) {
            recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word), freq_two$term)]), '\\s'), function(x) x[1])
          }
          recom <- unique(c(recom, recom_1x))
          if(8 - length(recom) > 0) {
            recom <- c(recom, as.character(freq_one$term[1:8]))
          }
        }
      }
      
      # Check input length if it has 1 word
      if(length(last_four_words) == 1) {
        one_word <- paste(last_four_words, sep = ' ', collapse = ' ') 
        recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word, ' '), freq_two$term)]), '\\s'), function(x) x[2])
        if ( length(recom_1x) == 0) {
          recom_1x <- sapply(strsplit(as.character(freq_two$term[grep(paste0('^', one_word), freq_two$term)]), '\\s'), function(x) x[1])
        }
        recom <- unique(c(recom, recom_1x))
        if(8 - length(recom) > 0) {
          recom <- c(recom, as.character(freq_one$term[1:8]))
        }
      }
      
      recom <- unique(recom)
      print(paste0(recom[1:8], sep = ' ', collapse = '| '))
    }
    
  })
  
})
# Run the application 
shinyApp(ui = ui, server = server)