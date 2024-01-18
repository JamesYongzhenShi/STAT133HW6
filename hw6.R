# ===============================================
# Fill in the following fields
# ===============================================
# Title: HW6
# Description:Text Analysis of The Simpsons Transcripts
# Author: Yongzhen Shi
# Date:12/02/2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(ggwordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph) 
library(shiny)


# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "simpsons-transcripts.txt")



# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Text Analysis of The Simpsons Transcripts"),
  fluidRow(
    column(3,
           p(em("Decide on the scope of the word frequency analysis. ")),
           radioButtons(inputId = "scope", 
                        label = "Choose one", 
                        choices = c("Among all seasons" = "all_seasons",
                                    "For a given season" = "given_season",
                                    "Per season" = "per_season"), 
                        selected = "all_seasons"), 
           # if the person selects "for a given season"
           p(em("specify which the given season. ")),
           selectInput(inputId = "season", 
                       label = "choose one", 
                       choices = c("01" = "01",
                                   "02" = "02",
                                   "03" = "03",
                                   "04" = "04",
                                   "05" = "05",
                                   "06" = "06",
                                   "07" = "07",
                                   "08" = "08",
                                   "09" = "09",
                                   "10" = "10",
                                   "11" = "11",
                                   "12" = "12",
                                   "13" = "13",
                                   "14" = "14",
                                   "15" = "15",
                                   "16" = "16",
                                   "17" = "17",
                                   "18" = "18",
                                   "19" = "19",
                                   "20" = "20"
                                   ), 
                       selected = "boy")
    ),
    
    # Input for whether to remove stopwords
    column(3,
           p(em("Stopwords are uninteresting words. ")),
           radioButtons(inputId = "stopwords", 
                        label = "Would you like to remove stopwords?",
                        choices = c("Yes" = "sw_yes",
                                    "No" = "sw_no"),
                        selected = "sw_yes")
    ),
    
    # Input for computing sentiment score
    column(3,
           # length of ranking
           p(em("how long would you like the ranking to be?")), 
           sliderInput(inputId = "rank", 
                       label = "Length of ranking", 
                       min = 5, 
                       max = 30, 
                       value = 10, 
                       step = 1)
    ),
    
    # Inputs for sentiments
    column(3,
           # scope of sentiment analysis
           p(em("Decide on the scope for computing sentiment scores. ")),
           radioButtons(inputId = "sa_scope", 
                        label = "Choose one", 
                        choices = c("For each episode" = "by_episode",
                                    "For each season" = "by_season"),
                        selected = "by_season"), 
           # positive or negative sentiments
           p(em("Choose positive sentiments only, negative sentiments only, or both. ")),
           radioButtons(inputId = "sa_type",
                        label = "Choose one", 
                        choices = c("Positive sentiments" = "positive", 
                                    "Negative sentiments" = "negative", 
                                    "Both types of sentiments" = "both"))
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency Analysis",
                       h3("Word Frequency Analysis"),
                       plotOutput("barplot"),
                       h3("Wordcloud"),
                       plotOutput("wordcloud"), 
                       hr(),
                       h3("Most Frequent Words"), 
                       dataTableOutput('table')),
              tabPanel("Sentiment Analysis", 
                       h3("Sentiment Analysis"),
                       plotOutput("sa_barplot"),
                       h3("Wordclouds"),
                       plotOutput("advanced_wc"),
                       hr(),
                       h3("Sentiment Scores (from more positive to more negative"), 
                       dataTableOutput('sa_table'), 
                       tableOutput('score_summary'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in plot1)
  dat <- reactive({
    raw_dat <- read.table("simpsons-transcripts.txt", sep = "^" ,header=TRUE, nrows = 727, stringsAsFactors= F)
    # tokenize
    if(input$scope == "all_seasons") {
      tokens = unnest_tokens(tbl = raw_dat, 
                             output = word, 
                             input = text)%>% select(word)
    }
    if(input$scope == "given_season") {
      tokens = unnest_tokens(tbl = raw_dat %>% 
                               filter(season == input$season), 
                             output = word, 
                             input = text)%>% select(word)
    }
    if(input$scope == "per_season") {
      tokens = unnest_tokens(tbl = raw_dat, 
                             output = word, 
                             input = text)%>% select(season, word)
    }
    
    # remove stopwords
    if(input$stopwords == "sw_yes")
    {
      tokens = tokens %>% anti_join(stop_words, by = "word")
    }
    
    # compute word frequencies
    if(input$scope == "per_season") {
      token_freqs = tokens %>% group_by(season) %>% count(word)
    }
    else {
      token_freqs = tokens %>% count(word)
    }
  })
  
  # reactive object for sentiment scores
  dat2 <- reactive({
    # import data from csv file
    raw_dat <- read.table("simpsons-transcripts.txt", sep = "^" ,header=TRUE, nrows = 727, stringsAsFactors= F)
    
    # tokenize
    tokens = raw_dat %>% 
      unnest_tokens(output = word, input = text) %>% 
      anti_join(stop_words, by = "word")
    
    # open lexicon data
    afinn = readRDS("afinn.rds")
    
    # compute sentiment scores
    if(input$sa_scope == "by_episode") {
      sent_scores = tokens %>% 
        inner_join(afinn) %>% 
        group_by(episode) %>% 
        summarise(sentiment = sum(value)) %>% 
        arrange(desc(sentiment))
      sent_scores %>% rename(sentiment_score = sentiment)
    }
    else {
      sent_scores = tokens %>% 
        inner_join(afinn) %>% 
        group_by(season) %>% 
        summarise(sentiment = sum(value)) %>% 
        arrange(desc(sentiment))
      sent_scores %>% rename(sentiment_score = sentiment)
    }
  })
  
  # reactive object for most common positive & negative words by season
  dat3 <- reactive({
    # input data
    raw_dat <- read.table("simpsons-transcripts.txt", sep = "^" ,header=TRUE, nrows = 727, stringsAsFactors= F)
    
    # tokenize
    tokens = raw_dat %>% 
      unnest_tokens(output = word, input = text) %>% 
      anti_join(stop_words, by = "word")
    
    # word count
    wc = tokens %>% count(season, word, sort = TRUE) %>% ungroup()
    
    # sentiment analysis
    sents = wc %>% 
      group_by(season) %>%
      inner_join(sentiments, by = "word")
  })
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$barplot <- renderPlot({
    # when facetting is required
    if(input$scope == "per_season") {
      ggplot(data = dat() %>% arrange(desc(n)) %>% slice_head(n = input$rank), 
             aes(x = reorder_within(word, n, season), y = n)) + 
        geom_col(fill = "#E6B9A1") + 
        scale_x_reordered() + 
        facet_wrap(~ season, scales = "free") + 
        coord_flip() + 
        theme_minimal() + 
        labs(title = paste0("Top ", input$rank, " most frequent words by season"), 
             x = "Word", 
             y = "Count") +
        theme(
          panel.grid.major.y = element_blank(), 
          plot.title = element_text(size = 15), 
          axis.title = element_text(size = 13), 
          strip.text = element_text(size = 8)
        )
    }
    # when facetting is not required (only displays one graph)
    else {
      ggplot(data = dat() %>% arrange(desc(n)) %>% slice_head(n = input$rank), 
             aes(x = reorder(word, n), y = n)) + 
        geom_col(fill = "#E6B9A1") + 
        coord_flip() + 
        theme_minimal() + 
        labs(title = paste0("Top ", input$rank, " most frequent words"), 
             x = "Word", 
             y = "Count") +
        theme(
          panel.grid.major.y = element_blank(), 
          plot.title = element_text(size = 15), 
          axis.text = element_text(size = 11), 
          axis.title = element_text(size = 13), 
          strip.text = element_text(size = 7)
        )
    }
  })
  
  # code for wordcloud output
  output$wordcloud <- renderPlot({
    # when a single wordcloud is required
    if(input$scope != "per_season") {
      wordcloud(
        words = dat()$word, 
        freq = dat()$n, 
        max.words = 400 + input$rank, 
        random.order = FALSE, 
        colors = brewer.pal(8, "Dark2"))
    }
    # when many wordclouds are required
    else {
      ggplot(
        data = dat() %>% arrange(desc(n)) %>% slice_head(n = input$rank), 
        aes(label = reorder(word, n), 
            size = 3 * n, 
            color = season)) +
        geom_text_wordcloud_area() +
        theme_minimal() +
        facet_wrap(~ season) + 
        scale_size_area(max_size = 10) 
    }
  })
  
  # code for data table of most frequent words
  output$table <- renderDataTable({
    dat() %>% arrange(desc(n)) %>% slice_head(n = input$rank)
  })
  

  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
    output$sa_barplot <- renderPlot({
      # if including both sentiments
      if(input$sa_type == "both") {
        ggplot(
          data = dat3() %>% 
            arrange(desc(n)) %>% 
            slice_head(n = input$rank), 
          aes(x = reorder(word, n), 
              y = n, 
              fill = sentiment)) +
          geom_col() +
          scale_x_reordered() + 
          facet_wrap(~ season, scales = "free") + 
          coord_flip() + 
          theme_minimal() + 
          theme(legend.position = "top", 
                panel.grid.major.y = element_blank(), 
                strip.text = element_text(size = 7), 
                axis.title = element_text(size = 13), 
                plot.title = element_text(size = 15)) + 
          labs(x = "Word", 
               y = "Count", 
               title = paste0("U2's ", input$rank, " most common words with an associated sentiment, by season"))
      }
      # if only looking at positive or negative sentiments
      else {
        ggplot(
          data = dat3() %>% 
            filter(sentiment == input$sa_type) %>% 
            arrange(desc(n)) %>% 
            slice_head(n = input$rank), 
          aes(x = reorder(word, n), 
              y = n)) +
          geom_col(fill = "#DFCCF1") +
          scale_x_reordered() + 
          facet_wrap(~ season, scales = "free") + 
          coord_flip() +
          theme_minimal() + 
          theme(legend.position = "none", 
                panel.grid.major.y = element_blank(), 
                strip.text = element_text(size = 7), 
                axis.title = element_text(size = 13), 
                plot.title = element_text(size = 15)) + 
          labs(x = "Word", 
               y = "Count", 
               title = paste0("U2's ", input$rank, " most common words with an associated sentiment, by season"))
      }
    })
    
    # code for advanced wordcloud
    output$advanced_wc <- renderPlot({
      # if including both sentiments
      if(input$sa_type == "both") {
        ggplot(
          data = dat3() %>% 
            arrange(desc(n)) %>% 
            slice_head(n = input$rank), 
          aes(label = reorder(word, n), 
              size = 6 * n, 
              x = sentiment, 
              color = sentiment)) + 
          geom_text_wordcloud_area() +
          facet_wrap(~ season) + 
          scale_x_discrete(breaks = NULL) +
          theme_minimal() + 
          labs(title = "Most common words associated with a sentiment, by season", 
               x = NULL, 
               y = NULL) + 
          theme(axis.title = element_text(size = 13))
      }
      # if only looking at positive or negative sentiments
      else{
        ggplot(
          data = dat3() %>% 
            filter(sentiment == input$sa_type) %>% 
            arrange(desc(n)) %>% 
            slice_head(n = input$rank), 
          aes(label = reorder(word, n), 
              size = 6 * n, 
              x = sentiment, 
              color = season)) + 
          geom_text_wordcloud_area() +
          facet_wrap(~ season) + 
          scale_x_discrete(breaks = NULL) +
          theme_minimal() + 
          labs(title = "Most common words associated with a sentiment, by season", 
               x = NULL, 
               y = NULL) +
          theme(axis.title = element_text(size = 13))
      }
    })
    
    # code for sentiment scores
    output$sa_table <- renderDataTable({
      dat2()
    })
    
    # code for summary of sentiment scores
    output$score_summary <- renderTable({
      summarise(dat2(), 
                "Minimum" = min(sentiment_score), 
                "Maximum" = max(sentiment_score), 
                "Mean" = mean(sentiment_score), 
                "Standard Deviation" = sd(sentiment_score), 
                "10th Percentile" = quantile(sentiment_score, probs = 0.1), 
                "25th Percentile" = quantile(sentiment_score, probs = 0.25),
                "Median" = median(sentiment_score), 
                "75th Percentile" = quantile(sentiment_score, probs = 0.75), 
                "90th Percentile" = quantile(sentiment_score, probs = 0.9)
      )
    }, 
    align = 'c', 
    bordered = TRUE)
  }

# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

