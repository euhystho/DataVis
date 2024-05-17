library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)
library(dplyr)

#| warning = FALSE,


most_engaging <- value_box(
  title = "Most Positive articles",
  value = "Sports and Finance", 
  showcase = bs_icon("journal-check"),
  theme = "success",
)


least_engaging <- value_box(
  title = "Most Negative articles",
  value = "Education and Politics", 
  showcase = bs_icon("journal-arrow-down"),
  theme = "danger",
)


# Define UI ----
ui <- navbarPage( 
  title = "Eugene's Shiny Adventure",
  underline = TRUE,
  tabsetPanel(
    tabPanel(title = "Insights and Findings", 
             layout_column_wrap(
               width = 1/2,
               fill = FALSE,
               most_engaging, least_engaging
             ),
             value_box(
               title = "Insights",
               value = "Addressing Problem", 
               showcase = bs_icon("card-checklist"),
               theme = "info",
             ),
             plotOutput("barGraph"),
             
),
         

    tabPanel(title = "Explaining Thea's VARIABLES :)", 
       card(
         full_screen = TRUE,
         card(
             value_box(
               title = "TL;DR",
               value = "Sentiment Analysis",
               showcase = bs_icon("emoji-wink"),
               showcase_layout = "top right",
               theme = "danger",
               p("The application of using a lexicon 
                 (dictionary with defined values),"),
               p("to find patterns and insights into how positive or negative words or sentences are"),
               p("by applying it to texts to analyze overall sentiment (feelings)")
             ),
           
             value_box(
               title = "TL;DR",
               value = "AFINN",
               showcase = bs_icon("file-text"),
               showcase_layout = "top right",
               theme = "info",
               p("AFINN is a lexicon that lists English terms scored between -5 and 5"),
               p("with -5 being the most negative sentiment and 5 being the most positive"),
               p("the lexicon was developed by Finn Årup Nielsen between 2009 and 2011.")
             ),
             card_footer (
               "Source: Ketchley, S. (2023, August 22)",
               popover(
                 a("Learn more...", href = "#"),
                 markdown (
                   "Source: Ketchley, S. (2023, August 22). Enhancements to Sentiment Analysis in Gale Digital Scholar Lab. Review.gale.com. [Click here to go to the website](https://review.gale.com/2023/08/22/understanding-recent-enhancements-to-sentiment-analysis-in-gale-digital-scholar-lab/)"
                 )
               )
             )
           ),
             
             value_box(
               title = "Variable Name:",
               value = "sentiment_val_change", 
               showcase = bs_icon("clipboard2-pulse-fill"),
               showcase_layout = "top right",
               theme = "light",
               p("Obtains every text (only responses and prompts) from the dataset,"),
               p("Assigns an expanded AFINN (developed by Thea) sentiment value per type of text,"),
               p("Computing the average sentiment value per each type,"),
               p("Taking difference of sentiment in the response and prompt,"),
               p("Thus, determining how much students changed their answer based on prompts.")
             ),

      ),
    ),
  
  ),
  theme = bs_theme(
    bootswatch = "superhero",
    base_font = font_google("Lexend"),
  
  ),

  sidebar = sidebar(
    title = "Filters",
    fillable = TRUE,
    accordion_filters <- accordion(
      accordion_panel(
        "Types of Articles", icon = bsicons::bs_icon("book"),
          checkboxGroupInput("type_selection", "Types of Articles:",
                             choices = c("Sports", "Politics", "Finance","Education"),
                             selected = c("Sports", "Politics")
          ),
        )
        
      ),
      accordion_panel(
        "Engagement and Sentiment", icon = bsicons::bs_icon("person-workspace"),
        checkboxGroupInput("sentiment_selection", "Engagement and Sentiment:",
                           choices = c("positive" = "pos", "negative" = "neg"),
                           selected = c("pos", "neg")
        ),
      
    )
  ),



)
# Define server logic ----
server <- function(input, output, session) {
  data <- reactive ({
  #Read the File 
    read.csv("data.csv")
  })
    
  
  observe({
    updateCheckboxGroupInput(session, "type_selection",
                             choices = c("Sports", "Politics", "Finance","Education"))
    updateCheckboxGroupInput(session, "sentiment_selection",
                             choices = c("positive", "negative"))
  })
  
  barGraphData <- reactive({
    df <- data.frame(category = c("Sports", "Politics", "Finance","Education"),
                     sentiment = c("positive", "negative"))
    selected_categories <- input$type_selection
    selected_sentiments <- input$sentiment_selection
    df <- df %>% filter (category %in% selected_categories, 
                         sentiment %in% selected_sentiments)
    return(df)
  })
  
  output$barGraph <- renderPlot({
    ggplot(barGraphData(), aes(x = category, y = sentiment, fill = sentiment)) +
      geom_bar(stat = "identity") +
      theme_gray() +
      labs(title = "Sentiment Analysis Per Article Type",
           x = "Article Categories",
           y = "Type of Sentiment"
           ) + scale_fill_manual(values = c("positive"= "#5db85b", "negative" = "#d9534f"))
  })


}

# Run the app ----
shinyApp(ui, server)