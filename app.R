# Preference distribution dashboard for the 2019 Australian federal election
# Written by Alex Lum / User:Canley / @metacoretechs

library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(shiny)
library(networkD3)

divs_dop <- read.csv("dop_summary.csv")
state_divs <- read.csv("divenrol2019.csv")
colours <- read.csv("party_colours.csv")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            p {
                display:block;
                padding:9.5px;
                margin:0 0 5px;
                margin-top:5px;
            }"))),
  titlePanel("Preference distribution dashboard for the 2019 Australian federal election"),
  fluidRow(
    column(3,
      selectInput("state", "State", choices = unique(state_divs$state)),
      selectInput("division", "Electoral division", choices = NULL),
      url <- a("Data source: Australian Electoral Commission 2019, 'House of representatives downloads - Distribution and flow of preferences', 2019 Virtual Tally Room, viewed 6 June 2022.", href="https://results.aec.gov.au/24310/Website/HouseDownloadsMenu-24310-Csv.htm"),
    ),
    column(4, plotOutput("fp_plot", brush = "plot_brush", height = "200px")),
    column(4, plotOutput("tcp_plot", brush = "plot_brush", height = "200px")
    )
  ),
  fluidRow(
    uiOutput("diagram")
  ),
  fluidRow(
    p("Australia uses a preferential voting system, where voters number candidates sequentially in the order they prefer them. Each candidate's first preference votes are tallied (where a voter has written the number 1 next to that candidate on their ballot paper)."),
    p("For each subsequent count, the candidate with the lowest number of votes is excluded, and their votes are transferred at full value to the next preferred candidate on each ballot paper. At the end of the count, there will be be two candidates remaining, with their new vote counts called a two-party (or two-candidate) preferred result. The Sankey diagram above shows how this works.")
  )
    )

server <- function(input, output) {
  state <- reactive({
    filter(state_divs, state == input$state)
  })
  observeEvent(state(), {
    choices <- unique(state()$division)
    updateSelectInput(inputId = "division", choices = choices) 
    textOutput("result")
  })
  
    div_dop <- reactive({
      filter(divs_dop, DivisionNm == input$division)
    })
    
    dop_nodes <- reactive({
      temp_dop <- req(div_dop())
      temp_dop %>%
        filter(CalculationType == "Preference Count") %>%
        filter(Votes > 0) %>%
        left_join(colours, by = c("PartyAb" = "aec_abbrev")) %>%
        arrange(CountNum, desc(Votes)) %>%
        rowid_to_column(var = "node_id") %>%
        mutate(node_id = node_id - 1) %>%
        mutate(label = paste0(str_to_title(Surname), " (", PartyAb, ")"))
    })
    
    dop_values <- reactive({
      temp_nodes <- req(dop_nodes())
      temp_values <- req(div_dop()) %>%
        filter(CalculationType == "Transfer Count") %>%
        filter(Votes > 0)
    values <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(values) <- c("source", "value", "target")
    
    for (countnum in 0:(max(temp_nodes$CountNum))) {
      this_count <- temp_nodes %>%
        filter(CountNum == countnum)
      next_count <- temp_nodes %>%
        filter(CountNum == countnum + 1)
      transfer <- this_count %>%
        filter(node_id < max(node_id)) %>%
        inner_join(next_count, by = c("PartyAb", "Surname")) %>%
        select(source = node_id.x, value = Votes.x, target = node_id.y)
      excluded <- max(this_count$node_id)
      excluded_dist <- temp_values %>%
        ungroup() %>%
        filter(CountNum == countnum + 1) %>%
        inner_join(next_count, by = c("PartyAb", "Surname")) %>%
        arrange(node_id) %>%
        mutate(source = excluded, value = Votes.x) %>%
        select(source, value, target = node_id)
      values <- bind_rows(values, transfer, excluded_dist)
    }
    values
    })
    
    colrange <- reactive({
      temp_cols <- req(dop_nodes())
      temp_colrange <- paste(temp_cols$colour_code, collapse='","')
      temp_colrange
    })
    
    colscale <- reactive({
      temp_range <- req(colrange())
      temp_scale <- paste('d3.scaleOrdinal() .range(["',temp_range,'"])')
      temp_scale
    })
    
    fp_data <- reactive({
      temp_fp <- req(dop_nodes())
      fp <- temp_fp %>%
        filter(CountNum == 0) %>%
        select(colour_code, label, Votes)
      fp
    })
    
    tcp_data <- reactive({
      temp_tcp <- req(dop_nodes())
      tcp <- temp_tcp %>%
        filter(CountNum == max(CountNum)) %>%
        select(colour_code, label, Votes)
      tcp
    })
    
    output$fp_plot <- renderPlot({
      ggplot(fp_data(), aes(x=reorder(label, Votes), y=Votes, fill = reorder(colour_code, -Votes))) +
        geom_col() +
        scale_fill_manual(name = "",
                          labels = fp_data()$label,
                          values = fp_data()$colour_code) +
        coord_flip() +
        labs(x="Candidate", y="Votes", title="First preference votes")},
      height = 200, width = "auto")
    
    output$tcp_plot <- renderPlot({
      ggplot(tcp_data(), aes(x=reorder(label, Votes), y=Votes, fill = reorder(colour_code, -Votes))) +
        geom_col() +
        scale_fill_manual(name = "",
                          labels = tcp_data()$label,
                          values = tcp_data()$colour_code) +
        coord_flip() +
        labs(x="Candidate", y="Votes", title="Two-candidate-preferred votes")},
      height = 200, width = "auto")
    
    output$dopSankeyNetwork <- renderSankeyNetwork({
      sankeyNetwork(Links = dop_values(), Nodes = dop_nodes(),
                                Source = "source", Target = "target",
                                Value = "value", NodeID = "label",
                                fontSize= 12, fontFamily = 'sans-serif', nodeWidth = 20, iterations = 0, sinksRight = TRUE,
                                colourScale = colscale(),
                                height = 600, width = 1800)
    })
    
    output$diagram <- renderUI({
      sankeyNetworkOutput("dopSankeyNetwork")
    })
  }

shinyApp(ui, server)