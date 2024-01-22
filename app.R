library(shiny)
meta <- readr::read_csv("data/sections.csv")

ui <- fluidPage(
  selectInput(
    inputId = "project",
    label = "Project",
    choices = unique(meta$project) |> sort()
  ),
  uiOutput("year"),
  uiOutput("section"),
  uiOutput("markdown")
  
)

server <- function(input, output, session) {
  
  output$year <- renderUI({
   req(input$project)
    year <- meta |> dplyr::filter(project == input$project) |>
      dplyr::arrange(year) |>
      dplyr::pull(year)
    selectInput(
      inputId = "year",
      label = "Year",
      choices = year,
      selected = 1
    )
  })
  
  output$section <- renderUI({
    req(input$project)
    req(input$year)
    section <- meta |> dplyr::filter(project == input$project, year == input$year) |>
      dplyr::pull(section)
    
    selectInput(
      inputId = "section_",
      label = "Section",
      choices = section,
      selected = section, 
      multiple = TRUE
    )
  })
  
  
  output$markdown <- renderUI({
    
    in_file <- meta |>
      dplyr::filter(
        project == input$project, 
        year == input$year
      ) |>
      dplyr::distinct(file) |>
      dplyr::pull()
    
    md <- readLines(file.path("data", in_file))
    
    from <- meta |> 
      dplyr::filter(
        project == input$project, 
        year == input$year,
        section %in% input$section_
      ) |> 
      dplyr::pull(from) |>
      min()
    
    to <- meta |> 
      dplyr::filter(
        project == input$project, 
        year == input$year,
        section %in% input$section_
      ) |> 
      dplyr::pull(to) |>
      max()
    
    markdown(md[from:to])
    
  })
  
  
  
}

shinyApp(ui, server)