library(shiny)

# section 1 
# questions and choices 
quiz_data <- list(
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila obscura", "Drosophila subobscura", "Drosophila simulans"),
    answer = "Drosophila subobscura",
    image_filename = "images/drosophila_subobscura.png"
  ),
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila melanogaster", "Drosophila histrio", "Drosophila simulans"),
    answer = "Drosophila histrio",
    image_filename = "images/drosophila_histrio.png"
  ), 
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila histrio", "Drosophila melanogaster", "Drosophila simulans"),
    answer = "Drosophila melanogaster",
    image_filename = "images/drosophila_melanogaster.png"
  ), 
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila simulans", "Drosophila funebris", "Drosophila suzukii"),
    answer = "Drosophila suzukii",
    image_filename = "images/drosophila_suzukii.png"
  ),
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila melanogaster", "Drosophila simulans", "Drosophila histrio"),
    answer = "Drosophila simulans",
    image_filename = "images/drosophila_simulans.png"
  ),
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila suzukii", "Drosophila subobscura", "Drosophila obscura"),
    answer = "Drosophila obscura",
    image_filename = "images/drosophila_obscura.png"
  ),
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila funebris", "Drosophila obscura", "Drosophila melanogaster"),
    answer = "Drosophila funebris",
    image_filename = "images/drosophila_funebris.png"
  ),
  list(
    question = "What is the species of the fly in the picture?",
    choices = c("Drosophila melanogaster", "Drosophila kuntzei", "Drosophila testacea"),
    answer = "Drosophila testacea",
    image_filename = "images/drosophila_testacea.png"
  ))

# creating a ui
ui <- fluidPage(
  titlePanel("Guess the drosophila!"),
  fluidRow(
    column(6, align = "center", 
           imageOutput("quiz_image") # will call it quiz image for now for reference 
    ),
    column(6,
           h3(textOutput("question")),
           radioButtons("choices", "Select your answer:", choices = ""), # wull give multiple choice options
           actionButton("submit", "Submit"),
           verbatimTextOutput("result")
    )
  )
)

# server
server <- function(input, output, session) {
  current_question <- reactiveVal(1)
  
  observe({
    question_data <- quiz_data[[current_question()]]
    updateRadioButtons(
      session, "choices",
      label = "Select the fly:",
      choices = question_data$choices
    )
    
    output$quiz_image <- renderImage({
      list(src = question_data$image_filename, alt = "", width = "100%")
    }, deleteFile = FALSE)
    output$question <- renderText(question_data$question)
  })
  
  output$result <- renderText({
    question_data <- quiz_data[[current_question()]]
    selected_choice <- input$choices
    if (!is.null(selected_choice)) {
      if (selected_choice == question_data$answer) {
        "CORRECT!! you are a drosophila expert!"
      } else {
        "Oopsie that isn't correct. Fly again!"
      }
    }
  })
  
  observeEvent(input$submit, {
    current_question(current_question() + 1)
    if (current_question() > length(quiz_data)) {
      showModal(modalDialog(
        title = "Drosophilic times, you finished!!",
        "Woah well done! A vial of white eyes mutants may be coming your way!!",
        easyClose = TRUE
      ))
      current_question(1)
    }
  })
}

# run application
shinyApp(ui, server)
