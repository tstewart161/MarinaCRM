
library(shiny)
library(rmarkdown)

# the purpose of this app is to create a the skeleton of the downloadable 
# report that will eventually go into the app, and after a renter is inputted,
# this file will be saved, and then hopefully printed


ui <- fluidPage(
  title = 'Download a PDF report',
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Build a regression model of mpg against:',
                  choices = names(mtcars)[-1]),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport')
    ),
    mainPanel(
      plotOutput('regPlot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  regFormula <- reactive({
    as.formula(paste('mpg ~', input$x))
  })
  
  output$regPlot <- renderPlot({
    par(mar = c(4, 4, .1, .1))
    plot(regFormula(), data = mtcars, pch = 19)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('my-example-report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
