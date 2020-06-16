#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import shinydashboard



AppUI <-shinydashboard::dashboardPage(
  #skin = 'red',

  shinydashboard::dashboardHeader(title = "Debarcoderapp"),
  shinydashboard::dashboardSidebar(

    shiny::fileInput('flowfile','Choose .fcs file', accept = '.fcs'),
    shiny::fileInput('key','Choose .txt file', accept = '.txt'),
    #  selectizeInput('state', label = NULL, choices = NULL, options = list( #klasse_name
    #   placeholder = 'Type a category name', maxOptions = length(kbnew$Klasse) )
    # ),
    shiny::textInput("SD","Please enter a SD",value = 1.5),

    shiny::textInput("PBdil","Please enter the number of\n PB dilutions used",value = 3),
    shiny::textInput("POdil","Please enter the number of\n PO dilutions used",value = 3),

    shiny::textInput("bw","Please enter a bw",value = 0.9),  #1.5
    shiny::textInput("threshold","Please enter a threshold\n to determine the ON-fraction",value = 1000),



    shiny::actionButton("save","Add"),

    shiny::selectizeInput('state', label = NULL, choices = NULL, options = list( #klasse_name
      placeholder = 'Type a channel name'
      # , maxOptions = total_channels
    )
    ),

    shiny::downloadButton("distribution", "Download debarcoded data")


  ),
  shinydashboard::dashboardBody(

    # girafeOutput('parcoord', width = "2%", height = "800px"
    #              #, click = "plot_click", dblclick = "plot_dblclick",
    #              # hover = "plot_hover",
    #              # brush = "plot_brush"
    # ),
    # verbatimTextOutput("info"),
    shiny::plotOutput('parcoord1'),
    shiny::plotOutput('parcoord'),
    shiny::plotOutput('parcoord2'),
    shiny::plotOutput('parcoord3'),
    shiny::plotOutput('parcoord4')

  )
)



options(shiny.maxRequestSize=30*1024^2)
