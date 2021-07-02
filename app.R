library(shiny)
library(shinythemes)
library(shinyWidgets)

# Prepare the test data set
# data_core <- data.frame(
#     word = c("a", "b", "c")
# )
# data_core$cate <- list(list())
# data_core$rate <- NA_character_
# data_core$media <- list(list())
# data_core$note <- NA_character_
# data_core$saved <- FALSE

word_index <- 1L
initialized <- FALSE

ui <- fixedPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "Health-related Dutch Words",
    tabPanel(
      "Code",
      fixedRow(
        column(
          6,
          div(textOutput("counter")),
          br(),
          h1(textOutput("word"), align = "center"),
          br(),
          br()
        ),
        column(
          6,
          uiOutput("cate_check"),
          uiOutput("rate_slider"),
          uiOutput("media_check")
        )
      ),
      fixedRow(
        column(6,
               uiOutput("note_area")
        ),
        column(
          6,
          actionButton("save_next", div(icon("save"), icon("angle-right"), "Save & Next"), width = "300px",  class = "btn-primary"),
          downloadButton("download_data", "Download data"),
          fileInput("archive",
            label = NULL, buttonLabel = "Choose archive (.rds) file",
            multiple = FALSE,
            accept = ".rds"
          )
        )
      ),
      fixedRow(
        align = "left",
        p(strong("Navigation Panel")),
        p("Only for checking and directing purposes; you won't need these functions in normal coding processes."),
        actionButton("previous", "Previous", icon = icon("angle-left"), width = "90px"),
        actionButton("nextone", "Next", icon = icon("angle-right"), width = "90px"),
        actionButton("save", "Save", icon = icon("save"), width = "90px"),
        searchInput("goto", "Go to word #", placeholder = "Press Enter to go")
      )
    ),
    tabPanel(
      "About",
      p(
        "This ShinyApp is developed by",
        a("Jingmeng Cui", href = "https://github.com/Sciurus365/"),
        "for the research at",
        a("Movez Lab", href = "https://www.eur.nl/en/essb/research/movez-lab"),
        ",",
        a("Erasmus University", href = "https://www.eur.nl/"), "."
      ),
      em("Other information/instruction")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # define several actions

  save_current <- function() {
    # --- save the current one
    data_core[word_index, "cate"] <<- list(list(input$cate))
    data_core[word_index, "media"] <<- list(list(input$media))
    data_core[word_index, "rate"] <<- if (is.null(input$rate)) {
      NA
    } else {
      input$rate
    }
    data_core[word_index, "note"] <<- if (is.null(input$note)) {
      NA
    } else {
      input$note
    }
    data_core[word_index, "saved"] <<- TRUE

    output$download_data <- downloadHandler(
      filename = paste0("code_data_", format(Sys.time(), "%Y%m%d%H%M"), ".rds"),
      content = function(file) saveRDS(data_core, file = file)
    )
  }

  index_plus <- function() {
    # --- index ++
    if (word_index < nrow(data_core)) word_index <<- word_index + 1
  }

  index_minus <- function() {
    # --- index --
    if (word_index > 1) word_index <<- word_index - 1
  }

  index_goto <- function(x) {
    # -- index goto
    if (1 <= as.numeric(x) & as.numeric(x) <= nrow(data_core)) word_index <<- as.numeric(x)
  }

  render_all <- function() {
    # --- render a new frame
    output$word <- renderText(data_core[word_index, "word"])

    output$rate_slider <- renderUI({
      sliderTextInput("rate",
        label = "How healthy is this word?",
        choices = c("Unhealthy", "Slightly unhealthy", "Neutral", "Slightly healthy", "Healthy"),
        selected = (if (is.na(data_core[word_index, "rate"])) {
          "Neutral"
        } else {
          data_core[word_index, "rate"]
        }),
        grid = TRUE
      )
    })

    output$cate_check <- renderUI({
      checkboxGroupButtons(
        inputId = "cate",
        label = "Category",
        choices = c("Physical activity", "Food", "Drink"),
        individual = FALSE,
        selected = (if (length(data_core[word_index, "cate"][[1]]) == 0) {
          NULL
        } else {
          data_core[word_index, "cate"][[1]]
        })
      )
    })

    output$media_check <- renderUI({
      checkboxGroupButtons(
        inputId = "media",
        label = "Is this word related to ... ?",
        choices = c("Media"),
        individual = TRUE,
        selected = (if (length(data_core[word_index, "media"][[1]]) == 0) {
          NULL
        } else {
          data_core[word_index, "media"][[1]]
        })
      )
    })

    output$note_area <- renderUI({
      textAreaInput("note",
        "If you are uncertain about a word or want to note down something, please write it here.",
        value = (if (is.null(data_core[word_index, "note"]) | is.na(data_core[word_index, "note"])) {
          NULL
        } else {
          data_core[word_index, "note"]
        }),
        width = "700px"
      )
    })

    output$counter <- renderText({
      counter_num <- sprintf("%d/%d", word_index, nrow(data_core))
      counter_saved <- ifelse(data_core[word_index, "saved"], "saved", "not saved")
      return(paste(counter_num, counter_saved))
    })
  }

  observeEvent(input$previous,
    {
      attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; previous; ", Sys.time()))
      index_minus()
      render_all()
    },
    ignoreInit = TRUE
  )
  observeEvent(input$nextone,
    {
      attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; nextone; ", Sys.time()))
      index_plus()
      render_all()
    },
    ignoreInit = TRUE
  )
  observeEvent(input$save,
    {
      attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; save; ", Sys.time()))
      save_current()
    },
    ignoreInit = TRUE
  )
  observeEvent(input$save_next,
    {
      attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; save_next; ", Sys.time()))
      save_current()
      index_plus()
      render_all()
    },
    ignoreInit = TRUE
  )
  observeEvent(input$goto,
    {
      attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; goto; ", Sys.time()))
      index_goto(input$goto)
      render_all()
    },
    ignoreInit = TRUE
  )

  observeEvent(input$archive, {
    data_core_new <<- readRDS(input$archive$datapath)
    if (!initialized) {
      data_core <<- data_core_new
      initialized <<- TRUE
    }
    else if (ncol(data_core_new) != ncol(data_core) | all(colnames(data_core_new) != colnames(data_core))) {
      showNotification(paste("Wrong input format. The column names of your file do not match with this session. Please check if you uploaded a correct file."),
        type = "error", duration = NULL
      )
    }
    else if (nrow(data_core_new) != nrow(data_core) | all(data_core_new$word != data_core$word)) {
      showNotification(paste("Wrong input. The words of your file do not match with this session. Please check if you uploaded a correct file."),
        type = "error", duration = NULL
      )
    }
    else {
      data_core <<- data_core_new
    }

    if (is.null(attr(data_core, "log"))) {
      attr(data_core, "log") <<- list()
    }
    attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; archive; ", Sys.time()))

    render_all()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
