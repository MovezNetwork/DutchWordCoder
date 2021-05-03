#' Run the coding app
#'
#' @param use_test_data (FOR DEVELOPERS ONLY) Do you want to use test data for this app?
#'
#' @export
runApp <- function(use_test_data = FALSE){
  data_core <- NULL
  data_core_new <- NULL

  if(use_test_data){
    # Prepare the test data set
    data_core <- data.frame(
        word = c("a", "b", "c")
    )
    data_core$cate <- list(list())
    data_core$rate <- NA_character_
    data_core$media <- list(list())
    data_core$note <- NA_character_
    data_core$saved <- FALSE
  }
  word_index <- 1L
  initialized <- FALSE

  ui <- shiny::fixedPage(
    theme = shinythemes::shinytheme("cerulean"),
    shiny::navbarPage(
      "Health-related Dutch Words",
      shiny::tabPanel(
        "Code",
        # first row
        shiny::fixedRow(
          # left column
          shiny::column(
            6,
            shiny::div(shiny::textOutput("counter")),
            shiny::br(),
            shiny::h1(shiny::textOutput("word"), align = "center"),
            shiny::br(),
            shiny::br()
          ),
          # right column
          shiny::column(
            6,
            shiny::uiOutput("cate_check"),
            shiny::uiOutput("spec_cate_check"),
            shiny::uiOutput("rate_slider"),
            shiny::uiOutput("media_check")
          )
        ),
        # second row
        shiny::fixedRow(
          #left column
          shiny::column(6,
                 align = "center",
                 shiny::actionButton("previous", "Previous", icon = shiny::icon("angle-left"), width = "90px"),
                 shiny::actionButton("nextone", "Next", icon = shiny::icon("angle-right"), width = "90px"),
                 shiny::actionButton("save", "Save", icon = shiny::icon("save"), width = "90px"),
                 shinyWidgets::searchInput("goto", "Go to word #", placeholder = "Press Enter to go")
          ),
          # right column
          shiny::column(
            6,
            shiny::actionButton("save_next", shiny::div(shiny::icon("save"), shiny::icon("angle-right"), "Save & Next"), width = "300px",  class = "btn-primary"),
            shiny::downloadButton("download_data", "Download data"),
            shiny::fileInput("archive",
                      label = NULL, buttonLabel = "Choose archive (.rds) file",
                      multiple = FALSE,
                      accept = ".rds"
            )
          )
        ),
        # third row (notes)
        shiny::uiOutput("note_area")
      ),
      shiny::tabPanel(
        "About",
        shiny::p(
          "This ShinyApp is developed by",
          shiny::a("Jingmeng Cui", href = "https://github.com/Sciurus365/"),
          "for the research at",
          shiny::a("Movez Lab", href = "https://www.eur.nl/en/essb/research/movez-lab"),
          ",",
          shiny::a("Erasmus University", href = "https://www.eur.nl/"), "."
        ),
        shiny::em("Other information/instruction")
      )
    )
  )

  server <- function(input, output) {

    # define several functions

    save_current <- function() {
      # --- save the current one
      data_core[word_index, "cate"] <<- list(list(input$cate))
      data_core[word_index, "spec_cate"] <<- list(list(input$spec_cate))
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

      output$download_data <- shiny::downloadHandler(
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

    render_spec_cate_check <- function(x){
      if("Food" %in% x){
        output$spec_cate_check <- shiny::renderUI({
          shinyWidgets::checkboxGroupButtons(
            inputId = "spec_cate",
            label = "Specific category",
            choices = c("Fruit", "Vegetable", "Fast food"),
            individual = FALSE,
            selected = (if (length(data_core[word_index, "spec_cate"][[1]]) == 0) {
              NULL
            } else {
              data_core[word_index, "spec_cate"][[1]]
            })
          )
        })
      }
      else{
        output$spec_cate_check <- shiny::renderUI({NULL})
      }
    }

    render_all <- function() {
      # --- render a new frame
      output$word <- shiny::renderText(data_core[word_index, "word"])

      output$rate_slider <- shiny::renderUI({
        shinyWidgets::sliderTextInput("rate",
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

      output$cate_check <- shiny::renderUI({
        shinyWidgets::checkboxGroupButtons(
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

      output$media_check <- shiny::renderUI({
        shinyWidgets::checkboxGroupButtons(
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

      output$note_area <- shiny::renderUI({
        shiny::textAreaInput("note",
                      "If you are uncertain about a word or want to note down something, please write it here.",
                      value = (if (is.null(data_core[word_index, "note"]) | is.na(data_core[word_index, "note"])) {
                        NULL
                      } else {
                        data_core[word_index, "note"]
                      }),
                      width = "700px"
        )
      })

      output$counter <- shiny::renderText({
        counter_num <- sprintf("%d/%d", word_index, nrow(data_core))
        counter_saved <- ifelse(data_core[word_index, "saved"], "saved", "not saved")
        return(paste(counter_num, counter_saved))
      })

      render_spec_cate_check(data_core[word_index, "cate"])
    }

    # The actions and their corresponding functions

    shiny::observeEvent(input$previous,
                 {
                   attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; previous; ", Sys.time()))
                   index_minus()
                   render_all()
                 },
                 ignoreInit = TRUE
    )
    shiny::observeEvent(input$nextone,
                 {
                   attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; nextone; ", Sys.time()))
                   index_plus()
                   render_all()
                 },
                 ignoreInit = TRUE
    )
    shiny::observeEvent(input$save,
                 {
                   attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; save; ", Sys.time()))
                   save_current()
                 },
                 ignoreInit = TRUE
    )
    shiny::observeEvent(input$save_next,
                 {
                   attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; save_next; ", Sys.time()))
                   save_current()
                   index_plus()
                   render_all()
                 },
                 ignoreInit = TRUE
    )
    shiny::observeEvent(input$goto,
                 {
                   attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; goto; ", Sys.time()))
                   index_goto(input$goto)
                   render_all()
                 },
                 ignoreInit = TRUE
    )

    shiny::observeEvent(input$cate,
                 {
                   attr(data_core, "log") <<- append(attr(data_core, "log"), paste0(word_index, "; cate; ", Sys.time()))
                   render_spec_cate_check(input$cate)
                 }
    )

    shiny::observeEvent(input$archive, {
      data_core_new <<- readRDS(input$archive$datapath)
      if (!initialized) {
        data_core <<- data_core_new
        initialized <<- TRUE
      }
      else if (ncol(data_core_new) != ncol(data_core) | all(colnames(data_core_new) != colnames(data_core))) {
        shiny::showNotification(paste("Wrong input format. The column names of your file do not match with this session. Please check if you uploaded a correct file."),
                         type = "error", duration = NULL
        )
      }
      else if (nrow(data_core_new) != nrow(data_core) | all(data_core_new$word != data_core$word)) {
        shiny::showNotification(paste("Wrong input. The words of your file do not match with this session. Please check if you uploaded a correct file."),
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
  shiny::shinyApp(ui = ui, server = server)

}
