#' @import shiny
#' @import miniUI
#' @import rstudioapi
#' @export
renviron_addin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "Gerenciador do .Renviron"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        title = "Gerenciar Renviron",
        icon = shiny::icon("sliders"),
        miniUI::miniContentPanel(
          shiny::radioButtons(
            inputId = "env_target",
            label = "Salvar em:",
            choices = c(
              "Projeto" = "project",
              "Usuário" = "user",
              "Outro" = "custom"
            ),
            inline = TRUE
          ),
          shiny::uiOutput("custom_path_ui"),
          shiny::hr(),
          shiny::uiOutput("select_item_ui"),
          shiny::textInput("key_input", "Chave:", value = ""),
          shiny::textInput("val_input", "Valor:", value = "")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    output$custom_path_ui <- shiny::renderUI({
      if (input$env_target == "custom")
        shiny::textInput("custom_path", "Caminho completo:", value = "")
    })
    get_file_path <- shiny::reactive({
      switch(
        input$env_target,
        "project" = file.path(
          rstudioapi::getActiveProject() %||% getwd(),
          ".Renviron"
        ),
        "user"    = file.path(Sys.getenv("HOME"), ".Renviron"),
        "custom"  = input$custom_path
      )
    })
    
    output$select_item_ui <- shiny::renderUI({
      path <- get_file_path()
      if (is.null(path) || path == "")
        return(NULL)
      if (!file.exists(path)) {
        return(shiny::selectInput(
          "existing_var",
          "Editar existente:",
          choices = c("Nova Variável" = "NEW")
        ))
      }
      lines <- tryCatch(
        readLines(path, warn = FALSE, encoding = "UTF-8"),
        error = function(e)
          character(0)
      )
      vars <- grep("^[^#].*=", lines, value = TRUE)
      choices <- if (length(vars) > 0) {
        c("Nova Variável" = "NEW", vars)
      } else {
        c("Nova Variável" = "NEW")
      }
      
      shiny::selectInput("existing_var", "Editar existente:", choices = choices)
    })
    
    shiny::observeEvent(input$existing_var, {
      if (input$existing_var != "NEW") {
        parts <- strsplit(input$existing_var, "=")[[1]]
        shiny::updateTextInput(session, "key_input", value = parts[1])
        shiny::updateTextInput(session, "val_input", value = paste(parts[-1], collapse = "="))
      } else {
        shiny::updateTextInput(session, "key_input", value = "")
        shiny::updateTextInput(session, "val_input", value = "")
      }
    })
    
    shiny::observeEvent(input$done, {
      path <- get_file_path()
      if (is.null(path) || path == "")
        return()
      
      key <- trimws(input$key_input)
      val <- trimws(input$val_input)
      
      lines <- if (file.exists(path))
        readLines(path, warn = FALSE)
      else
        character(0)
      
      new_entry <- paste0(key, "=", val)
      
      if (input$existing_var != "NEW") {
        lines[lines == input$existing_var] <- new_entry
      } else {
        lines <- c(lines, new_entry)
      }
      
      writeLines(lines, path)
      shiny::showNotification("Arquivo .Renviron atualizado!")
      
      invisible(shiny::stopApp())
      if (rstudioapi::hasFun("restartSession"))
        rstudioapi::restartSession()
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(returnValue = NULL)
    })
  }
  
  result <- tryCatch(
    shiny::runGadget(
      ui,
      server,
      viewer = shiny::dialogViewer("Gerenciar .Renviron", width = 500, height = 500)
    ),
    error = function(e) {
      if (grepl("User cancel", e$message))
        return(NULL)
      stop(e)
    }
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
