#' new application
#'
#' An application to perform hydrological modelling of tracers.
#'
#' @param ... No argument are needed to launch the application.
#'
#' @export
#'
#' @import shiny
#' @import waiter
#' @import ggplot2

new <- function(...) {

  # Increase the upload limit file size to 100
  options(shiny.maxRequestSize = 100*1024^2)

  # Define variables
  file_format <- c("text/csv","text/comma-separated-values, text/plain", ".csv")
  MC_type <- c("EMM", "EPM", "PEM", "DM")
  WOBJ <- c("KGE", "NSE", "KGENP", "KGEABS", "RMSE")
  warmup_method <- c("u_Cin", "u_Cobs", "1_Cin", "1_Cobs")

  #------------------ UI ------------------#

  ui <- fluidPage(

    shinyWidgets::chooseSliderSkin("Flat", "#202020"),

    # CSS
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      #eval_range-label {
        color: #E69F00;
      }

      #msg_obj {
        overflow-y:scroll;
        max-height: 200px;
        height: 200px;
      }"))
    ),

    # Specify the use of Waiter and Shinyjs
    waiter::useWaiter(),
    shinyjs::useShinyjs(),

    # UI elements
    sidebarPanel(

      fluidRow(
        column(4, fileInput("import", "Import dataset", accept = file_format)),
        column(4, shinyWidgets::pickerInput("type", "Type of MC", choices = MC_type,selected = MC_type[1],
                                            options = shinyWidgets::pickerOptions(
                                              actionsBox = TRUE,
                                              size = 10,
                                              selectedTextFormat = "count > 3"
                                            ), multiple = TRUE)),
        column(4, selectInput("warmup_m", "Warmup method",
                              choices = warmup_method, selected = warmup_method[1]))),
      fluidRow(
        column(4, fileInput("import_mc", "Import MC", accept = file_format)),
        column(4, numericInput("max_t", "Length of MC", value = 1000, min = 1, max = 10000, step = 1)),
        column(4, numericInput("warmup_t", "Warmup time", value = 1000, min = 1, max = 10000, step = 1))),
      fluidRow(
        column(4, uiOutput("ui_p1")),
        column(4, uiOutput("ui_p2")),
        column(4, uiOutput("ui_ratio"))
      ),
      uiOutput("ui_eval_range"),

      h3("Parameter optimisation"),
      fluidRow(
        column(3, numericInput("n_run", "Iterations", value = 1000, min = 1, max = 100000, step = 100)),
        column(3, selectInput("obj", "Obj function", choices = WOBJ, selected = "KGE")),
        column(3, br(), checkboxInput("allow_NA", "Allow NA")),
        column(3, br(), checkboxInput("ci_mode", "CI mode")),
        column(3,
               uiOutput("ui_percentile_min"),
               uiOutput("ui_percentile_max"))),
      fluidRow(
        column(5, uiOutput("ui_p1_range")),
        column(5, uiOutput("ui_p2_range")),
        column(2, uiOutput("ui_ratio_range"))
      ),
      actionButton("run", "Run")

    ),

    mainPanel(

      # tags$img(src = "logo_hse.png", height = "5%", width = "5%", align = "right",
      #          style = "margin-top:10px;"),

      br(), br(),
      fluidRow(column(4,
                      tableOutput("perf"),
                      shinyjs::hidden(downloadButton("download_results", "Download results"))),
               column(6, shinyjs::hidden(verbatimTextOutput("msg_obj", placeholder = TRUE)), offset = 2)
      ),
      br(), br(),
      fluidRow(column(12, uiOutput("ui_plot_slider"))),
      fluidRow(column(12, plotOutput("plot"))),
      br(),
      fluidRow(
        column(2,
               fluidRow(shinyjs::hidden(numericInput("min_obj", "Min obj.", value = 0, step = 0.1))),
               fluidRow(shinyjs::hidden(textOutput("n_sim")))),
        column(3, uiOutput("p1_sa"), offset = 1),
        column(3, uiOutput("p2_sa")),
        column(3, uiOutput("ratio_sa")))

    )

  )

  #------------------ Server ------------------#

  server <- function(input, output, session) {

    # Hide some UI elements if no dataset
    observe({
      if (!is.na(res$best[1])) {
        shinyjs::show("unit_title")
        shinyjs::show("msg_obj")
        shinyjs::show("min_obj")
        shinyjs::show("n_sim")
        shinyjs::show("download_results")
      } else {
        shinyjs::hide("unit_title")
        shinyjs::hide("msg_obj")
        shinyjs::hide("min_obj")
        shinyjs::hide("n_sim")
        shinyjs::hide("download_results")
      }
    })

    # Reactive variables ------------------------------------------------------

    df <- reactive({
      req(input$import)
      utils::read.delim(input$import$datapath, sep = "\t")
    })

    custom_MC <- reactive({
      if (!("custom" %in% type())) {NULL} else {
        req(input$import_mc)
        scan(input$import_mc$datapath)
      }
    })

    eval_range <- reactive({
      c(input$eval_range[1], input$eval_range[2])
    })

    res <- reactiveValues(best = NA,
                          all = NA)

    p1_name <- reactive({
      dplyr::case_when(type() == "EMM" ~ list(c("T [T]", "T")),
                       type() == "EPM" ~ list(c("T [T]", "T")),
                       type() == "PEM" ~ list(c("T [T]", "T")),
                       type() == "DM" ~ list(c("T [T]", "T")),
                       type() == "custom" ~ list(c("NOT USED", "NOT USED")))
    })

    p2_name <- reactive({
      dplyr::case_when(type() == "EMM" ~ list(c("NOT USED", "NOT USED")),
                       type() == "EPM" ~ list(c("n [-]", "n")),
                       type() == "PEM" ~ list(c("n [-]", "n")),
                       type() == "DM" ~ list(c("DP [-]", "DP")),
                       type() == "custom" ~ list(c("NOT USED", "NOT USED")))
    })

    n_component <- reactive(length(type()))

    type <- reactive({
      x <- sub("_.*", "", input$type)
      names(x) <- add_unique_suffix(x, 0)
      return(x)
    })

    type_name <- reactive(names(type()))

    # Reactive functions ------------------------------------------------------

    # Update available type depending on the current selection
    observeEvent(input$type, {
      cur_type <- isolate(input$type)
      new_suffix <- add_unique_suffix(sub("_.*", "", cur_type))
      all_choices <- c(MC_type, new_suffix)

      shinyWidgets::updatePickerInput(session, inputId = "type",
                                      choices = all_choices,
                                      selected = cur_type)
    })

    # Run model with default n_run=1 when modifying UI elements
    observeEvent(c(lapply(names(input)[grep("p1_|p2_|ratio_", names(input))], function(name) input[[name]]),
                   input$max_t,
                   input$import, input$import_mc, input$eval_range, input$allow_NA,
                   input$warmup_m, input$warmup_t), {
                     req(eval_range(), type())

                     # Get input according to number of component
                     p1_val <- NULL
                     p2_val <- NULL
                     ratio <- NULL
                     for (n in seq(n_component())) {
                       p1_val <- c(p1_val, input[[paste0("p1_", type_name()[n])]])
                       p2_val <- c(p2_val, input[[paste0("p2_", type_name()[n])]])
                       ratio <- c(ratio, input[[paste0("ratio_", type_name()[n])]])
                     }

                     # Run model function
                     x <- model_tracer(df()$obs, df()$Cin, crit = input$obj,
                                       type = isolate(type()), MC = custom_MC(), max_t = input$max_t,
                                       eval = c(eval_range()[1], eval_range()[2]), allow_NA = input$allow_NA,
                                       warmup_method = input$warmup_m, warmup_time = input$warmup_t,
                                       p1_min = p1_val, p1_max = p1_val,
                                       p2_min = p2_val, p2_max = p2_val,
                                       ratio_min = ratio, ratio_max = ratio)

                     res$best <- x$best
                   })

    # Run model with iterations when pressing "Run" button
    observeEvent(input$run, {
      req(type())
      waiter$show()
      freezeReactiveValue(input, "plot_dotty")
      withCallingHandlers({ # this update msg_obj with message() from the model function
        shinyjs::html("msg_obj", "")

        # Get input according to number of component
        p1_min_val <- NULL
        p1_max_val <- NULL
        p2_min_val <- NULL
        p2_max_val <- NULL
        ratio_min <- NULL
        ratio_max <- NULL
        for (n in seq(n_component())) {
          p1_min_val <- c(p1_min_val, input[[paste0("p1_range_", type_name()[n])]][1])
          p1_max_val <- c(p1_max_val, input[[paste0("p1_range_", type_name()[n])]][2])
          p2_min_val <- c(p2_min_val, input[[paste0("p2_range_", type_name()[n])]][1])
          p2_max_val <- c(p2_max_val, input[[paste0("p2_range_", type_name()[n])]][2])
          ratio_min <- c(ratio_min, input[[paste0("ratio_range_", type_name()[n])]][1])
          ratio_max <- c(ratio_max, input[[paste0("ratio_range_", type_name()[n])]][2])
        }

        # Run model function
        x <- model_tracer(df()$obs, df()$Cin, n_run = input$n_run, crit = input$obj,
                          type = type(), MC = custom_MC(), max_t = input$max_t,
                          eval = c(eval_range()[1], eval_range()[2]), allow_NA = input$allow_NA,
                          warmup_method = input$warmup_m, warmup_time = input$warmup_t,
                          p1_min = p1_min_val, p1_max = p1_max_val,
                          p2_min = p2_min_val, p2_max = p2_max_val,
                          ratio_min = ratio_min, ratio_max = ratio_max)
        res$best <- x$best
        res$all <- x$all
      },
      message = function(m) {
        shinyjs::html(id = "msg_obj", html = m$message, add = TRUE)
      })

      for (n in seq(n_component())) {
        MC <- type_name()[n]
        updateSliderInput(session, paste0("p1_", MC), value = res$best$p1[[MC]])
        updateSliderInput(session, paste0("p2_", MC), value = res$best$p2[[MC]])
        updateSliderInput(session, paste0("ratio_", MC), value = res$best$ratio[[MC]])
      }
    })

    # Outputs -----------------------------------------------------------------

    # Main plot
    output$plot <- renderPlot({
      req(res$best)
      req(eval_range())

      # Get "sim" time series depending on input$ci_mode
      # If CI mode is activated, "sim" corresponds to the mean of the distribution at each time step
      # If CI mode is not activated, "sim" corresponds to the best obj
      if (input$ci_mode) {
        req(input$percentile_min)
        req(input$percentile_max)
        ble1 <- res$all$sim[which(res$all$obj > input$min_obj)]
        ble <- lapply(seq(1, length(res$best$sim)), function(i) unname(sapply(ble1, "[", i)))
        CI_min <- unname(sapply(ble, function(x) stats::quantile(x, input$percentile_min)))
        CI_max <- unname(sapply(ble, function(x) stats::quantile(x, input$percentile_max)))
        mean_distrib <- sapply(ble, function(x) mean(x))
      } else {
        mean_distrib <- res$best$sim
      }

      x <- tidyr::tibble(t = 1:length(res$best$sim),
                         obs = res$best$obs,
                         sim = mean_distrib) |>
        dplyr::mutate(eval = ifelse(dplyr::row_number() %in% eval_range()[1]:eval_range()[2], obs, NA)) |>
        tidyr::pivot_longer(c(obs, sim, eval)) |>
        dplyr::mutate(name = factor(name, c("eval", "obs", "sim"))) |> # same order as scale_manual below
        ggplot() +
        # add ribbon if ci_mode only
        {if (input$ci_mode) geom_ribbon(data = tidyr::tibble(t = 1:length(res$best$sim), CI_min, CI_max),
                                        aes(x = t, ymin = CI_min, ymax = CI_max),
                                        alpha = 0.2, fill = unname(grDevices::palette.colors()[3]))} +
        annotate("segment",
                 x = eval_range()[1], xend = eval_range()[1],
                 y = -50, yend = res$best$obs[eval_range()[1]],
                 color = unname(grDevices::palette.colors()[2]), alpha = 0.5, linetype = "dashed") +
        annotate("segment",
                 x = eval_range()[2], xend = eval_range()[2],
                 y = -50, yend = res$best$obs[eval_range()[2]],
                 color = unname(grDevices::palette.colors()[2]), alpha = 0.5, linetype = "dashed") +
        geom_line(aes(t, value, color = name, linetype = name, linewidth = name, alpha = name)) +
        geom_point(aes(t, value, color = name, size = name, alpha = name), shape = 15) +
        scale_color_manual(name = "", values = unname(grDevices::palette.colors()[c(2, 1, 3)])) +
        scale_linetype_manual(name = "", values = c("solid", "solid", "solid")) +
        scale_linewidth_manual(name = "", values = c(2.5, 1, 1)) +
        scale_alpha_manual(name = "", values = c(0.75, 1, 1)) +
        scale_size_manual(name = "", values = c(0, 2, 0)) +
        coord_cartesian(xlim = c(input$plot_slider[1], input$plot_slider[2]),
                        ylim = c(min(c(res$best$obs, mean_distrib), na.rm = TRUE),
                                 max(c(res$best$obs, mean_distrib), na.rm = TRUE))) +
        scale_y_continuous(expand = expansion(mult = c(0.13, 0.08))) +
        xlab("t [T]") +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom")

      x

    })

    # SA plots

    output$p1_sa <- renderUI({
      plot_output_list <- lapply(seq(n_component()), function(n) {
        plotOutput(paste0("p1_sa_", type_name()[n]), height = "300px")
      })

      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })

    output$p2_sa <- renderUI({
      plot_output_list <- lapply(seq(n_component()), function(n) {
        plotOutput(paste0("p2_sa_", type_name()[n]), height = "300px")
      })

      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })

    output$ratio_sa <- renderUI({
      plot_output_list <- lapply(seq(n_component()), function(n) {
        plotOutput(paste0("ratio_sa_", type_name()[n]), height = "300px")
      })

      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })

    observe({
      req(res$all)
      req(all(names(res$all$ratio) == type_name()))

      # p1_sa
      lapply(seq(n_component()), function(n) {
        p <- tidyr::tibble(p1 = res$all$p1[[type_name()[n]]], obj = res$all$obj) |>
          dplyr::filter(obj >= input$min_obj) |>
          ggplot(aes(p1, obj)) +
          geom_point() +
          xlab(p1_name()[[n]][2]) +
          ylab(isolate(res$best$obj_name)) +
          ggtitle(type_name()[n]) +
          theme_bw(base_size = 16) +
          theme(axis.title.y = element_blank())

        output[[paste0("p1_sa_", type_name()[n])]] <- renderPlot(p)
      })

      # p2_sa
      lapply(seq(n_component()), function(n) {
        p <- tidyr::tibble(p2 = res$all$p2[[type_name()[n]]], obj = res$all$obj) |>
          dplyr::filter(obj >= input$min_obj) |>
          ggplot(aes(p2, obj)) +
          geom_point() +
          xlab(p2_name()[[n]][2]) +
          ylab(isolate(res$best$obj_name)) +
          ggtitle(type_name()[n]) +
          theme_bw(base_size = 16) +
          theme(axis.title.y = element_blank())

        output[[paste0("p2_sa_", type_name()[n])]] <- renderPlot(p)
      })

      # ratio_sa
      lapply(seq(n_component()), function(n) {
        p <- tidyr::tibble(ratio = res$all$ratio[[type_name()[n]]], obj = res$all$obj) |>
          dplyr::filter(obj >= input$min_obj) |>
          ggplot(aes(ratio, obj)) +
          geom_point() +
          xlab("ratio") +
          ylab(isolate(res$best$obj_name)) +
          ggtitle(type_name()[n]) +
          theme_bw(base_size = 16) +
          theme(axis.title.y = element_blank())

        output[[paste0("ratio_sa_", type_name()[n])]] <- renderPlot(p)
      })
    })

    # Table
    output$perf <- renderTable({
      req(res$best)
      req(eval_range())
      x <- score(res$best$sim[eval_range()[1]:eval_range()[2]],
                 res$best$obs[eval_range()[1]:eval_range()[2]],
                 crit = c("NSE", "KGE", "KGENP", "KGE_abs", "alpha", "beta", "rpearson"),
                 allow_NA = input$allow_NA)

      data.frame(as.list(x))
    })

    # Text
    output$n_sim <- renderText({
      req(res$all)

      filter <- res$all$obj[res$all$obj > input$min_obj]
      filter <- filter[!is.na(filter)]
      paste("Nb of sim: ", length(filter))
    })

    output$ui_eval_range <- renderUI({
      sliderInput("eval_range",
                  "Evaluation range",
                  value = c(max(1, min(which(!is.na(df()$obs)))),
                            min(length(df()$obs), max(which(!is.na(df()$obs))))),
                  min = max(1, min(which(!is.na(df()$obs)))),
                  max = min(length(df()$obs), max(which(!is.na(df()$obs)))),
                  step = 1)
    })

    # UI output ---------------------------------------------------------------

    output$ui_plot_slider <- renderUI({
      req(df())
      min <- 1
      max <- nrow(df())
      sliderInput("plot_slider", NULL, value = c(min, max), min = min, max = max, step = 1, width = "100%")
    })

    output$ui_p1 <- renderUI({
      req(type())
      gnr <- function(n_component, type) {
        sliderInput(paste0("p1_", type[n_component]),
                    HTML(p1_name()[[n_component]][1]), value = 6, min = 1, max = 1000, step = 0.1)
      }
      x <- lapply(seq(n_component()), function(n) gnr(n, type_name()))
      return(x)
    })

    output$ui_p2 <- renderUI({
      req(type())
      gnr <- function(n_component, type) {
        sliderInput(paste0("p2_", type[n_component]),
                    HTML(p2_name()[[n_component]][1]), value = 3, min = 0.001, max = 20, step = 0.001)
      }
      x <- lapply(seq(n_component()), function(n) gnr(n, type_name()))
      return(x)
    })

    output$ui_ratio <- renderUI({
      req(type())
      gnr <- function(n_component, type) {
        numericInput(paste0("ratio_", type[n_component]),
                     paste0("ratio_", type[n_component]), value = 1, min = 0, step = 0.01)
      }
      x <- lapply(seq(n_component()), function(n) gnr(n, type_name()))
      return(x)
    })

    output$ui_p1_range <- renderUI({
      req(type())
      gnr <- function(n_component, type) {
        sliderInput(paste0("p1_range_", type[n_component]),
                    HTML(p1_name()[[n_component]][1]), value = c(1, 1000), min = 1, max = 1000, step = 0.1)
      }
      x <- lapply(seq(n_component()), function(n) gnr(n, type_name()))
      return(x)
    })

    output$ui_p2_range <- renderUI({
      req(type())
      gnr <- function(n_component, type) {
        sliderInput(paste0("p2_range_", type[n_component]),
                    HTML(p2_name()[[n_component]][1]), value = c(0.001, 20), min = 0.001, max = 20, step = 0.001)
      }
      x <- lapply(seq(n_component()), function(n) gnr(n, type_name()))
      return(x)
    })

    output$ui_ratio_range <- renderUI({
      req(type())
      gnr <- function(n_component, type) {
        sliderInput(paste0("ratio_range_", type[n_component]),
                    paste0("ratio_", type[n_component]), value = c(0, 1), min = 0, max = 1, step = 0.1)
      }
      x <- lapply(seq(n_component()), function(n) gnr(n, type_name()))
      return(x)
    })

    # CI mode
    output$ui_percentile_min <- renderUI({
      req(input$ci_mode)
      numericInput("percentile_min", NULL, value = 0.05, min = 0, max = 1, step = 0.05)
    })

    output$ui_percentile_max <- renderUI({
      req(input$ci_mode)
      numericInput("percentile_max", NULL, value = 0.95, min = 0, max = 1, step = 0.05)
    })

    # Download
    output$download_results <- downloadHandler(
      filename = paste0("results.txt"),
      content = function(filename) {
        utils::write.table(tidyr::tibble(t = 1:length(res$best$obs),
                                         Cin = res$best$Cin,
                                         obs = res$best$obs,
                                         sim = res$best$sim),
                           filename, sep = ";", row.names = FALSE)
      },
    )

    # Waiter
    waiter <- Waiter$new(id = c("plot"),
                         html = spin_3(),
                         color = transparent(.7))

  }

  shinyApp(ui, server, ...)

}
