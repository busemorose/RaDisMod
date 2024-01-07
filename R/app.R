#' RaDisMod application
#'
#' An application to perform hydrological modelling of single flood events.
#'
#' @param ... No argument are needed to launch the application.
#'
#' @export
#'
#' @import shiny
#' @import waiter
#' @import ggplot2

RaDisMod <- function(...) {

  # Increase the upload limit file size to 100
  options(shiny.maxRequestSize = 100*1024^2)

  # Define variables
  file_format <- c("text/csv","text/comma-separated-values, text/plain", ".csv")
  HU_type <- c("socose", "gamma", "lnorm", "triangle", "custom")
  WOBJ <- c("KGE", "NSE", "KGENP", "KGEABS", "RMSE")

  #------------------ UI ------------------#

  ui <- fluidPage(

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
        column(4, selectInput("type", "Type of HU", choices = HU_type, selected = HU_type[1])),
        column(4, fileInput("import_hu", "Import HU", accept = file_format))),
      fluidRow(
        column(4, numericInput("max_t", "Length of HU", value = 1000, min = 1, max = 10000), offset = 4)),
      sliderInput("J", "J [L]", value = 250, min = 1, max = 600, step = 1),
      uiOutput("ui_p1"),
      uiOutput("ui_p2"),
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
      sliderInput("J_range", "J [L]", value = c(1, 600), min = 1, max = 600, step = 1),
      uiOutput("ui_p1_range"),
      uiOutput("ui_p2_range"),
      actionButton("run", "Run")

    ),

    mainPanel(

      # tags$img(src = "logo_hse.png", height = "5%", width = "5%", align = "right",
      #          style = "margin-top:10px;"),

      br(), br(), br(), br(), br(),
      fluidRow(column(4, tableOutput("perf"), offset = 1),
               column(1, br(), shinyjs::hidden(downloadButton("download_results", "Download results")), offset = 1),
               column(3, shinyjs::hidden(div(id = "unit_title", h3("Unit hydrograph"))), offset = 2)),
      fluidRow(column(8, plotOutput("plot")),
               column(4, plotOutput("HU"))),
      br(),
      fluidRow(
        column(2,
               fluidRow(shinyjs::hidden(verbatimTextOutput("msg_obj", placeholder = TRUE))),
               fluidRow(shinyjs::hidden(numericInput("min_obj", "Min obj.", value = 0, step = 0.1))),
               fluidRow(shinyjs::hidden(textOutput("n_sim")))),
        column(3, plotOutput("J_sa", height = "300px"), offset = 1),
        column(3, plotOutput("p1_sa", height = "300px")),
        column(3, plotOutput("p2_sa", height = "300px")))

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
      utils::read.delim(input$import$datapath, sep = ";")
    })

    custom_HU <- reactive({
      if (input$type != "custom") {NULL} else {
        req(input$import_hu)
        scan(input$import_hu$datapath)
      }
    })

    eval_range <- reactive({
      c(input$eval_range[1], input$eval_range[2])
    })

    res <- reactiveValues(best = NA,
                          all = NA)

    p1_name <- reactive({
      switch(input$type,
             "socose" = c("Tm [T]", "Tm"),
             "gamma" = c("shape [-]", "shape"),
             "lnorm" = c("mean [L/T]", "mean"),
             "triangle" = c("Tm [T]", "Tm"),
             "custom" = c("NOT USED", "NOT USED"))
    })

    p2_name <- reactive({
      switch(input$type,
             "socose" = c("alpha [-]", "alpha"),
             "gamma" = c("rate [-]", "rate"),
             "lnorm" = c("mean [L/T]", "sd"),
             "triangle" = c("alpha [-]", "alpha"),
             "custom" = c("NOT USED", "NOT USED"))
    })

    # Reactive functions ------------------------------------------------------

    # Run model with default n_run=1 when modifying UI elements
    observeEvent(c(input$J, input$p1, input$p2, input$max_t, input$type,
                   input$import, input$import_hu, input$eval_range, input$allow_NA), {
                     req(eval_range())
                     x <- model(df()$obs, df()$P, crit = input$obj,
                                J_min = input$J, J_max = input$J,
                                type = isolate(input$type), HU = custom_HU(), max_t = input$max_t,
                                eval = c(eval_range()[1], eval_range()[2]), allow_NA = input$allow_NA,
                                p1_min = input$p1, p1_max = input$p1,
                                p2_min = input$p2, p2_max = input$p2)

                     res$best <- x$best
                   })

    # Run model with iterations when pressing "Run" button
    observeEvent(input$run, {
      waiter$show()
      withCallingHandlers({ # this update msg_obj with message() from the model function
        shinyjs::html("msg_obj", "")
        x <- model(df()$obs, df()$P, n_run = input$n_run, crit = input$obj,
                   J_min = input$J_range[1], J_max = input$J_range[2],
                   type = input$type, HU = custom_HU(), max_t = input$max_t,
                   eval = c(eval_range()[1], eval_range()[2]), allow_NA = input$allow_NA,
                   p1_min = input$p1_range[1], p1_max = input$p1_range[2],
                   p2_min = input$p2_range[1], p2_max = input$p2_range[2])
        res$best <- x$best
        res$all <- x$all
      },
      message = function(m) {
        shinyjs::html(id = "msg_obj", html = m$message, add = TRUE)
      })


      updateSliderInput(session, "J", value = res$best$J)
      updateSliderInput(session, "p1", value = res$best$p1)
      updateSliderInput(session, "p2", value = res$best$p2)
    })

    # Update slide depending on HU type
    observeEvent(input$type, {
      updateSliderInput(session, "p1", label = HTML(p1_name()[1]))
      updateSliderInput(session, "p2", label = HTML(p2_name()[1]))
      updateSliderInput(session, "p1_range", label = HTML(p1_name()[1]))
      updateSliderInput(session, "p2_range", label = HTML(p2_name()[1]))
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
        geom_segment(aes(x = eval_range()[1], xend = eval_range()[1],
                         y = -1, yend = res$best$obs[eval_range()[1]]),
                     color = unname(grDevices::palette.colors()[2]), alpha = 0.5, linetype = "dashed") +
        geom_segment(aes(x = eval_range()[2], xend = eval_range()[2],
                         y = -1, yend = res$best$obs[eval_range()[2]]),
                     color = unname(grDevices::palette.colors()[2]), alpha = 0.5, linetype = "dashed") +
        geom_line(aes(t, value, color = name, linetype = name, linewidth = name, alpha = name)) +
        geom_point(aes(t, value, color = name, size = name, alpha = name), shape = 15) +
        scale_color_manual(name = "", values = unname(grDevices::palette.colors()[c(2, 1, 3)])) +
        scale_linetype_manual(name = "", values = c("solid", "solid", "solid")) +
        scale_linewidth_manual(name = "", values = c(2.5, 1, 1)) +
        scale_alpha_manual(name = "", values = c(0.75, 1, 1)) +
        scale_size_manual(name = "", values = c(0, 2, 0)) +
        coord_cartesian(xlim = c(1, length(res$best$sim)), ylim = c(0, max(c(res$best$obs, mean_distrib)))) +
        scale_y_continuous(expand = expansion(mult = c(0.13, 0.08))) +
        xlab("t [T]") +
        ylab(expression(paste("Discharge [L"~T^-1, "]"))) +
        theme_bw(base_size = 16) +
        theme(legend.position = "bottom",
              plot.margin = margin(-3.5, 8, 8, 8, "pt"))

      # Workaround for dynamic P scale
      if (round(max(res$best$PN)) < 5.5) P_scale <- round(max(res$best$PN)) / 2 else P_scale <- 5
      if (P_scale == 0) P_scale <- round(max(res$best$PN), 1) / 2
      if (P_scale == 0) P_scale <- round(max(res$best$PN), 2) / 2

      y <- tidyr::tibble(t = 1:length(res$best$PN),
                         PN = res$best$PN) |>
        ggplot(aes(t, PN)) +
        geom_bar(stat = "identity", color = "black", fill = unname(grDevices::palette.colors()[6])) +
        scale_y_reverse(expand = expansion(mult = c(0.1, 0)),
                        breaks = seq(0, P_scale * 10, min(5, P_scale))) +
        coord_cartesian(xlim = c(1, length(res$best$sim))) +
        ylab("PN [mm]") +
        theme_bw(base_size = 16) +
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.margin = margin(8, 8, -1, 8, "pt"))

      cowplot::plot_grid(y, x, ncol = 1, align = "v", rel_heights = c(0.2, 1))
    })

    # HU plot
    output$HU <- renderPlot({
      req(res$best)
      req(eval_range())
      tidyr::tibble(t = 0:(length(res$best$HU) - 1),
                    HU = res$best$HU) |>
        ggplot(aes(x = t, y= HU)) +
        geom_segment(aes(x = which.max(res$best$HU) - 1, xend = (length(res$best$HU) / 2),
                         y = max(res$best$HU), yend = max(res$best$HU)),
                     color = "#CC79A7", linetype = "dashed", alpha = 0.05) +
        geom_line() +
        annotate("text", x = (length(res$best$HU) / 2), y = max(res$best$HU), hjust = -0.1,
                 label = paste0("Tm = ", which.max(res$best$HU) - 1)) +
        ylab(expression(paste("Discharge [L"~T^-1, "]"))) +
        theme_bw(base_size = 16)
    })

    # J parameter plot
    output$J_sa <- renderPlot({
      req(res$all)
      tidyr::tibble(J = res$all$J, obj = res$all$obj) |>
        dplyr::filter(obj >= input$min_obj) |>
        ggplot(aes(J, obj)) +
        geom_point() +
        xlab("J") +
        ylab(isolate(res$best$obj_name)) +
        theme_bw(base_size = 16)
    })

    # p1 parameter plot
    output$p1_sa <- renderPlot({
      req(res$all)
      tidyr::tibble(p1 = res$all$p1, obj = res$all$obj) |>
        dplyr::filter(obj >= input$min_obj) |>
        ggplot(aes(p1, obj)) +
        geom_point() +
        xlab(p1_name()[2]) +
        ylab(isolate(res$best$obj_name)) +
        theme_bw(base_size = 16) +
        theme(axis.title.y = element_blank()) # Remove Y axis title
    })

    # p2 parameter plot
    output$p2_sa <- renderPlot({
      req(res$all)
      tidyr::tibble(p2 = res$all$p2, obj = res$all$obj) |>
        dplyr::filter(obj >= input$min_obj) |>
        ggplot(aes(p2, obj)) +
        geom_point() +
        xlab(p2_name()[2]) +
        ylab(isolate(res$best$obj_name)) +
        theme_bw(base_size = 16) +
        theme(axis.title.y = element_blank()) # Remove Y axis title
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

    output$ui_p1 <- renderUI({
      sliderInput("p1",
                  "Tm [T]",
                  value = 6,
                  min = 0.1,
                  max = 10,
                  step = 0.1)
    })

    output$ui_p2 <- renderUI({
      sliderInput("p2",
                  "alpha [-]",
                  value = 3,
                  min = 0.1,
                  max = 10,
                  step = 0.1)
    })

    output$ui_p1_range <- renderUI({
      sliderInput("p1_range",
                  "Tm [T]",
                  value = c(0.1, 10),
                  min = 0.1,
                  max = 10,
                  step = 0.1)
    })

    output$ui_p2_range <- renderUI({
      sliderInput("p2_range",
                  "alpha [-]",
                  value = c(0.1, 10),
                  min = 0.1,
                  max = 10,
                  step = 0.1)
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
        utils::write.table(tidyr::tibble(t = 1:length(res$best$PN),
                                  PN = res$best$PN,
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
