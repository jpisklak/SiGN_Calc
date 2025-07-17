# Try loading SiGN package, install from GitHub if not already installed
if (!requireNamespace("SiGN", quietly = TRUE)) {
  devtools::install_github("jpisklak/SiGN")
}
library(SiGN)

# Functions to correct inputs
parse_numeric_input <- function(x) {
  as.numeric(strsplit(x, ",\\s*")[[1]])
}

parse_character_input <- function(x) {
  x_clean <- gsub("\\s+", "", x)        # Remove all whitespace
  strsplit(toupper(x_clean), ",")[[1]]  # Split and uppercase
}

# User interface
ui <- fluidPage(
  fluidPage(
    tags$head(
      tags$title("SiGN Model Calculator")
    )
    ),
  titlePanel(
    title = div(
      img(
        src = "hex_sticker.png",
        width = "7%",
        style = "margin-right: 15px;"
      ),
      "SiGN Model Prediction Calculator"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Initial Link Durations"),
      textInput("il_dur_a", "A", "1"),
      textInput("il_dur_b", "B", "1"),
      h3("Initial Link Schedules"),
      textInput("il_sched_a", "A", "FR"),
      textInput("il_sched_b", "B", "FR"),
      h4("Terminal Link Durations"),
      textInput("tl_dur_a1", "A1", "10"),
      textInput("tl_dur_a2", "A2", "10"),
      textInput("tl_dur_b1", "B1", "10"),
      textInput("tl_dur_b2", "B2", "10"),
      h4("Terminal Link Entry Probabilities"),
      textInput("tl_p_a1", "A1", "0.2"),
      textInput("tl_p_a2", "A2", "0.8"),
      textInput("tl_p_b1", "B1", "0.2"),
      textInput("tl_p_b2", "B2", "0.8"),
      h4("Terminal (i.e., Primary) Reinforcement Probabilities"),
      textInput("tr_p_a1", "A1", "1"),
      textInput("tr_p_a2", "A2", "0"),
      textInput("tr_p_b1", "B1", "0.5"),
      textInput("tr_p_b2", "B2", "0.5"),
      sliderInput("round_digits",
        "Decimal places to round output:",
        min = 0, max = 6, value = 3
      ),
    ),
    mainPanel(
      h3("Model Output:"),
      h4("Predicted Choice Proportion for A"),
      verbatimTextOutput("choice_prop"),
      hr(),
      h4("Rate of Terminal Reinforcement"),
      verbatimTextOutput("rein_rate"),
      hr(),
      h4("Conditional Reinforcement (δ)"),
      verbatimTextOutput("cond_rein"),
      hr(),
      h4("Additional Details"),
      verbatimTextOutput("add_details"),
      hr(),
      h5("SiGN R Package Version"),
      verbatimTextOutput("pkg_version"),
      hr(),
      br(),

# Notes, Reference and Citation info
      helpText(HTML("
      <h4>Using This Calculator:</h4>\n\n
      <p><b>Input Formatting:</b>
      <br>
      To input multiple values, use commas to separate them (e.g., 1, 2, 3, 4).
      <br><br>
      Note that the SiGN model is temporally relative, meaning it allows 
      initial and terminal link durations to be treated as any unit of time 
      (e.g., seconds, minutes, hours, etc.). However, it is crucial that, 
      whatever units are used, they are consistent across all link durations. 
      <br><br>
      <b>Signalling Function:</b>
      <br>
      A choice alternative is treated <i>by the calculator</i> as signalled 
      if both terminal link (TL) durations are non-zero and either the 
      durations or the reinforcement probabilities differ between the TLs.
      <br><br>
      This definition assumes that differences in duration or reinforcement 
      probability imply discriminability between the stimuli associated with 
      each TL. For example, if one TL lasts 10 seconds and the other 20 
      seconds, and both have a reinforcement probability of 1, it is assumed 
      that the organism can fully discriminate between them—e.g., the TLs may 
      be visually distinct (such as different colours) and thus are 
      recognisable from the moment of onset.
      <br><br>
      The SiGN model assumes that the (behavioural) function of terminal link 
      stimuli remains fixed across the entire duration of the terminal 
      link—that is, the probability of terminal reinforcement does not change 
      partway through the terminal link. Procedures that depart from this 
      assumption may not be appropriately handled by the model.
      <br><br>
      <b>Schedule Selection:</b>
      <br>
      Setting the initial link schedules as 'FR' (fixed-ratio) is most 
      suitable for cases involving an FR-1 schedule or when a single timer is 
      employed in the initial links of long VI (variable-interval) schedules. 
      This is because the 'FR' setting does not take into account the 
      switching behaviour present with concurrent schedules using independent 
      timers. For example, if a single timer is used for two concurrent VI 30 
      schedules, setting the initial link schedule as an FR is preferable 
      because the time spent in the initial links is controlled by one timer, 
      not two independent timers operating concurrently. However, if 
      independent timers are used for each initial link, the model requires 
      this setting to be VI.
      <br><br>
      Note that the SiGN model does not directly compute predictions for ratio 
      schedules. Instead, it represents the ratio as a duration with an 
      individual reinforcement rate, rather than a common/shared one.
      <br><br>
      <b>Model Information:</b>
      <br>
      Additional details about the model can be found in Dunn et al. (2024).
                    ")),
      br(), br(),
      tags$img(
        src = "diagram.svg",
        width = "50%",
        alt = "Diagram of concurrent-chains paradigm"
      ),
      br(), br(),
      h4("References:"),
      helpText(HTML("
      Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024). 
      Suboptimal choice: A review and quantification of the signal for good 
      news (SiGN) model. <i>Psychological Review</i>. <i>131</i>(1), 58-78. 
      <a href = 'https://doi.org/10.1037/rev0000416' target = '_blank'>
      https://doi.org/10.1037/rev0000416</a>
                    ")),
      br(),
      h4("To Cite This Page:"),
      uiOutput("citation")
    )
  )
)

# Server
server <- function(input, output, session) {
  model_result <- reactive({
    tryCatch(
      {
        pars <- choice_params(
          profile = "zentall",
          il_dur_a = parse_numeric_input(input$il_dur_a),
          il_dur_b = parse_numeric_input(input$il_dur_b),
          tl_dur_a1 = parse_numeric_input(input$tl_dur_a1),
          tl_dur_a2 = parse_numeric_input(input$tl_dur_a2),
          tl_dur_b1 = parse_numeric_input(input$tl_dur_b1),
          tl_dur_b2 = parse_numeric_input(input$tl_dur_b2),
          tl_p_a1 = parse_numeric_input(input$tl_p_a1),
          tl_p_a2 = parse_numeric_input(input$tl_p_a2),
          tl_p_b1 = parse_numeric_input(input$tl_p_b1),
          tl_p_b2 = parse_numeric_input(input$tl_p_b2),
          tr_p_a1 = parse_numeric_input(input$tr_p_a1),
          tr_p_a2 = parse_numeric_input(input$tr_p_a2),
          tr_p_b1 = parse_numeric_input(input$tr_p_b1),
          tr_p_b2 = parse_numeric_input(input$tr_p_b2),
          il_sched_a = parse_character_input(input$il_sched_a),
          il_sched_b = parse_character_input(input$il_sched_b)
        )
        SiGN(pars)
      },
      error = function(e) {
        e
      }
    )
  })

  output$choice_prop <- renderPrint({
    result <- model_result()
    if (inherits(result, "error")) {
      cat("--------🐦 Error in model computation:--------\n\n", result$message)
    } else {
      result$cp <- round(result$cp, input$round_digits)
      result$cp
    }
  })

  output$rein_rate <- renderPrint({
    result <- model_result()
    if (inherits(result, "error")) {
      cat("-------- 🐦 --------")
    } else {
      result$details[2:5] <- round(result$details[2:5], input$round_digits)
      cat("--------Individual IL:--------\n", sep = ", ")
      cat("Alternative A: \n", sep = ", ")
      cat(result$details[[2]], sep = ", ")

      cat("\n\nAlternative B: \n", sep = ", ")
      cat(result$details[[3]], sep = ", ")

      cat("\n\n--------Common Effective IL:--------\n", sep = ", ")
      cat("Alternative A: \n", sep = ", ")
      cat(result$details[[4]], sep = ", ")

      cat("\n\nAlternative B: \n", sep = ", ")
      cat(result$details[[5]], sep = ", ")
    }
  })

  output$cond_rein <- renderPrint({
    result <- model_result()
    if (inherits(result, "error")) {
      cat("-------- 🐦 --------")
    } else {
      result$details[7:8] <- round(result$details[7:8], input$round_digits)
      cat("Alternative A: \n", sep = ", ")
      cat(result$details[[7]], sep = ", ")

      cat("\n\nAlternative B: \n", sep = ", ")
      cat(result$details[[8]], sep = ", ")
    }
  })

  output$add_details <- renderPrint({
    result <- model_result()
    if (inherits(result, "error")) {
      cat("-------- 🐦 --------")
    } else {
      result$details[6:14] <- round(result$details[6:14], input$round_digits)

      cat("Big T:\n")
      cat(result$details[[6]], sep = ", ")

      cat("\nAverage Delay Reduction on Alternative A:\n", sep = ", ")
      cat(result$details[[9]], sep = ", ")
      cat("\nAverage Delay Reduction on Alternative B:\n", sep = ", ")
      cat(result$details[[10]], sep = ", ")
      cat("\nBonus Delay Reduction on Alternative A:\n", sep = ", ")
      cat(result$details[[11]], sep = ", ")
      cat("\nBonus Delay Reduction on Alternative B:\n", sep = ", ")
      cat(result$details[[12]], sep = ", ")
      cat("\nBeta on Alternative A:\n", sep = ", ")
      cat(result$details[[13]], sep = ", ")
      cat("\nBeta on Alternative B:\n", sep = ", ")
      cat(result$details[[14]], sep = ", ")
      cat(
        "\n\nAlternative A Signalled:\n ", result$details[[15]],
        "\nAlternative B Signalled:\n ", result$details[[16]]
      )
    }
  })

  output$pkg_version <- renderPrint({
    as.character(utils::packageVersion("SiGN"))
  })

  # Citation Information
  output$citation <- renderUI({
    pkg_ver <- as.character(utils::packageVersion("SiGN"))
    built <- strsplit(packageDescription("SiGN")$Built, ";")[[1]][3]
    year <- substr(trimws(built), 1, 4)

    HTML(paste0(
      "This calculator uses version <b>", pkg_ver, "</b> of the ",
      "<a href='https://jpisklak.github.io/SiGN/' target='_blank'>SiGN R ",
      "package</a>. If you use it in your work, please cite the package as ",
      "follows:",
      "<br><br>",
      "Pisklak, J., Dunn, R., McDevitt, M., & Spetch, M. (", year, "). <i>SiGN: ",
      "Signal for Good News Model R Package</i> (Version ", pkg_ver, ") ",
      "[R package]. <a href = 'https://doi.org/10.5281/zenodo.15955616'>",
      "https://doi.org/10.5281/zenodo.15955616</a>",
      "<br><br>",
      "<pre>",
      "@Manual{ ,
        title   = {SiGN: Signal for Good News Model R Package},
        author  = {Jeffrey Pisklak and Roger Dunn and Margaret McDevitt and Marcia Spetch},
        year    = {", year, "},
        note    = {Version ", pkg_ver, "},
        url     = {https://github.com/jpisklak/SiGN}
        doi     = {10.5281/zenodo.15955616}
      }",
      "</pre>"
    ))
  })
}

shinyApp(ui = ui, server = server)