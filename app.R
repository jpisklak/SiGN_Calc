library(shiny)
source('SiGN Function.R')

sigDigs = 4

#Put in caveat about greater than 0
#Trip error if entry probs are uneven or there are uneven vectors?

u <- shinyUI(pageWithSidebar(
  
  headerPanel("SiGN Model Prediction Calculator (v1.0)"),
  sidebarPanel(
    
    #Initial Link Durations
    h4('Initial Link Durations'),
    textInput('ILa_dur', 'A', "1"),
    textInput('ILb_dur', 'B', "1"),
    br(),
    
    #Initial Link Schedules
    h4('Initial Link Schedules'),
    textInput('Sched_IL_a', 'A (FR or VI only)', "FR"),
    textInput('Sched_IL_b', 'B (FR or VI only)', "FR"),
    br(),

    #Terminal Link Durations
    h4('Terminal Link Durations'),
    textInput('TLa1_dur', 'A1', "10"),
    textInput('TLa2_dur', 'A2', "10"),
    
    textInput('TLb1_dur', 'B1', "10"),
    textInput('TLb2_dur', 'B2', "10"),
    br(),
    
    #Entry Probabilities
    h4('Terminal Link Entry Probabilities'),
    textInput('TLa1_p', 'A1', "0.2"),
    textInput('TLa2_p', 'A2', "0.8"),
    
    textInput('TLb1_p', 'B1', "0.2"),
    textInput('TLb2_p', 'B2', "0.8"),
    br(),
    
    #Terminal/Primary Reinforcement Probabilities
    h4('Terminal (i.e., Primary) Reinforcement Probabilities'),
    textInput('TLa1_rp', 
              'A1', 
              "1"),
    textInput('TLa2_rp', 
              'A2', 
              "0"),
    
    textInput('TLb1_rp', 
              'B1', 
              "0.5"),
    textInput('TLb2_rp', 
              'B2', 
              "0.5")
 
  ),
  
  mainPanel(
    h3('Calculated Values'),
    verbatimTextOutput("out_1"),
    br(),
    h4(strong("Notes:")),
    h5(strong('Output Precision: ')), 
    p('The output displays values rounded to 4 decimal places, although calculations are performed with much higher levels of precision.'),
    
    h5(strong('Input Formatting: ')),
    p("To input multiple values, use commas to separate them (e.g., 1, 2, 3, 4). Ensure that each cell contains an equal number of inputs. Note that the SiGN model is temporally relative, meaning it allows initial and terminal link durations to be treated as any unit of time (e.g., seconds, minutes, hours, etc.). However, it is crucial that, whatever units are used, they are consistent across all link durations."),
    p("The calculator assumes that terminal links are distinct stimuli (e.g., A1 and A2 differ in colour). If the terminal stimuli are not distinct (e.g., identical in colour), the associated delays and probabilities must be entered as weighted averages to avoid erroneously treating the choice alternative as signalled. For further clarification, please contact the authors of Dunn et al. (2024)."),
    
    h5(strong('Initial Link Scheduling: ')),
       p('Setting the initial link schedules as ',
       em('FR'),
       'is most suitable for cases involving an FR 1 schedule or when a single timer is employed in the initial links of long VI schedules. This is because the ',
       em('FR'),
       ' setting does not take into account the switching behaviour present with concurrent schedules using independent timers. For example, if a single timer is used for two concurrent VI 30 schedules, setting the ',
       em('Initial Link Schedule'),
       ' as an ',
       em('FR'),
       ' is preferable because the time spent in the initial links is controlled by one timer, not two independent timers operating concurrently. However, if independent timers are used for each initial link, the calculator requires this setting to be ',
       em('VI'),
       '. Note that the SiGN model does not directly compute predictions for ratio schedules. Instead, it represents the ratio as a duration with an individual reinforcement rate, rather than a common/shared one.'),

    h5(strong('Model Information: ')),
       p('Additional details about the model can be found in Dunn et al. (2024)'),
    
    br(),
    
    img(src="diagram.svg", height="50%", width="50%"),
    
    br(),
    
    h4(strong('References:')),
    
    h5('Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024).',
       'Suboptimal choice: A review and quantification of the signal for good news (SiGN) model. ',
       em('Psychological Review, 131'), '(1), 58-78.',
       HTML("<a href='https://doi.org/10.1037/rev0000416'>https://doi.org/10.1037/rev0000416</a>")
       ),
    
    br(),
    
    h4(strong('To reference this page, please use:')),
    
    h5('Pisklak, J. M. (2024).',
       em('SiGN Model Prediction Calculator '), 
       '[version 1.0]. ', 
       HTML("<a href='https://jpisklak.shinyapps.io/SiGN_Calc/'>https://jpisklak.shinyapps.io/SiGN_Calc/</a>")
    ),
    
    br(),
    
    h5(strong('BibTeX entry for LaTeX users:')),
    
    h5(code('@Manual{,'), br(),
    code('title = {{SiGN} Model Prediction Calculator},'), br(),
    code('author = {Pisklak, J. M.},'), br(),
    code('date = {2024-02},'), br(),
    code('note = {version 1.0},'), br(),
    code('url = {https://jpisklak.shinyapps.io/SiGN_Calc/}'), br(),
    code('}')
  ),
  
  br(), br()
    
  )
))

s <- shinyServer(function(input, output) {
  
  output$out_1 <- renderPrint({
    #Initial Link Durations
    ILa_dur <- as.numeric(unlist(strsplit(input$ILa_dur, ",")))
    ILb_dur <- as.numeric(unlist(strsplit(input$ILb_dur, ",")))
    
    #Initial Link Schedules
    Sched_IL_a <- toupper(gsub(" ", "", unlist(strsplit(input$Sched_IL_a, ","))))
    Sched_IL_b <- toupper(gsub(" ", "", unlist(strsplit(input$Sched_IL_b, ","))))
    
    #Terminal Link Durations
    TLa1_dur <- as.numeric(unlist(strsplit(input$TLa1_dur, ",")))
    TLa2_dur <- as.numeric(unlist(strsplit(input$TLa2_dur, ",")))
    
    TLb1_dur <- as.numeric(unlist(strsplit(input$TLb1_dur, ",")))
    TLb2_dur <- as.numeric(unlist(strsplit(input$TLb2_dur, ",")))
    
    #Entry Probabilities
    TLa1_p <- as.numeric(unlist(strsplit(input$TLa1_p, ",")))
    TLa2_p <- as.numeric(unlist(strsplit(input$TLa2_p, ",")))
    
    TLb1_p <- as.numeric(unlist(strsplit(input$TLb1_p, ",")))
    TLb2_p <- as.numeric(unlist(strsplit(input$TLb2_p, ",")))
    
    #Terminal/Primary Reinforcement Probabilities
    TLa1_rp <- as.numeric(unlist(strsplit(input$TLa1_rp, ",")))
    TLa2_rp <- as.numeric(unlist(strsplit(input$TLa2_rp, ",")))
    
    TLb1_rp <- as.numeric(unlist(strsplit(input$TLb1_rp, ",")))
    TLb2_rp <- as.numeric(unlist(strsplit(input$TLb2_rp, ",")))
    
    results <- SiGN(ILa_dur, ILb_dur,
                    TLa1_dur, TLa2_dur,
                    TLb1_dur, TLb2_dur,
                    TLa1_p, TLa2_p,
                    TLb1_p, TLb2_p,
                    TLa1_rp, TLa2_rp,
                    TLb1_rp, TLb2_rp,
                    Sched_IL_a, Sched_IL_b,
                    Sdelt_dur = 1,
                    useBeta = TRUE)
    
    l <- list(ILa_dur, ILb_dur,
              TLa1_dur, TLa2_dur,
              TLb1_dur, TLb2_dur,
              TLa1_p, TLa2_p,
              TLb1_p, TLb2_p,
              TLa1_rp, TLa2_rp,
              TLb1_rp, TLb2_rp,
              Sched_IL_a, Sched_IL_b)

    flag = FALSE
    
    if (length(unique(sapply(l, length))) != 1 &
        !anyNA(l, recursive = TRUE)) {
      cat('WARNING:Input values are unequal lengths.\n',
          '--------------------------------------------\n\n')
      flag = TRUE
    }
    
    if (sum(unlist(l[15:16]) %in% c('FR', 'VI')) != length(unlist(l[15:16]))) {
      cat('WARNING: Only FR and VI are accepted inputs.\n',
          '--------------------------------------------\n\n')
      flag = TRUE
    }
    
    if (sum((TLa1_p + TLa2_p)) + sum((TLb1_p + TLb2_p)) != length(c(TLa1_p, TLb1_p))) {
      cat('WARNING: Entry probabilities must sum to 1 for each alternative.\n',
          '--------------------------------------------\n\n')
      flag = TRUE
    }
    
    if (!all(Sched_IL_a == Sched_IL_b)) {
      cat('WARNING: A and B must contain the same schedule types.\n',
          '--------------------------------------------\n\n')
      flag = TRUE
    }
    
    if (flag == FALSE) {
    cat('Predicted Choice Proportion for A: \n')
    cat(round(results$SiGN_cp, sigDigs), sep = ', ')
    
    cat('\n\nBig T: \n', sep = ', ')
    cat(round(results$BigT, sigDigs), sep = ', ')
    
    cat('\n\nConditional Reinforcement (δ) on A: \n', sep = ', ')
    cat(round(results$CRa, sigDigs), sep = ', ')
    
    cat('\n\nConditional Reinforcement (δ) on B: \n', sep = ', ')
    cat(round(results$CRb, sigDigs), sep = ', ')
    
    cat('\n\nRate of Terminal Reinforcement on A\n', sep = ', ')
    cat(round(results$r_a, sigDigs), sep = ', ')
    
    cat('\n\nRate of Terminal Reinforcement on B\n', sep = ', ')
    cat(round(results$r_b, sigDigs), sep = ', ')
    
    cat('\n\nRate of Terminal Reinforcement on A w/switching\n', sep = ', ')
    cat(round(results$r_a_Com, sigDigs), sep = ', ')
    
    cat('\n\nRate of Terminal Reinforcement on B w/switching\n', sep = ', ')
    cat(round(results$r_b_Com, sigDigs), sep = ', ')
    
    cat('\n\nAlternative A Signalled: ', results$A_Sig,
        '\nAlternative B Signalled: ', results$B_Sig)
    }

  }
  )

  
}
)
shinyApp(ui = u, server = s)