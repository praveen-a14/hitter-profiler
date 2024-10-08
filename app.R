# app.R

library(shiny)
library(dplyr)
library(rsconnect)
library(formattable)
library(DT)
library(ggplot2)

ui <- fluidPage(
  tags$head(tags$style(HTML('
                             #metrics table {
                               width: 100%;
                             }
                             #scores table {
                               width: 100%;
                             }
                             #scores {
                               margin-top: 20px;
                             }
                             #trax_metrics table {
                               width: 100%;
                             }
                             #trax_metrics {
                               margin-top: 20px;
                             }
                             #time_plot {
                               margin-top: 20px;
                             }
                              '))),
  titlePanel("KPI Hitter Profiler"),
  sidebarLayout(
    sidebarPanel(
      fileInput("trax", "Upload HitTrax CSV", accept = ".csv"),
      fileInput("file", "Upload Blast CSV", accept = ".csv"),
      uiOutput("level_select")
   ),
    mainPanel(
      plotOutput("color_legend", height = "50px", width = "400px"),
      dataTableOutput("metrics"),
      dataTableOutput("trax_metrics"),
      dataTableOutput("scores"),
      textOutput("scale"),
      plotOutput("time_plot")
    )
  )
)

server <- function(input, output) {
  clrs <- c("#507dfa","#7c9efc","#ebe6ff","#fce1e1", "#ff6e6e", "#f73131")
  labels <- c("---", "--", "-", "+", "++", "+++")
  
  output$color_legend <- renderPlot({
    req(input$file, input$trax, input$level)
    
    par(mar = c(0, 0, 0, 0), xpd = TRUE)
    plot(1:6 + 3, rep(1, 6), pch = 15, cex = 7, col = clrs, axes = FALSE, xlab = "", ylab = "",, xlim = c(0.5, 10.5), ylim = c(0, 2))
    
    for (i in 1:6) {
      rect(i + 3 - 0.5, 0.2, i + 3 + 0.5, 1.8, border = "black", lwd = 1)
      text(i + 3, 1, labels[i], col = "black", cex = 1.5)
    }
  })
  
  data <- reactive({
    req(input$file)
    
    raw_data <- read.csv(input$file$datapath, skip = 8)
    raw_data$Date.Time <- as.POSIXct(raw_data$Date, format = "%b %d, %Y %I:%M:%S %p")
    
    #create metrics
    raw_data <- raw_data %>%
      filter(Swing.Details == "Pitching Machine" | Swing.Details == "In Game")
    
    a_swing_speed <- quantile(raw_data$Bat.Speed..mph., 0.9, na.rm = TRUE)

    raw_data <- raw_data %>% 
      summarise(
              `Bat Speed (mph)` = mean(Bat.Speed..mph.),
              `Rotational Acceleration (g)` = mean(Rotational.Acceleration..g.),
              `Attack Angle (deg)` = mean(Attack.Angle..deg.),
              `Vertical Bat Angle (deg)` = mean(abs(Vertical.Bat.Angle..deg.)),
              `Swings` = nrow(raw_data),
              `A-Swing Speed` = a_swing_speed,
              `Max Bat Speed` = max(Bat.Speed..mph., na.rm = TRUE),
              `A-Swing RA` = mean(Rotational.Acceleration..g.[Bat.Speed..mph. > a_swing_speed], na.rm = TRUE)
              )
    
    return(raw_data)
  })
  
  tdata <- reactive({
    req(input$trax)
    raw_trax <- read.csv(input$trax$datapath)
    raw_trax <- raw_trax %>%
      summarise(`Avg EV` = mean(AvgV[AB > 5], na.rm = TRUE),
                `Max EV` = max(MaxV, na.rm = TRUE),
                `LD%` = mean(LD..[AB > 5], na.rm = TRUE),
                `AVG` = mean(AVG[AB > 5], na.rm = TRUE),
                `SLG` = mean(SLG[AB > 5], na.rm = TRUE),
                `AVG DIST` = mean(Dist[AB > 5], na.rm = TRUE))
    return(raw_trax)
  })

  output$level_select <- renderUI({
    req(input$file, input$trax)
    selectInput("level", "Select Level", choices = c("", "High School", "College", "Pro", "Youth"), selected = "")
  })
  
  output$metrics <- renderDataTable({
    req(input$file, input$trax, input$level)
    
    metrics_data <- data() %>% 
      select(`Swings`, `Bat Speed (mph)`, `Max Bat Speed`, `A-Swing Speed`, `Rotational Acceleration (g)`, `A-Swing RA`, `Attack Angle (deg)`, `Vertical Bat Angle (deg)`)
    
    if(input$level == "Pro"){
      bs_brks <- c(67, 69, 71, 73, 75)
      ra_brks <- c(15, 17.5, 20, 22.5, 25)
      aa_brks <- c(6, 8.25, 10.5, 12.75, 15)
      vba_brks <- c(22, 25.5, 29, 32.5, 36)
      mbs_brks <- c(76, 77.75, 79.5, 81.25, 83)
      a_brks <- c(74, 75.5, 77, 78.5, 80)
      ara_brks <- c(17, 19.5, 22, 24.5, 27)
    } else if (input$level == "College"){
      bs_brks <- c(64, 66, 68, 70, 72)
      ra_brks <- c(12, 14.25, 16.5, 18.75, 21)
      aa_brks <- c(5, 7.25, 9.5, 11.75, 14)
      vba_brks <- c(22, 25.25, 28.5, 31.75, 35)
      mbs_brks <- c(72, 74, 76, 78, 80)
      a_brks <- c(70, 71.5, 73, 74.5, 76)
      ara_brks <- c(14, 16.25, 18.5, 20.75, 23)
    } else if (input$level == "High School"){
      bs_brks <- c(60, 62.5, 65, 67.5, 70)
      ra_brks <- c(7, 9, 11, 13, 15)
      aa_brks <- c(5, 7, 9, 11, 13)
      vba_brks <- c(18, 22, 26, 30, 34)
      mbs_brks <- c(68, 70, 72, 74, 76)
      a_brks <- c(63, 65.5, 68, 70.5, 73)
      ara_brks <- c(9, 11, 13, 15, 17)
    } else if (input$level == "Youth"){
      bs_brks <- c(46, 48.5, 51, 53.5, 56)
      ra_brks <- c(4, 5.75, 7.5, 9.25, 11)
      aa_brks <- c(2, 4, 6, 8, 10)
      vba_brks <- c(14, 18, 22, 26, 28)
      mbs_brks <- c(52, 54.75, 57.5, 60.25, 63)
      a_brks <- c(47, 49.5, 52, 54.5, 57)
      ara_brks <- c(6, 7.75, 9.5, 11.25, 13)
    }
    
    clrs <- c("#507dfa","#7c9efc","#ebe6ff","#fce1e1", "#ff6e6e", "#f73131")
    
    datatable(metrics_data,
              options = list(
                paging = FALSE,
                ordering = FALSE,
                searching = FALSE,
                lengthChange = FALSE,
                info = FALSE,           
                striped = TRUE,        
                hover = TRUE,          
                border = TRUE,
                columnDefs = list(
                  list(visible = FALSE, targets = 0)
                )
              ))%>%
      formatRound(columns = c('Bat Speed (mph)', 'Max Bat Speed', 'A-Swing Speed', 'Rotational Acceleration (g)', 'A-Swing RA'), digits = 2) %>%
      formatRound(columns = c('Attack Angle (deg)', 'Vertical Bat Angle (deg)'), digits = 0) %>%
      formatStyle(
        columns = "Bat Speed (mph)",
        backgroundColor = styleInterval(bs_brks, clrs),
        border = "solid 1px #000000"  # Border between cells
      ) %>%
      formatStyle(
        columns = "Rotational Acceleration (g)",
        backgroundColor = styleInterval(ra_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "Attack Angle (deg)",
        backgroundColor = styleInterval(aa_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "Vertical Bat Angle (deg)",
        backgroundColor = styleInterval(vba_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "Swings",
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "Max Bat Speed",
        backgroundColor = styleInterval(mbs_brks, clrs),
        border = "solid 1px #000000"
      )%>%
      formatStyle(
        columns = "A-Swing Speed",
        backgroundColor = styleInterval(a_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "A-Swing RA",
        backgroundColor = styleInterval(ara_brks, clrs),
        border = "solid 1px #000000"
      )
    
  }, server = FALSE)
  
  output$trax_metrics <- renderDataTable({
    req(input$file, input$trax, input$level)
    traxdata <- tdata() %>%
      select(`Avg EV`, `Max EV`, `LD%`, `AVG`, `SLG`, `AVG DIST`)
    if(input$level == "Pro"){
      aev_brks <- c(83, 85, 87, 89, 91)
      mev_brks <- c(104, 105.5, 107, 108.5, 110)
      ld_brks <- c(40, 45, 50, 55, 60)
      avg_brks <- c(.55, .6, .65, .7, .75)
      slg_brks <- c(.8, .9, 1, 1.1, 1.2)
      dist_brks <- c(380, 390, 400, 410, 420)
    } else if (input$level == "College"){
      aev_brks <- c(80, 82, 84, 86, 88)
      mev_brks <- c(95, 97.5, 100, 102.5, 105)
      ld_brks <- c(38, 43, 48, 53, 58)
      avg_brks <- c(.5, .55, .6, .65, .7)
      slg_brks <- c(.7, .775, .85, .925, 1)
      dist_brks <- c(350, 362.5, 375, 387.5, 400)
    } else if (input$level == "High School"){
      aev_brks <- c(74, 76, 78, 80, 82)
      mev_brks <- c(88, 90.25, 92.5, 94.75, 97)
      ld_brks <- c(35, 40, 45, 50, 55)
      avg_brks <- c(.4, .45, .5, .55, .6)
      slg_brks <- c(.5, .575, .65, .725, .8)
      dist_brks <- c(270, 285.5, 305, 322.5, 340)
    } else if (input$level == "Youth"){
      aev_brks <- c(55, 58.5, 62, 66, 70)
      mev_brks <- c(68, 71.5, 75, 78.5, 82)
      ld_brks <- c(30, 35, 40, 45, 50)
      avg_brks <- c(.3, .35, .4, .45, .5)
      slg_brks <- c(.3, .375, .45, .525, .6)
      dist_brks <- c(185, 202.5, 220, 237.5, 255)
    }
    
    clrs <- c("#507dfa","#7c9efc","#ebe6ff","#fce1e1", "#ff6e6e", "#f73131")
    
    datatable(traxdata,
              options = list(
                paging = FALSE,
                ordering = FALSE,
                searching = FALSE,
                lengthChange = FALSE,
                info = FALSE,           
                striped = TRUE,        
                hover = TRUE,          
                border = TRUE,
                columnDefs = list(
                  list(visible = FALSE, targets = 0)
                )
              )) %>%
      formatRound(columns = c('LD%', 'AVG DIST'), digits = 0) %>%
      formatRound(columns = c('Avg EV', 'Max EV'), digits = 1) %>%
      formatRound(columns = c('AVG', 'SLG'), digits = 3) %>%
      formatStyle(
        columns = "Avg EV",
        backgroundColor = styleInterval(aev_brks, clrs),
        border = "solid 1px #000000" 
      ) %>%
      formatStyle(
        columns = "Max EV",
        backgroundColor = styleInterval(mev_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "LD%",
        backgroundColor = styleInterval(ld_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "AVG",
        backgroundColor = styleInterval(avg_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "SLG",
        backgroundColor = styleInterval(slg_brks, clrs),
        border = "solid 1px #000000"
      ) %>%
      formatStyle(
        columns = "AVG DIST",
        backgroundColor = styleInterval(dist_brks, clrs),
        border = "solid 1px #000000"
      )
    
  }, server = FALSE)
  
  combined_data <- reactive({
    req(data(), tdata())
    
    raw_data <- data()
    raw_trax <- tdata()

    if (input$level == "Pro") {
      raw_data$nAA <- abs((raw_data$`Attack Angle (deg)` - 2) / 16)
      raw_data$nVBA <- (abs(raw_data$`Vertical Bat Angle (deg)`) - 18) / 24
      raw_data$nBS <- (raw_data$`Bat Speed (mph)` - 62) / 16
      raw_data$nRA <- (raw_data$`Rotational Acceleration (g)` - 10) / 14
      raw_trax$naEV <- (raw_trax$`Avg EV` - 81) / 15
      raw_trax$nmEV <- (raw_trax$`Max EV` - 100) / 22
    } else if (input$level == "College") {
      raw_data$nAA <- abs((raw_data$`Attack Angle (deg)` - 1) / 17)
      raw_data$nVBA <- (abs(raw_data$`Vertical Bat Angle (deg)`) - 16) / 26
      raw_data$nBS <- (raw_data$`Bat Speed (mph)` - 58) / 17
      raw_data$nRA <- (raw_data$`Rotational Acceleration (g)` - 8) / 13
      raw_trax$naEV <- (raw_trax$`Avg EV` - 75) / 17
      raw_trax$nmEV <- (raw_trax$`Max EV` - 92) / 24
    } else if (input$level == "High School") {
      raw_data$nAA <- abs(raw_data$`Attack Angle (deg)` / 18)
      raw_data$nVBA <- (abs(raw_data$`Vertical Bat Angle (deg)`) - 15) / 25
      raw_data$nBS <- (raw_data$`Bat Speed (mph)` - 50) / 23
      raw_data$nRA <- (raw_data$`Rotational Acceleration (g)` - 4) / 15
      raw_trax$naEV <- (raw_trax$`Avg EV` - 68) / 18
      raw_trax$nmEV <- (raw_trax$`Max EV` - 83) / 22
    } else if (input$level == "Youth") {
      raw_data$nAA <- abs(raw_data$`Attack Angle (deg)` / 18)
      raw_data$nVBA <- (abs(raw_data$`Vertical Bat Angle (deg)`) - 10) / 27
      raw_data$nBS <- (raw_data$`Bat Speed (mph)` - 40) / 22
      raw_data$nRA <- (raw_data$`Rotational Acceleration (g)` - 2) / 10
      raw_trax$naEV <- (raw_trax$`Avg EV` - 50) / 25
      raw_trax$nmEV <- (raw_trax$`Max EV` - 60) / 30
    }
    
    # Calculate scores
    raw_data$comp_path <- (raw_data$nAA + raw_data$nVBA) / 2
    raw_data$path_score <- 20 + (raw_data$comp_path * 60)
    raw_data$comp_input_power <- (0.7 * raw_data$nBS) + (0.3 * raw_data$nRA)
    raw_data$input_power_score <- 20 + (raw_data$comp_input_power * 60)
    raw_data$overall <- (0.6 * raw_data$input_power_score) + (0.4 * raw_data$path_score)
    raw_trax$comp_output <- (0.7 * raw_trax$naEV) + (0.3 * raw_trax$nmEV)
    raw_trax$output_power_score <- 20 + (raw_trax$comp_output * 60)
    raw_trax$overall_grade <- (0.4 * raw_data$path_score) + (0.3 * raw_data$input_power_score) + (0.3 * raw_trax$output_power_score)
    
    combined <- data.frame(
      'Path Grade' = raw_data$path_score,
      'Input Power Grade' = raw_data$input_power_score,
      'Output Power Grade' = raw_trax$output_power_score,
      'Overall' = raw_trax$overall_grade
    )
    
    return(combined)
  })
  
  output$scores <- renderDataTable({
    req(input$file, input$trax, input$level)
    
    scores_data <- combined_data() 
    
    score_brks <- c(40, 45, 50, 55, 60)
    
    clrs <- c("#507dfa","#7c9efc","#ebe6ff","#fce1e1", "#ff6e6e", "#f73131")
    
    datatable(scores_data,
              options = list(
                paging = FALSE,
                ordering = FALSE,
                searching = FALSE,
                lengthChange = FALSE,
                info = FALSE,           
                striped = TRUE,        
                hover = TRUE,          
                border = TRUE,
                columnDefs = list(
                  list(visible = FALSE, targets = 0)
                )
              )) %>%
    formatRound(columns = c('Path.Grade', 'Input.Power.Grade', 'Output.Power.Grade', 'Overall'), digits = 2) %>%
      formatStyle(
        columns = 'Path.Grade',
        backgroundColor = styleInterval(score_brks, clrs),
        border = "solid 1px #000000" 
      ) %>%
      formatStyle(
        columns = 'Input.Power.Grade',
        backgroundColor = styleInterval(score_brks, clrs),
        border = "solid 1px #000000" 
      ) %>%
      formatStyle(
        columns = 'Output.Power.Grade',
        backgroundColor = styleInterval(score_brks, clrs),
        border = "solid 1px #000000" 
      ) %>%
      formatStyle(
        columns = 'Overall',
        backgroundColor = styleInterval(score_brks, clrs),
        border = "solid 1px #000000" 
      )
  })
  
  output$scale <- renderText({
    req(input$file, input$trax, input$level)
    "Grades are on 20-80 scale, 50 is average. Adjusted to level of play"
  })
  
  output$time_plot <- renderPlot({
    req(input$file, input$trax, input$level)
    time_data <- read.csv(input$file$datapath, skip = 8)
    time_data$Date.Time <- as.POSIXct(time_data$Date, format = "%b %d, %Y %I:%M:%S %p")
    
    time_data$Date <- as.Date(time_data$Date.Time)
    
    # Group by Date and calculate the average Bat Speed for each day
    session_avg <- time_data %>%
      group_by(Date) %>%
      summarize(avg_bs = mean(`Bat.Speed..mph.`, na.rm = TRUE))
    
    # Plot the line graph
    ggplot(session_avg, aes(x = Date, y = avg_bs)) +
      geom_line() +
      ggtitle("Avg Bat Speed per Session") +
      labs(x = "Date", y = "Average Bat Speed (mph)")
  }, width = 550, height = 300)
  
}
shinyApp(ui = ui, server = server)



