library(shiny)
library(data.table)
library(ggplot2)
library(gridExtra)
library(readr)
library(tidyverse)
library(DT)
library(dplyr)
library(plotly)
library(listviewer)
library(xts)
library(rsconnect)

plot1_data = read.csv("sampled_fixed_data.csv", stringsAsFactors=FALSE)
downloadData = read.csv("myFile.csv")

invalidateBarplots = 11500
invalidateTables = 6000

function(input, output, session) {

  plot1_data <- read.csv("sampled_fixed_data.csv", stringsAsFactors=FALSE)
  ystart1 <- plot1_data$sensor_1
  ystart2 <- plot1_data$sensor_2
  ystart3 <- plot1_data$sensor_3
  ystart4 <- plot1_data$sensor_4
  xstart <-  plot1_data$timeline
  hline = function(y = 0, color = "#FF3F3F") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1,
      xref = "paper", 
      y0 = y, 
      y1  = y, 
      line = list(color = color, 
                  width = 0.75), 
      name = "threshold"
    )
  }
  
  data <- data.frame(xstart, 
                     ystart1, 
                     ystart2,
                     ystart3,
                     ystart4)
  
  tooltip_data = tibble(
    text1 = "Machine(s): CPS 1; 
             Latency: 6ms", 
    text2 = "Machine(s): CPS 2;
             Latency: 9ms",
    text3 = "Machine(s): CPS 2;
             Latency: 2ms",
    text4 = "Machine(s): CPS 3;
             Latency: 11ms")

  output$plot1<-renderPlotly({
    figLine =  plot_ly(data = tooltip_data,x = ~xstart,  hoverinfo = "text") %>%
      add_trace(y = ~ystart3, name = "Sensor 3 l (CPS2)", mode = "lines", line = list(color = "#0055b3"),  text = ~text3) %>% #color = "#0033CC",
      add_trace(y = ~ystart4, name = "Sensor 4 l (CPS3)", mode = "lines", line = list(color = "#43AAE0"), text = ~text4) %>%
    
      layout(shapes = list(hline(4), hline(15)), 
             xaxis = list(title = list(text = "Seconds after start",
                                       font = list(color = "#515A5A", 
                                                   size = 15)),
                          tickfont = list(size = 12)), 
             yaxis = list(title = list(text = "Water level (l)",
                                       font = list(color = "#515A5A",
                                                   size = 15)),
                          tickfont = list(size = 12),
                          side = "left", 
                          range = c(0,16)), 
             legend = list(orientation = "h", 
                           x = 0, 
                           y = 1.25),
             hovermode = "x") 
    }) 
  

  rv <- reactiveValues(
    timeline_r = length(xstart),
    sensor_3_r = mean(ystart3)+25,
    sensor_4_r = mean(ystart4)+10)
   
  observe({
    invalidateLater(1000)
    isolate({
      rv$timeline_r <- rv$timeline_r + 1
      rv$sensor_3_r <- rnorm(1, mean(ystart3)+4, 2)
      rv$sensor_4_r <- rnorm(1, mean(ystart4), 1)
    })
    plotlyProxy("plot1", session) %>%
      plotlyProxyInvoke(
        "extendTraces", 
        list(
          y = list(list(rv$sensor_3_r)), 
          x = list(list(rv$timeline_r))
        ), 
        list(0), 40
      ) %>%
      plotlyProxyInvoke(
        "extendTraces", 
        list(
          y = list(list(rv$sensor_4_r)), 
          x = list(list(rv$timeline_r))
        ), 
        list(1), 40
      )  
    plotlyProxy('plot1', session) %>%
      plotlyProxyInvoke(
        method = "relayout", 
        list(
          xaxis = list('[0,100]')
       )
      )
  })
  
  output$plot4<-renderPlotly({
    figLine2 =  plot_ly(data = tooltip_data,x = ~xstart,  hoverinfo = "text") %>%
      add_trace(y = ~ystart1, name = "Sensor 1 °C (CPS1)", mode = "lines", line = list(color = "#d45500"),  text = ~text1)%>% #color = "#FF9900",
      add_trace(y = ~ystart2, name = "Sensor 2 °C (CPS2)", mode = "lines", line = list(color = "#ffa05f"), text = ~text2) %>%
      
      layout(title = list(text = "Sensor data (seconds after start)",
                          font = list(color ="#515A5A"), 
                          x = 0.6),
             shapes = list(hline(5), hline(25)), 
             xaxis = list(title = list(text = "Seconds after start",
                                       font = list(color = "#515A5A", 
                                                   size = 15)),
                          tickfont = list(size = 12)), 
             yaxis = list(title = list(text = "Temperature (°C)",
                                       font = list(color = "#515A5A",
                                                   size = 15)),
                          tickfont = list(size = 12),
                          side = "left", 
                          range = c(0,30)), 
             showlegend = FALSE,
             hovermode = "x") 
  }) 
  
  rv2 <- reactiveValues(
    timeline_r = length(xstart),
    sensor_1_r = mean(ystart1)+25,
    sensor_2_r = mean(ystart2)+25, 
    lowerThreshold = 5)
    
    observe({
      invalidateLater(1000)
      isolate({
        rv2$timeline_r <- rv2$timeline_r + 1
        rv2$sensor_1_r <- rnorm(1, mean(ystart1), 4)
        rv2$sensor_2_r <- rnorm(1, mean(ystart2)-10, 1.5)
        rv2$lowerThreshold = 5
      })
      
      plotlyProxy("plot4", session) %>%
        plotlyProxyInvoke(
          "extendTraces", 
          list(
            y = list(list(rv2$sensor_1_r)), 
            x = list(list(rv2$timeline_r))
          ), 
          list(0), 40
        ) %>%
        plotlyProxyInvoke(
          "extendTraces", 
          list(
            y = list(list(rv2$sensor_2_r)), 
            x = list(list(rv2$timeline_r))
          ), 
          list(1), 40
        ) 
      
      plotlyProxy('plot4', session) %>%
        plotlyProxyInvoke(
          method = "relayout", 
          list(
            xaxis = list('[0,100]')
          )
        )
    })
  output$plot2 = renderPlotly({
    invalidateLater(invalidateBarplots)
    nNeg = sample(1:100, 1, replace = TRUE)
    nRel = sample(1:70, 1, replace = TRUE)
    nCrit = sample(1:50, 1, replace = TRUE)
    nSev = sample(1:35, 1, replace = TRUE)
    
    sharenNeg = round(nNeg/(nNeg+nRel+nCrit+nSev)*100,2)
    sharenRel = round(nRel/(nNeg+nRel+nCrit+nSev)*100,2)
    sharenCrit = round(nCrit/(nNeg+nRel+nCrit+nSev)*100,2)
    sharenSev = round(nSev/(nNeg+nRel+nCrit+nSev)*100,2)
    
    Neglectables = rep('Neglectable', nNeg)
    Relevants = rep('Relevant', nRel)
    Criticals = rep('Critical', nCrit)
    Severes = rep('Critical', nSev)
    
    critLevels = factor(c("Neglectable",
                   "Relevant",
                   "Critical",
                   "Severe"), 
                   levels = c("Neglectable",
                              "Relevant",
                              "Critical",
                              "Severe"))
    figBar = plot_ly(
      x = critLevels,
      y = c(length(Neglectables), 
            length(Relevants), 
            length(Criticals), 
            length(Severes)),
      textposition = "auto",
      type = "bar",
      marker = list(color = c(
                              "#ADEDC1", 
                              "#FFFFA3", 
                              "#FFE48F",
                              "#FF9393"),
                    line = list(color = c(
                                          "#26AC4F", 
                                          "#FFFF01",
                                          "#FFC001",
                                          "#FF3F3F"), 
                                width = 1.5)),
    text = c(str_glue('{nNeg}'),
             str_glue('{nRel}'),
             str_glue('{nCrit}'),
             str_glue('{nSev}')),
    
    hovertext = c(str_glue('Current number of neglectable anomalies in overall CPS configuration: {nNeg}, this makes up {sharenNeg}%'),
                  str_glue('Current number of relevant anomalies in overall CPS configuration: {nRel}, this makes up {sharenRel}%'),
                  str_glue('Current number of critical anomalies in overall CPS configuration: {nCrit}, this makes up {sharenCrit}%'),
                  str_glue('Current number of severe anomalies in overall CPS configuration: {nSev}, this makes up {sharenSev}%')),
    
    hoverinfo = 'hovertext'
    )
    
    figBar = figBar %>% layout(title = "Criticality of unsolved anomalies",
                               font = list(color="#515A5A"),
                               xaxis = list(
                                 title ="",
                                 tickfont = list(
                                   size = 13,
                                   color = 'color = "#515A5A"')),
                               yaxis = list(
                                 title = 'Criticality Levels',
                                 tickfont = list(
                                   size = 11.5,
                                   color = 'color = "#515A5A"')),
                               legend = list(x = 0, 
                                             y = 1, 
                                             bgcolor = 'color = "#515A5A"', 
                                             bordercolor = 'color = "#515A5A"'))
    
    figBar  
  })
  table_input = reactive({
    read.csv("table_base_data.csv",
             sep =",")
  })
  myData =  reactive({
    invalidateLater(invalidateTables)
    
    ID1 = sample(1:50, 1)
    
    critTab1 = sample(c("Neglegtable", 
                        "Relevant", 
                        "Critical", 
                        "Severe"), 
                      1, 
                      replace = TRUE, 
                      c(0.55, 
                        0.35, 
                        0.95, 
                        0.05))
    
    causTab1 = sample(c("CPS 1 water low", 
                        "CPS 1 temperature low", 
                        "CPS 3 water missing", 
                        "CPS 2 filter contaminated", 
                        "CPS 3 output weight low", 
                        "CPS 3 output weight declines steadily"), 
                      1, 
                      replace = TRUE, 
                      c(0.167, 
                        0.167,
                        0.167,
                        0.167,
                        0.167,
                        0.165))
    
    casOrRulTab1 = sample(c("Case 2", 
                            "Rule 4", 
                            "Case 2", 
                            "Case 1", 
                            "Rule 1"), 
                          1, 
                          replace = TRUE, 
                          c(0.2,
                            0.2,
                            0.2,
                            0.2,
                            0.2))
    
    evalTab1 = sample(c("Tolerated", 
                        "Solved", 
                        "Prevented"), 
                      1, 
                      replace = TRUE, 
                      c(0.1, 
                        0.5, 
                        0.4))
    
    comTab1 = sample(c("Unclear how that works", 
                       "Case should not be changed", 
                       "-", 
                       "Should work with rule 5", 
                       "Requires new rule", 
                       "Should work with rule 4",
                       "Also works with case 5", 
                       "Also works with case 3", 
                       "Should work with rule 6"), 
                     1, 
                     replace = TRUE, 
                     c(0.11, 
                       0.11,
                       0.11,
                       0.11,
                       0.11,
                       0.11,
                       0.11,
                       0.11,
                       0.11))
    
    timeTab1 = format(Sys.time(), "%H:%M:%S")
    
    if(input$sensors == 1) {readTable0 = "myFile.csv"}
    if(input$sensors == 2) {readTable0 = "myFile.csv"}
    if(input$sensors == 3) {readTable0 = "myFile.csv"}
    if(input$sensors == 4) {readTable0 = "myFile.csv"}
    
    currentTab1 = data.frame(read.table(readTable0,
                                        sep = ",",
                                        stringsAsFactors = FALSE))
    
    colnames(currentTab1) = c("ID1",
                              "timeTab1",
                              "critTab1",
                              "causTab1", 
                              "casOrRulTab1",
                              "evalTab1",
                              "comTab1")
    
    newLine = data.frame(ID1,
                         timeTab1,
                         critTab1,
                         causTab1,
                         casOrRulTab1,
                         evalTab1,
                         comTab1, 
                         row.names = FALSE)
    
    colnames(newLine) = c("ID1",
                          "timeTab1",
                          "critTab1",
                          "causTab1", 
                          "casOrRulTab1",
                          "evalTab1",
                          "comTab1")
    
    tableData = rbind(newLine, 
                      head(currentTab1, 24)
    )
    row.names(tableData) = NULL
    
    write.table(tableData,
                file = readTable0,
                row.names = FALSE, 
                col.names = FALSE,
                quote = FALSE, 
                sep = ","
    )
    tableData
  })
  
  
  output$table = DT::renderDT(
    server = TRUE,
    expr = {
      isolate(datatable(myData(),
                        filter = list(position = "top"),
                        options = list( 
                          dom = 'tp',
                          pageLength = 5,
                          lengthMenu = c(5,10,15,20),
                          processing = FALSE, 
                          autoWidth = TRUE,
                          columnDefs = list(
                            list(className = 'dt-left',
                                 targets = "_all")
                          )),
                        colnames = c("ID",
                                     "Time",
                                     "Criticality", 
                                     "Cause", 
                                     "Case or Rule", 
                                     "Solutions", 
                                     "Comment"), 
                        callback = JS("
    var tips = [
    '',
    'This column shows the ID of anomaly events',
    'This column shows the timestamp of anomaly events',
    'This column shows an automized assignment of anomalies to criticality levels', 
    'This column shows an automized assignment of anomalies to their causes', 
    'This column shows which case or rule was used to mitigate the anomalies',
    'This column shows whether the mitigation of the anomalies using the cases and rules from the previous column were successful', 
    'This column shows a comment from a worker to a single incident'
    ],
    header = table.columns().header();
    for (var i = 0; i < tips.length; i++) {$(header[i]).attr('title', tips[i]);}
                  ")))%>%
                formatStyle(
                  "critTab1",
                  backgroundColor = styleEqual(c("Neglegtable", 
                                                 "Relevant", 
                                                 "Critical", 
                                                 "Severe"),
                                               c("#ADEDC1", 
                                                 "#FFFFA3", 
                                                 "#FFE48F",
                                                 "#FF9393")))})
    observe({
      replaceData(
        dataTableProxy("table"), 
        myData(), 
        resetPaging = FALSE, 
        clearSelection = FALSE
      )
    })
  output$barplot <- renderPlotly({
    
    invalidateLater(invalidateBarplots)
    
    nNeg2 = sample(10:100, 1, replace = TRUE)
    nRel2 = sample(10:70, 1, replace = TRUE)
    nCrit2 = sample(10:50, 1, replace = TRUE)
    nSev2 = sample(10:35, 1, replace = TRUE)
    
    Neglectables2 = rep('Neglectable', nNeg2)
    Relevants2 = rep('Relevant', nRel2)
    Criticals2 = rep('Critical', nCrit2)
    Severes2 = rep('Critical', nSev2)
    
    sharenNeg2 = round(nNeg2/(nNeg2+nRel2+nCrit2+nSev2)*100,2)
    sharenRel2 = round(nRel2/(nNeg2+nRel2+nCrit2+nSev2)*100,2)
    sharenCrit2 = round(nCrit2/(nNeg2+nRel2+nCrit2+nSev2)*100,2)
    sharenSev2 = round(nSev2/(nNeg2+nRel2+nCrit2+nSev2)*100,2)
    
    critLevels2 = factor(c("Neglectable",
                          "Relevant",
                          "Critical",
                          "Severe"), 
                        levels = c("Neglectable",
                                   "Relevant",
                                   "Critical",
                                   "Severe"))
    
    yValues = c(length(Neglectables2), 
                length(Relevants2), 
                length(Criticals2), 
                length(Severes2))
    
    if(input$sensors == 1){yValues <- c(length(Neglectables2), 
                                       length(Relevants2), 
                                       length(Criticals2), 
                                       length(Severes2))}
    if(input$sensors == 2){yValues <- c(length(Neglectables2)-7, 
                                       length(Relevants2)-10, 
                                       length(Criticals2), 
                                       length(Severes2)+5)}
    if(input$sensors == 3){yValues <- c(length(Neglectables2)-7, 
                                       length(Relevants2)-9, 
                                       length(Criticals2)+5, 
                                       length(Severes2)+3)}
    if(input$sensors == 4){yValues <- c(length(Neglectables2)+5, 
                                       length(Relevants2)-9, 
                                       length(Criticals2)-7, 
                                       length(Severes2)-3)}

    bar <- plot_ly(
      x = critLevels2,
      y = yValues,
      type = "bar",
      textposition = "auto",
      marker = list(color = c( 
                              "#ADEDC1", 
                              "#FFFFA3", 
                              "#FFE48F",
                              "#FF9393"),
                    line = list(color = c(
                                          "#26AC4F",
                                          "#FFFF01",
                                          "#FFC001",
                                          "#FF3F3F"),
                                width = 1.5)),
      text = c(str_glue('{nNeg2}'), 
               str_glue('{nRel2}'), 
               str_glue('{nCrit2}'), 
               str_glue('{nSev2}')),
      
      hovertext = c(str_glue('Current number of neglectable anomalies for sensor {input$sensors}: {nNeg2}, this makes up {sharenNeg2}%'),
               str_glue('Current number of relevant anomalies for sensor {input$sensors}: {nRel2}, this makes up {sharenRel2}%'),
               str_glue('Current number of critical anomalies for sensor {input$sensors}: {nCrit2}, this makes up {sharenCrit2}%'),
               str_glue('Current number of severe anomalies for sensor {input$sensors}: {nSev2}, this makes up {sharenSev2}%')),
      hoverinfo = 'hovertext')


    bar <- bar %>% layout(title = "Criticality of unsolved anomalies",
                          font = list(color = "#515A5A"),
                          xaxis = list(
                            title = "",
                            tickfont = list(
                              size = 13,
                              color = "#515A5A")),
                          yaxis = list(
                            title = 'Criticality Levels',
                            tickfont = list(
                              size = 11.5,
                              color = "#515A5A")),
                          legend = list(x = 0, y = 1, color = "#515A5A", color = "#515A5A"))

    bar
  })

#######################################################################################################  

  output$barplot2 <- renderPlotly({
    invalidateLater(invalidateBarplots)
    
    nSol = sample(15:35, 1, replace = TRUE)
    nTol = sample(5:15, 1, replace = TRUE)
    nPrev = sample(15:30, 1, replace = TRUE)
    
    sharenSol = round(nSol/(nSol+nTol+nPrev)*100,2)
    sharenTol = round(nTol/(nSol+nTol+nPrev)*100,2)
    sharenPrev = round(nPrev/(nSol+nTol+nPrev)*100,2)
    
    Solveds = rep("Solved", nSol)
    Tolerateds = rep("Tolerated", nTol)
    Preventeds = rep("Prevented", nPrev)
    
    prevLevels = factor(c("Solved", 
                          "Prevented", 
                          "Tolerated"), 
                        levels = c("Solved", 
                                   "Prevented", 
                                   "Tolerated"))
    xValues = c(length(Solveds), 
                 length(Preventeds), 
                 length(Tolerateds))
    
    if(input$sensors == 1){xValues <- c(length(Solveds), 
                                         length(Preventeds), 
                                         length(Tolerateds))}
    if(input$sensors == 2){xValues <- c(length(Solveds)-4, 
                                         length(Preventeds)+3, 
                                         length(Tolerateds)-2)}
    if(input$sensors == 3){xValues <- c(length(Solveds)+4, 
                                         length(Preventeds)-3, 
                                         length(Tolerateds))}
    if(input$sensors == 4){xValues <- c(length(Solveds)-2, 
                                         length(Preventeds)+5, 
                                         length(Tolerateds)-5)}

    bar <- plot_ly(
      type = "bar",
      y = prevLevels,
      x = xValues,
      textposition = "auto",
      marker = list(color =  c("#ADEDC1", 
                               "#FFFFA3", 
                               "#FF9393"), 
                    line = list(color = c("#26AC4F", 
                                          "#FFFF01", 
                                          "#FF3F3F"),
                                width = 1.5), 
                    pattern = list(shape = "/", 
                                   bgcolor = "white", 
                                   solidity = 0.4)),
      orientation = 'h',
      text = c(str_glue('{nSol}'), 
               str_glue('{nPrev}'), 
               str_glue('{nTol}')),
      
      hovertext = c(str_glue('Current number of solved anomalies for sensor {input$sensors}: {nSol}, this makes up {sharenSol}%'),
                    str_glue('Current number of prevented anomalies for sensor {input$sensors}: {nPrev}, this makes up {sharenPrev}%'),
                    str_glue('Current number of tolerated anomalies for sensor {input$sensors}: {nTol}, this makes up {sharenTol}%')),
      hoverinfo = 'hovertext'
    )
    
    bar <- bar %>% layout(title = "Solutions",
                          font = list(color = "#515A5A"),
                          xaxis = list(
                            tickfont = list(
                              size = 11.5,
                              color = "#515A5A")),
                          yaxis = list(
                              title = "",
                              tickfont = list(
                              size = 13,
                              color = "#515A5A")),
                          legend = list(x = 0, y = 1, color = "#515A5A", color = "#515A5A"), 
                          margin = list(l = 50, r = 50, b = 50, t = 50, pad = 6))
    
    bar
  })

################################################################################################
  
  output$lineplot <- renderPlot({
    sampled_data <- sampled_data()
    
    ggplot(sampled_data, aes(x=timestamp, y=value, fill=category)) +
      geom_line(color="steelblue") +
      geom_point() +
      geom_area() +
      scale_fill_manual(values=c("lightblue", "lightgreen")) +
      xlab("") +
      scale_x_date(date_breaks = "1 week", date_labels = "%W") +
      scale_x_date(date_minor_breaks = "2 day")
  })
  
  output$metricstable <- renderTable({
    metrics <- read.csv("metric.csv", 
                        stringsAsFactors=FALSE)
    tbl_data <- filter(metrics, 
                       sensor_name == paste0('sensor',
                                             input$sensors) & 
                         algo_name == paste0('algo',
                                             input$algorithms))
    output_tbl <- data.frame(
      'KPI' = tbl_data$metric_name,
      'Value' = tbl_data$value)
  },
  digits = 4, 
  bordered = TRUE)

  output$title_occtable <- renderText({
    paste("Recent occurences for Sensor ", input$sensors)
  })

  myData2 =  reactive({

    invalidateLater(invalidateBarplots)
    critTab2 = sample(c("Neglegtable",
                        "Relevant",
                        "Critical",
                        "Severe"),
                      1,
                      replace = TRUE,
                      c(0.55,
                        0.35,
                        0.95,
                        0.05))
    
    ID2 = sample(1:50, 1)

    causTab2 = sample(c("CPS 1 water temperature low",
                        "CPS 1 water low",
                        "CPS 2 polluted",
                        "CPS 2 out of water",
                        "CPS 1 temperature too high",
                        "CPS 3 water missing",
                        "CPS 2 filter contaminated",
                        "CPS 3 output weight low",
                        "CPS 3 output weight declines steadily"),
                      1,
                      replace = TRUE,
                      c(0.11,
                        0.11,
                        0.11,
                        0.11,
                        0.11,
                        0.11,
                        0.11,
                        0.11,
                        0.12))

    casOrRulTab2 = sample(c("Case 1",
                            "Case 2",
                            "Case 3",
                            "Case 4",
                            "Rule 1",
                            "Rule 2",
                            "Rule 3",
                            "Rule 4"),
                          1,
                          replace = TRUE,
                          c(0.125,
                            0.125,
                            0.125,
                            0.125,
                            0.125,
                            0.125,
                            0.125,
                            0.125))

    evalTab2 = sample(c("Tolerated",
                        "Solved", 
                        "Prevented"),
                      1,
                      replace = TRUE,
                      c(0.4,
                        0.5, 
                        0.1))

    comTab2 = sample(c("Unclear how that works",
                       "Case should not be changed",
                       "-",
                       "-",
                       "Never works with rule 1",
                       "Always works",
                       "Should work with rule 5",
                       "Requires new rule",
                       "Should work with rule 4",
                       "Also works with case 5",
                       "Also works with case 3",
                       "Should work with rule 6"),
                     1,
                     replace = TRUE,
                     c(0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085,
                       0.085))
    
    timeTab2 = format(Sys.time(), "%H:%M:%S")

    if(input$sensors == 1) {readTable = "myFileSensors1.csv"}
    if(input$sensors == 2) {readTable = "myFileSensors2.csv"}
    if(input$sensors == 3) {readTable = "myFileSensors3.csv"}
    if(input$sensors == 4) {readTable = "myFileSensors4.csv"}

    currentTab2 = data.frame(read.table(readTable,
                                        sep = ",",
                                        stringsAsFactors = FALSE))

    colnames(currentTab2) = c("ID2",
                              "timeTab2",
                              "critTab2",
                              "causTab2",
                              "casOrRulTab2",
                              "evalTab2",
                              "comTab2")

    newLine2 = data.frame(ID2,
                          timeTab2,
                          critTab2,
                          causTab2,
                          casOrRulTab2,
                          evalTab2,
                          comTab2,
                          row.names = FALSE)
    
    colnames(newLine2) = c("ID2",
                           "timeTab2",    
                           "critTab2",
                           "causTab2",
                           "casOrRulTab2",
                           "evalTab2",
                           "comTab2")

    tableData2 = rbind(newLine2,
                       head(currentTab2, 24)
    )
    row.names(tableData2) = NULL

    write.table(tableData2,
                file = readTable,
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE,
                sep = ","
    )
    tableData2
  })

  output$occtable = DT::renderDT(
    server = TRUE,
    expr = {
      isolate(datatable(myData2(), 
              filter = "top",
    options = list(
      dom = 'tp',
      pageLength = 5,
      lengthMenu = c(5,10,15,20),
      processing = FALSE, 
      autoWidth = TRUE
    ),
    colnames = c("ID",
                 "Time",
                 "Criticality",
                 "Cause",
                 "Case or Rule",
                 "Solutions",
                 "Comment"), 
    callback = JS("
    var tips = [
    '',
    'This column shows the ID of anomaly events',
    'This column shows the timestamp of anomaly events',
    'This column shows an automized assignment of anomalies to criticality levels for the selected sensor',
    'This column shows an automized assignment of anomalies to their causes for the selected sensor',
    'This column shows which case or rule was used to mitigate the anomalies for the selected sensor',
    'This column shows whether the mitigation of the anomalies using the cases and rules from the previous column were successful for the selected sensor',
    'This column shows a comment from a worker to a single incident for the selected sensor'
    ],
    header = table.columns().header();
    for (var i = 0; i < tips.length; i++) {$(header[i]).attr('title', tips[i]);}
                  ")))%>%
      formatStyle(
        "critTab2",
        backgroundColor = styleEqual(c("Neglegtable", 
                                       "Relevant", 
                                       "Critical", 
                                       "Severe"),
                                     c("#ADEDC1", 
                                       "#FFFFA3", 
                                       "#FFE48F",
                                       "#FF9393")))
      })

  observe({
    replaceData(
      dataTableProxy("occtable"),
      myData2(),
      resetPaging = FALSE,
      clearSelection = FALSE
    )
  })
  rules_table_data_input <- reactive({
    read.csv("rulestable.csv", stringsAsFactors=FALSE)
  })
  shinyInput <- function(FUN, len, id, ...) {
                 inputs <- character(len)
                 for (i in seq_len(len)) {
                       inputs[i] <- as.character(FUN(paste0(id, 1), ...))
                   }
                 inputs
  }
  output$ruletable = DT::renderDataTable({
    
    base_table_data_get = rules_table_data_input()
    table_data = data.frame(
      ID = base_table_data_get$ID,
      Priority = base_table_data_get$Priority,
      "CPS involved" = base_table_data_get$CPS_involved,
      Description = base_table_data_get$Description,
      "Success rate in %" = base_table_data_get$Success_rate,
      Source = base_table_data_get$Source,
      Comment = base_table_data_get$Comment,
      stringsAsFactors = FALSE, 
      check.names = FALSE
    )
    DT::datatable(
      table_data,
      options = list( 
        pageLength = 5,
        lengthMenu = c(5,10,15,20),
        processing = FALSE, 
        dom = 't'),
      rownames = FALSE,
      colnames = c("ID",
                   "Priority",
                   "CPS involved",
                   "Description",
                   "Success rate [%]",
                   "Source",
                   "Comment"),
                   callback = JS("
                   var tips = [
                  'This column shows the rules identification number',
                  'This column shows the rules priority assigned by a worker',
                  'This column shows to which CPS the rule applies',
                  'This column shows a rule description assigned by a worker',
                  'This column shows the rules success rate in % over all rule applications so far',
                  'This column shows whether the rule was created by a machine recommender or by a worker',
                  'This column shows a comment from a worker to the rule'
                  ],
                  header = table.columns().header();
                  for (var i = 0; i < tips.length; i++) {$(header[i]).attr('title', tips[i]);}
                                ")
    )})
  observeEvent(input$addrule, {
    showModal(modalDialog(
      title = "Add a comment",
      "Please enter your comment.",
      textInput("dataset", "Enter ID of rule",
                placeholder = 'Enter rule id'
      ),
      textInput("dataset1", "Enter comment here",
                placeholder = 'Enter comment'
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        modalButton("Submit"),
      )
    ))
  })
  observeEvent(input$addcase, {
    showModal(modalDialog(
      title = "Add a comment",
      "Please enter your comment.",
      textInput("dataset", "Enter ID of case",
                placeholder = 'Enter rule id'
      ),
      textInput("dataset1", "Enter comment here",
                placeholder = 'Enter comment'
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        modalButton("Submit"),
      )
    ))
  })
  observeEvent(input$ruletable_cell_edit, {
    info = input$ruletable_cell_edit
    x <- rules_table_data_input()
    x[info$row, info$col] <- info$value
    new_df <- data.frame(
      id = x$id,
      priority = x$priority,
      cps_involved = x$cps_involved,
      description = x$description,
      success_rate = x$success_rate,
      source = x$source,
      comment = x$comment
    )
    write.csv(new_df, "rulestable.csv", row.names = FALSE)
  })
  
  cases_table_data_input <- reactive({
    invalidateLater(50000)
    base_cases_table_data_get <- read.csv("casestable.csv", stringsAsFactors=FALSE)
    cases_table_data <- data.frame(
      ID = base_cases_table_data_get$ID,
      Priority = base_cases_table_data_get$Priority,
      "CPS involved" = base_cases_table_data_get$CPS_involved,
      Description = base_cases_table_data_get$Description,
      "Success rate in %" = base_cases_table_data_get$Success_rate,
      Source = base_cases_table_data_get$Source,
      Comment = base_cases_table_data_get$Comment,
      stringsAsFactors = FALSE
    )
  })
  output$casetable = DT::renderDT(
      server = TRUE,
      expr = {
        isolate(cases_table_data_input())
      },
      options = list( 
        pageLength = 5,
        lengthMenu = c(5,10,15,20),
        processing = FALSE, 
        dom = 't'
        ),
        rownames = FALSE,
      colnames = c("ID",
                   "Priority",
                   "CPS involved",
                   "Description",
                   "Success rate [%]",
                   "Source",
                   "Comment"),
      callback = JS("
      var tips = [
      'This column shows the case identification number',
      'This column shows the case priority assigned by a worker',
      'This column shows to which CPS the case applies',
      'This column shows a case description assigned by a worker',
      'This column shows the case success rate in % over all case applications so far',
      'This column shows whether the case was created by a machine recommender or by a worker',
      'This column shows a comment from a worker to the case'
      ],
      header = table.columns().header();
      for (var i = 0; i < tips.length; i++) {$(header[i]).attr('title', tips[i]);}
                    ")
  )
  
  configData = reactive({read.csv("configtable.csv", 
                        sep = ",", 
                        stringsAsFactors = FALSE)})

  output$configtable <- DT::renderDT(
    expr = {
      isolate(configData())
      },
    server = TRUE,
    options = list( 
      paging = TRUE, 
      searching = TRUE, 
      lengthMenu = c(5,10,15,20), 
      dom = 't'),
    rownames = FALSE, 
    colnames = c("Production process order", 
                 "Machine", 
                 "Sensor(s) attached", 
                 "Lower threshold", 
                 "Upper threshold"),
                 callback = JS("
      var tips = [
      'This column shows the order of production steps',
      'This column shows the CPS involved in the production steps',
      'This column shows the sensors applied to CPS involved in the production steps', 
      'This column shows the lower threshold values',
      'This column shows the upper threshold values'
      ],
      header = table.columns().header();
      for (var i = 0; i < tips.length; i++) {$(header[i]).attr('title', tips[i]);}
                    ")
  )
  cases_proxy <- dataTableProxy(outputId = "casetable")
  observeEvent(eventExpr = input$addcase, {
    cases_proxy %>% addRow(cases_table_data)
  })
  
  output$download <- downloadHandler(
    filename = function() {"Data_History.csv"}, 
    content = function(file) {write.csv(downloadData, file)}
    )
}