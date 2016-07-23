library(shiny)
library(shinydashboard)

if(!require(plotrix)){
  install.packages('plotrix')
}
if(!require(data.table)){
  install.packages('data.table')
}
if(!require(lattice)){
  install.packages('lattice')
}
library(data.table)
library(plotrix)
library(lattice)
jumps <- readRDS('jump.rds')
jumpDetail <- readRDS("jumpdetail.rds")
tradeVolume <- readRDS('tradevolume.rds')
contractInfo <- fread('allfuture.csv')
contractInfo <- contractInfo[,.(name, startStr)]
setorderv(tradeVolume, c('year', 'code'))

FSSCode <- jumpDetail[,.(festival, sector, syb, code)]
setDF(FSSCode)
FSSCode <- FSSCode[!duplicated(FSSCode),] 
setDT(FSSCode)

kPlot <- function(open, high, low, close, xaxis, main = 'test'){
  N <- length(open)
  w <- 0.3
  d <- close - open
  centreX <- which(xaxis == 0)
  beforeClose <- close[centreX - 1]
  afterOpen <- open[centreX]
  jumps = (afterOpen - beforeClose)/beforeClose * 100
  jumps = round(jumps, digits = 2)
  xlabValue <- paste0('time',"(price chages: ", jumps, "%)" )
  plot(1:N, close, type = 'n', xaxt='n', xlab=xlabValue, ylab = 'price')
  title(main = main)
  for(i in 1:N){
    lines(c(i,i), c(low[i], high[i]), col = 'black',ylim = c(min(low)**0.98, max(high)*1.02),lwd=1)
    x <- c(i-w, i-w, i+w, i+w)
    y <- c(open[i], close[i], close[i], open[i])
    if(d[i] < 0){
      polygon(x,y, col = 'green', border = 'green')
    }
    else{
      polygon(x, y, col= 'red', border = 'red')
    }
  }
  
  abline(v = centreX - 0.5, lwd = 3 )
  xaxis[xaxis >= 0] = xaxis[xaxis >= 0] + 1
  axis(side=1, 1:N, xaxis)
}



shinyServer(function(input, output, session) {
  #k_line_output$IF1510.CFE <- plotOutput(kline_selected[1L])

  output$k_line_syb_select <- renderUI({
    #holiday <- input$k_line_contract_holiday
    sector <- input$k_line_contract_sector
    rawChoice = contractInfo[startStr %in% unique(FSSCode$syb[FSSCode$sector == sector])]
    choice <- rawChoice$startStr
    names(choice) <- rawChoice$name
    selectInput(
      'k_line_syb',
      'Choose a contract symbol:',
      choice = choice,
      selected = choice[1L],
      multiple = F
    )
  })
  
  output$k_line_contract_select <- renderUI({
    hday <- input$k_line_contract_holiday
    symbol <- input$k_line_syb
    #print(symbol)
    #print(hday)
    codes <<- FSSCode[festival == hday & syb == symbol]$code
    selectedValue <- c()
    if(length(codes)){
      selectedValue <- codes[1]
    }
    selectInput(
      'k_line_contract',
      'Choose a contract:',
      choices = codes,
      multiple = F,
      selected = selectedValue
    )
  })
  output$k_line_output <- renderPlot({
    cd <- input$k_line_contract
    hday <- input$k_line_contract_holiday
    if(length(code)){
      tempData <- jumpDetail[festival == hday & code == cd]
      setorder(tempData, testPos)
      kPlot(tempData$open, tempData$high, tempData$low, tempData$close, tempData$testPos,
            paste(tempData$festival[1],tempData$sector[1], tempData$year[1], tempData$code[1], sep = "/") )
    }
  })
  
  
  observe({
    sectors <- input$national_day_sector
    orderMethod <- input$national_day_jump_order_param
    groupMethod <- input$national_day_jump_group_param
    setDT(jumps)
    tempData <- jumps[sector %in% sectors & festival == "National Day", .(code, syb, chg = round(chg, digits = 4),sector)]
    if(groupMethod != F){
      tempData <- tempData[,{
        if(orderMethod == 'mean_abs_value'){
          meanValue <- mean(abs(chg))
        }
        if(orderMethod == 'max_abs_value'){
          meanValue <- chg[which.max(abs(chg))]
        }
        .(
          meanValue = meanValue
        )
      }, by = groupMethod]
    }
    else{
      if(orderMethod == 'mean_abs_value'){
        tempData <- tempData[,.(
         groupKey = code, 
         meanValue = abs(chg)
        )]
      }
      else if(orderMethod == 'max_abs_value'){
        tempData <- tempData[,.(
          code,
          meanValue = chg
        )]
      }
    }
    names(tempData)[1] <- 'groupKey'
    #print(nrow(tempData))
    #print(head(tempData))
    setorder(tempData, - meanValue)
    updateSliderInput(
      session,
      'national_data_jump_data_row_show',
      min = 1,
      max = nrow(tempData)
    )
    numRowsToShow <- min(input$national_data_jump_data_row_show, nrow(tempData))
    absValue <- abs(tempData$meanValue)[order(abs(tempData$meanValue), decreasing = T)[numRowsToShow]]
    outputData <- tempData[abs(meanValue) >= absValue]
    test <- as.matrix(outputData$meanValue)
    rownames(test) <- outputData$groupKey
    output$national_day_detail_data <- renderPlot({
      par(mar= c(2,12,1,8))
      color2D.matplot(test, 
                      show.values = 4,
                      axes = FALSE,
                      xlab = "",
                      ylab = "",
                      vcex = 1,
                      vcol = "black",
                      extremes = c("green", "red"))
      axis(2, at = seq_len(nrow(test)) - 0.5,
           labels = rev(rownames(test)), tick = FALSE, las = 1, cex.axis = 1)
      
    })
    output$national_day_jump_bar_plot <- renderPlot({
      if(input$national_day_jump_group_param == F){
        name <- rownames(test)
        rownames(test) <- substr(rownames(test), 1, nchar(rownames(test)) - 4)
        barplot(test[,1],width =1, space = 1, las = 2, col =c('red', 'green')[2- (test> 0)])
        #print(length(test))
        #axis(1, labels = rownames(test), las = 2)
        
      }
      else{
        temp <- jumps[festival == "National Day"]
        setDF(temp)
        #print(head(temp))
        #print(groupMethod)
        bwplot(temp$chg ~ temp[[groupMethod]], ylab = 'jumps', panel = function(...){
          panel.grid(h = -1, v = 0)
          panel.bwplot(...)
        })
      }
    })
  })
  observe({
    sectors <- input$spring_festival_sector
    orderMethod <- input$spring_festival_jump_order_param
    groupMethod <- input$spring_festival_jump_group_param
    setDT(jumps)
    tempData <- jumps[sector %in% sectors & festival ==  "Spring Festival", .(code, syb, chg = round(chg, digits = 4),sector)]
    if(groupMethod != F){
      tempData <- tempData[,{
        if(orderMethod == 'mean_abs_value'){
          meanValue <- mean(abs(chg))
        }
        if(orderMethod == 'max_abs_value'){
          meanValue <- chg[which.max(abs(chg))]
        }
        .(
          meanValue = meanValue
        )
      }, by = groupMethod]
    }
    else{
      if(orderMethod == 'mean_abs_value'){
        tempData <- tempData[,.(
          groupKey = code, 
          meanValue = abs(chg)
        )]
      }
      else if(orderMethod == 'max_abs_value'){
        tempData <- tempData[,.(
          code,
          meanValue = chg
        )]
      }
    }
    names(tempData)[1] <- 'groupKey'
    #print(nrow(tempData))
    #print(head(tempData))
    setorder(tempData, - meanValue)
    updateSliderInput(
      session,
      'spring_festival_jump_data_row_show',
      min = 1,
      max = nrow(tempData)
    )
    numRowsToShow <- input$spring_festival_jump_data_row_show
    absValue <- abs(tempData$meanValue)[order(abs(tempData$meanValue), decreasing = T)[numRowsToShow]]
    outputData <- tempData[abs(meanValue) >= absValue]
    test <- as.matrix(outputData$meanValue)
    rownames(test) <- outputData$groupKey
    output$spring_festival_detail_data <- renderPlot({
      par(mar= c(2,12,1,8))
      color2D.matplot(test, 
                      show.values = 4,
                      axes = FALSE,
                      xlab = "",
                      ylab = "",
                      vcex = 1,
                      vcol = "black",
                      extremes = c("green", "red"))
      axis(2, at = seq_len(nrow(test)) - 0.5,
           labels = rev(rownames(test)), tick = FALSE, las = 1, cex.axis = 1)
      
    })
    output$spring_festival_jump_bar_plot <- renderPlot({
      if(input$spring_festival_jump_group_param == F){
        name <- rownames(test)
        rownames(test) <- substr(rownames(test), 1, nchar(rownames(test)) - 4)
        barplot(test[,1],width =1, space = 1, las = 2, col =c('red', 'green')[2- (test> 0)])
        #print(length(test))
        #axis(1, labels = rownames(test), las = 2)
        
      }
      else{
        temp <- jumps[festival ==  "Spring Festival"]
        setDF(temp)
        #print(head(temp))
        #print(groupMethod)
        bwplot(temp$chg ~ temp[[groupMethod]], ylab = 'jumps', panel = function(...){
          panel.grid(h = -1, v = 0)
          panel.bwplot(...)
          })
      }
    })
  })
  
  
  output$national_day_trade_syb_select <- renderUI({
    #holiday <- input$k_line_contract_holiday
    sector <- input$national_day_trade_sector_select
    #print(sector)
    rawChoice = contractInfo[startStr %in% unique(FSSCode$syb[FSSCode$sector == sector & FSSCode$festival == 'National Day'])]
    choice <- rawChoice$startStr
    names(choice) <- rawChoice$name
    selectInput(
      'national_day_trade_syb',
      'Choose a contract symbol:',
      choice = choice,
      selected = choice[1L],
      multiple = F
    )
  })
  
  observe({
    symbol <- input$national_day_trade_syb
    if(!is.null(symbol)){
      tempData <- tradeVolume[syb == symbol & festival == 'National Day']
      print(tradeVolume)
      tradeData <- tempData$volume
      col = tempData$year - 2012
      names(tradeData) <- substr(tempData$code, 1, nchar(tempData$code) - 4)
      output$national_day_contract_trade_plot <- renderPlot({
        print(tradeData)
        barplot(tradeData, las = 2, col = col, 
                legend.text = unique(col) + 2012, args.legend = list(fill = unique(col))
                )
      })
    }
  })
  output$spring_festival_trade_syb_select <- renderUI({
    #holiday <- input$k_line_contract_holiday
    sector <- input$spring_festival_trade_sector_select
    rawChoice = contractInfo[startStr %in% unique(FSSCode$syb[FSSCode$sector == sector & FSSCode$festival == 'Spring Festival'])]
    choice <- rawChoice$startStr
    names(choice) <- rawChoice$name
    selectInput(
      'spring_festival_trade_syb',
      'Choose a contract symbol:',
      choice = choice,
      selected = choice[1L],
      multiple = F
    )
  })
  observe({
    symbol <- input$spring_festival_trade_syb
    if(!is.null(symbol)){
      tempData <- tradeVolume[syb == symbol & festival == 'Spring Festival']
      tradeData <- tempData$volume
      col = tempData$year - 2012
      names(tradeData) <- substr(tempData$code, 1, nchar(tempData$code) - 4)
      
      output$spring_festival_contract_trade_plot <- renderPlot({
        barplot(tradeData, las = 2, col = col, 
                legend.text = unique(col) + 2012, args.legend = list(fill = unique(col)))
      })
    }
  })
})






