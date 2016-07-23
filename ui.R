library(shiny)
library(shinydashboard)
jumps <- readRDS('jump.rds')
sectorChoices <- unique(jumps$sector)
names(sectorChoices) <- sectorChoices
orderParam <- c(
  "空绝对值均值" = 'mean_abs_value',
  "最大跳空极值" = 'max_abs_value'
 )
shinyUI(dashboardPage(
  dashboardHeader(title = "Long Festival Effect Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Festival Price Jump Detail',
               menuSubItem('National Day', tabName ='national_day_jump'),
               menuSubItem('Spring Festival', tabName = 'spring_festival_jump')
               ),
      menuItem("Main Contract Detail",
               menuSubItem('National Day', tabName = 'nation_day_contract'),
               menuSubItem("Spring Festival", tabName = 'spring_festival_contract')
               ),
      menuItem("Contract Jumps K-line", tabName = "k_line")
      #menuSubItem()
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "national_day_jump",
        fluidRow(
          box(
            width = 12, solidHeader = T,
            title = 'National day future price jump summary(box/bar graph)',
            plotOutput('national_day_jump_bar_plot')
          ),
          box(width = 8, height = 1000, solidHeader = T, 
              title = 'detail data',
              plotOutput('national_day_detail_data', height = 800)
              ),
          box(
            width = 4, solidHeader = T,
            title = 'data selection:',
            selectInput(
              'national_day_sector',
              'Choose one or more sector:',
              choices = sectorChoices,
              selected =  sectorChoices, 
              multiple = T
            ),
            selectInput(
              'national_day_jump_order_param',
              "Choose order parameter:",
              choices = orderParam,
              selected =  orderParam[2L],
              multiple = F
            ),
            selectInput(
              'national_day_jump_group_param',
              'Choose group parameter:',
              choices = c(
                '品种' = 'syb',
                '行业' = 'sector',
                '不聚集' = F
              ),
              selected = 'sector',
              multiple = F
            ),
            sliderInput(
              "national_data_jump_data_row_show",
              'choose number of rows to show:',
              min = 1, 
              max = 200,
              step = 1,
              value = 20
            )
          )
        )
      ),
      tabItem(
        "spring_festival_jump",
        fluidRow(
          box(
            width = 12, solidHeader = T,
            title = 'Spring festival future price jump summary(box/bar graph)',
            plotOutput('spring_festival_jump_bar_plot')
          ),
          box(width = 8, height = 1000, solidHeader = T, 
              title = 'detail data',
              plotOutput('spring_festival_detail_data', height = 800)
          ),
          box(
            width = 4, solidHeader = T,
            title = 'data selection:',
            selectInput(
              'spring_festival_sector',
              'Choose one or more sector:',
              choices = sectorChoices,
              selected =  sectorChoices, 
              multiple = T
            ),
            selectInput(
              'spring_festival_jump_order_param',
              "Choose order parameter:",
              choices = orderParam,
              selected =orderParam[2],
              multiple = F
            ),
            selectInput(
              'spring_festival_jump_group_param',
              'Choose group parameter:',
              choices = c(
                '品种' = 'syb',
                '行业' = 'sector',
                '不聚集' = F
              ),
              selected = 'sector',
              multiple = F
            ),
            sliderInput(
              "spring_festival_jump_data_row_show",
              'choose number of rows to show:',
              min = 1, 
              max = 200,
              step = 1,
              value = 20
            )
          )
        )
      ),
      tabItem(
        'nation_day_contract',
        fluidRow(
          box(
            width = 8, solidHeader = T, height = 600,
            title = 'Future contract trading volume(2 days before and after)',
            plotOutput('national_day_contract_trade_plot')
          ),
          box(
            width = 4, solidHeader = T,
            title = 'Selective parameter:',
            selectInput(
              'national_day_trade_sector_select',
              'Select the contract Sector:',
              c(
                "Agriculture" = "Agriculture",
                "Precious"= "Precious",
                "Metal" =  "Metal",
                "Industry" ="Industry" ,
                "Mineral" =  "Mineral",
                "Stock Index" = "StockIndex",
                "Debt Index" = "DebtIndex" 
              ),
              selected = 'Precious',
              multiple = F
            ),
            uiOutput(
              'national_day_trade_syb_select'
            )
          )
        )
      ),
      tabItem(
        'spring_festival_contract',
        fluidRow(
          box(
            width = 8, solidHeader = T, height = 600,
            title = 'Future contract trading volume(2 days before and after)',
            plotOutput('spring_festival_contract_trade_plot')
          ),
          box(
            width = 4, solidHeader = T,
            title = 'Selective parameter:',
            selectInput(
              'spring_festival_trade_sector_select',
              'Select the contract Sector:',
              c(
                "Agriculture" = "Agriculture",
                "Precious"= "Precious",
                "Metal" =  "Metal",
                "Industry" ="Industry" ,
                "Mineral" =  "Mineral",
                "Stock Index" = "StockIndex",
                "Debt Index" = "DebtIndex" 
              ),
              selected = 'Precious',
              multiple = F
            ),
            uiOutput(
              'spring_festival_trade_syb_select'
            )
          )
        )
      ),
      tabItem(
        'k_line',
        fluidRow(
          box(
            width = 8, solidHeader = T,
            title = '5 minutes k line',
            plotOutput('k_line_output')
          ),
          box(
            width = 4, solidHeader = T,
            title = 'Contract to select:',
            selectInput('k_line_contract_holiday',
                        'Select the holiday:',
                        c(
                          'National Day' = 'National Day',
                          'Spring Festival' = 'Spring Festival'
                          ),
                        selected = 'national_day',
                        multiple = F
                        ),
            selectInput(
              'k_line_contract_sector',
              'Select the contract Sector:',
              c(
                "Agriculture" = "Agriculture",
                "Precious"= "Precious",
                "Metal" =  "Metal",
                "Industry" ="Industry" ,
                "Mineral" =  "Mineral",
                "Stock Index" = "StockIndex",
                "Debt Index" = "DebtIndex" 
              ),
              selected = 'Precious',
              multiple = F
            ),
            uiOutput('k_line_syb_select'),

            uiOutput('k_line_contract_select')
          )
        )
      )
    )
  )
))
      
#              fluidRow(
#                box(
#                  width = 12, solidHeader = T,
#                  title = 'Contract Jump Graphs',
#                  plotOutput('jumps_rank_bar')
#                )
#             ),
#             fluidRow(
#                box(
#                  width = 8, status = "info", solidHeader = TRUE,
#                  title = "detail Data",
#                  plotOutput('jumps_detail_mat_plot')
#                ),
#                box(
#                  width = 4, status = "info",
#                  title = "Top packages (last 5 min)",
#                 tableOutput("packageTable")
#                )
#              )
#     ),
#      tabItem("rawdata",
#              numericInput("maxrows", "Rows to show", 25),
#      )
#    )
#  )
#))

#library('plotrix')
#test <- matrix(rnorm(12))
#
#rownames(test) <- test[,1]
#par(mar = c(0.5, 8, 3.5, 0.5))
#color2D.matplot(test, 
#                show.values = TRUE,
#                axes = FALSE,
#                xlab = "",
#                ylab = "",
#                vcex = 1,
#                vcol = "black",
#                extremes = c("green", "red"))
#axis(2, at = seq_len(nrow(test)) - 0.5,
#     labels = rev(rownames(test)), tick = FALSE, las = 1, cex.axis = 1)

























