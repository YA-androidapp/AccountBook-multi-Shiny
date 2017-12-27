# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

reqPackage <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop('Package not found')
  }
}
reqPackage('colorspace')
reqPackage('plyr')
reqPackage('dplyr')
reqPackage('DT')
reqPackage('ggplot2')
reqPackage('lubridate')
reqPackage('reshape2')
reqPackage('shiny')
reqPackage('xts')

##############################

# 列名の編集
colsnames <- c('取引日', '受入金額', '払出金額', '詳細１', '詳細２', '現在高')
colsnames.ga <- c('取引年月', '入出金額', '詳細')
colsnames.gb <- c('取引日', '入出金額', '現在高')
colsnames.gd <- c('日', '詳細')

##############################

# 列の編集
colsort <- function(data.colsort, order) {
  data.colsort <- data.colsort[, order]
  colnames(data.colsort) <- colsnames

  data.colsort[, 1] <-
    ifelse(is.na(data.colsort[, 1]), '', data.colsort[, 1])
  data.colsort[, 2] <-
    ifelse(is.na(data.colsort[, 2]), 0,  data.colsort[, 2])
  data.colsort[, 3] <-
    ifelse(is.na(data.colsort[, 3]), 0,  data.colsort[, 3])
  data.colsort[, 4] <-
    ifelse(is.na(data.colsort[, 4]), '', data.colsort[, 4])
  data.colsort[, 5] <-
    ifelse(is.na(data.colsort[, 5]), '', data.colsort[, 5])
  data.colsort[, 6] <-
    ifelse(is.na(data.colsort[, 6]), 0,  data.colsort[, 6])

  data.colsort[, 1] <- gsub('/', '',  as.matrix(data.colsort[, 1]))
  data.colsort[, 1] <- gsub('-', '',  as.matrix(data.colsort[, 1]))
  data.colsort[, 1] <- gsub('年', '',  as.matrix(data.colsort[, 1]))
  data.colsort[, 1] <- gsub('月', '',  as.matrix(data.colsort[, 1]))
  data.colsort[, 1] <- gsub('日', '',  as.matrix(data.colsort[, 1]))

  data.colsort[, 2] <- gsub(',', '',  as.matrix(data.colsort[, 2]))
  data.colsort[, 3] <- gsub(',', '',  as.matrix(data.colsort[, 3]))
  data.colsort[, 6] <- gsub(',', '',  as.matrix(data.colsort[, 6]))

  data.colsort[, 2] <- gsub('円', '', as.matrix(data.colsort[, 2]))
  data.colsort[, 3] <- gsub('円', '', as.matrix(data.colsort[, 3]))
  data.colsort[, 6] <- gsub('円', '', as.matrix(data.colsort[, 6]))

  data.colsort[, 2] <- gsub(' ', '',  as.matrix(data.colsort[, 2]))
  data.colsort[, 3] <- gsub(' ', '',  as.matrix(data.colsort[, 3]))
  data.colsort[, 6] <- gsub(' ', '',  as.matrix(data.colsort[, 6]))


  data.colsort[, 1] <- as.character(as.matrix(data.colsort[, 1]))
  data.colsort[, 2] <- as.numeric(as.matrix(data.colsort[, 2]))
  data.colsort[, 3] <- as.numeric(as.matrix(data.colsort[, 3]))
  data.colsort[, 4] <- as.character(as.matrix(data.colsort[, 4]))
  data.colsort[, 5] <- as.character(as.matrix(data.colsort[, 5]))
  data.colsort[, 6] <- as.numeric(as.matrix(data.colsort[, 6]))

  return(data.colsort)
}

# 日付文字列の書式
checkDateFormat <- function(dateStr) {
  fmt <- '%Y%m%d'
  if(nchar(dateStr) == 6){
   fmt <- '%Y%m%d'
  } else if (grepl('/', dateStr) && nchar(dateStr) == 8) {
    fmt <- '%Y/%m/%d'
  } else if (grepl('年', dateStr) && nchar(dateStr) == 11) {
    fmt <- '%Y年%m月%d日'
  }
  return(fmt)
}

# 行の編集
rowfilter <- function(data.rowfilter, dateRange) {
  Sys.setlocale('LC_TIME', 'C')
  data.rowfilter.char <- as.character(data.rowfilter[, 1])
  data.rowfilter[, 1] <-
    as.Date(data.rowfilter.char, format = '%Y-%m-%d')

  cond <-
    ((as.Date(data.rowfilter$取引日) >= dateRange[1]) &
       (as.Date(data.rowfilter$取引日) <= dateRange[2]))
  data.rowfilter <- data.rowfilter[cond, ]
  return(data.rowfilter)
}

readAndSort <- function(input) {

  banks <- NULL
  for(file_i in 1:length(input$files[,1])){
    fpath <- input$files[[file_i, 'datapath']]

    if (endsWith(fpath, '.csv')) {
      # ヘッダ行数検出
      f <- file(fpath, 'r', encoding = ifelse(grepl('Rireki_', fpath),'UTF-8','Shift_JIS'))
      i <- 0
      j <- 0
      s <- 0
      repeat {
        str <- readLines(con = f, 1)
        if (length(unlist(strsplit(str, ','))) > 2) {
          j <- j + 1
          if (j > 3) {
            # ヘッダ行1 + データ行3
            s <- i - j + 1
            break
          }
        } else {
          j <- 0
        }
        i <- i + 1
      }
      close(f)

      df.csv <- read.csv(
        fpath,
        header = T,
        na.strings = '',
        skip = s,
        stringsAsFactors = F,
        fileEncoding = ifelse(grepl('Rireki_', fpath),'utf8','cp932')
      )

      fo <- c()
      if (colnames(df.csv)[4] == 'お預け入れ額') {
        fo <- c(1, 4, 5, 2, 3, 6)
        df.csv[is.na(df.csv)] <- 0
      } else if (colnames(df.csv)[3] == '受入金額.円.') {
        df.csv[1, 3] <- df.csv[1, 3] + df.csv[1, 7]
        fo <- c(1, 3, 4, 5, 6, 7)
      } else if (colnames(df.csv)[2] == '契約番号') {
        df.csv.min <- df.csv %>%
          group_by(契約番号) %>%
          filter(お取り引き日 == min(お取り引き日)) %>%
          select(契約番号, お取り引き日)
        df.csv.max <- df.csv %>%
          group_by(契約番号) %>%
          filter(お取り引き日 == max(お取り引き日)) %>%
          filter(差し引き残高 != 0) %>%
          select(契約番号, 差し引き残高, 商品名, 摘要)


        df.csv <- right_join(df.csv.min, df.csv.max, by = "契約番号")
        df.csv <- within(df.csv, emp<-0)
        df.csv <- within(df.csv, cum_sum<-cumsum(as.integer(gsub(',', '', df.csv$差し引き残高))))

        fo <- c(2, 3, 6, 4, 5, 7)
      }

      bank <- colsort(df.csv, fo)

      print('bank2')
      print(bank)

      if(is.null(banks)){
        banks <- bank
      } else {
        banks <- union_all(banks, bank)
      }
    }
  }

  banks.grouped <- banks %>%
      dplyr::group_by(取引日) %>%
      dplyr::summarise(受入金額=sum(受入金額) , 払出金額=-1*sum(払出金額))
  banks.grouped <- as.data.frame(banks.grouped)

  banks.grouped[,1] <- as.Date(as.character(banks.grouped$取引日), format=checkDateFormat(as.character(banks.grouped$取引日)))
  banks.grouped[,2] <- apply(banks.grouped[,2:3], 1, sum)
  banks.grouped[,3] <- cumsum(banks.grouped[,2])
  colnames(banks.grouped) <- colsnames.gb

  # 年月日テーブルの生成
  df.m <- data.frame(seq(from=min(banks.grouped$取引日),to=today(),by="days"))
  colnames(df.m) <- c('取引日')
  df.m <- left_join(df.m, banks.grouped, by='取引日')
  df.m[is.na(df.m)] <- 0
  df.m[,3] <- cumsum(df.m[,2])

  return(df.m)
}

##############################

# Define UI for application that draws a histogram
ui <- fluidPage(# App title ----
                titlePanel('AccountBook'),

                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput(
                      'files',
                      'Choose CSV File',
                      multiple = TRUE,
                      accept = c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')
                    ),

                    # Input: Choose start date and end date
                    dateRangeInput(
                      'dateRange',
                      label = 'Date range input: yyyy-mm-dd',
                      start = Sys.Date() + years(-10) ,
                      end = Sys.Date()
                    ),

                    # Horizontal line ----
                    tags$hr(),

                    # Input: Select number of rows to display ----
                    radioButtons(
                      'disp',
                      'Display',
                      choices = c(
                        All = 'all',
                        Head = 'head',
                        Tail = 'tail'
                      ),
                      selected = 'all'
                    )

                  ),

                  # Main panel for displaying outputs ----
                  mainPanel(
                    # Output: Data file ----
                    #tableOutput('contents')
                    DT::dataTableOutput('table_d'),

                    plotOutput(
                      outputId = 'plot_d',
                      width = '800px',
                      height = '400px'
                    )
                  )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table_d <- DT::renderDataTable(DT::datatable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$files)
    df.sorted <- readAndSort(input)

    if (mode(df.sorted[, 3]) == 'numeric') {
      df.filtered <- rowfilter(df.sorted, input$dateRange)

      if (input$disp == 'head') {
        data <- (head(df.filtered))
      } else if (input$disp == 'tail') {
        data <- (tail(df.filtered))
      } else {
        data <- df.filtered
      }
    }
  },
  options = list(paging = FALSE)))

  output$plot_d <- renderPlot({
    req(input$files)
    df.sorted <- readAndSort(input)

    if (mode(df.sorted[, 3]) == 'numeric') {
      df.filtered <- rowfilter(df.sorted, input$dateRange)
      df.filtered.2 <- df.filtered[, c(1, 3)]
      df.filtered.2[, 2] <- as.numeric(df.filtered.2[, 2])

      df.filtered.2 <-
        df.filtered.2 %>% group_by(df.filtered.2$取引日) %>%
        arrange(df.filtered.2$現在高) %>%
        filter(row_number() == 1)
      df.xts <<- xts(df.filtered.2[,c('現在高')], as.POSIXct(df.filtered.2$取引日))
      storage.mode(df.xts$現在高) <- "numeric"
      df.xts$現在高 <- as.numeric(df.xts$現在高)

      if (.Platform$OS.type!="unix")
        windowsFonts(gothic = windowsFont('MS Gothic'))
      par(family = 'gothic')

      g <- ggplot(df.xts,
                  aes(x =   Index,
                      y =   現在高,
                      group = 1))
      g <- g + geom_line(colour = 'red',
                         linetype = 1,
                         size = 0.5)
      g <- g + geom_smooth (method = 'lm')
      g <- g + xlab('Date')
      g <- g + ylab('Amount')
      g <- g + ggtitle('AccountBook')
      plot(g)

    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
