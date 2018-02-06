library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
require(shinysky)
library(rCharts)
source("data_helper.R")
# options(
#   rcharts.mode = 'iframesrc', 
#   rcharts.cdn = TRUE
# )
jscode <-'$(document).on("shiny:connected", function(e) {
                                    var jsWidth =  window.innerWidth;
          Shiny.onInputChange("GetScreenWidth",jsWidth);
          });
          $(window).resize(function(e) {
          var jsWidth =  window.innerWidth;
          Shiny.onInputChange("GetScreenWidth",jsWidth);
          });'
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Отчет"),
    dashboardSidebar(sidebarMenu(id='sidebar', 
                                 menuItem("Отчет за сезон" ,tabName="SEASON_REPORT"),
                                 menuItem("Оперативный отчет",tabName='OPER_REPORT'),
                                menuItem("Настройка",tabName='CONFIG')
                                 
                                 
    )),
    dashboardBody(title = "Отчет",
      tags$script(jscode),
      tags$head(tags$style(".rChart {width: 100% !important; height: 400px} ")),
      useShinyjs(),
      tabItems(
        tabItem('SEASON_REPORT',wellPanel(fluidRow( selectInput("S_P_YEAR","Выберите сезон",choices = ""))),
                                fluidRow(
                                 column(width=12,
                                        box(width = 12,title =  "Динамика",
                                                   showOutput("PLOT_P_DIN", "nvd3")
                                            )
                                        )
                                 ),
                               fluidRow(
                                 column(width=8,  
                                        
                                                 box(width = 12,title = "Структура",
                                                   fluidRow(
                                                     column(width=6 ,selectInput("S_P_STRUC_VAR","Разрез",choices = c("Месяц","Категория"))),
                                                     column(width=6 ,selectInput("S_P_STRUC_TYPE","Тип",choices = c("Продажа","Покупка","Прибыль"))))
                                                     , showOutput("PLOT_P_STRUCT", "nvd3") 
                                                     ,br()
                                                 ) 
                                 
                                 ),
                                column(width=4,
                                        box(width = 12,title = "Сводная информация",
                                            dataTableOutput("desc_table")
                                            )
                                        )
                                 )
                               ),
        tabItem('OPER_REPORT',
                wellPanel(h3("Настройка"),
                          dateRangeInput("OPER_DATES","Период",language='ru',separator = " - "),
                          selectInput("OPER_FILTERS","Фильтры",choices = "",multiple = T),
                          wellPanel(id="OPER_FILTERS_UI")),
                tabsetPanel(id="OPER_REPORT_TABS",
                            tabPanel("Графики",value = "OPER_GRAPH",
                                     wellPanel(
                                       selectInput("GRAPH_TYPE",label = "Тип графика",choices=c("Круг"="Pie","Столбцы"="Bar","Линия"="Line"), multiple =F,selected="Pie"),
                                       selectInput("GRAPH_X",label = "Ось Х",choices="", multiple =F,selected=""),
                                       wellPanel(div(id="Y_CORD",selectInput("GRAPH_Y_",label = "Ось Y",choices="", multiple =F,selected="")),
                                                 actionButton("ADD_Y_CORD","Добавить еще")
                                                 ),
                                       selectInput("GRAPH_GROUP",label = "Группировка",choices="", multiple =F,selected="")
                                     ),
                                     plotOutput("OPER_PLOT")),
                            tabPanel("Таблица",value = "OPER_TABLE",dataTableOutput("OPER_DT")),
                            tabPanel("Сводная",value = "OPER_PIVOT",
                                     wellPanel(
                                       wellPanel(
                                         selectInput("S_T_AXIS_Y",label = "Строки",choices="", multiple =T,selected=""),
                                         uiOutput('S_T_AXIS_Y_sub'),
                                         selectInput("S_T_AXIS_X",label = "Стоблцы",choices="", multiple =T,selected=""),
                                         uiOutput('S_T_AXIS_X_sub')
                                       ),
                                       selectInput("S_T_VAR",label = "Переменные",choices="", multiple =T)
                                       ,uiOutput('S_T_VAR_UI'),
                                       actionButton("SAVE_SEASON_DT","Сохранить")
                                     )
                                     ,
                                     dataTableOutput("SEASON_DT"))
                            )
                ),
        tabItem('CONFIG',
                shinyalert(id='msg_db', click.hide = TRUE, auto.close.after = 2000),
                box(title = "База данных",width = 12,
                    textInput(inputId = "db_path",label = "Путь к используемой базе",value = "s"),
                    textInput(inputId = "db_pass",label = "Пароль от базы",value = "Microinvest6380"),
                    actionButton("save_path","Сохранить")))
      )

    )
    )
)
