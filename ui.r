library(shiny)
library(ggplot2)

options(warn=-1)

ui <- fluidPage(
  #shinythemes::themeSelector(),
  titlePanel("S3 Cost Calculator"),
  
  tabsetPanel(type="tab",
              
              tabPanel("Standard",br(),
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           fluidRow(column(12, h4("Total Amount of Storage") )),
                           fluidRow(
                             column(5,
                                    textInput("stdStorage", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("stdUnit1", NULL,
                                                choices = c("GB","TB","PB"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Total Number of Objects") )),
                           fluidRow(
                             column(5,
                                    textInput("stdObj", NULL, width="225px",value=0)
                             )
                           ),
                           fluidRow(column(12, h4("Data Retrieval Amount") )),
                           fluidRow(
                             column(5,
                                    textInput("stdRetrieval", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("stdUnit2", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Storage Growth") )),
                           fluidRow(
                             column(5,
                                    textInput("stdGrowth", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("stdUnit3", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(8,align="center",h4("Months to View") )),
                           fluidRow(
                             column(8,align="center",
                                    sliderInput('stdMonths',NULL,min=3,max=36,value=3)
                             )
                           ),
                           fluidRow(column(12, h4("Copy Inputs Into:") )),
                           fluidRow(
                             column(5, selectInput("copySelect1", NULL,
                                                   choices = c("All","Infrequent Access","One Zone-IA","Glacier","Glacier-Deep Archive"),width="225px")
                             ),
                             column(6, actionButton("copy1","Copy",width="65px")
                             )
                             
                           ),
                           actionButton("clr1","Clear")
                         ),
                         mainPanel(
                           plotOutput("stdPlot",width="80%"),br(),
                           tableOutput("stdTable")
                         )
                       )
              ),
              
              tabPanel("Infrequent Access",br(),
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           fluidRow(column(12, h4("Total Amount of Storage") )),
                           fluidRow(
                             column(5,
                                    textInput("IAStorage", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("IAUnit1", NULL,
                                                choices = c("GB","TB","PB"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Total Number of Objects") )),
                           fluidRow(
                             column(5,
                                    textInput("IAObj", NULL, width="225px",value=0)
                             )
                           ),
                           fluidRow(column(12, h4("Data Retrieval Amount") )),
                           fluidRow(
                             column(5,
                                    textInput("IARetrieval", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("IAUnit2", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Storage Growth") )),
                           fluidRow(
                             column(5,
                                    textInput("IAGrowth", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("IAUnit3", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(8,align="center",h4("Months to View") )),
                           fluidRow(
                             column(8,align="center",
                                    sliderInput('IAMonths',NULL,min=3,max=36,value=3)
                             )
                           ),
                           fluidRow(column(12, h4("Copy Inputs Into:") )),
                           fluidRow(
                             column(5, selectInput("copySelect2", NULL,
                                                   choices = c("All","Standard","One Zone-IA","Glacier","Glacier-Deep Archive"),width="225px")
                             ),
                             column(6, actionButton("copy2","Copy",width="65px")
                             )
                             
                           ),
                           actionButton("clr2","Clear")
                         ),
                         mainPanel(
                           plotOutput("IAPlot",width="80%"),br(),
                           tableOutput("IATable")
                         )
                       )
              ),
              tabPanel("One Zone-IA",br(),
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           fluidRow(column(12, h4("Total Amount of Storage") )),
                           fluidRow(
                             column(5,
                                    textInput("OIAStorage", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("OIAUnit1", NULL,
                                                choices = c("GB","TB","PB"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Total Number of Objects") )),
                           fluidRow(
                             column(5,
                                    textInput("OIAObj", NULL, width="225px",value=0)
                             )
                           ),
                           fluidRow(column(12, h4("Data Retrieval Amount") )),
                           fluidRow(
                             column(5,
                                    textInput("OIARetrieval", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("OIAUnit2", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Storage Growth") )),
                           fluidRow(
                             column(5,
                                    textInput("OIAGrowth", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("OIAUnit3", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(8,align="center",h4("Months to View") )),
                           fluidRow(
                             column(8,align="center",
                                    sliderInput('OIAMonths',NULL,min=3,max=36,value=3)
                             )
                           ),
                           fluidRow(column(12, h4("Copy Inputs Into:") )),
                           fluidRow(
                             column(5, selectInput("copySelect3", NULL,
                                                   choices = c("All","Standard","Infrequent Access","Glacier","Glacier-Deep Archive"),width="225px")
                             ),
                             column(6, actionButton("copy3","Copy",width="65px")
                             )
                             
                           ),
                           actionButton("clr3","Clear")
                         ),
                         mainPanel(
                           plotOutput("OIAPlot",width="80%"),br(),
                           tableOutput("OIATable")
                         )
                       )
              ),
              tabPanel("Glacier",br(),
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           fluidRow(column(12, h4("Total Amount of Storage") )),
                           fluidRow(
                             column(5,
                                    textInput("glacierStorage", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("glacierUnit1", NULL,
                                                choices = c("GB","TB","PB"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Total Number of Objects") )),
                           fluidRow(
                             column(5,
                                    textInput("glacierObj", NULL, width="225px",value=0)
                             )
                           ),
                           fluidRow(column(12, h4("Data Retrieval Amount") )),
                           fluidRow(
                             column(5,
                                    textInput("glacierRetrieval", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("glacierUnit2", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Retrieval Speed") )),
                           fluidRow(
                             column(7,
                                    selectInput("glacierSpeed", NULL,
                                                choices = c("Expedited","Standard","Bulk"), selected = "Standard",width="225px")
                             )
                           ),
                           fluidRow(column(12, h4("Storage Growth") )),
                           fluidRow(
                             column(5,
                                    textInput("glacierGrowth", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("glacierUnit3", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(8,align="center",h4("Months to View") )),
                           fluidRow(
                             column(8,align="center",
                                    sliderInput('glacierMonths',NULL,min=3,max=36,value=3)
                             )
                           ),
                           fluidRow(column(12, h4("Copy Inputs Into:") )),
                           fluidRow(
                             column(5, selectInput("copySelect4", NULL,
                                                   choices = c("All","Standard","Infrequent Access","One Zone-IA","Glacier-Deep Archive"),width="225px")
                             ),
                             column(6, actionButton("copy4","Copy",width="65px")
                             )
                             
                           ),
                           actionButton("clr4","Clear")
                         ),
                         mainPanel(
                           plotOutput("glacierPlot",width="80%"),br(),
                           tableOutput("glacierTable")
                         )
                       )
              ),
              tabPanel("Glacier-Deep Archive",br(),
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           fluidRow(column(12, h4("Total Amount of Storage") )),
                           fluidRow(
                             column(5,
                                    textInput("DAStorage", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("DAUnit1", NULL,
                                                choices = c("GB","TB","PB"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Total Number of Objects") )),
                           fluidRow(
                             column(5,
                                    textInput("DAObj", NULL, width="225px",value=0)
                             )
                           ),
                           fluidRow(column(12, h4("Data Retrieval Amount") )),
                           fluidRow(
                             column(5,
                                    textInput("DARetrieval", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("DAUnit2", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(12, h4("Retrieval Speed") )),
                           fluidRow(
                             column(7,
                                    selectInput("DASpeed", NULL,
                                                choices = c("Standard","Bulk"), selected = "Standard",width="225px")
                             )
                           ),
                           fluidRow(column(12, h4("Storage Growth") )),
                           fluidRow(
                             column(5,
                                    textInput("DAGrowth", NULL,width="225px",value=0)
                             ),
                             column(6,
                                    selectInput("DAUnit3", NULL,
                                                choices = c("GB","TB","PB","%"), selected = "GB",width="65px")
                             )
                           ),
                           fluidRow(column(8,align="center",h4("Months to View") )),
                           fluidRow(
                             column(8,align="center",
                                    sliderInput('DAMonths',NULL,min=3,max=36,value=3)
                             )
                           ),
                           fluidRow(column(12, h4("Copy Inputs Into:") )),
                           fluidRow(
                             column(5, selectInput("copySelect5", NULL,
                                                   choices = c("All","Standard","Infrequent Access","One Zone-IA","Glacier"),width="225px")
                             ),
                             column(6, actionButton("copy5","Copy",width="65px")
                             )
                             
                           ),
                           actionButton("clr5","Clear")
                         ),
                         mainPanel(
                           plotOutput("DAPlot",width="80%"),br(),
                           tableOutput("DATable")
                         )
                       )
              ),
              tabPanel("Storage Class Comparison",br(),
                       
                       #fluidRow(column(3,h3("Plot & Table Options") )),
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           
                           #fluidRow(column(8,align="center",h4("Storage Costs") )),
                           #fluidRow(column(3,actionButton("storageShow","Show") ),
                                    #column(3,actionButton("storageHide","Hide") ),
                                    #column(3,actionButton("storageOnly","Show Only") )
                           #),br(),
                           #fluidRow(column(8,align="center",h4("Data Upload Costs") )),
                           #fluidRow(column(3,actionButton("uploadShow","Show") ),
                                    #column(3,actionButton("uploadHide","Hide") ),
                                    #column(3,actionButton("uploadOnly","Show Only") )
                           #),br(),
                           #fluidRow(column(8,align="center",h4("Data Retrieval Costs") )),
                           #fluidRow(column(3,actionButton("retrievalShow","Show") ),
                                    #column(3,actionButton("retrievalHide","Hide") ),
                                    #column(3,actionButton("retrievalOnly","Show Only") )
                           #),br(),
                           fluidRow(column(8,align="center",h4("Months to View") )),
                           fluidRow(
                             column(8,
                                    sliderInput('compMonths',NULL,min=3,max=36,value=3)
                             )
                           )
                         ),
                         mainPanel(
                           tabsetPanel(type = "tab",
                                       tabPanel("Totals",br(),plotOutput("compPlotTotals",width="80%"),br(),tableOutput("compTableTotals")),
                                       tabPanel("Storage",br(),plotOutput("compPlotStorage",width="80%"),br(),tableOutput("compTableStorage")),
                                       tabPanel("Upload",br(),plotOutput("compPlotUpload",width="80%"),br(),tableOutput("compTableUpload")),
                                       tabPanel("Data Retrieval",br(),plotOutput("compPlotDataRetrieval",width="80%"),br(),tableOutput("compTableDataRetrieval"))
                                       
                           
                         )
                       )
              )
  )
)
)
