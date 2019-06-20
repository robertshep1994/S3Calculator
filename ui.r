library(shiny)
library(ggplot2)

options(warn=-1)

ui <- fluidPage(
  #shinythemes::themeSelector(),
  titlePanel("S3 Cost Calculator"),
  
  p("This cost calculator is intended to be used to estimate and compare the costs of storage, upload, and data retrieval for each of the S3 storage classes. The page tabs represent each of the S3 storage classes currently available in AWS."),
  p("To begin, click one of the storage class tabs and input the information listed."),
  
  tabsetPanel(type="tab",
                  
              tabPanel("Standard",br(),
                       p("Standard is the default storage class which is designed for frequently accessed data."),
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
                       p("Infrequent Access is designed for data that is accessed less frequently, but requires rapid access when needed."),
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
                       p("One Zone-Infrequent Access is designed similarly to Infrequent Access, but unlike other storage class which store data in a minimum of three Availability Zones, One Zone-IA stores data in a single Availability Zone."),
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
                       p("Glacier is a low-cost data archiving solution and provides three retrieval options of varying speed."),
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
                       p("Glacier-Deep Archive is designed for long-term data storage that is accessed once or twice in a year with two retrieval speeds to choose from."),
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
                       p("Easily compare the storage, upload, data retrieval, and total cumulative costs of the different storage classes based on the inputs provided."),
                       sidebarLayout(
                         
                         sidebarPanel(
    
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
                     ),
              
              tabPanel("Instructions",br(),
                       
                       p("The following instructions explain how to use the cost calculator in further detail."),
                       h4("Total Amount of Storage"),
                       tags$li("Input the total amount of storage being uploaded into S3"),
                       tags$li("Select the storage unit (GB, TB, or PB)"),
                       h4("Total Number of Objects"),
                       tags$li("Input the total number of objects/files that make up the amount of storage inputted"),
                       h4("Data Retrieval Amount"),
                       tags$li("Input the estimated amount of monthly data retrieval"),
                       tags$li("Select the storage unit (GB, TB, PB, or %)"),
                       tags$li("Selecting % will calculate the monthly data retrieval amount as a percentage of the total amount of storage"),
                       h4("Storage Growth"),
                       tags$li("Input the estimated amount of monthly storage growth"),
                       tags$li("Select the storage unit (GB, TB, PB, or %)"),
                       tags$li("Selecting % will calculate the monthly growth amount as a percentage of the total amount of storage"),
                       h4("Months to View"),
                       tags$li("Use the slider to indicate how many months to show on the plots and tables"),
                       tags$li("Choose between 3 and 36 months"),
                       h4("Copy Inputs"),
                       tags$li("Once all the inputs are entered for a given storage class, the inputs can be copied over to other storage classes"),
                       tags$li("Select which storage classes the inputs should be copied to then click the Copy button"),
                       tags$li("Selecting All will copy the inputs to all other storage classes"),
                       h4("Retrieval Speed (Glacier & Glacier-Deep Archive Only)"),
                       tags$li("Select the retrieval speed interested in using for data retrievals"),
                       tags$li("Defaults to Standard"),
                       h4("Storage Class Comparison Tab"),
                       tags$li("Use this tab to easily compare each of the storage classes separated by storage, upload, data retrieval, and cumulative costs")
              )
  )
)
