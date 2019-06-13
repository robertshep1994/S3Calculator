library(shiny)
library(ggplot2)
library(Cairo);options(shiny.usecairo=TRUE)

options(warn=-1)

server <- function(session, input, output) {
  
  standardStorageTier1 <- 0.023
  standardStorageTier2 <- 0.022
  standardStorageTier3 <- 0.021
  standardPUT <- 0.005 / 1000
  standardGET <- 0.0004 / 1000
  
  infrequentStorage <- 0.0125
  infrequentPUT <- 0.01 / 1000
  infrquentGET <- 0.001 / 1000
  infrequentRetrieval <- 0.01
  
  oneZoneStorage <- 0.01
  oneZonePUT <- 0.01 / 1000
  oneZoneGET <- 0.001 / 1000
  oneZoneRetrieval <- 0.01
  
  glacierstorage <- 0.004
  glacierPUT <- 0.05 / 1000
  glacierRetrievalExpedited <- 0.03
  glacierRetrievalStandard <- 0.01
  glacierRetrievalBulk <- 0.0025
  glacierRetrievalExpeditedReq <- 10 / 1000
  glacierRetrievalStandardReq <- 0.05 / 1000
  glacierRetrievalBulkReq <- 0.025 / 1000
  
  deepGlacierStorage <- 0.00099
  deepGlacierPUT <- 0.05 / 1000
  deepGlacierRetrievalStandard <- 0.02
  deepGlacierRetrievalBulk <- 0.0025
  deepGlacierRetrievalStandardReq <- 0.10 / 1000
  deepGlacierRetrievalBulkReq <- 0.025 / 1000
  
  transferOutTier1 <- 0.09
  transferOutTier2 <- 0.085
  transferOutTier3 <- 0.07
  transferOutTier4 <- 0.05
  
  calculateGB <- function(storageAmount,storageUnit){
    if(storageUnit == "GB"){
      storageAmount
    }else if(storageUnit == "TB"){
      storageAmount * 1024
    }else if(storageUnit == "PB"){
      storageAmount * 1024 * 1024
    }
  }
  
  convertBack <- function(storageAmount){
    ifelse(storageAmount >= 1024 & storageAmount < 1048576,paste(round(storageAmount / 1024,2),"TB"),
           ifelse(storageAmount >= 1048576,paste(round(storageAmount / 1024 / 1024,2),"PB"),paste(round(storageAmount,2),"GB")))
  }
  
  calculateGrowthPerc <- function(storageAmount,percentage,n){
    storageAmount * (1+percentage/100)^(0:(n-1))
  }
  
  calculateGrowthStatic <- function(storageAmount,growth,n){
    storageAmount + (growth)*(1:n-1)
  }
  
  calculateStorageTier <- function(storageAmount){
    ifelse(storageAmount <= (50*1024),storageAmount * standardStorageTier1,
           ifelse(storageAmount < (500*1024) & storageAmount > (50*1024),((storageAmount-(50*1024))*standardStorageTier2) + (50*1024*standardStorageTier1),
                  ((storageAmount-(500*1024)) * standardStorageTier3) + (450*1024*standardStorageTier2) + (50*1024*standardStorageTier1)))
  }
  
  calculateDataTransferTier <- function(transferAmount){
    ifelse(transferAmount <= (10*1024),transferAmount * transferOutTier1,
           ifelse(transferAmount <= (50*1024) & transferAmount > (10*1024),((transferAmount-(10*1024))*transferOutTier2) + (10*1024*transferOutTier1),
                  ifelse(transferAmount <= (150*1024) & transferAmount > (50*1024),((transferAmount-(50*1024))*transferOutTier3) + (40*1024*transferOutTier2) + (10*1024*transferOutTier1),
                         ((transferAmount-(150*1024)) * transferOutTier4) + (100*1024*transferOutTier3) + (40*1024*transferOutTier2) + (10*1024*transferOutTier1))))
  }
  
  calculateStdStorageAmount <- function(storageAmount,unitType1,unitType2){
    storageGB <- calculateGB(storageAmount,unitType1)
    if(unitType2 == "%"){
      temp1 <- calculateGrowthPerc(storageGB,vals$stdF,vals$stdH)
    }else{
      growthGB <- calculateGB(vals$stdF,unitType2)
      temp1 <- calculateGrowthStatic(storageGB,growthGB,vals$stdH)
    }
    temp1
  }
  
  calculateStdStorageCosts <- function(stdStorageAmount,unitType1,unitType2){
    storageGB <- calculateGB(stdStorageAmount,unitType1)
    if(unitType2 == "%"){
      temp1 <- calculateGrowthPerc(storageGB,vals$stdF,vals$stdH)
    }else{
      growthGB <- calculateGB(vals$stdF,unitType2)
      temp1 <- calculateGrowthStatic(storageGB,growthGB,vals$stdH)
    }
    calculateStorageTier(temp1)
  }
  
  calculateGrowthObj <- function(storageAmount,growth,n,objPerGB,unitType){
    growthGB <- calculateGB(growth,unitType)
    if(unitType == "%"){
      objGrowth <- c(storageAmount*objPerGB,diff(storageAmount * (1+growth/100)^(0:(n-1)) * objPerGB))
    }else{
      objGrowth <- c(storageAmount*objPerGB,diff((storageAmount + (growthGB)*(1:n-1)) * objPerGB))
    }
    if(is.na(objGrowth)==F){
      objGrowth * standardPUT
    }else{
      0
    }
  }
  
  calculateDataRetrievalCosts <- function(storageAmount,retrievalAmount,n,unitType1,unitType2,unitType3,objPerGB){
    if(unitType2 == "%"){
      temp1 <- calculateStdStorageAmount(storageAmount,unitType1,unitType3) * (retrievalAmount/100)
      temp2 <- temp1 * objPerGB * standardGET
    }else{
      retrievalGB <- calculateGB(retrievalAmount,unitType2)
      temp1 <- rep(retrievalGB,n)
      temp2 <- rep(retrievalGB*objPerGB,n) * standardGET
    }
    if(is.na(calculateDataTransferTier(temp1) + temp2)==F){
      calculateDataTransferTier(temp1) + temp2
    }else{
      0
    }
    
  }
  
  headers <- c("Month","Storage Amount","Storage Cost","Upload Cost","Data Retrieval Cost","Monthly Cost","Cumulative Total")
  
  dataVals <- reactiveValues(df=NULL)
  observe({
    dataVals$df <- as.data.frame(matrix(,0,length(headers)))
  })
  
  vals <- reactiveValues()
  observe({
    vals$stdA <- as.numeric(paste(gsub("[^0-9.-]","",input$stdStorage)))
    vals$stdB <- input$stdUnit1
    vals$stdC <- as.numeric(paste(gsub("[^0-9.-]","",input$stdObj)))
    vals$stdD <- as.numeric(paste(gsub("[^0-9.-]","",input$stdRetrieval)))
    vals$stdE <- input$stdUnit2
    vals$stdF <- as.numeric(paste(gsub("[^0-9.-]","",input$stdGrowth)))
    vals$stdG <- input$stdUnit3
    vals$stdH <- input$stdMonths
    vals$stdStorageGB <- calculateGB(vals$stdA,vals$stdB)
    vals$stdRetrievalGB <- calculateGB(vals$stdD,vals$stdE)
    vals$stdObjPerGB <- vals$stdC / vals$stdStorageGB 
    vals$stdMonths <- c(1:vals$stdH)
    
    vals$stdStorageAmount <- convertBack(calculateStdStorageAmount(vals$stdA,vals$stdB,vals$stdG))
    vals$stdStorageCost <- calculateStdStorageCosts(vals$stdA,vals$stdB,vals$stdG)
    vals$stdUploadCost <- calculateGrowthObj(vals$stdStorageGB,vals$stdF,vals$stdH,vals$stdObjPerGB,vals$stdG)
    vals$stdDataTransferCost <- calculateDataRetrievalCosts(vals$stdA,vals$stdD,vals$stdH,vals$stdB,vals$stdE,vals$stdG,vals$stdObjPerGB)
    
    vals$stdMonthlyCost <- vals$stdStorageCost + vals$stdUploadCost + vals$stdDataTransferCost
    vals$stdCumulativeCost <- cumsum(vals$stdMonthlyCost)
    
    vals$stdTempDf <- data.frame(vals$stdMonths,vals$stdStorageAmount,vals$stdStorageCost,vals$stdUploadCost,vals$stdDataTransferCost,vals$stdMonthlyCost,vals$stdCumulativeCost)
    
    vals$IAA <- gsub("[^0-9.-]","",input$IAStorage)
    vals$IAB <- input$IAUnit1
    vals$IAC <- gsub("[^0-9.-]","",input$IAObj)
    vals$IAD <- gsub("[^0-9.-]","",input$IARetrieval)
    vals$IAE <- input$IAUnit2
    vals$IAF <- gsub("[^0-9.-]","",input$IAGrowth)
    vals$IAG <- input$IAUnit3
    vals$IAH <- input$IAMonths
    
    vals$OIAA <- gsub("[^0-9.-]","",input$OIAStorage)
    vals$OIAB <- input$OIAUnit1
    vals$OIAC <- gsub("[^0-9.-]","",input$OIAObj)
    vals$OIAD <- gsub("[^0-9.-]","",input$OIARetrieval)
    vals$OIAE <- input$OIAUnit2
    vals$OIAF <- gsub("[^0-9.-]","",input$OIAGrowth)
    vals$OIAG <- input$OIAUnit3
    vals$OIAH <- input$OIAMonths
    
    vals$glacierA <- gsub("[^0-9.-]","",input$OIAStorage)
    vals$glacierB <- input$OIAUnit1
    vals$glacierC <- gsub("[^0-9.-]","",input$OIAObj)
    vals$glacierD <- gsub("[^0-9.-]","",input$OIARetrieval)
    vals$glacierE <- input$OIAUnit2
    vals$glacierF <- input$glacierSpeed
    vals$glacierG <- gsub("[^0-9.-]","",input$OIAGrowth)
    vals$glacierH <- input$OIAUnit3
    vals$glacierI <- input$OIAMonths
    
    vals$glacierA <- gsub("[^0-9.-]","",input$glacierStorage)
    vals$glacierB <- input$glacierUnit1
    vals$glacierC <- gsub("[^0-9.-]","",input$glacierObj)
    vals$glacierD <- gsub("[^0-9.-]","",input$glacierRetrieval)
    vals$glacierE <- input$glacierUnit2
    vals$glacierF <- input$glacierSpeed
    vals$glacierG <- gsub("[^0-9.-]","",input$glacierGrowth)
    vals$glacierH <- input$glacierUnit3
    vals$glacierI <- input$glacierMonths
    
    vals$DAA <- gsub("[^0-9.-]","",input$DAStorage)
    vals$DAB <- input$DAUnit1
    vals$DAC <- gsub("[^0-9.-]","",input$DAObj)
    vals$DAD <- gsub("[^0-9.-]","",input$DARetrieval)
    vals$DAE <- input$DAUnit2
    vals$DAF <- input$DASpeed
    vals$DAG <- gsub("[^0-9.-]","",input$DAGrowth)
    vals$DAH <- input$DAUnit3
    vals$DAI <- input$DAMonths
  })
  
  observeEvent(input$clr1, {
    updateTextInput(session,"stdStorage",value="0")
    updateTextInput(session,"stdObj",value="0")
    updateTextInput(session,"stdRetrieval",value="0")
    updateTextInput(session,"stdGrowth",value="0")
    updateSliderInput(session,"stdMonths",value=3)
  })
  
  observeEvent(input$clr2, {
    updateTextInput(session,"IAStorage",value="0")
    updateTextInput(session,"IAObj",value="0")
    updateTextInput(session,"IARetrieval",value="0")
    updateTextInput(session,"IAGrowth",value="0")
  })
  
  observeEvent(input$clr3, {
    updateTextInput(session,"OIAStorage",value="0")
    updateTextInput(session,"OIAObj",value="0")
    updateTextInput(session,"OIARetrieval",value="0")
    updateTextInput(session,"OIAGrowth",value="0")
  })
  
  observeEvent(input$clr4, {
    updateTextInput(session,"glacierStorage",value="0")
    updateTextInput(session,"glacierObj",value="0")
    updateTextInput(session,"glacierRetrieval",value="0")
    updateTextInput(session,"glacierGrowth",value="0")
  })
  
  observeEvent(input$clr5, {
    updateTextInput(session,"DAStorage",value="0")
    updateTextInput(session,"DAObj",value="0")
    updateTextInput(session,"DARetrieval",value="0")
    updateTextInput(session,"DAGrowth",value="0")
  })
  
  output$stdPlot <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Total Cost ($)\n")+
      geom_line(aes(x=c(1:vals$stdH),y=vals$stdStorageCost,color='Storage Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$stdH),y=vals$stdUploadCost,color='Upload Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$stdH),vals$stdDataTransferCost,color='Data Transfer Cost'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$stdH,by=1))+
      theme(text=element_text(size=15))+
      scale_color_manual(values=c("Storage Cost"="royalblue4","Upload Cost"="orangered3","Data Transfer Cost"="green4"))+
      labs(color='')
    
  })
  
  output$stdTable <- renderTable({
    names(vals$stdTempDf) <- headers
    vals$stdTempDf
  },align='c')
  
}
