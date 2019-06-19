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
  infrequentGET <- 0.001 / 1000
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
  
  calculateStorageAmount <- function(storageAmount,unitType1,unitType2,growth,n){
    storageGB <- calculateGB(storageAmount,unitType1)
    if(unitType2 == "%"){
      temp1 <- calculateGrowthPerc(storageGB,growth,n)
    }else{
      growthGB <- calculateGB(growth,unitType2)
      temp1 <- calculateGrowthStatic(storageGB,growthGB,n)
    }
    temp1
  }
  
  calculateStorageCosts <- function(storageAmount,unitType1,unitType2,growth,n){
    storageGB <- calculateGB(storageAmount,unitType1)
    if(unitType2 == "%"){
      temp1 <- calculateGrowthPerc(storageGB,growth,n)
    }else{
      growthGB <- calculateGB(growth,unitType2)
      temp1 <- calculateGrowthStatic(storageGB,growthGB,n)
    }
    temp1
  }
  
  calculateGrowthObj <- function(storageAmount,growth,n,objPerGB,unitType,pricing){
    growthGB <- calculateGB(growth,unitType)
    if(unitType == "%"){
      objGrowth <- c(storageAmount*objPerGB,diff(storageAmount * (1+growth/100)^(0:(n-1)) * objPerGB))
    }else{
      objGrowth <- c(storageAmount*objPerGB,diff((storageAmount + (growthGB)*(1:n-1)) * objPerGB))
    }
    if(is.na(objGrowth)==F & objGrowth != Inf){
      objGrowth * pricing
    }else{
      (1:n) * 0
    }
  }
  
  calculateStdDataRetrievalCosts <- function(storageAmount,retrievalAmount,n,unitType1,unitType2,unitType3,objPerGB,growth){
    if(unitType2 == "%"){
      temp1 <- calculateStorageAmount(storageAmount,unitType1,unitType3,growth,n) * (retrievalAmount/100)
      temp2 <- temp1 * objPerGB * standardGET
    }else{
      retrievalGB <- calculateGB(retrievalAmount,unitType2)
      temp1 <- rep(retrievalGB,n)
      temp2 <- rep(retrievalGB*objPerGB,n) * standardGET
    }
    if(is.na(calculateDataTransferTier(temp1) + temp2)==F & calculateDataTransferTier(temp1) + temp2 != Inf){
      calculateDataTransferTier(temp1) + temp2
    }else{
      (1:n) * 0
    }
  }
  
  calculateDataRetrievalCosts <- function(storageAmount,retrievalAmount,n,unitType1,unitType2,unitType3,objPerGB,growth,pricing1,pricing2,retrievalSpeed,expedited1,standard1,bulk1,expedited2,standard2,bulk2){
    if(unitType2 == "%"){
      if(retrievalSpeed == "Expedited"){
        temp1 <- calculateStorageAmount(storageAmount,unitType1,unitType3,growth,n) * (retrievalAmount/100)
        temp2 <- (temp1 * objPerGB * expedited1) + (temp1 * expedited2)
      }else if(retrievalSpeed == "Standard"){
        temp1 <- calculateStorageAmount(storageAmount,unitType1,unitType3,growth,n) * (retrievalAmount/100)
        temp2 <- (temp1 * objPerGB * standard1) + (temp1 * standard2)
      }else if(retrievalSpeed == "Bulk"){
        temp1 <- calculateStorageAmount(storageAmount,unitType1,unitType3,growth,n) * (retrievalAmount/100)
        temp2 <- (temp1 * objPerGB * bulk1) + (temp1 * bulk2)
      }else{
        temp1 <- calculateStorageAmount(storageAmount,unitType1,unitType3,growth,n) * (retrievalAmount/100)
        temp2 <- (temp1 * objPerGB * pricing1) + (temp1 * pricing2)
      }
    }else{
      retrievalGB <- calculateGB(retrievalAmount,unitType2)
      if(retrievalSpeed == "Expedited"){
        temp1 <- rep(retrievalGB,n)
        temp2 <- (rep(retrievalGB*objPerGB,n) * expedited1) + (temp1 * expedited2)
      }else if(retrievalSpeed == "Standard"){
        temp1 <- rep(retrievalGB,n)
        temp2 <- (rep(retrievalGB*objPerGB,n) * standard1) + (temp1 * standard2)
      }else if(retrievalSpeed == "Bulk"){
        temp1 <- rep(retrievalGB,n)
        temp2 <- (rep(retrievalGB*objPerGB,n) * bulk1) + (temp1 * bulk2)
      }else{
        temp1 <- rep(retrievalGB,n)
        temp2 <- (rep(retrievalGB*objPerGB,n) * pricing1) + (temp1 * pricing2)
      }
    }
    if(is.na(calculateDataTransferTier(temp1) + temp2)==F & calculateDataTransferTier(temp1) + temp2 != Inf){
      calculateDataTransferTier(temp1) + temp2
    }else{
      (1:n) * 0
    }
  }
  
  headers <- c("Month","Storage Amount","Storage Cost","Upload Cost","Data Retrieval Cost","Monthly Cost","Cumulative Total")
  
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
    vals$stdI <- input$copySelect1
    vals$stdStorageGB <- calculateGB(vals$stdA,vals$stdB)
    vals$stdRetrievalGB <- calculateGB(vals$stdD,vals$stdE)
    vals$stdObjPerGB <- vals$stdC / vals$stdStorageGB 
    vals$stdMonths <- c(1:vals$stdH)
    
    vals$stdStorageAmount <- convertBack(calculateStorageAmount(vals$stdA,vals$stdB,vals$stdG,vals$stdF,vals$stdH))
    vals$stdStorageCost <- calculateStorageTier(calculateStorageCosts(vals$stdA,vals$stdB,vals$stdG,vals$stdF,vals$stdH))
    vals$stdUploadCost <- calculateGrowthObj(vals$stdStorageGB,vals$stdF,vals$stdH,vals$stdObjPerGB,vals$stdG,standardPUT)
    vals$stdDataTransferCost <- calculateStdDataRetrievalCosts(vals$stdA,vals$stdD,vals$stdH,vals$stdB,vals$stdE,vals$stdG,vals$stdObjPerGB,vals$stdF)
    
    vals$stdMonthlyCost <- vals$stdStorageCost + vals$stdUploadCost + vals$stdDataTransferCost
    vals$stdCumulativeCost <- cumsum(vals$stdMonthlyCost)
    
    vals$stdDf <- data.frame(vals$stdMonths,vals$stdStorageAmount,vals$stdStorageCost,vals$stdUploadCost,vals$stdDataTransferCost,vals$stdMonthlyCost,vals$stdCumulativeCost)
    
    vals$IAA <- as.numeric(paste(gsub("[^0-9.-]","",input$IAStorage)))
    vals$IAB <- input$IAUnit1
    vals$IAC <- as.numeric(paste(gsub("[^0-9.-]","",input$IAObj)))
    vals$IAD <- as.numeric(paste(gsub("[^0-9.-]","",input$IARetrieval)))
    vals$IAE <- input$IAUnit2
    vals$IAF <- as.numeric(paste(gsub("[^0-9.-]","",input$IAGrowth)))
    vals$IAG <- input$IAUnit3
    vals$IAH <- input$IAMonths
    vals$IAI <- input$copySelect2
    vals$IAStorageGB <- calculateGB(vals$IAA,vals$IAB)
    vals$IARetrievalGB <- calculateGB(vals$IAD,vals$IAE)
    vals$IAObjPerGB <- vals$IAC / vals$IAStorageGB 
    vals$IAMonths <- c(1:vals$IAH)
    
    vals$IAStorageAmount <- convertBack(calculateStorageAmount(vals$IAA,vals$IAB,vals$IAG,vals$IAF,vals$IAH))
    vals$IAStorageCost <- calculateStorageAmount(vals$IAA,vals$IAB,vals$IAG,vals$IAF,vals$IAH) * infrequentStorage
    vals$IAUploadCost <- calculateGrowthObj(vals$IAStorageGB,vals$IAF,vals$IAH,vals$IAObjPerGB,vals$IAG,infrequentPUT)
    vals$IADataTransferCost <- calculateDataRetrievalCosts(vals$IAA,vals$IAD,vals$IAH,vals$IAB,vals$IAE,vals$IAG,vals$IAObjPerGB,vals$IAF,infrequentGET,infrequentRetrieval,"None",1,1,1,1,1,1)
    
    vals$IAMonthlyCost <- vals$IAStorageCost + vals$IAUploadCost + vals$IADataTransferCost
    vals$IACumulativeCost <- cumsum(vals$IAMonthlyCost)
    
    vals$IADf <- data.frame(vals$IAMonths,vals$IAStorageAmount,vals$IAStorageCost,vals$IAUploadCost,vals$IADataTransferCost,vals$IAMonthlyCost,vals$IACumulativeCost)
    
    vals$OIAA <- as.numeric(paste(gsub("[^0-9.-]","",input$OIAStorage)))
    vals$OIAB <- input$OIAUnit1
    vals$OIAC <- as.numeric(paste(gsub("[^0-9.-]","",input$OIAObj)))
    vals$OIAD <- as.numeric(paste(gsub("[^0-9.-]","",input$OIARetrieval)))
    vals$OIAE <- input$OIAUnit2
    vals$OIAF <- as.numeric(paste(gsub("[^0-9.-]","",input$OIAGrowth)))
    vals$OIAG <- input$OIAUnit3
    vals$OIAH <- input$OIAMonths
    vals$OIAI <- input$copySelect3
    vals$OIAStorageGB <- calculateGB(vals$OIAA,vals$OIAB)
    vals$OIARetrievalGB <- calculateGB(vals$OIAD,vals$OIAE)
    vals$OIAObjPerGB <- vals$OIAC / vals$OIAStorageGB 
    vals$OIAMonths <- c(1:vals$OIAH)
    
    vals$OIAStorageAmount <- convertBack(calculateStorageAmount(vals$OIAA,vals$OIAB,vals$OIAG,vals$OIAF,vals$OIAH))
    vals$OIAStorageCost <- calculateStorageAmount(vals$OIAA,vals$OIAB,vals$OIAG,vals$OIAF,vals$OIAH) * oneZoneStorage
    vals$OIAUploadCost <- calculateGrowthObj(vals$OIAStorageGB,vals$OIAF,vals$OIAH,vals$OIAObjPerGB,vals$OIAG,oneZonePUT)
    vals$OIADataTransferCost <- calculateDataRetrievalCosts(vals$OIAA,vals$OIAD,vals$OIAH,vals$OIAB,vals$OIAE,vals$OIAG,vals$OIAObjPerGB,vals$OIAF,oneZoneGET,oneZoneRetrieval,"None",1,1,1,1,1,1)
    
    vals$OIAMonthlyCost <- vals$OIAStorageCost + vals$OIAUploadCost + vals$OIADataTransferCost
    vals$OIACumulativeCost <- cumsum(vals$OIAMonthlyCost)
    
    vals$OIADf <- data.frame(vals$OIAMonths,vals$OIAStorageAmount,vals$OIAStorageCost,vals$OIAUploadCost,vals$OIADataTransferCost,vals$OIAMonthlyCost,vals$OIACumulativeCost)
    
    vals$glacierA <- as.numeric(paste(gsub("[^0-9.-]","",input$glacierStorage)))
    vals$glacierB <- input$glacierUnit1
    vals$glacierC <- as.numeric(paste(gsub("[^0-9.-]","",input$glacierObj)))
    vals$glacierD <- as.numeric(paste(gsub("[^0-9.-]","",input$glacierRetrieval)))
    vals$glacierE <- input$glacierUnit2
    vals$glacierF <- as.numeric(paste(gsub("[^0-9.-]","",input$glacierGrowth)))
    vals$glacierG <- input$glacierUnit3
    vals$glacierH <- input$glacierMonths
    vals$glacierI <- input$glacierSpeed
    vals$glacierJ <- input$copySelect4
    vals$glacierStorageGB <- calculateGB(vals$glacierA,vals$glacierB)
    vals$glacierRetrievalGB <- calculateGB(vals$glacierD,vals$glacierE)
    vals$glacierObjPerGB <- vals$glacierC / vals$glacierStorageGB 
    vals$glacierMonths <- c(1:vals$glacierH)
    
    vals$glacierStorageAmount <- convertBack(calculateStorageAmount(vals$glacierA,vals$glacierB,vals$glacierG,vals$glacierF,vals$glacierH))
    vals$glacierStorageCost <- calculateStorageAmount(vals$glacierA,vals$glacierB,vals$glacierG,vals$glacierF,vals$glacierH) * glacierstorage
    vals$glacierUploadCost <- calculateGrowthObj(vals$glacierStorageGB,vals$glacierF,vals$glacierH,vals$glacierObjPerGB,vals$glacierG,glacierPUT)
    vals$glacierDataTransferCost <- calculateDataRetrievalCosts(vals$glacierA,vals$glacierD,vals$glacierH,vals$glacierB,vals$glacierE,vals$glacierG,vals$glacierObjPerGB,vals$glacierF,1,1,vals$glacierI,glacierRetrievalExpeditedReq,glacierRetrievalStandardReq,glacierRetrievalBulkReq,glacierRetrievalExpedited,glacierRetrievalStandard,glacierRetrievalBulk)
    
    vals$glacierMonthlyCost <- vals$glacierStorageCost + vals$glacierUploadCost + vals$glacierDataTransferCost
    vals$glacierCumulativeCost <- cumsum(vals$glacierMonthlyCost)
    
    vals$glacierDf <- data.frame(vals$glacierMonths,vals$glacierStorageAmount,vals$glacierStorageCost,vals$glacierUploadCost,vals$glacierDataTransferCost,vals$glacierMonthlyCost,vals$glacierCumulativeCost)
    
    vals$DAA <- as.numeric(paste(gsub("[^0-9.-]","",input$DAStorage)))
    vals$DAB <- input$DAUnit1
    vals$DAC <- as.numeric(paste(gsub("[^0-9.-]","",input$DAObj)))
    vals$DAD <- as.numeric(paste(gsub("[^0-9.-]","",input$DARetrieval)))
    vals$DAE <- input$DAUnit2
    vals$DAF <- as.numeric(paste(gsub("[^0-9.-]","",input$DAGrowth)))
    vals$DAG <- input$DAUnit3
    vals$DAH <- input$DAMonths
    vals$DAI <- input$DASpeed
    vals$DAJ <- input$copySelect5
    vals$DAStorageGB <- calculateGB(vals$DAA,vals$DAB)
    vals$DARetrievalGB <- calculateGB(vals$DAD,vals$DAE)
    vals$DAObjPerGB <- vals$DAC / vals$DAStorageGB 
    vals$DAMonths <- c(1:vals$DAH)
    
    vals$DAStorageAmount <- convertBack(calculateStorageAmount(vals$DAA,vals$DAB,vals$DAG,vals$DAF,vals$DAH))
    vals$DAStorageCost <- calculateStorageAmount(vals$DAA,vals$DAB,vals$DAG,vals$DAF,vals$DAH) * deepGlacierStorage
    vals$DAUploadCost <- calculateGrowthObj(vals$DAStorageGB,vals$DAF,vals$DAH,vals$DAObjPerGB,vals$DAG,deepGlacierPUT)
    vals$DADataTransferCost <- calculateDataRetrievalCosts(vals$DAA,vals$DAD,vals$DAH,vals$DAB,vals$DAE,vals$DAG,vals$DAObjPerGB,vals$DAF,1,1,vals$DAI,1,deepGlacierRetrievalStandardReq,deepGlacierRetrievalBulkReq,1,deepGlacierRetrievalStandard,deepGlacierRetrievalBulk)
    
    vals$DAMonthlyCost <- vals$DAStorageCost + vals$DAUploadCost + vals$DADataTransferCost
    vals$DACumulativeCost <- cumsum(vals$DAMonthlyCost)
    
    vals$DADf <- data.frame(vals$DAMonths,vals$DAStorageAmount,vals$DAStorageCost,vals$DAUploadCost,vals$DADataTransferCost,vals$DAMonthlyCost,vals$DACumulativeCost)
    
    vals$compMonths <- input$compMonths
    vals$compMonthsDf <- c(1:vals$compMonths)
    
    vals$compStdStorage <- calculateStorageTier(calculateStorageCosts(vals$stdA,vals$stdB,vals$stdG,vals$stdF,vals$compMonths))
    vals$compStdUpload <- calculateGrowthObj(vals$stdStorageGB,vals$stdF,vals$compMonths,vals$stdObjPerGB,vals$stdG,standardPUT)
    vals$compStdDataTransfer <- calculateStdDataRetrievalCosts(vals$stdA,vals$stdD,vals$compMonths,vals$stdB,vals$stdE,vals$stdG,vals$stdObjPerGB,vals$stdF)
    vals$compStdStorageCumulative <- cumsum(vals$compStdStorage)
    vals$compStdUploadCumulative <- cumsum(vals$compStdUpload)
    vals$compStdDataTransferCumulative <- cumsum(vals$compStdDataTransfer)
    
    vals$compIAStorage <- calculateStorageAmount(vals$IAA,vals$IAB,vals$IAG,vals$IAF,vals$compMonths) * infrequentStorage
    vals$compIAUpload <- calculateGrowthObj(vals$IAStorageGB,vals$IAF,vals$compMonths,vals$IAObjPerGB,vals$IAG,infrequentPUT)
    vals$compIADataTransfer <- calculateDataRetrievalCosts(vals$IAA,vals$IAD,vals$compMonths,vals$IAB,vals$IAE,vals$IAG,vals$IAObjPerGB,vals$IAF,infrequentGET,infrequentRetrieval,"None",1,1,1,1,1,1)
    vals$compIAStorageCumulative <- cumsum(vals$compIAStorage)
    vals$compIAUploadCumulative <- cumsum(vals$compIAUpload)
    vals$compIADataTransferCumulative <- cumsum(vals$compIADataTransfer)
    
    vals$compOIAStorage <- calculateStorageAmount(vals$OIAA,vals$OIAB,vals$OIAG,vals$OIAF,vals$compMonths) * oneZoneStorage
    vals$compOIAUpload <- calculateGrowthObj(vals$OIAStorageGB,vals$OIAF,vals$compMonths,vals$OIAObjPerGB,vals$OIAG,oneZonePUT)
    vals$compOIADataTransfer <- calculateDataRetrievalCosts(vals$OIAA,vals$OIAD,vals$compMonths,vals$OIAB,vals$OIAE,vals$OIAG,vals$OIAObjPerGB,vals$OIAF,oneZoneGET,oneZoneRetrieval,"None",1,1,1,1,1,1)
    vals$compOIAStorageCumulative <- cumsum(vals$compOIAStorage)
    vals$compOIAUploadCumulative <- cumsum(vals$compOIAUpload)
    vals$compOIADataTransferCumulative <- cumsum(vals$compOIADataTransfer)
    
    vals$compGlacierStorage <- calculateStorageAmount(vals$glacierA,vals$glacierB,vals$glacierG,vals$glacierF,vals$compMonths) * glacierstorage
    vals$compGlacierUpload <- calculateGrowthObj(vals$glacierStorageGB,vals$glacierF,vals$compMonths,vals$glacierObjPerGB,vals$glacierG,glacierPUT)
    vals$compGlacierDataTransfer <- calculateDataRetrievalCosts(vals$glacierA,vals$glacierD,vals$compMonths,vals$glacierB,vals$glacierE,vals$glacierG,vals$glacierObjPerGB,vals$glacierF,1,1,vals$glacierI,glacierRetrievalExpeditedReq,glacierRetrievalStandardReq,glacierRetrievalBulkReq,glacierRetrievalExpedited,glacierRetrievalStandard,glacierRetrievalBulk)
    vals$compGlacierStorageCumulative <- cumsum(vals$compGlacierStorage)
    vals$compGlacierUploadCumulative <- cumsum(vals$compGlacierUpload)
    vals$compGlacierDataTransferCumulative <- cumsum(vals$compGlacierDataTransfer)
    
    vals$compDAStorage <- calculateStorageAmount(vals$DAA,vals$DAB,vals$DAG,vals$DAF,vals$compMonths) * deepGlacierStorage
    vals$compDAUpload <- calculateGrowthObj(vals$DAStorageGB,vals$DAF,vals$compMonths,vals$DAObjPerGB,vals$DAG,deepGlacierPUT)
    vals$compDADataTransfer <- calculateDataRetrievalCosts(vals$DAA,vals$DAD,vals$compMonths,vals$DAB,vals$DAE,vals$DAG,vals$DAObjPerGB,vals$DAF,1,1,vals$DAI,1,deepGlacierRetrievalStandardReq,deepGlacierRetrievalBulkReq,1,deepGlacierRetrievalStandard,deepGlacierRetrievalBulk)
    vals$compDAStorageCumulative <- cumsum(vals$compDAStorage)
    vals$compDAUploadCumulative <- cumsum(vals$compDAUpload)
    vals$compDADataTransferCumulative <- cumsum(vals$compDADataTransfer)
    
    vals$compStdMonthly <- vals$compStdStorage + vals$compStdUpload + vals$compStdDataTransfer
    vals$compStdCumulative <- cumsum(vals$compStdMonthly)
    
    vals$compIAMonthly <- vals$compIAStorage + vals$compIAUpload + vals$compIADataTransfer
    vals$compIACumulative <- cumsum(vals$compIAMonthly)
    
    vals$compOIAMonthly <- vals$compOIAStorage + vals$compOIAUpload + vals$compOIADataTransfer
    vals$compOIACumulative <- cumsum(vals$compOIAMonthly)
    
    vals$compGlacierMonthly <- vals$compGlacierStorage + vals$compGlacierUpload + vals$compGlacierDataTransfer
    vals$compGlacierCumulative <- cumsum(vals$compGlacierMonthly)
    
    vals$compDAMonthly <- vals$compDAStorage + vals$compDAUpload + vals$compDADataTransfer
    vals$compDACumulative <- cumsum(vals$compDAMonthly)
    
    vals$compStorageDf <- data.frame(vals$compMonthsDf,vals$compStdStorageCumulative,vals$compIAStorageCumulative,vals$compOIAStorageCumulative,vals$compGlacierStorageCumulative,vals$compDAStorageCumulative)
    vals$compUploadDf <- data.frame(vals$compMonthsDf,vals$compStdUploadCumulative,vals$compIAUploadCumulative,vals$compOIAUploadCumulative,vals$compGlacierUploadCumulative,vals$compDAUploadCumulative)
    vals$compDataRetrievalDf <- data.frame(vals$compMonthsDf,vals$compStdDataTransferCumulative,vals$compIADataTransferCumulative,vals$compOIADataTransferCumulative,vals$compGlacierDataTransferCumulative,vals$compDADataTransferCumulative)
    vals$compTotalsDf <- data.frame(vals$compMonthsDf,vals$compStdCumulative,vals$compIACumulative,vals$compOIACumulative,vals$compGlacierCumulative,vals$compDACumulative)
    
  })
  
  compHeaders <- c("Month","Standard Cost","Infrequent Access Cost","One Zone-IA Cost","Glacier Cost","Glacier-Deep Archive Cost")
  
  updateStdInputs <- function(storage,unit1,obj,retrieval,unit2,growth,unit3,months){
    updateTextInput(session,"stdStorage",value=storage)
    updateSelectInput(session,"stdUnit1",selected=unit1)
    updateTextInput(session,"stdObj",value=obj)
    updateTextInput(session,"stdRetrieval",value=retrieval)
    updateSelectInput(session,"stdUnit2",selected=unit2)
    updateTextInput(session,"stdGrowth",value=growth)
    updateSelectInput(session,"stdUnit3",selected=unit3)
    updateSliderInput(session,"stdMonths",value=months)
    
  }
  
  updateIAInputs <- function(storage,unit1,obj,retrieval,unit2,growth,unit3,months){
    updateTextInput(session,"IAStorage",value=storage)
    updateSelectInput(session,"IAUnit1",selected=unit1)
    updateTextInput(session,"IAObj",value=obj)
    updateTextInput(session,"IARetrieval",value=retrieval)
    updateSelectInput(session,"IAUnit2",selected=unit2)
    updateTextInput(session,"IAGrowth",value=growth)
    updateSelectInput(session,"IAUnit3",selected=unit3)
    updateSliderInput(session,"IAMonths",value=months)
  }
  
  updateOIAInputs <- function(storage,unit1,obj,retrieval,unit2,growth,unit3,months){
    updateTextInput(session,"OIAStorage",value=storage)
    updateSelectInput(session,"OIAUnit1",selected=unit1)
    updateTextInput(session,"OIAObj",value=obj)
    updateTextInput(session,"OIARetrieval",value=retrieval)
    updateSelectInput(session,"OIAUnit2",selected=unit2)
    updateTextInput(session,"OIAGrowth",value=growth)
    updateSelectInput(session,"OIAUnit3",selected=unit3)
    updateSliderInput(session,"OIAMonths",value=months)
  }
  
  updateGlacierInputs <- function(storage,unit1,obj,retrieval,unit2,growth,unit3,months){
    updateTextInput(session,"glacierStorage",value=storage)
    updateSelectInput(session,"glacierUnit1",selected=unit1)
    updateTextInput(session,"glacierObj",value=obj)
    updateTextInput(session,"glacierRetrieval",value=retrieval)
    updateSelectInput(session,"glacierUnit2",selected=unit2)
    updateTextInput(session,"glacierGrowth",value=growth)
    updateSelectInput(session,"glacierUnit3",selected=unit3)
    updateSliderInput(session,"glacierMonths",value=months)
  }
  
  updateDAInputs <- function(storage,unit1,obj,retrieval,unit2,growth,unit3,months){
    updateTextInput(session,"DAStorage",value=storage)
    updateSelectInput(session,"DAUnit1",selected=unit1)
    updateTextInput(session,"DAObj",value=obj)
    updateTextInput(session,"DARetrieval",value=retrieval)
    updateSelectInput(session,"DAUnit2",selected=unit2)
    updateTextInput(session,"DAGrowth",value=growth)
    updateSelectInput(session,"DAUnit3",selected=unit3)
    updateSliderInput(session,"DAMonths",value=months)
  }
  
  observeEvent(input$clr1, {
    updateTextInput(session,"stdStorage",value="0")
    updateTextInput(session,"stdObj",value="0")
    updateTextInput(session,"stdRetrieval",value="0")
    updateTextInput(session,"stdGrowth",value="0")
    updateSliderInput(session,"stdMonths",value=3)
  })
  
  observeEvent(input$copy1, {
    if(vals$stdI == "All"){
      updateIAInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
      updateOIAInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
      updateGlacierInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
      updateDAInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
    }else if(vals$stdI == "Infrequent Access"){
      updateIAInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
    }else if(vals$stdI == "One Zone-IA"){
      updateOIAInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
    }else if(vals$stdI == "Glacier"){
      updateGlacierInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
    }else if(vals$stdI == "Glacier-Deep Archive"){
      updateDAInputs(vals$stdA,vals$stdB,vals$stdC,vals$stdD,vals$stdE,vals$stdF,vals$stdG,vals$stdH)
    }
  })
  
  observeEvent(input$clr2, {
    updateTextInput(session,"IAStorage",value="0")
    updateTextInput(session,"IAObj",value="0")
    updateTextInput(session,"IARetrieval",value="0")
    updateTextInput(session,"IAGrowth",value="0")
    updateSliderInput(session,"IAMonths",value=3)
  })
  
  observeEvent(input$copy2, {
    if(vals$IAI == "All"){
      updateStdInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
      updateOIAInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
      updateGlacierInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
      updateDAInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
    }else if(vals$IAI == "Standard"){
      updateStdInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
    }else if(vals$IAI == "One Zone-IA"){
      updateOIAInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
    }else if(vals$IAI == "Glacier"){
      updateGlacierInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
    }else if(vals$IAI == "Glacier-Deep Glacier"){
      updateDAInputs(vals$IAA,vals$IAB,vals$IAC,vals$IAD,vals$IAE,vals$IAF,vals$IAG,vals$IAH)
    }
  })
  
  observeEvent(input$clr3, {
    updateTextInput(session,"OIAStorage",value="0")
    updateTextInput(session,"OIAObj",value="0")
    updateTextInput(session,"OIARetrieval",value="0")
    updateTextInput(session,"OIAGrowth",value="0")
    updateSliderInput(session,"OIAMonths",value=3)
  })
  
  observeEvent(input$copy3, {
    if(vals$OIAI == "All"){
      updateStdInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
      updateIAInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
      updateGlacierInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
      updateDAInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
    }else if(vals$OIAI == "Standard"){
      updateStdInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
    }else if(vals$OIAI == "Infrequent Access"){
      updateIAInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
    }else if(vals$OIAI == "Glacier"){
      updateGlacierInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
    }else if(vals$OIAI == "Glacier-Deep Glacier"){
      updateDAInputs(vals$OIAA,vals$OIAB,vals$OIAC,vals$OIAD,vals$OIAE,vals$OIAF,vals$OIAG,vals$OIAH)
    }
  })
  
  observeEvent(input$clr4, {
    updateTextInput(session,"glacierStorage",value="0")
    updateTextInput(session,"glacierObj",value="0")
    updateTextInput(session,"glacierRetrieval",value="0")
    updateTextInput(session,"glacierGrowth",value="0")
    updateSliderInput(session,"glacierMonths",value=3)
  })
  
  observeEvent(input$copy4, {
    if(vals$glacierJ == "All"){
      updateStdInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
      updateIAInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
      updateOIAInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
      updateDAInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
    }else if(vals$glacierJ == "Standard"){
      updateStdInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
    }else if(vals$glacierJ == "Infrequent Access"){
      updateStdInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
    }else if(vals$glacierJ == "One Zone-IA"){
      updateOIAInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
    }else if(vals$glacierJ == "Glacier-Deep Archive"){
      updateDAInputs(vals$glacierA,vals$glacierB,vals$glacierC,vals$glacierD,vals$glacierE,vals$glacierF,vals$glacierG,vals$glacierH)
    }
  })
  
  observeEvent(input$clr5, {
    updateTextInput(session,"DAStorage",value="0")
    updateTextInput(session,"DAObj",value="0")
    updateTextInput(session,"DARetrieval",value="0")
    updateTextInput(session,"DAGrowth",value="0")
    updateSliderInput(session,"DAMonths",value=3)
  })
  
  observeEvent(input$copy5, {
    if(vals$DAJ == "All"){
      updateStdInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
      updateIAInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
      updateOIAInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
      updateGlacierInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
    }else if(vals$DAJ == "Standard"){
      updateStdInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
    }else if(vals$DAJ == "Infrequent Access"){
      updateIAInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
    }else if(vals$DAJ == "One Zone-IA"){
      updateOIAInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
    }else if(vals$DAJ == "Glacier"){
      updateGlacierInputs(vals$DAA,vals$DAB,vals$DAC,vals$DAD,vals$DAE,vals$DAF,vals$DAG,vals$DAH)
    }
  })
  
  observeEvent(input$storageShow, {
    storage <- rep(T,vals$compMonths)
  })
  
  observeEvent(input$storageHide, {
    storageHide <- rep(F,vals$compMonths)
  })
  
  observeEvent(input$storageOnly, {
    storage <- rep(T,vals$compMonths)
    upload <- rep(F,vals$compMonths)
    retrieval <- rep(F,vals$compMonths)
  })
  
  observeEvent(input$uploadShow, {
    upload <- rep(T,vals$compMonths)
  })
  
  observeEvent(input$uploadHide, {
    upload <- rep(F,vals$compMonths)
  })
  
  observeEvent(input$uploadOnly, {
    storage <- rep(F,vals$compMonths)
    upload <- rep(T,vals$compMonths)
    retrieval <- rep(F,vals$compMonths)
  })
  
  observeEvent(input$retrievalShow, {
    retrieval <- rep(T,vals$compMonths)
  })
  
  observeEvent(input$retrievalHide, {
    retrieval <- rep(F,vals$compMonths)
  })
  
  observeEvent(input$retrievalOnly, {
    storage <- rep(F,vals$compMonths)
    upload <- rep(F,vals$compMonths)
    retrieval <- rep(T,vals$compMonths)
  })
  
  output$stdPlot <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Monthly Cost ($)\n")+
      geom_line(aes(x=c(1:vals$stdH),y=vals$stdStorageCost,color='Storage Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$stdH),y=vals$stdUploadCost,color='Upload Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$stdH),vals$stdDataTransferCost,color='Data Retrieval Cost'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$stdH,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Storage Cost"="#B32F2F","Upload Cost"="#CE5858","Data Retrieval Cost"="#DE8F8F"))+
      labs(color='')
    
  })
  
  output$stdTable <- renderTable({
    names(vals$stdDf) <- headers
    vals$stdDf
  },align='c',spacing='xs')
  
  output$IAPlot <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Monthly Cost ($)\n")+
      geom_line(aes(x=c(1:vals$IAH),y=vals$IAStorageCost,color='Storage Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$IAH),y=vals$IAUploadCost,color='Upload Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$IAH),vals$IADataTransferCost,color='Data Retrieval Cost'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$IAH,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Storage Cost"="#306B19","Upload Cost"="#419222","Data Retrieval Cost"="#86B972"))+
      labs(color='')
    
  })
  
  output$IATable <- renderTable({
    names(vals$IADf) <- headers
    vals$IADf
  },align='c',spacing='xs')
  
  output$OIAPlot <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Monthly Cost ($)\n")+
      geom_line(aes(x=c(1:vals$OIAH),y=vals$OIAStorageCost,color='Storage Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$OIAH),y=vals$OIAUploadCost,color='Upload Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$OIAH),vals$OIADataTransferCost,color='Data Retrieval Cost'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$OIAH,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Storage Cost"="#09332D","Upload Cost"="#136F63","Data Retrieval Cost"="#93BDB8"))+
      labs(color='')
    
  })
  
  output$OIATable <- renderTable({
    names(vals$OIADf) <- headers
    vals$OIADf
  },align='c',spacing='xs')
  
  output$glacierTable <- renderTable({
    names(vals$glacierDf) <- headers
    vals$glacierDf
  },align='c',spacing='xs')
  
  output$glacierPlot <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Monthly Cost ($)\n")+
      geom_line(aes(x=c(1:vals$glacierH),y=vals$glacierStorageCost,color='Storage Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$glacierH),y=vals$glacierUploadCost,color='Upload Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$glacierH),vals$glacierDataTransferCost,color='Data Retrieval Cost'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$glacierH,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Storage Cost"="#1D3E5A","Upload Cost"="#3F88C5","Data Retrieval Cost"="#A7C8E4"))+
      labs(color='')
    
  })
  
  output$DATable <- renderTable({
    names(vals$DADf) <- headers
    vals$DADf
  },align='c',spacing='xs')
  
  output$DAPlot <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Monthly Cost ($)\n")+
      geom_line(aes(x=c(1:vals$DAH),y=vals$DAStorageCost,color='Storage Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$DAH),y=vals$DAUploadCost,color='Upload Cost'),size=I(2))+
      geom_line(aes(x=c(1:vals$DAH),vals$DADataTransferCost,color='Data Retrieval Cost'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$DAH,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Storage Cost"="#6B5565","Upload Cost"="#93748A","Data Retrieval Cost"="#BAA6B4"))+
      labs(color='')
    
  })
  
  output$compPlotStorage <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Total Cost ($)\n")+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compStdStorageCumulative,color='Standard'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compIAStorageCumulative,color='Infrequent Access'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compOIAStorageCumulative,color='One Zone-IA'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compGlacierStorageCumulative,color='Glacier'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compDAStorageCumulative,color='Glacier-Deep Archive'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$compMonths,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Standard"="#CE5858","Infrequent Access"="#419222","One Zone-IA"="#136F63","Glacier"="#3F88C5","Glacier-Deep Archive"="#93748A"))+
      labs(color='')
    
  })
  
  output$compTableStorage <- renderTable({
    names(vals$compStorageDf) <- compHeaders
    vals$compStorageDf
  },align='c',spacing='xs')
  
  output$compPlotUpload <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Total Cost ($)\n")+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compStdUploadCumulative,color='Standard'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compIAUploadCumulative,color='Infrequent Access'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compOIAUploadCumulative,color='One Zone-IA'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compGlacierUploadCumulative,color='Glacier'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compDAUploadCumulative,color='Glacier-Deep Archive'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$compMonths,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Standard"="#CE5858","Infrequent Access"="#419222","One Zone-IA"="#136F63","Glacier"="#3F88C5","Glacier-Deep Archive"="#93748A"))+
      labs(color='')
    
  })
  
  output$compTableUpload <- renderTable({
    names(vals$compUploadDf) <- compHeaders
    vals$compUploadDf
  },align='c',spacing='xs')
  
  output$compPlotDataRetrieval <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Total Cost ($)\n")+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compStdDataTransferCumulative,color='Standard'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compIADataTransferCumulative,color='Infrequent Access'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compOIADataTransferCumulative,color='One Zone-IA'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compGlacierDataTransferCumulative,color='Glacier'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compDADataTransferCumulative,color='Glacier-Deep Archive'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$compMonths,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Standard"="#CE5858","Infrequent Access"="#419222","One Zone-IA"="#136F63","Glacier"="#3F88C5","Glacier-Deep Archive"="#93748A"))+
      labs(color='')
    
  })
  
  output$compTableDataRetrieval <- renderTable({
    names(vals$compDataRetrievalDf) <- compHeaders
    vals$compDataRetrievalDf
  },align='c',spacing='xs')
  
  output$compPlotTotals <- renderPlot({
    
    qplot(xlab="\nMonth",ylab="Total Cost ($)\n")+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compStdCumulative,color='Standard'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compIACumulative,color='Infrequent Access'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compOIACumulative,color='One Zone-IA'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compGlacierCumulative,color='Glacier'),size=I(2))+
      geom_line(aes(x=c(1:vals$compMonths),y=vals$compDACumulative,color='Glacier-Deep Archive'),size=I(2))+
      scale_x_continuous(breaks=seq(1,vals$compMonths,by=1))+
      theme(text=element_text(size=15))+
      theme(legend.text=element_text(size=13))+
      theme(legend.key.size=unit(2,"line"))+
      scale_color_manual(values=c("Standard"="#CE5858","Infrequent Access"="#419222","One Zone-IA"="#136F63","Glacier"="#3F88C5","Glacier-Deep Archive"="#93748A"))+
      labs(color='')
    
  })
  
  output$compTableTotals <- renderTable({
    names(vals$compTotalsDf) <- compHeaders
    vals$compTotalsDf
  },align='c',spacing='xs')
  
}
