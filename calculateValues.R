
indir <- "~/CQM/NDR/NDR/"
library(openxlsx)

# load raw data
raw <- read.csv2( file.path( indir, "RawData_allCellLines_v2.csv"), stringsAsFactors = FALSE,sep=",")
raw$Conc <- raw$Conc %>% gsub(" /.*$", "", .)
raw$Conc <- as.numeric(raw$Conc)

resultsNDR= data_frame()
resultsGR= data_frame()

cellLines <- unique(raw$Cell_Line)

for (i in 1:length(cellLines)){
  cellLineData= subset(raw,raw$Cell_Line == cellLines[i])
  medNeg= median(cellLineData$Final_reading[cellLineData$DRUG_NAME=="DMSO"]/cellLineData$Initial_reading[cellLineData$DRUG_NAME=="DMSO"])
  medPos= median(cellLineData$Final_reading[cellLineData$DRUG_NAME=="BzCl"]/cellLineData$Initial_reading[cellLineData$DRUG_NAME=="BzCl"])
  drugNames= unique(cellLineData$DRUG_NAME)
  for (j in 1:length(drugNames)){
    drug= drugNames[j]
    if (!(drug == "DMSO" | drug == "BzCl" | drug == "xBzCl" | drug == "Cytarabine/Idarubicin" )){
      drugData= subset(cellLineData,DRUG_NAME == drug)
      drugData= drugData[order(drugData$Conc),]
      drugRatio= drugData$Final_reading/drugData$Initial_reading
      
      NDRvalues= pmax(-1,(1 - 2 ^ (log2(drugRatio) / log2(medPos))) / (1 - 2 ^ (log2(medNeg) / log2(medPos))))
      GRvalues= 2 ^ (log2(drugRatio) / log2(medNeg)) - 1
      
      resultsNDR= rbind(resultsNDR, cbind(data.frame(Cell_line= drugData$Cell_Line[1], Drug= drugData$DRUG_NAME[1]),t(NDRvalues)))
      
      resultsGR= rbind(resultsGR, cbind(data.frame(Cell_line= drugData$Cell_Line[1], Drug= drugData$DRUG_NAME[1]),t(GRvalues)))
    }
    
  }
}

colnames(resultsNDR)[3:7] <- c("Conc.1","Conc.2","Conc.3","Conc.4","Conc.5")
colnames(resultsGR)[3:7] <- c("Conc.1","Conc.2","Conc.3","Conc.4","Conc.5")

write.xlsx(resultsNDR, file = "NDRvalues_allCellLines.xlsx", colNames = TRUE)
write.xlsx(resultsGR, file = "GRvalues_allCellLines.xlsx", colNames = TRUE)