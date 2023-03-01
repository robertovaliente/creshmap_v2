#ALCOHOL AND TOBACCO CRESHMAP##

# This is a Shiny web application. 
# UI and server have been coded together in this script
# You'll need to run the entire script to build the UI and server and load databases. 
# To see the application, please, run command "ShinyApp(ui, server)" at the end of the script. The "Run App" botton may produce error.

#Last data modification: 01/03/2023.


# 1. Setting up coding ####
#setwd("Set/Local/Path")

#Installing libraries:
library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
require(RCurl)
require(RJSONIO)
require(plyr)
library(BAMMtools)
library(rgeos)
library(dplyr)
library(jsonlite)
library(mongolite)
library(DT)
library(leaflet.extras)
library(sweetalertR)
library(readxl)
library(sf)
library(stringr)
#install.packages(spplot)
#library(spplot)

#Memory checking:
memory.size() # To check the memory size (Units= Mb)
memory.limit() # To check the memory limit pre-defined in our project (Units= Mb)
memory.limit(size = 20000) #We increase the memory limit pre-defined

#----


# 2. Loading and managing databases for the App ####
#    2.1. DZ Data ####
geog_dz <- st_read("geography/DZ_simd_ar_tr.shp") #Loading datazones shapefile with data.
geog_dz <- rename(geog_dz,
                  AR12_all = AR12_All_P,
                  AR12_on = AR12_On_Po, 
                  AR12_off = AR12_Off_P,
                  TR12 = TR12_PopAd,
                  AR16_all = AR16_All_P,
                  AR16_on = AR16_On_Po,
                  AR16_off = AR16_Off_P,
                  TR16 = TR16_PopAd,
                  AR20_all = AR20_All_P,
                  AR20_on = AR20_On_Po,
                  AR20_off = AR20_Off_P,
                  TR20 = TR20_PopAd) #Rename column names in DZ shp to avoid problems.
#Defining NA data (3 datazones in Glasgow city for AR/TR densities 2020):
#geog_dz[geog_dz == -9999] <- NA #Gives error:unsupported matrix index in replacement
geog_dz$AR20_all[geog_dz$AR20_all == -9999] <- NA
geog_dz$AR20_on[geog_dz$AR20_on == -9999] <- NA
geog_dz$AR20_off[geog_dz$AR20_off == -9999] <- NA
geog_dz$TR20[geog_dz$TR20 == -9999] <- NA
geog_dz$AR16_all[geog_dz$AR16_all == -9999] <- NA
geog_dz$AR16_on[geog_dz$AR16_on == -9999] <- NA
geog_dz$AR16_off[geog_dz$AR16_off == -9999] <- NA
geog_dz$TR16[geog_dz$TR16 == -9999] <- NA
geog_dz$PopDen_201[geog_dz$PopDen_201 == -9999] <- NA
geog_dz$PopDen_201[geog_dz$PopDen_202 == -9999] <- NA

#Setting categories of classification to display data in the map - That's very ugly, but some issues arise due to NAs in some columns in geog_dz when applying the palette function to colour the map: 'Error - breaks are not unique'.
quantile(geog_dz$AR20_all, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Quartiles
geog_dz$AR20_all_q[geog_dz$AR20_all <= 0] <- 1
geog_dz$AR20_all_q[geog_dz$AR20_all > 0 & geog_dz$AR20_all <= 1.433] <- 2
geog_dz$AR20_all_q[geog_dz$AR20_all > 1.433 & geog_dz$AR20_all <= 3.704] <- 3
geog_dz$AR20_all_q[geog_dz$AR20_all > 3.704] <- 4
geog_dz$AR20_all_q[is.na(geog_dz$AR20_all)] <- 5
class(geog_dz$AR20_all_q) #Character
geog_dz$AR20_all_q <- as.factor(geog_dz$AR20_all_q) #to factor
table(geog_dz$AR20_all_q)


#Uploading DZ from 2016 and 2012:
geog_dz_16 <- read.csv("data/simd2016_withinds.csv") #Loading 2016 SIMD data by datazones.
names(geog_dz_16) = c(
    "DataZone_2016", "Intermediate_Zone_2016", "LAName_2016",                    
    "Total_population_2016", "Working_age_population_2016", "Rankv2_2016",               
    "Percentilev2_2016", "Vigintilv2_2016", "Decilev2_2016", "Quintilev2_2016", 
    "IncRankv2_2016", "EmpRank_2016", "HlthRank_2016", "EduRank_2016", "GAccRank_2016",
    "CrimeRank_2016", "HouseRank_2016", "IncRate_2016", "IncNumDep_2016", 
    "EmpRate_2016", "EmpNumDep_2016",
    "HlthCIF_2016", "HlthAlcSR_2016", "HlthDrugSR_2016", "HlthSMR_2016", "HlthDprsPc_2016", "HlthLBWTPc_2016", "HlthEmerSR_2016", 
    "EduAttend_2016", "EduAttain_2016", "EduNoQuals_2016", "EduNotPart_2016", "EduUniver_2016",
    "GAccPetrol_2016", "GAccDTGP_2016", "GAccDTPost_2016", "GAccDTPsch_2016", "GAccDTRet_2016", "GAccDTSsch_2016", "GAccPTGP_2016", "GAccPTPost_2016", "GAccPTRet_2016",
    "crimeRate_2016",
    "HouseNumOC_2016", "HouseNumNC_2016", "HouseOCrat_2016", "HouseNCrat_2016" 
)#Changing/Customizing names to columns for homogenisation.
geog_dz <- merge(geog_dz, geog_dz_16, by.x="DataZone", by.y="DataZone_2016", all.x = TRUE)#Merging geog_dz 2020 and 2016 (they have the same number of DZs - 2012 data should be in another shapefile).
remove(geog_dz_16)


simd_dz_12 <- read.csv("data/simd2012_data_00410767_plusintervals.csv") #Loading 2012 SIMD data by datazones.
simd_dz_12 <- simd_dz_12[, 1:28]
geog_dz_12 <- st_read("geography/DZ_2011_EoR_Scotland.shp")
geog_dz_12 <- merge(geog_dz_12, simd_dz_12, by.x="DZ_CODE", by.y="Data.Zone", all.x = TRUE)
remove(simd_dz_12)
names(geog_dz_12) = c(
    "DataZone_2012",                                                       
    "DZ_NAME_2012",                                                            
    "DZ_GAELIC_2012",                                                           
    "CouncilArea",                                                          
    "Intermedia",                                                          
    "CouncilA_2",                                                          
    "NRSCouncil",                                                          
    "LACode",                                                
    "LAName_2012",                                                
    "Intermediate_Geography_Name",                                         
    "Nearest_settlement.",                                        
    "Total_Population_2012",                                        
    "Working_Age_Population_2012",
    "Overall_SIMD_score_2012",
    "Rankv2_2012",                                             
    "IncRate_2012",                                         
    "IncNumDep_2012",                               
    "IncRankv2_2012",                              
    "EmpRate_2012",                                     
    "EmpNumDep_2012",                           
    "EmpRank_2012",                          
    "Health.domain.2012.score",                                        
    "HlthRank_2012",                                           
    "EduScore_2012",                
    "EduRank_2012",                   
    "Housing_score_2012",                    
    "HouseRank_2012",                       
    "Geographic.Access.domain.2012.score",                        
    "GAccRank_2012",                                
    "Crime.2012.score",                                 
    "CrimeRAnK_2012",                                              
    "Decilev2_2012",                                               
    "Quintilev2_2012",                                                            
    "Vigintilv2_2012",                                                           
    "geometry"
) #Changing/Customizing names to columns for homogenisation.

#Converting some factor variables to numeric:
geog_dz$IncRate2 <- as.character(geog_dz$IncRate) #Replicating column as character
geog_dz$IncRate2 <- str_sub(geog_dz$IncRate2, 1, str_length(geog_dz$IncRate2)-1) #Removing % symbol
geog_dz$IncRate2 <- as.numeric(geog_dz$IncRate2) #Converting to numeric format column
geog_dz$IncRate <- geog_dz$IncRate2 #Replacing data
geog_dz <- select(geog_dz, -IncRate2)#Removing duplicate column.

geog_dz$IncRate2_2016 <- as.character(geog_dz$IncRate_2016) #Replicating column as character
geog_dz$IncRate2_2016 <- str_sub(geog_dz$IncRate2_2016, 1, str_length(geog_dz$IncRate2_2016)-1) #Removing % symbol
geog_dz$IncRate2_2016 <- as.numeric(geog_dz$IncRate2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$IncRate_2016 <- geog_dz$IncRate2_2016 #Replacing data
geog_dz <- select(geog_dz, -IncRate2_2016)#Removing duplicate column.


geog_dz$EmpRate2 <- as.character(geog_dz$EmpRate) #Replicating column as character
geog_dz$EmpRate2 <- str_sub(geog_dz$EmpRate2, 1, str_length(geog_dz$EmpRate2)-1) #Removing % symbol
geog_dz$EmpRate2 <- as.numeric(geog_dz$EmpRate2) #Converting to numeric format column
geog_dz$EmpRate <- geog_dz$EmpRate2 #Replacing data
geog_dz <- select(geog_dz, -EmpRate2)#Removing duplicate column.

geog_dz$EmpRate2_2016 <- as.character(geog_dz$EmpRate_2016) #Replicating column as character
geog_dz$EmpRate2_2016 <- str_sub(geog_dz$EmpRate2_2016, 1, str_length(geog_dz$EmpRate2_2016)-1) #Removing % symbol
geog_dz$EmpRate2_2016 <- as.numeric(geog_dz$EmpRate2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$EmpRate_2016 <- geog_dz$EmpRate2_2016 #Replacing data
geog_dz <- select(geog_dz, -EmpRate2_2016)#Removing duplicate column.


geog_dz$HlthDprsPc2 <- as.character(geog_dz$HlthDprsPc) #Replicating column as character
geog_dz$HlthDprsPc2 <- str_sub(geog_dz$HlthDprsPc2, 1, str_length(geog_dz$HlthDprsPc2)-1) #Removing % symbol
geog_dz$HlthDprsPc2 <- as.numeric(geog_dz$HlthDprsPc2) #Converting to numeric format column
geog_dz$HlthDprsPc <- geog_dz$HlthDprsPc2 #Replacing data
geog_dz <- select(geog_dz, -HlthDprsPc2)#Removing duplicate column.

geog_dz$HlthDprsPc2_2016 <- as.character(geog_dz$HlthDprsPc_2016) #Replicating column as character
geog_dz$HlthDprsPc2_2016 <- str_sub(geog_dz$HlthDprsPc2_2016, 1, str_length(geog_dz$HlthDprsPc2_2016)-1) #Removing % symbol
geog_dz$HlthDprsPc2_2016 <- as.numeric(geog_dz$HlthDprsPc2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$HlthDprsPc_2016 <- geog_dz$HlthDprsPc2_2016 #Replacing data
geog_dz <- select(geog_dz, -HlthDprsPc2_2016)#Removing duplicate column.

geog_dz$HlthLBWTPc2 <- as.character(geog_dz$HlthLBWTPc) #Replicating column as character
geog_dz$HlthLBWTPc2 <- str_sub(geog_dz$HlthLBWTPc2, 1, str_length(geog_dz$HlthLBWTPc2)-1) #Removing % symbol
geog_dz$HlthLBWTPc2 <- as.numeric(geog_dz$HlthLBWTPc2) #Converting to numeric format column
geog_dz$HlthLBWTPc <- geog_dz$HlthLBWTPc2 #Replacing data
geog_dz <- select(geog_dz, -HlthLBWTPc2)#Removing duplicate column.

geog_dz$HlthLBWTPc2_2016 <- as.character(geog_dz$HlthLBWTPc_2016) #Replicating column as character
geog_dz$HlthLBWTPc2_2016 <- str_sub(geog_dz$HlthLBWTPc2_2016, 1, str_length(geog_dz$HlthLBWTPc2_2016)-1) #Removing % symbol
geog_dz$HlthLBWTPc2_2016 <- as.numeric(geog_dz$HlthLBWTPc2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$HlthLBWTPc_2016 <- geog_dz$HlthLBWTPc2_2016 #Replacing data
geog_dz <- select(geog_dz, -HlthLBWTPc2_2016)#Removing duplicate column.


geog_dz$EduAttend2 <- as.character(geog_dz$EduAttend) #Replicating column as character
geog_dz$EduAttend2 <- str_sub(geog_dz$EduAttend2, 1, str_length(geog_dz$EduAttend2)-1) #Removing % symbol
geog_dz$EduAttend2 <- as.numeric(geog_dz$EduAttend2) #Converting to numeric format column
geog_dz$EduAttend <- geog_dz$EduAttend2 #Replacing data
geog_dz <- select(geog_dz, -EduAttend2)#Removing duplicate column.

geog_dz$EduAttend2_2016 <- as.character(geog_dz$EduAttend_2016) #Replicating column as character
geog_dz$EduAttend2_2016 <- str_sub(geog_dz$EduAttend2_2016, 1, str_length(geog_dz$EduAttend2_2016)-1) #Removing % symbol
geog_dz$EduAttend2_2016 <- as.numeric(geog_dz$EduAttend2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$EduAttend_2016 <- geog_dz$EduAttend2_2016 #Replacing data
geog_dz <- select(geog_dz, -EduAttend2_2016)#Removing duplicate column.

geog_dz$EduNotPart2 <- as.character(geog_dz$EduNotPart) #Replicating column as character
geog_dz$EduNotPart2 <- str_sub(geog_dz$EduNotPart2, 1, str_length(geog_dz$EduNotPart2)-1) #Removing % symbol
geog_dz$EduNotPart2 <- as.numeric(geog_dz$EduNotPart2) #Converting to numeric format column
geog_dz$EduNotPart <- geog_dz$EduNotPart2 #Replacing data
geog_dz <- select(geog_dz, -EduNotPart2)#Removing duplicate column.

geog_dz$EduNotPart2_2016 <- as.character(geog_dz$EduNotPart_2016) #Replicating column as character
geog_dz$EduNotPart2_2016 <- str_sub(geog_dz$EduNotPart2_2016, 1, str_length(geog_dz$EduNotPart2_2016)-1) #Removing % symbol
geog_dz$EduNotPart2_2016 <- as.numeric(geog_dz$EduNotPart2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$EduNotPart_2016 <- geog_dz$EduNotPart2_2016 #Replacing data
geog_dz <- select(geog_dz, -EduNotPart2_2016)#Removing duplicate column.

geog_dz$EduUniver2 <- as.character(geog_dz$EduUniver) #Replicating column as character
geog_dz$EduUniver2 <- str_sub(geog_dz$EduUniver2, 1, str_length(geog_dz$EduUniver2)-1) #Removing % symbol
geog_dz$EduUniver2 <- as.numeric(geog_dz$EduUniver2) #Converting to numeric format column
geog_dz$EduUniver <- geog_dz$EduUniver2 #Replacing data
geog_dz <- select(geog_dz, -EduUniver2)#Removing duplicate column.

geog_dz$EduUniver2_2016 <- as.character(geog_dz$EduUniver_2016) #Replicating column as character
geog_dz$EduUniver2_2016 <- str_sub(geog_dz$EduUniver2_2016, 1, str_length(geog_dz$EduUniver2_2016)-1) #Removing % symbol
geog_dz$EduUniver2_2016 <- as.numeric(geog_dz$EduUniver2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$EduUniver_2016 <- geog_dz$EduUniver2_2016 #Replacing data
geog_dz <- select(geog_dz, -EduUniver2_2016)#Removing duplicate column.

geog_dz$GAccBrdbnd2 <- as.character(geog_dz$GAccBrdbnd) #Replicating column as character
geog_dz$GAccBrdbnd2 <- str_sub(geog_dz$GAccBrdbnd2, 1, str_length(geog_dz$GAccBrdbnd2)-1) #Removing % symbol
geog_dz$GAccBrdbnd2 <- as.numeric(geog_dz$GAccBrdbnd2) #Converting to numeric format column
geog_dz$GAccBrdbnd <- geog_dz$GAccBrdbnd2 #Replacing data
geog_dz <- select(geog_dz, -GAccBrdbnd2)#Removing duplicate column.

geog_dz$crimeRate2_2016 <- as.character(geog_dz$crimeRate_2016) #Replicating column as character
geog_dz$crimeRate2_2016 <- str_sub(geog_dz$crimeRate2_2016, 1, str_length(geog_dz$crimeRate2_2016)-1) #Removing % symbol
geog_dz$crimeRate2_2016 <- as.numeric(geog_dz$crimeRate2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$crimeRate_2016 <- geog_dz$crimeRate2_2016 #Replacing data
geog_dz <- select(geog_dz, -crimeRate2_2016)#Removing duplicate column.

geog_dz$HouseOCrat2 <- as.character(geog_dz$HouseOCrat) #Replicating column as character
geog_dz$HouseOCrat2 <- str_sub(geog_dz$HouseOCrat2, 1, str_length(geog_dz$HouseOCrat2)-1) #Removing % symbol
geog_dz$HouseOCrat2 <- as.numeric(geog_dz$HouseOCrat2) #Converting to numeric format column
geog_dz$HouseOCrat <- geog_dz$HouseOCrat2 #Replacing data
geog_dz <- select(geog_dz, -HouseOCrat2)#Removing duplicate column.

geog_dz$HouseOCrat2_2016 <- as.character(geog_dz$HouseOCrat_2016) #Replicating column as character
geog_dz$HouseOCrat2_2016 <- str_sub(geog_dz$HouseOCrat2_2016, 1, str_length(geog_dz$HouseOCrat2_2016)-1) #Removing % symbol
geog_dz$HouseOCrat2_2016 <- as.numeric(geog_dz$HouseOCrat2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$HouseOCrat_2016 <- geog_dz$HouseOCrat2_2016 #Replacing data
geog_dz <- select(geog_dz, -HouseOCrat2_2016)#Removing duplicate column.

geog_dz$HouseNCrat2 <- as.character(geog_dz$HouseNCrat) #Replicating column as character
geog_dz$HouseNCrat2 <- str_sub(geog_dz$HouseNCrat2, 1, str_length(geog_dz$HouseNCrat2)-1) #Removing % symbol
geog_dz$HouseNCrat2 <- as.numeric(geog_dz$HouseNCrat2) #Converting to numeric format column
geog_dz$HouseNCrat <- geog_dz$HouseNCrat2 #Replacing data
geog_dz <- select(geog_dz, -HouseNCrat2)#Removing duplicate column.

geog_dz$HouseNCrat2_2016 <- as.character(geog_dz$HouseNCrat_2016) #Replicating column as character
geog_dz$HouseNCrat2_2016 <- str_sub(geog_dz$HouseNCrat2_2016, 1, str_length(geog_dz$HouseNCrat2_2016)-1) #Removing % symbol
geog_dz$HouseNCrat2_2016 <- as.numeric(geog_dz$HouseNCrat2_2016) #Converting to numeric format column - THERE ARE NA's.
geog_dz$HouseNCrat_2016 <- geog_dz$HouseNCrat2_2016 #Replacing data
geog_dz <- select(geog_dz, -HouseNCrat2_2016)#Removing duplicate column.

geog_dz$HlthCIF_2016 <- as.numeric(geog_dz$HlthCIF_2016)
geog_dz$HlthAlcSR_2016 <- as.numeric(geog_dz$HlthAlcSR_2016)
geog_dz_12$Rankv2_2012 <- as.numeric(geog_dz_12$Rankv2_2012)
geog_dz_12$IncRankv2_2012 <- as.numeric(geog_dz_12$IncRankv2_2012)
geog_dz_12$EmpRank_2012 <- as.numeric(geog_dz_12$EmpRank_2012)
geog_dz_12$HlthRank_2012 <- as.numeric(geog_dz_12$HlthRank_2012)
geog_dz_12$EduRank_2012 <- as.numeric(geog_dz_12$EduRank_2012)
geog_dz_12$GAccRank_2012 <- as.numeric(geog_dz_12$GAccRank_2012)
geog_dz_12$CrimeRAnK_2012 <- as.numeric(geog_dz_12$CrimeRAnK_2012)
geog_dz_12$HouseRank_2012 <- as.numeric(geog_dz_12$HouseRank_2012)



#    2.2. LA data ####
geog_la <- st_read("geography/LA_ar_tr.shp") #Loading LA shapefile with data.
geog_la <- rename(geog_la,
                  Av_AR12_all = Ave_AR12_A,
                  Av_AR12_on = Ave_AR12_O, 
                  Av_AR12_off = Ave_AR12_1,
                  Av_TR12 = Ave_TR12_P,
                  Av_AR16_all = Ave_AR16_A,
                  Av_AR16_on = Ave_AR16_O,
                  Av_AR16_off = Ave_AR16_1,
                  Av_TR16 = Ave_TR16_P,
                  Av_AR20_all = Ave_AR20_A,
                  Av_AR20_on = Ave_AR20_O,
                  Av_AR20_off = Ave_AR20_1,
                  Av_TR20 = Ave_TR20_P
                  ) #Rename column names in LA shp to avoid problems.


#Estimating LA population data:
a <- data.frame(geog_dz)
pop <- group_by(a, LAName) %>%
    summarise(
        "MidYr2012D" = sum(MidYr2012D),
        "MidYr2016D" = sum(MidYr2016D),
        "MidYr2020D" = sum(MidYr2020D)
    )
remove(a)
pop$LAName <- as.character(pop$LAName)
pop$LAName[pop$LAName == "Na h-Eileanan an Iar"] <- "Eilean Siar" #Homogenizing names.

geog_la <- merge(geog_la, pop, by = "LAName", all.x = TRUE)

remove(pop)

#----


# 3. Functions ####
#Copied from Mark's code:
textInputRow<-function (inputId, label, value = "") 
{
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

BING <- function(str){
    u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=Apo4HssxpmYvVbDEUA464pmX5Y30xsQNlJ4pES6Z6D056puS63D90MLZlQ1yVeTG"))
    d <- getURL(u)
    j <- RJSONIO::fromJSON(d,simplify = FALSE) 
    if (j$resourceSets[[1]]$estimatedTotal > 0) {
        lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
        lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
    }
    else {    
        lat <- lng <- NA
    }
    data<-c(lat,lng)
    data[3]<-"BING"
    return(data)
}  


simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}


sp.na.omit <- function(x, margin=2) {
    if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
        stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
    na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
    if(margin == 1) {  
        cat("DELETING ROWS: ", na.index, "\n") 
        return( x[-na.index,]  ) 
    }
    if(margin == 2) {  
        cat("DELETING COLUMNS: ", na.index, "\n") 
        return( x[,-na.index]  ) 
    }
}
#----


# 4. Define UI for creshmap: ####
ui <- fluidPage(

    useShinyjs(),  # Include shinyjs
    
    # geolocation
    tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }
              
              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
              '),
    
    tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
    
    tags$head(
        tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))),
    
    headerPanel(
        fluidRow(
            column(11, h1("Alcohol and Tobacco Environments in Scotland", 
                          style = "font-family: 'Roboto Slab', cursive;
     font-weight: bold; font-size: 39px")),
            column(1, offset=-1, tags$a(href="https://cresh.org.uk/",img(height = 79.5225, width = 78.384, src = "https://pbs.twimg.com/profile_images/2668855659/bb74b7d787ae29cfba996bdc74dbfddb_400x400.png"))) #PROBLEMS WITH SRC HERE - NO LOAD IMAGE FROM WWW FOLDER
        ), 
        windowTitle = "Alcohol and Tobacco Environments in Scotland"), 
    
    ### CRESH favicon
    tags$head(tags$link(rel = "shortcut icon", href="http://www.iconj.com/ico/g/g/ggtzbwew2b.ico", type="image/x-icon")), #href="http://www.iconj.com/ico/g/g/ggtzbwew2b.ico"
    tags$head(tags$style("#summary{
                      position: relative;
                      display: inline-block;
                       width: 20%;
                       height: 10%;
                       top: 10px;
                       padding: 10% 0;
                       border-radius:50%;
                       line-height:0;
                       /* further display options */
                       @shadow: rgba(0, 0, 0, .1);
                       @shadow-length: 4px;
                       -webkit-box-shadow: 0 @shadow-length 0 0 @shadow;
                       box-shadow: 0 @shadow-length 0 0 @shadow;
                       text-shadow: 0 @shadow-length 0 @shadow;
                       background: #428bca;
                       color: white;
                       font-family: Helvetica, Arial Black, sans;
                       font-size: 24px;
                       text-align: center;
                       }"
                         
    )),
    
    sidebarPanel( 
        strong("Description"),
        helpText("This application allows you to map the number of alcohol and tobacco outlets for small neighbourhoods across Scotland. Further information is available from the 'About' tab on the right hand side."),
                   
        bsTooltip("geounit", "You are able to choose among 2 types of geographies: Local Authorities and Datazones. There are 32 Local Authorities around Scotland so they will help you to analyses a broad picture on how the alcohol and tobacco environments may change around the country. In contrast, Datazones represent our smallest neighbourhood units.
        There are nearly 7,000 datazones around Scotland. By selecting datazones, please be patient, this will result in longer loading times due to the level of data detailing.", "top"
                  ),
        selectInput("geounit", "Select Geography:",
                    list("Local Authorities" = "lauthority",
                         "Datazones" = "datazones"),
                    selected = "lauthority"),
        

        bsTooltip("datatype", "Select the data that you want to display. In the list below you are able to choose among Alcohol/Tobacco outlet densities, neighbourhood socioeconomic deprivation, and health-related data. A definition of each variable and further notes for its interpretation will be displayed in the aside panel.", "top"
        ),
        selectInput("datatype", "Select Variable:",
                    list("Alcohol All Outlets" = "alcoholTOTAL",
                      "Alcohol On-premise Outlets" = "alcoholOn",
                      "Alcohol Off-premise Outlets" = "alcoholOff",
                      "Tobacco All Outlets" = "tobaccoTOTAL",
                      "Total Population" = "MidYrPopD",
                      "Overall deprivation rank" = "overallSIMD",
                      "Income deprivation rank" = "IncRankv2",
                      "People income deprived" = "IncRate", 
                      "Employment deprivation Rank" = "EmpRank",
                      "People employment deprived" = "EmpRate",
                      "Health deprivation rank" = "HlthRank",
                      "Comparative Illness factor" = "HlthCIFSR",
                      "Alcohol hospitalizations" = "HlthAlcSR", 
                      "Drug hospitalizations" = "HlthDrugSR",
                      "Mortality" = "HlthSMR",
                      "Drugs prescriptions" = "HlthDprsPc",
                      "Low birth weight" = "HlthLBWTPc",
                      "Hospital emergency Stays" = "HlthEmerSR",
                      "Education deprivation rank" = "EduRank",
                      "School pupils attendance" = "EduAttend",
                      #"School leavers attainment" = "EduAttain",
                      "People with no qualifications" = "EduNoQuals",
                      "No School/work adolescents" = "EduNotPart",
                      "Adolescents in University" = "EduUniver",
                      "Access domain Rank" = "GAccRank",
                      "Drive time to petrol station" = "GAccPetrol",
                      "Drive time to GP" = "GAccDTGP",
                      "Drive time to post office" = "GAccDTPost",
                      "Drive time to primary school" = "GAccDTPsch",
                      "Drive time to secondary school" = "GAccDTSsch",
                      "Drive time to retail centre" = "GAccDTRet",
                      "Public transport time to GP" = "GAccPTGP",
                      "Public transport time to post office" = "GAccPTPost",
                      "Public transport time to retail centre" = "GAccPTRet",
                      "Premises without superfast broadband" = "GAccBrdbnd",
                      "Crime domain Rank" = "CrimeRank",
                      #"Number of crimes" = "CrimeCount",
                      "Crimes per population ratio" = "CrimeRate",
                      "Housing domain rank" = "HouseRank",
                      #"People in overcrowded households" = "HouseNumOC",
                      "Percentage of people in overcrowded households" = "HouseOCrat",
                      #"People in no central heating households" = "HouseNumNC",
                      "Percentage of people in no central heating households" = "HouseNCrat"
                      ),
                      
                      selected="alcoholTOTAL"),
        
        selectInput("year", "Year",
                    list("2012" = "2012",
                         "2016" = "2016",
                         "2020" = "2020"),
                    selected="2020"),
        
        uiOutput("info_box")
    ),

    tags$br(),
    
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Map", leafletOutput("map")),
                    tabPanel("About", includeHTML("About_text.html")), #if section in HTML format: change uiOutput by includeHTML
                    #tabPanel("How to use", includeHTML("howtouse_text.html")), #if section in HTML format: change uiOutput by includeHTML
                    tabPanel("Download", uiOutput("download"))) #if section in HTML format: change uiOutput by includeHTML
        )
    )
#----


# 5. Define server logic required to run App ####


server <- function(input, output) {

    
    # 5.1. Checking data input projection and defining colour palettes####
    class(geog_dz) #Checking data type - if it returns sf "data.frame" you should use the function "st_transform" instead of "spTransform"
    class(geog_la)
    geog_dz <- st_transform(geog_dz, CRS("+proj=longlat +datum=WGS84")) #Setting shapefile projection
    geog_dz_12 <- st_transform(geog_dz_12, CRS("+proj=longlat +datum=WGS84")) #Setting shapefile projection
    geog_la <- st_transform(geog_la, CRS("+proj=longlat +datum=WGS84")) #Setting shapefile projection
    
    #Creating colour palettes
    #Simd variables (used for both DZ and LA):
    pal_simd_rank <- colorQuantile("YlOrBr", NULL, n=5, na.color = "grey", reverse = TRUE)#This is good example for simd ranks - no error with complete data (no NAs in column).
    pal_simd_quant <- colorQuantile("YlOrBr", NULL, n=5, na.color = "grey")#This is good example for simd ranks - no error with complete data (no NAs in column).
    
    
    {#LA palettes (customised by variable - I know this is ugly but I didn't find another effective way to do this)::
    pal_la_AR20_all <- colorBin("Greens", NULL, bins = c(0, 2.491, 2.902, 3.967, 7.300), na.color = "grey") #Quartiles
    pal_la_AR20_on <- colorBin("Greens", NULL, bins = c(0, 1.531, 1.955, 2.998, 5.200), na.color = "grey") #Quartiles
    pal_la_AR20_off <- colorBin("Greens", NULL, bins = c(0, 0.854, 0.973, 1.059, 2.100), na.color = "grey") #Quartiles
    pal_la_TR20 <- colorBin("Oranges", NULL, bins = c(0, 1.394, 1.500, 1.862, 3.500), na.color = "grey") #Quartiles
    
    pal_la_AR16_all <- colorBin("Greens", NULL, bins = c(0, 2.368, 2.887, 3.803, 7.100), na.color = "grey") #Quartiles
    pal_la_AR16_on <- colorBin("Greens", NULL, bins = c(0, 1.497, 1.923, 2.745, 5.300), na.color = "grey") #Quartiles
    pal_la_AR16_off <- colorBin("Greens", NULL, bins = c(0, 0.855, 0.9391, 1.066, 2.100), na.color = "grey") #Quartiles
    pal_la_TR16 <- colorBin("Oranges", NULL, bins = c(0, 1.492, 1.656, 1.826, 3.700), na.color = "grey") #Quartiles
    
    pal_la_AR12_all <- colorBin("Greens", NULL, bins = c(0, 2.400, 2.883, 3.752, 6.900), na.color = "grey") #Quartiles
    pal_la_AR12_on <- colorBin("Greens", NULL, bins = c(0, 1.530, 1.987, 2.700, 5.100), na.color = "grey") #Quartiles
    pal_la_AR12_off <- colorBin("Greens", NULL, bins = c(0, 0.826, 0.893, 1.045, 1.800), na.color = "grey") #Quartiles
    pal_la_TR12 <- colorBin("Oranges", NULL, bins = c(0, 1.654, 1.870, 2.178, 3.900), na.color = "grey") #Quartiles
    
    quantile(geog_la$MidYr2020D, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    pal_MidYr20_la <- colorBin("YlOrBr", NULL, bins = c(0, 89302, 110106, 148608, 234156, 635641), na.color = "grey")
    quantile(geog_la$MidYr2016D, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    pal_MidYr16_la <- colorBin("YlOrBr", NULL, bins = c(0, 88860, 109512, 149020, 233784, 615071), na.color = "grey")
    quantile(geog_la$MidYr2012D, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    pal_MidYr12_la <- colorBin("YlOrBr", NULL, bins = c(0, 87596, 108696, 147764, 231294, 595070), na.color = "grey")
    
    }#LA palettes
    
    
    {#DZ palettes (customised by variable - I know this is ugly but I didn't find another effective way to do this):
    #2020 DZ variables:
    #SIMD:
    quantile(geog_dz$MidYr2020D, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    pal_MidYr20 <- colorBin("YlOrBr", NULL, bins = c(0, 630, 749, 784, 883, 3884), na.color = "grey")
    quantile(geog_dz$IncRate, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_incrate <- colorBin("YlOrBr", NULL, bins = c(0, 4, 10, 18, 60), na.color = "grey")
    quantile(geog_dz$EmpRate, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_emprate <- colorBin("YlOrBr", NULL, bins = c(0, 4, 8, 14, 48), na.color = "grey")
    
    #quantile(geog_dz$HlthCIF, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_hlthcif <- colorBin("RdBu", NULL, bins = c(0, 50, 100, 150, 371), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthAlcSR, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_hlthalc <- colorBin("RdBu", NULL, bins = c(0, 35, 74, 138, 1657), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthDrugSR, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
    pal_hlthdrug <- colorBin("RdBu", NULL, bins = c(0, 23, 93, 1827), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthSMR, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    pal_hlthsmr <- colorBin("RdBu", NULL, bins = c(0, 70, 93, 120, 824), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthDprsPc, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    pal_hlthdprs <- colorBin("YlOrBr", NULL, bins = c(0, 15, 19, 23, 48), na.color = "grey")
    quantile(geog_dz$HlthLBWTPc, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
    pal_hlthlbw <- colorBin("YlOrBr", NULL, bins = c(0, 3, 7, 51), na.color = "grey")
    quantile(geog_dz$HlthEmergS, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    pal_hlthemer <- colorBin("RdBu", NULL, bins = c(0, 74, 95, 123, 400), na.color = "grey", reverse = TRUE)
    
    quantile(geog_dz$EduAttend, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_eduattend <- colorBin("YlOrBr", NULL, bins = c(0, 72, 81, 87, 100), na.color = "grey")
    quantile(geog_dz$EduNoQuals, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_edunoquals <- colorBin("YlOrBr", NULL, bins = c(3, 58, 93, 140, 354), na.color = "grey")
    quantile(geog_dz$EduNotPart, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_edunotpart <- colorBin("YlOrBr", NULL, bins = c(0, 2, 5, 10, 50), na.color = "grey")
    quantile(geog_dz$EduUniver, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_eduuniver <- colorBin("YlOrBr", NULL, bins = c(0, 5, 8, 12, 83), na.color = "grey")
    
    quantile(geog_dz$GAccPetrol, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccpetrol <- colorBin("YlOrBr", NULL, bins = c(0, 2.29, 3.17, 4.42, 65.00), na.color = "grey")
    quantile(geog_dz$GAccDTGP, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtgp <- colorBin("YlOrBr", NULL, bins = c(0, 2.02, 2.90, 4.21, 90.00), na.color = "grey")
    quantile(geog_dz$GAccDTPost, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtpost <- colorBin("YlOrBr", NULL, bins = c(0, 1.81, 2.54, 3.47, 24.00), na.color = "grey")
    quantile(geog_dz$GAccDTPsch, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtpsch <- colorBin("YlOrBr", NULL, bins = c(0, 2.22, 2.80, 3.53, 30.00), na.color = "grey")
    quantile(geog_dz$GAccDTSsch, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtssch <- colorBin("YlOrBr", NULL, bins = c(0, 3.63, 4.89, 6.82, 120.00), na.color = "grey")
    quantile(geog_dz$GAccDTRet, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtret <- colorBin("YlOrBr", NULL, bins = c(0, 2.88, 4.10, 6.04, 191.00), na.color = "grey")
    quantile(geog_dz$GAccPTGP, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccptgp <- colorBin("YlOrBr", NULL, bins = c(0, 6.43, 8.96, 12.65, 120.00), na.color = "grey")
    quantile(geog_dz$GAccPTPost, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccptpost <- colorBin("YlOrBr", NULL, bins = c(0, 5.72, 7.72, 10.62, 42.00), na.color = "grey")
    quantile(geog_dz$GAccPTRet, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccptret <- colorBin("YlOrBr", NULL, bins = c(0, 7.98, 11.25, 16.06, 191.00), na.color = "grey")
    quantile(geog_dz$GAccBrdbnd, probs = c(0, 0.33, 0.66, 1)) 
    getJenksBreaks(geog_dz$GAccBrdbnd, 4) #Estimating Jenks breaks
    pal_gaccbrdbnd <- colorBin("YlOrBr", NULL, bins = c(0, 0, 15, 50, 100), na.color = "grey") #Jenks breaks using here.
    
    quantile(geog_dz$CrimeRate, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_crimerate <- colorBin("YlOrBr", NULL, bins = c(0, 94, 194, 361, 12442), na.color = "grey")
    
    quantile(geog_dz$HouseOCrat, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_houseocrat <- colorBin("YlOrBr", NULL, bins = c(0, 5, 9, 15, 58), na.color = "grey")
    quantile(geog_dz$HouseNCrat, probs = c(0, 0.33, 0.66, 1))
    getJenksBreaks(geog_dz$HouseNCrat, 4) #Estimating Jenks breaks
    pal_housencrat <- colorBin("YlOrBr", NULL, bins = c(0, 1, 6, 21), na.color = "grey")
    
    
    
    
    #AR/TR
    quantile(geog_dz$AR20_all, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Show Quartiles for column x
    pal_AR20_all <-colorBin("Greens", NULL, bins = c(0, 0.1, 1.433, 3.703, 210), na.color = "grey")
    quantile(geog_dz$AR20_on, probs = c(0.33, 0.66), na.rm = TRUE) #Show tertiles for column x
    pal_AR20_on <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.563, 194), na.color = "grey")
    quantile(geog_dz$AR20_off, probs = c(0.33, 0.66), na.rm = TRUE) #Show tertiles for column x
    pal_AR20_off <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.21, 30), na.color = "grey")
    quantile(geog_dz$TR20, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Show tertiles for column x
    pal_TR20 <- colorBin("Oranges", NULL, bins = c(0, 0.1, 0.963, 2.273, 72), na.color = "grey")
    
    #2016 DZ variables:
    #SIMD:
    quantile(geog_dz$MidYr2016D, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    pal_MidYr16 <- colorBin("YlOrBr", NULL, bins = c(0, 608, 710, 804, 918, 3560), na.color = "grey")
    quantile(geog_dz$IncRate_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_incrate_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 5, 10, 18, 74), na.color = "grey")
    quantile(geog_dz$EmpRate_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_emprate_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 5, 9, 16, 54), na.color = "grey")
    
    quantile(geog_dz$HlthCIF_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_hlthcif_2016 <- colorBin("RdBu", NULL, bins = c(0, 14, 47, 70, 77), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthAlcSR_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_hlthalc_2016 <- colorBin("RdBu", NULL, bins = c(0, 75, 215, 416, 483), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthDrugSR_2016, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
    pal_hlthdrug_2016 <- colorBin("RdBu", NULL, bins = c(0, 0, 85, 1865), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthSMR_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    pal_hlthsmr_2016 <- colorBin("RdBu", NULL, bins = c(0, 71, 93, 121, 951), na.color = "grey", reverse = TRUE)
    quantile(geog_dz$HlthDprsPc_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    pal_hlthdprs_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 14, 17, 21, 43), na.color = "grey")
    quantile(geog_dz$HlthLBWTPc_2016, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
    pal_hlthlbw_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 2, 6, 41), na.color = "grey")
    quantile(geog_dz$HlthEmerSR_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    pal_hlthemer_2016 <- colorBin("RdBu", NULL, bins = c(0, 74, 95, 122, 325), na.color = "grey", reverse = TRUE)
    
    quantile(geog_dz$EduAttend_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_eduattend_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 48, 76, 84, 100), na.color = "grey")
    quantile(geog_dz$EduNoQuals_2016, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_edunoquals_2016 <- colorBin("YlOrBr", NULL, bins = c(3, 58, 93, 140, 354), na.color = "grey")
    quantile(geog_dz$EduNotPart_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_edunotpart_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 2, 5, 10, 50), na.color = "grey")
    quantile(geog_dz$EduUniver_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_eduuniver_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 4, 7, 11, 64), na.color = "grey")
    
    quantile(geog_dz$GAccPetrol_2016, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccpetrol_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 2.10, 2.95, 4.10, 64.00), na.color = "grey")
    quantile(geog_dz$GAccDTGP_2016, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtgp_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 1.90, 2.70, 3.90, 88.00), na.color = "grey")
    quantile(geog_dz$GAccDTPost_2016, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtpost_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 1.70, 2.40, 3.20, 18.00), na.color = "grey")
    quantile(geog_dz$GAccDTPsch_2016, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_gaccdtpsch_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 1.60, 2.10, 2.80, 187.00), na.color = "grey")
    quantile(geog_dz$GAccDTSsch_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_gaccdtssch_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 3.70, 4.80, 6.70, 117.00), na.color = "grey")
    quantile(geog_dz$GAccDTRet_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_gaccdtret_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 2.80, 4.00, 5.90, 191.00), na.color = "grey")
    quantile(geog_dz$GAccPTGP_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_gaccptgp_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 6.30, 8.80, 12.30, 109.00), na.color = "grey")
    quantile(geog_dz$GAccPTPost_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_gaccptpost_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 5.60, 7.50, 10.20, 41.00), na.color = "grey")
    quantile(geog_dz$GAccPTRet_2016, probs = c(0, 0.25, 0.5, 0.75, 1))
    pal_gaccptret_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 8.00, 11.20, 16.10, 191.00), na.color = "grey")
    
    quantile(geog_dz$crimeRate_2016, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)  
    pal_crimerate_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 10, 21, 39, 1458), na.color = "grey")
    
    quantile(geog_dz$HouseOCrat_2016, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_houseocrat_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 5, 9, 15, 58), na.color = "grey")
    quantile(geog_dz$HouseNCrat_2016, probs = c(0, 0.33, 0.66, 1))
    getJenksBreaks(geog_dz$HouseNCrat_2016, 4) #Estimating Jenks breaks
    pal_housencrat_2016 <- colorBin("YlOrBr", NULL, bins = c(0, 1, 6, 21), na.color = "grey")
    
    
    
    #AR/TR
    quantile(geog_dz$AR16_all, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Show Quartiles for column x
    pal_AR16_all <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.406, 3.679, 242), na.color = "grey")
    quantile(geog_dz$AR16_on, probs = c(0.33, 0.66), na.rm = TRUE) #Show tertiles for column x
    pal_AR16_on <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.524, 225), na.color = "grey")
    quantile(geog_dz$AR16_off, probs = c(0.33, 0.66), na.rm = TRUE) #Show tertiles for column x
    pal_AR16_off <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.20, 24), na.color = "grey")
    quantile(geog_dz$TR16, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Show tertiles for column x
    pal_TR16 <- colorBin("Oranges", NULL, bins = c(0, 0.1, 1.07, 2.358, 79), na.color = "grey")
    
    #2012 DZ variables
    #SIMD:
    quantile(geog_dz$MidYr2012D, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
    pal_MidYr12 <- colorBin("YlOrBr", NULL, bins = c(0, 609, 752, 761, 874, 2879), na.color = "grey")
    quantile(geog_dz_12$IncRate_2012, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_incrate_2012 <- colorBin("YlOrBr", NULL, bins = c(0, 6, 11, 20, 66), na.color = "grey")
    quantile(geog_dz_12$EmpRate_2012, probs = c(0, 0.25, 0.5, 0.75, 1))  
    pal_emprate_2012 <- colorBin("YlOrBr", NULL, bins = c(0, 6, 11, 19, 74), na.color = "grey")
    
    
    
    #AR/TR
    quantile(geog_dz$AR12_all, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Show Quartiles for column x
    pal_AR12_all <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.407, 3.656, 262), na.color = "grey")
    quantile(geog_dz$AR12_on, probs = c(0.33, 0.66), na.rm = TRUE) #Show tertiles for column x
    pal_AR12_on <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.56, 246), na.color = "grey")
    quantile(geog_dz$AR12_off, probs = c(0.33, 0.66), na.rm = TRUE) #Show tertiles for column x
    pal_AR12_off <- colorBin("Greens", NULL, bins = c(0, 0.1, 1.185, 22), na.color = "grey")
    quantile(geog_dz$TR12, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Show tertiles for column x
    pal_TR12 <- colorBin("Oranges", NULL, bins = c(0, 0.1, 1.16, 2.645, 111), na.color = "grey")
    }#DZ palettes 
    
    # 5.2. Setting reactive elements: ####
    observe({
        if(input$geounit=="lauthority"){
            #5.2.1. Defining variables at Local authority level (input$geounit==lauthority):
            #       5.2.1.1. AR/TR Data ####
            #Alcohol all:
            if(input$datatype=="alcoholTOTAL"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is where the data come:
                            addPolygons(data = geog_la, 
                                        fillColor = ~pal_la_AR20_all(Av_AR20_all), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR20_all,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR20_all, values = geog_la$Av_AR20_all, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    }) #Defining input datatype == Alcohol Total Sales
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                            "Average number of total alcohol outlets per 1,000 residents (including all on- and off-premises) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                            "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is where the data come:
                            addPolygons(data = geog_la, 
                                        fillColor = ~pal_la_AR16_all(Av_AR16_all), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR16_all,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR16_all, values = geog_la$Av_AR16_all, na.label = "NA", opacity = 0.8, title = "OUtlets per 1,000 persons", position = "bottomright")
                    }) #Defining input datatype == Alcohol Total Sales
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of total alcohol outlets per 1,000 residents (including all on- and off-premises) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is where the data come:
                            addPolygons(data = geog_la, 
                                        fillColor = ~pal_la_AR12_all(Av_AR12_all), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR12_all,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR12_all, values = geog_la$Av_AR12_all, na.label = "NA", opacity = 0.8, title = "OUtlets per 1,000 persons", position = "bottomright")
                    }) #Defining input datatype == Alcohol Total Sales
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of total alcohol outlets per 1,000 residents (including all on- and off-premises) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Alcohol on:
            else if(input$datatype=="alcoholOn"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_AR20_on(Av_AR20_on), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR20_on,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR20_on, values = geog_la$Av_AR20_on, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of alcohol on-premise outlets per 1,000 residents (outlets where you can only consume alcohol on-site) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_AR16_on(Av_AR16_on), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR16_on,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR16_on, values = geog_la$Av_AR16_on, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of alcohol on-premise outlets per 1,000 residents (outlets where you can only consume alcohol on-site) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_AR12_on(Av_AR12_on), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR12_on,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR12_on, values = geog_la$Av_AR12_on, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of alcohol on-premise outlets per 1,000 residents (outlets where you can only consume alcohol on-site) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } #Defining input datatype == Alcohol On Sales
            
            #Alcohol Off:
            else if(input$datatype=="alcoholOff"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_AR20_off(Av_AR20_off), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR20_off,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR20_off, values = geog_la$Av_AR20_off, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of alcohol off-premise outlets per 1,000 residents (outlets where can buy alcohol, but not consume it on-site) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected: 
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_AR16_off(Av_AR16_off), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR16_off,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR16_off, values = geog_la$Av_AR16_off, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of alcohol off-premise outlets per 1,000 residents (outlets where can buy alcohol, but not consume it on-site) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_AR12_off(Av_AR12_off), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_AR12_off,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_AR16_off, values = geog_la$Av_AR12_off, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of alcohol off-premise outlets per 1,000 residents (outlets where can buy alcohol, but not consume it on-site) within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } #Defining input datatype == Alcohol Off Sales
            
            #Tobacco:
            else if(input$datatype=="tobaccoTOTAL"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_TR20(Av_TR20), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_TR20,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_TR20, values = geog_la$Av_TR20, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of tobacco outlets per 1,000 residents within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_TR16(Av_TR16), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_TR20,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_TR16, values = geog_la$Av_TR16, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of tobacco outlets per 1,000 residents within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_la_TR12(Av_TR12), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$NAME,"<br/>",
                                            "<b>Value</b>: ", geog_la$Av_TR20,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_la_TR12, values = geog_la$Av_TR12, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Average number of tobacco outlets per 1,000 residents within the datazones
                             nested in a given local authority.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } #Defining input datatype == Tobacco Total Sales
            
            
            #       5.2.1.2. Population Estimates Data ####
            #Population estimates:
            else if(input$datatype=="MidYrPopD"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_MidYr20_la(MidYr2020D), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_la$MidYr2020D,"inhabitants","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_MidYr20_la, values = geog_la$MidYr2020D, na.label = "NA", opacity = 0.8, title = "Number of inhabitants", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Total population (number of all persons). 2020 Local Authority level population estimates.", 
                             "Classification method: quantiles.", "<br>",
                             "Source: National Records of Scotland (NRS).", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_MidYr16_la(MidYr2016D), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_la$MidYr2016D,"inhabitants","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_MidYr16_la, values = geog_la$MidYr2016D, na.label = "NA", opacity = 0.8, title = "Number of inhabitants", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Total population (number of all persons). 2016 Local Authority level population estimates.",
                             "Classification method: quantiles.", "<br>",
                             "Source: National Records of Scotland (NRS).", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_la,
                                        fillColor = ~pal_MidYr12_la(MidYr2012D), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_la$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_la$MidYr2012D,"inhabitants","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_MidYr12_la, values = geog_la$MidYr2012D, na.label = "NA", opacity = 0.8, title = "Number of inhabitants", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Total population (number of all persons). 2012 Local Authority level population estimates.",
                             "Classification method: quantiles.", "<br>",
                             "Source: National Records of Scotland (NRS).", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } 
            
            
            #       5.2.1.3. Deprivation Variables ####
            else{
                output$map <- renderLeaflet({
                    leaflet() %>%
                        addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                        setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                        addScaleBar(position = c("bottomleft"))%>%
                        addFullscreenControl()
                    #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                })
                output$info_box <- renderText({
                    HTML("<br>",
                         "<b>We are sorry, the selected variable is not currently available to display at Local Authority geography level. Please, select Datazones to explore data.</b>:", "<br>",
                         "<br>")
                })#Defining infobox
            }

        }
        
        
        # 5.2.2 Defining AR/TR varaibles at DZ level (input$datatype). #####
        else{ if(input$geounit=="datazones") {
            #5.2.2.1. AR/TR Data ####
            #Alcohol all:
            if(input$datatype=="alcoholTOTAL"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is where the data come:
                            addPolygons(data = geog_dz, 
                                        fillColor = ~pal_AR20_all(AR20_all), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$AR20_all,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR20_all, values = geog_dz$AR20_all, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    }) #Defining input datatype == Alcohol Total Sales
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of total alcohol outlets per 1,000 residents (including all on- and off-premises).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is where the data come:
                            addPolygons(data = geog_dz, 
                                        fillColor = ~pal_AR16_all(AR16_all), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$AR16_all,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR16_all, values = geog_dz$AR16_all, na.label = "NA", opacity = 0.8, title = "OUtlets per 1,000 persons", position = "bottomright")
                    }) #Defining input datatype == Alcohol Total Sales
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of total alcohol outlets per 1,000 residents (including all on- and off-premises).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is where the data come:
                            addPolygons(data = geog_dz, 
                                        fillColor = ~pal_AR12_all(AR12_all), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$AR12_all,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR12_all, values = geog_dz$AR12_all, na.label = "NA", opacity = 0.8, title = "OUtlets per 1,000 persons", position = "bottomright")
                    }) #Defining input datatype == Alcohol Total Sales
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of total alcohol outlets per 1,000 residents (including all on- and off-premises).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Alcohol on:
            else if(input$datatype=="alcoholOn"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_AR20_on(AR20_on), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>SIMD Rank</b>: ", geog_dz$AR20_on,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR20_on, values = geog_dz$AR20_on, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    }) 
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of alcohol on-premise outlets per 1,000 residents (outlets where you can only consume alcohol on-site).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_AR16_on(AR16_on), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>SIMD Rank</b>: ", geog_dz$AR16_on,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR16_on, values = geog_dz$AR16_on, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of alcohol on-premise outlets per 1,000 residents (outlets where you can only consume alcohol on-site).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_AR12_on(AR12_on), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>SIMD Rank</b>: ", geog_dz$AR12_on,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR12_on, values = geog_dz$AR12_on, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of alcohol on-premise outlets per 1,000 residents (outlets where you can only consume alcohol on-site).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } #Defining input datatype == Alcohol On Sales
            
            #Alcohol Off:
            else if(input$datatype=="alcoholOff"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_AR20_off(AR20_off), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$AR20_off,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR20_off, values = geog_dz$AR20_off, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of alcohol off-premise outlets per 1,000 residents (outlets where can buy alcohol, but not consume it on-site).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected: 
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_AR16_off(AR16_off), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$AR16_off,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR16_off, values = geog_dz$AR16_off, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of alcohol off-premise outlets per 1,000 residents (outlets where can buy alcohol, but not consume it on-site).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_AR12_off(AR12_off), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$AR12_off,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_AR12_off, values = geog_dz$AR12_off, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of alcohol off-premise outlets per 1,000 residents (outlets where can buy alcohol, but not consume it on-site).", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } #Defining input datatype == Alcohol Off Sales
            
            #Tobacco:
            else if(input$datatype=="tobaccoTOTAL"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_TR20(TR20), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$TR20,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_TR20, values = geog_dz$TR20, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of tobacco outlets per 1,000 residents.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_TR16(TR16), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$TR16,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_TR16, values = geog_dz$TR16, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of tobacco outlets per 1,000 residents.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_TR12(TR12), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$TR12,"outlets per 1,000 persons","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_TR12, values = geog_dz$TR12, na.label = "NA", opacity = 0.8, title = "Outlets per 1,000 persons", position = "bottomright")
                    })
                    output$info_box <- renderText({
                      HTML("<br>",
                           "<b>Variable information</b>:", "<br>",
                           "Number of tobacco outlets per 1,000 residents.", "<br>",
                           "Classification method: quantiles.", "<br>",
                           "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } #Defining input datatype == Tobacco Total Sales
            
            
            #5.2.2.2. Population estimates Data ####
            #Population estimates:
            else if(input$datatype=="MidYrPopD"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_MidYr20(MidYr2020D), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$MidYr2020D,"inhabitants","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_MidYr20, values = geog_dz$MidYr2020D, na.label = "NA", opacity = 0.8, title = "Number of inhabitants", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Total population (number of all persons). 2020 datazone level population estimates.", 
                             "Classification method: quantiles.", "<br>",
                             "Source: National Records of Scotland (NRS).", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_MidYr16(MidYr2016D), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$MidYr2016D,"inhabitants","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_MidYr16, values = geog_dz$MidYr2016D, na.label = "NA", opacity = 0.8, title = "Number of inhabitants", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Total population (number of all persons). 2016 datazone level population estimates.",
                             "Classification method: quantiles.", "<br>",
                             "Source: National Records of Scotland (NRS).", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_MidYr12(MidYr2012D), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$MidYr2012D,"inhabitants","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_MidYr12, values = geog_dz$MidYr2012D, na.label = "NA", opacity = 0.8, title = "Number of inhabitants", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Total population (number of all persons). 2012 datazone level population estimates.",
                             "Classification method: quantiles.", "<br>",
                             "Source: National Records of Scotland (NRS).", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } 
            
            
            #5.2.2.3. Deprivation variables ####
            #Overall Deprivation Rank:
            else if(input$datatype=="overallSIMD"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(Rankv2), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$Rankv2,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$Rankv2), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Overall Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(Rankv2_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$Rankv2_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$Rankv2_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Overall Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(Rankv2_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$Rankv2_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$Rankv2_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Overall Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            } 
            
            #Income Rank:
            else if(input$datatype=="IncRankv2"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(IncRankv2), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$IncRankv2,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$IncRankv2), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Income Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(IncRankv2_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$IncRankv2_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$IncRankv2_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Income Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(IncRankv2_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$IncRankv2_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$IncRankv2_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Income Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Income Rate:
            else if(input$datatype=="IncRate"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_incrate(IncRate), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$IncRate,"%","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_incrate, values = c(geog_dz$IncRate), na.label = "NA", opacity = 0.8, title = "Percent of people income deprived", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people who are income deprived (proportion of population receiving state benefits (unemployed or dependent people, pensioners, and adults and children from in-work families with an income of less than the poverty threshold - less than £222.55 per week).",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_incrate_2016(IncRate_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$IncRate_2016,"%","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_incrate_2016, values = c(geog_dz$IncRate_2016), na.label = "NA", opacity = 0.8, title = "Percent of people income deprived", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people who are income deprived (proportion of population receiving state benefits (unemployed or dependent people, pensioners, and adults and children from in-work families with an income of less than the poverty threshold - less than £222.55 per week).",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_incrate_2012(IncRate_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Value</b>: ", geog_dz_12$IncRate_2012,"%","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_incrate_2012, values = c(geog_dz_12$IncRate_2012), na.label = "NA", opacity = 0.8, title = "Percent of people income deprived", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people who are income deprived (proportion of population receiving state benefits (unemployed or dependent people, pensioners, and adults and children from in-work families with an income of less than the poverty threshold - less than £222.55 per week).",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2012. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Employment Rank:
            else if(input$datatype=="EmpRank"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(EmpRank), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$EmpRank,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$EmpRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Employment Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(EmpRank_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$EmpRank_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$EmpRank_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Employment Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(EmpRank_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$EmpRank_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$EmpRank_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Employment Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Employment Rate:
            else if(input$datatype=="EmpRate"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_emprate(EmpRate), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EmpRate,"%","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_emprate, values = c(geog_dz$EmpRate), na.label = "NA", opacity = 0.8, title = "Percent of people employment deprived", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people who are employment deprived (proportion of working age people receiving Jobseeker's Allowance, Incapacity Benefit, Employment and support Allowance, Severe Disablement Allocance or Universal Credit Claimants in not employment).",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_emprate_2016(EmpRate_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EmpRate_2016,"%","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_emprate_2016, values = c(geog_dz$EmpRate_2016), na.label = "NA", opacity = 0.8, title = "Percent of people employment deprived", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people who are employment deprived (proportion of working age people receiving Jobseeker's Allowance, Incapacity Benefit, Employment and support Allowance, Severe Disablement Allocance or Universal Credit Claimants in not employment).",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_emprate_2012(EmpRate_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Value</b>: ", geog_dz_12$EmpRate_2012,"%","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_emprate_2012, values = c(geog_dz$EmpRate_2012), na.label = "NA", opacity = 0.8, title = "Percent of people employment deprived", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people who are employment deprived (proportion of working age people receiving Jobseeker's Allowance, Incapacity Benefit, Employment and support Allowance, Severe Disablement Allocance or Universal Credit Claimants in not employment).",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2012. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health Rank:
            else if(input$datatype=="HlthRank"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(HlthRank), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$HlthRank,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$HlthRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Health Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(HlthRank_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$HlthRank_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$HlthRank_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Health Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(HlthRank_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$HlthRank_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$HlthRank_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Health Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Comparative Illness factor:
            else if(input$datatype=="HlthCIFSR"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthcif(HlthCIF), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthCIF,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthcif, values = c(geog_dz$HlthCIF), na.label = "NA", opacity = 0.8, title = "Comparative Illness factor", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "The Comparative Illness Factor (CIF) is a combined count of the total number of people claiming one or more of:
                              Disability Living Allowance, Attendance Allowance, Incapacity Benefit (not receiving DLA), Employment and Support Allowance, Severe Disablement Allowance, Income Support with disability premium, Personal Independence Payment and Universal Credit claimants with an accepted restricted ability to work.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthcif_2016(HlthCIF_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthCIF_2016,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthcif_2016, values = c(geog_dz$HlthCIF_2016), na.label = "NA", opacity = 0.8, title = "Comparative Illness factor", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "The Comparative Illness Factor (CIF) is a combined count of the total number of people claiming one or more of:
                              Disability Living Allowance, Attendance Allowance, Incapacity Benefit (not receiving DLA), Employment and Support Allowance, Severe Disablement Allowance, Income Support with disability premium, Personal Independence Payment and Universal Credit claimants with an accepted restricted ability to work.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2020 or 2016 to see more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Alcohol-related hospitalizations:
            else if(input$datatype=="HlthAlcSR"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthalc(HlthAlcSR), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthAlcSR,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthalc, values = c(geog_dz$HlthAlcSR), na.label = "NA", opacity = 0.8, title = "Alcohol-related hospitalizations", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Hospital stays related to alcohol use", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthalc_2016(HlthAlcSR_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthAlcSR_2016,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthalc_2016, values = c(geog_dz$HlthAlcSR), na.label = "NA", opacity = 0.8, title = "Alcohol-related hospitalizations", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Hospital stays related to alcohol use", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Tobacco-related hospitalizations:
            else if(input$datatype=="HlthDrugSR"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthdrug(HlthDrugSR), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthDrugSR,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthdrug, values = c(geog_dz$HlthDrugSR), na.label = "NA", opacity = 0.8, title = "Tobacco-related hospitalizations", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Hospital stays related to tobacco use", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthdrug_2016(HlthDrugSR_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthDrugSR_2016,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthdrug_2016, values = c(geog_dz$HlthDrugSR), na.label = "NA", opacity = 0.8, title = "Alcohol-related hospitalizations", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Hospital stays related to tobacco use", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Standarised Mortality Ratio:
            else if(input$datatype=="HlthDrugSR"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthsmr(HlthSMR), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthSMR,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthsmr, values = c(geog_dz$HlthSMR), na.label = "NA", opacity = 0.8, title = "Standardised Mortality Ratio", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Standardised Mortality Ratio", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthsmr_2016(HlthSMR_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthSMR_2016,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthsmr_2016, values = c(geog_dz$HlthSMR_2016), na.label = "NA", opacity = 0.8, title = "Standardised Mortality Ratio", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Standardised Mortality Ratio", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Drug Prescription:
            else if(input$datatype=="HlthDprsPc"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthdprs(HlthDprsPc), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthDprsPc,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthdprs, values = c(geog_dz$HlthDprsPc), na.label = "NA", opacity = 0.8, title = "Percent of people being prescribed drugs", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percent of people being prescribed drugs for anxiety, depression or psychosis", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthdprs_2016(HlthDprsPc_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName, "<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthDprsPc_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthdprs_2016, values = c(geog_dz$HlthDprsPc_2016), na.label = "NA", opacity = 0.8, title = "Percent of people being prescribed drugs", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percent of people being prescribed drugs for anxiety, depression or psychosis", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Low weight at birth:
            else if(input$datatype=="HlthLBWTPc"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthlbw(HlthLBWTPc), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthLBWTPc,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthlbw, values = c(geog_dz$HlthLBWTPc), na.label = "NA", opacity = 0.8, title = "Proportion of live singleton births of low birth weight", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Proportion of live singleton births of low birth weight", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthlbw_2016(HlthLBWTPc_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName, "<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthLBWTPc_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthlbw_2016, values = c(geog_dz$HlthLBWTPc_2016), na.label = "NA", opacity = 0.8, title = "Proportion of live singleton births of low birth weight", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Proportion of live singleton births of low birth weight", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Health: Emergency stays hospital:
            else if(input$datatype=="HlthLBWTPc"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthemer(HlthEmergS), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthEmergS, "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthemer, values = c(geog_dz$HlthEmergS), na.label = "NA", opacity = 0.8, title = "Emergency stays in hospital", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Emergency stays in hospitals", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_hlthemer_2016(HlthEmerSR_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName, "<br/>",
                                            "<b>Value</b>: ", geog_dz$HlthEmerSR_2016, "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_hlthemer_2016, values = c(geog_dz$HlthEmerSR_2016), na.label = "NA", opacity = 0.8, title = "Emergency stays in hospital", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Emergency stays in hospitals", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Education Rank:
            else if(input$datatype=="EduRank"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(EduRank), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$EduRank,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$EduRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Education Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(EduRank_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$EduRank_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$EduRank_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Education Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(EduRank_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$EduRank_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$EduRank_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Education Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Education Attend:
            else if(input$datatype=="HlthLBWTPc"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_eduattend(EduAttend), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduAttend,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_eduattend, values = c(geog_dz$EduAttend), na.label = "NA", opacity = 0.8, title = "School pupil attendance", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Proportion of school pupil attendance", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_eduattend_2016(EduAttend_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName, "<br/>",
                                            "<b>Value</b>: ", geog_dz$EduAttend_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_eduattend_2016, values = c(geog_dz$EduAttend_2016), na.label = "NA", opacity = 0.8, title = "School pupil attendance", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Proportion of school pupil attendance", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Education No Quals:
            else if(input$datatype=="EduNoQuals"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_edunoquals(EduNoQuals), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduNoQuals,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_edunoquals, values = c(geog_dz$EduNoQuals), na.label = "NA", opacity = 0.8, title = "Working age people with no qualifications", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Working age people with no qualifications", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_edunoquals_2016(EduNoQuals_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduNoQuals_2016,"<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_edunoquals_2016, values = c(geog_dz$EduNoQuals_2016), na.label = "NA", opacity = 0.8, title = "Working age people with no qualifications", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Working age people with no qualifications", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Values are shown in an standarised ratio: a value of 100 is the Scotland average for a population with the same age and sex profile. Indirectly standardised ratios have limitations for drawing
                              comparisons between different years. In comparing one data zone between different SIMD releases, if it had a ratio of 100 in SIMD16 and 110 in SIMD 2020, you could say that the data zone has got worse relative to Scotland. However, you cannot
                              say whether the data zone has actually worsened – only that it has worsened relative to the overall Scotland level. In comparing two data zones for the same SIMD release, if one data zone had a ratio of 110 and another a ratio of 115, you could say
                              that both data zones are worse than the Scottish average (100), but for statistical reasons you cannot say that the data zone with the ratio of 115 is worse than the one with the ratio of 110.
                              Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Education Not Part:
            else if(input$datatype=="EduNotPart"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_edunotpart(EduNotPart), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduNotPart,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_edunotpart, values = c(geog_dz$EduNotPart), na.label = "NA", opacity = 0.8, title = "Proportion of people not participating in education, employment or training", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Proportion of people aged 16-19 not participating in education, employment or training", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_edunotpart_2016(EduNotPart_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduNotPart_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_edunotpart_2016, values = c(geog_dz$EduNotPart_2016), na.label = "NA", opacity = 0.8, title = "Proportion of people not participating in education, employment or training", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Proportion of people aged 16-19 not participating in education, employment or training", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Education University:
            else if(input$datatype=="EduUniver"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_eduuniver(EduUniver), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduUniver,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_eduuniver, values = c(geog_dz$EduUniver), na.label = "NA", opacity = 0.8, title = "Percent of adolescents attending University", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percent of adolescents (17-21 years old) entering University as proportion of the 17-21 population", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_eduuniver_2016(EduUniver_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$EduUniver_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_eduuniver_2016, values = c(geog_dz$EduUniver_2016), na.label = "NA", opacity = 0.8, title = "Percent of adolescents attending University", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percent of adolescents (17-21 years old) entering University as proportion of the 17-21 population", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Geographical Access Rank:
            else if(input$datatype=="GAccRank"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(GAccRank), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$GAccRank,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$GAccRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Geographical Access Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(GAccRank_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$GAccRank_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$GAccRank_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Geographical Access Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(GAccRank_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$GAccRank_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$GAccRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Geographical Access Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc Petrol:
            else if(input$datatype=="GAccPetrol"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccpetrol(GAccPetrol), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPetrol," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccpetrol, values = c(geog_dz$GAccPetrol), na.label = "NA", opacity = 0.8, title = "Drive time to a petrol station", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a petrol station in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccpetrol_2016(GAccPetrol_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPetrol_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccpetrol_2016, values = c(geog_dz$GAccPetrol_2016), na.label = "NA", opacity = 0.8, title = "Drive time to a petrol station", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a petrol station in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc DT GP:
            else if(input$datatype=="GAccDTGP"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtgp(GAccDTGP), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTGP," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtgp, values = c(geog_dz$GAccDTGP), na.label = "NA", opacity = 0.8, title = "Drive time to a GP surgery", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a GP surgery in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtgp_2016(GAccDTGP_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTGP_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtgp_2016, values = c(geog_dz$GAccDTGP_2016), na.label = "NA", opacity = 0.8, title = "Drive time to a GP surgery", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a GP surgery in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc DT Post:
            else if(input$datatype=="GAccDTPost"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtpost(GAccDTPost), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTPost," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtpost, values = c(geog_dz$GAccDTPost), na.label = "NA", opacity = 0.8, title = "Drive time to a post office", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a post office in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtpost_2016(GAccDTPost_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTPost_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtpost_2016, values = c(geog_dz$GAccDTPost_2016), na.label = "NA", opacity = 0.8, title = "Drive time to a post office", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a post office in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc DT Primary school:
            else if(input$datatype=="GAccDTPsch"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtpsch(GAccDTPsch), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTPsch," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtpsch, values = c(geog_dz$GAccDTPsch), na.label = "NA", opacity = 0.8, title = "Drive time to a primary school", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a primary school in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtpsch_2016(GAccDTPsch_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTPsch_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtpsch_2016, values = c(geog_dz$GAccDTPsch_2016), na.label = "NA", opacity = 0.8, title = "Drive time to a primary school", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a primary school in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc DT Secondary school:
            else if(input$datatype=="GAccDTSsch"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtssch(GAccDTSsch), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTSsch," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtssch, values = c(geog_dz$GAccDTSsch), na.label = "NA", opacity = 0.8, title = "Drive time to a secondary school", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a secondary school in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtssch_2016(GAccDTSsch_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTSsch_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtssch_2016, values = c(geog_dz$GAccDTSsch_2016), na.label = "NA", opacity = 0.8, title = "Drive time to a secondary school", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a secondary school in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc DT Retail centre:
            else if(input$datatype=="GAccDTRet"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtret(GAccDTRet), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTRet," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtret, values = c(geog_dz$GAccDTRet), na.label = "NA", opacity = 0.8, title = "Drive time to a retail centre", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a retail centre in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccdtret_2016(GAccDTRet_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccDTRet_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccdtret_2016, values = c(geog_dz$GAccDTRet_2016), na.label = "NA", opacity = 0.8, title = "Drive time to a retail centre", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average drive time to a retail centre in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc PT GP:
            else if(input$datatype=="GAccPTGP"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccptgp(GAccPTGP), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPTGP," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccptgp, values = c(geog_dz$GAccPTGP), na.label = "NA", opacity = 0.8, title = "Public transport time to a GP surgery", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average public transport travel time to a GP surgery in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccptgp_2016(GAccPTGP_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPTGP_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccptgp_2016, values = c(geog_dz$GAccPTGP_2016), na.label = "NA", opacity = 0.8, title = "PUblic transport time to a GP surgery", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average public transport travel time to a GP surgery in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc PT Post:
            else if(input$datatype=="GAccPTPost"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccptpost(GAccPTPost), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPTPost," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccptpost, values = c(geog_dz$GAccPTPost), na.label = "NA", opacity = 0.8, title = "Public transport time to a post office", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average public transport travel time to a post office in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccptpost_2016(GAccPTPost_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPTPost_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccptpost_2016, values = c(geog_dz$GAccPTPost_2016), na.label = "NA", opacity = 0.8, title = "PUblic transport time to a post office", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average public transport travel time to a post office in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc PT Retail centre:
            else if(input$datatype=="GAccPTRet"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccptret(GAccPTRet), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPTRet," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccptret, values = c(geog_dz$GAccPTRet), na.label = "NA", opacity = 0.8, title = "Public transport time to a retail centre", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average public transport travel time to a retail centre in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccptret_2016(GAccPTRet_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccPTRet_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccptret_2016, values = c(geog_dz$GAccPTRet_2016), na.label = "NA", opacity = 0.8, title = "PUblic transport time to a retail centre", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Average public transport travel time to a retail centre in minutes.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #GAcc Broadband:
            else if(input$datatype=="GAccBrdbnd"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_gaccbrdbnd(GAccBrdbnd), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$GAccBrdbnd," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_gaccbrdbnd, values = c(geog_dz$GAccBrdbnd), na.label = "NA", opacity = 0.8, title = "Percent of premises without superfast broadband", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of premises without access to superfast broadband (at least 30Mb/s download speed).", "<br>",
                             "Classification method: Jenks breaks.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Crime Rank:
            else if(input$datatype=="CrimeRank"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(CrimeRank), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$CrimeRank,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$CrimeRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Crime Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(CrimeRank_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$CrimeRank_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$CrimeRank_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Crime Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(CrimeRAnk_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$CrimeRAnK_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$CrimeRAnK_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Crime Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Crime Rate:
            else if(input$datatype=="CrimeRate"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_crimerate(CrimeRate), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$CrimeRate," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_crimerate, values = c(geog_dz$CrimeRate), na.label = "NA", opacity = 0.8, title = "Crimes per 10,000 population", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Recorded crimes of violence, sexual offences, domestic housebreaking, vandalism, drugs offences, and common assault per 10,000 population.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_crimerate_2016(CrimeRate_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$CrimeRate_2016," minutes", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_crimerate_2016, values = c(geog_dz$CrimeRate_2016), na.label = "NA", opacity = 0.8, title = "Crimes per 10,000 population", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Recorded crimes of violence, sexual offences, domestic housebreaking, vandalism, drugs offences, and common assault per 10,000 population.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Hosing Rank:
            else if(input$datatype=="HouseRank"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(HouseRank), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$HouseRank,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$HouseRank), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Housing Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2020. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_simd_rank(HouseRank_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Rank</b>: ", geog_dz$HouseRank_2016,"of 6,976 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz$HouseRank_2016), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Housing Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2016. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz_12,
                                        fillColor = ~pal_simd_rank(HouseRank_2012), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz_12$LAName_2012,"<br/>",
                                            "<b>Rank</b>: ", geog_dz_12$HouseRank_2012,"of 6,505 datazones","<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_simd_rank, values = c(geog_dz_12$HouseRank_2012), na.label = "NA", opacity = 0.8, title = "Deprivation quintile range", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Housing Deprivation Rank. 0% is the most deprived datazone while 100% represents the least deprived datazone.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Scottish Index of Multiple Deprivation. Source: Scottish Government, 2012. For more information about this index, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Housing overcrowded:
            else if(input$datatype=="HouseOCrat"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_houseocrat(HouseOCrat), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HouseOCrat,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_houseocrat, values = c(geog_dz$HouseOCrat), na.label = "NA", opacity = 0.8, title = "Percent of overcrowded houses", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people in households that are overcrowded.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_houseocrat_2016(HouseOCrat_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HouseOCrat_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_houseocrat_2016, values = c(geog_dz$HouseOCrat_2016), na.label = "NA", opacity = 0.8, title = "Percent of overcrowded houses", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people in households that are overcrowded.", "<br>",
                             "Classification method: quantiles.", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #Housing no central heating:
            else if(input$datatype=="HouseNCrat"){
                #Year 2020 selected:
                if(input$year=="2020"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_housencrat(HouseNCrat), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HouseNCrat,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_housencrat, values = c(geog_dz$HouseNCrat), na.label = "NA", opacity = 0.8, title = "Percent of houses without central heating", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people in households without central heating.", "<br>",
                             "Classification method: Jenks breaks", "<br>",
                             "Source: Scottish Government, 2020. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2016 selected:
                else if(input$year=="2016"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>%
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl() %>%
                            #Here is when the data come:
                            addPolygons(data = geog_dz,
                                        fillColor = ~pal_housencrat_2016(HouseNCrat_2016), fillOpacity = 0.6, #Fill parameters
                                        stroke = TRUE, weight = 0.3, color = "#999", #Stroke parameters
                                        highlightOptions = highlightOptions(
                                            weight = 2, color = "#666", bringToFront = TRUE),
                                        popup = paste(
                                            "<b>Local Authority</b>: ", geog_dz$LAName,"<br/>",
                                            "<b>Value</b>: ", geog_dz$HouseNCrat_2016,"%", "<br/>",
                                            sep = " ")
                            ) %>%
                            addLegend(pal = pal_housencrat_2016, values = c(geog_dz$HouseNCrat_2016), na.label = "NA", opacity = 0.8, title = "Percent of houses without central heating", position = "bottomright")
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>Variable information</b>:", "<br>",
                             "Percentage of people in households without central heating.", "<br>",
                             "Classification method: Jenks breaks", "<br>",
                             "Source: Scottish Government, 2016. For more information about this variable, please, check the official technical documents <a href = https://www.gov.scot/news/scottish-index-of-multiple-deprivation-2020/>here</a>.", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #Year 2012 selected:
                else if(input$year=="2012"){
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                    output$info_box <- renderText({
                        HTML("<br>",
                             "<b>We are sorry, this variable is not currently available for the selected year. Please, select 2016 or 2020 to explore more recent data.</b>", "<br>",
                             "<br>")
                    })#Defining infobox
                }
                #No option: only show basemap.
                else{
                    output$map <- renderLeaflet({
                        leaflet() %>%
                            addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                            setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                            addScaleBar(position = c("bottomleft"))%>%
                            addFullscreenControl()
                        #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                    })
                }
            }
            
            #No option - show basemap.
            else{
                output$map <- renderLeaflet({
                    leaflet() %>%
                        addProviderTiles("Stamen.TonerLite", group = "Stamen Toner") %>% #Just display the basemap if any option selected.
                        setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
                        addScaleBar(position = c("bottomleft"))%>%
                        addFullscreenControl()
                    #leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7)
                })
            }
        }
        }
    })#Closing observe bracket

    # 5.3. Performing the rest of tabs: ####
    output$download <- renderUI({
        HTML("<p>This feature will be available soon.</p>") #To complete when information were ready!!
    })
    
}
#----


#Run the CRESHMAP: ####
shinyApp(ui, server)


