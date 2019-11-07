################################################################################
### Angus McLean
###
### Intro to Data Science
### Fall 2019
###
### Project Script: Comparing International Endangered Species and Commercial Trade
################################################################################

# Clear global environment
rm(list=ls())

#load relevant libraries
library(ggplot2)
library(dplyr)
library(rjson)
library(RCurl)
library(circlize)


#####################################################################
### Import worldwide trade data for selected counrties then compare totals
#####################################################################

#Comtrade data import

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))


get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p="0"
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(raw.data)
  }}

cmtrd1 <- get.Comtrade(ps="2016", r="842,156,276,381,392", p="0", cc="01,02,03,04,05,06,07,08,10,41,42,43", fmt="csv")
cmtrd2 <- get.Comtrade(ps="2016", r="410,251,826,124,528", p="0", cc="01,02,03,04,05,06,07,08,10,41,42,43", fmt="csv")

cmtrd <- rbind(`cmtrd1`, `cmtrd2`)
cmtrd$TradeValue <- cmtrd$Trade.Value..US..
cmtrd <- cmtrd[-c(32)]


#CITES data import

cites <- read.csv(text=getURL("https://raw.githubusercontent.com/AngusLMcLean/CITES-Trade-Analysis/master/CITES_2016.csv"), header=T)

cites[is.na(cites)] <- 0

cites$ImpQ <- cites$Importer.reported.quantity
cites$ExpQ <- cites$Exporter.reported.quantity
cites <- select(cites, -c(Importer.reported.quantity, Exporter.reported.quantity))

is.num <- sapply(cites, is.numeric)
cites[is.num] <- lapply(cites[is.num], round, 1)

cites$MaxQuant <- apply(cites[,17:18], 1, max)

###Compare total exports of listed species and all selected commodities

#prepare cites data
citesexp <- cites %>% select(Exporter, MaxQuant) %>% 
  group_by(Exporter) %>% 
  summarise(Total.1 = sum(MaxQuant)) %>% 
  filter(Exporter %in% c("FR", "DE", "US", "CH", "GB", "IT", "NL", "KR", "JP", "CA"))
citesexp <- citesexp[order(citesexp$Exporter),]

#prepare comtrade data
cmtrdexp <- cmtrd %>% filter(Trade.Flow=="Export") %>%
  select(Reporter.ISO, TradeValue) %>%
  group_by(Reporter.ISO) %>%
  summarise(Total.2 = sum(TradeValue))
vec <- c("CA", "CH", "FR", "IT", "US","DE", "GB", "JP", "KR", "NL")
cmtrdexp$Exporter <- vec
cmtrdexp$Reporter.ISO <- NULL

#merge into one dataframe
allexp <- merge(cmtrdexp, citesexp, by="Exporter")
allexp <- reshape(allexp, varying=c("Total.1", "Total.2"), direction="long", idvar="Exporter", sep=".")
allexp$time <- as.character(allexp$time)
str(allexp)
allexp$time <- factor(allexp$time, levels = c("1", "2"),
                      labels = c("CITES", "Commodities"))

#make plot
p <- ggplot(allexp, aes(x=reorder(Exporter, -Total), Total, fill=time, reorder(Exporter, Total)))
p + geom_col() +
  facet_grid(rows = vars(time), scales = "free") +
  theme_classic() +
  theme(text=element_text(family = "Helvetica"),
        legend.position = "none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size=14)) +
  labs(title="Total Export Comparison",
       subtitle = "All commodities and species for 2016",
       y="Trade Volume",
       x="Exporter")

###Compare leather, meat, live exports

#prepare cites data
citesexp1 <- cites %>% select(Exporter, MaxQuant, Term) %>% 
  group_by(Exporter, Term) %>% 
  summarise(Total.1 = sum(MaxQuant)) %>%
  filter(Exporter %in% c("FR", "DE", "US", "CH", "GB", "IT", "NL", "KR", "JP", "CA")) %>%
  filter(Term %in% c("meat", "live", "leather products (small)"))
citesexp1$Term <- factor(citesexp1$Term, levels = c("meat", "live", "leather products (small)"),
                         labels = c("meat", "live", "leather"))
citesexp1$Commodity <- citesexp1$Term
citesexp1$Term <- NULL

#prepare comtrade data
cmtrdexp1 <- cmtrd %>% filter(Trade.Flow=="Export", Commodity.Code %in% c(1:3)) %>%
  select(Reporter.ISO, TradeValue, Commodity.Code) %>%
  group_by(Reporter.ISO, Commodity.Code) %>%
  summarise(Total.2 = sum(TradeValue))
cmtrdexp1$Commodity.Code <- factor(cmtrdexp1$Commodity.Code, levels = c("1", "2", "3"),
                                   labels = c("meat", "live", "leather"))
cmtrdexp1$Reporter.ISO <- factor(cmtrdexp1$Reporter.ISO,
                                 levels = c("CHN", "DEU", "ITA", "JPN", "USA", "CAN", "FRA", "GBR", "KOR", "NLD"),
                                 labels = c("CH", "DE", "IT", "JP", "US", "CA", "FR", "GB", "KR", "NL"))
cmtrdexp1$Exporter <- cmtrdexp1$Reporter.ISO
cmtrdexp1$Reporter.ISO <- NULL
cmtrdexp1$Commodity <- cmtrdexp1$Commodity.Code
cmtrdexp1$Commodity.Code <- NULL

#merge into one dataframe
allexp1 <- merge(cmtrdexp1, citesexp1, by=c("Exporter", "Commodity"))
allexp1 <- reshape(allexp1, varying=c("Total.2", "Total.1"), direction="long", idvar=c("Exporter","Commodity"), sep=".")
allexp$time <- as.character(allexp$time)
str(allexp)
allexp1$time <- factor(allexp1$time, levels = c("1", "2"),
                       labels = c("CITES", "Commodities"))

#Make plot
p <- ggplot(allexp1, aes(x=reorder(Exporter, -Total), Total, fill=time, reorder(Exporter, Total)))
p + geom_col() +
  facet_grid(time ~ Commodity, scales = "free") +
  theme_classic() +
  theme(text=element_text(family = "Helvetica"),
        legend.position = "none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size=14)) +
  labs(title="Total Export Comparison",
       subtitle = "All commodities and species for 2016",
       y="Trade Volume",
       x="Exporter")

################################################################################
#Import data of trades only between selected countries and create chord diagrams
################################################################################

#CITES shipment-level data between top 10 countries shaped into appropriate format and plotted
citesbtwn <- cites%>% filter(Exporter %in% c("FR", "DE", "US", "CH", "GB", "IT", "NL", "KR", "JP", "CA"),
                             Importer %in% c("FR", "DE", "US", "CH", "GB", "IT", "NL", "KR", "JP", "CA")) %>%
  select(Exporter, Importer, MaxQuant) %>% 
  group_by(Exporter, Importer) %>%
  summarise(Total.1 = sum(MaxQuant))

citesbtwn[,3]  <- citesbtwn[,3] / 10000000

chordDiagram(citesbtwn)




