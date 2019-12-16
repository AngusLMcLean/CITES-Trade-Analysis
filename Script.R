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
library(scales)
library(igraph)
library(visNetwork)
library(tibble)

#Set working directory
setwd("/Users/angus/Documents/Data Science/CITES-Trade-Analysis")

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Import and clean data for CITES and Comtrade #
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

##Comtrade import
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

#Combine, clean comtrade dataframes
cmtrd <- rbind(`cmtrd1`, `cmtrd2`)
cmtrd$TradeValue <- cmtrd$Trade.Value..US..
cmtrd <- cmtrd[-c(32)]

##CITES import
cites <- read.csv(text=getURL("https://raw.githubusercontent.com/AngusLMcLean/CITES-Trade-Analysis/master/CITES_2016.csv"), header=T)

#Clean CITES data
cites[is.na(cites)] <- 0

cites$ImpQ <- cites$Importer.reported.quantity
cites$ExpQ <- cites$Exporter.reported.quantity
cites <- select(cites, -c(Importer.reported.quantity, Exporter.reported.quantity))

is.num <- sapply(cites, is.numeric)
cites[is.num] <- lapply(cites[is.num], round, 1)

levels(cites$Class)[levels(cites$Class) == ""] <- "Plants"

#Convert CITES trade quantities to whole-organism equivalents. 
cites$MaxQuant <- apply(cites[,15:16], 1, max)
cites$WOE <- cites$MaxQuant

levels(cites$Term)
levels(cites$Class)

woe_convert <- function(taxon, term, multiplier) {
  cites$WOE <- ifelse(test = cites$Class == taxon & cites$Term == term, cites$WOE*multiplier, cites$WOE)
  return(cites$WOE)
}

cites$WOE <- woe_convert("Mammalia", "ears", .5)
cites$WOE <- woe_convert("Mammalia", "feet", .25)
cites$WOE <- woe_convert("Mammalia", "horns", .5)
cites$WOE <- woe_convert("Mammalia", "sides", .5)
cites$WOE <- woe_convert("Mammalia", "tusks", .5)
cites$WOE <- woe_convert("Reptilia", "sides", .25)
cites$WOE <- woe_convert("Reptilia", "feet", .25)
cites$WOE <- woe_convert("Amphibia", "legs", .5)

#filter out units that can't be standardized (ex. kg, m^3 cannot be converted to WOE, so they are removed).
levels(cites$Unit)
cites1 <- cites %>% filter(Unit %in% "")
cites1$Unit <- NULL

#filter out terms that can't be converted to WOE
cites1 <- cites1 %>% filter(Term %in% c("bodies", "ears", "feet", "genitalia", "heads", "horns", "live", "sides", "skeletons",
                                        "skins", "skulls", "tails", "teeth", "trophies", "tusks", "legs", "carapaces", "fingerlings",
                                        "fins", "raw corals", "shells", "dried plants", "rootstocks", "roots"))

#Consolidate taxa into more useful categories
cites1$Class2 <- ifelse(cites1$Class %in% c("Anthozoa", "Arachnida", "Bivalvia", "Gastropoda", "Hirudinoidea", "Holothuroidea",
                                            "Hydrozoa", "Insecta"), "Invertebrates", as.character(cites1$Class))
cites1$Class2 <- ifelse(cites1$Class %in% c("Dipneusti", "Elasmobranchii","Actinopteri"), "Fish", as.character(cites1$Class2))
cites1$Class2 <- as.factor(cites1$Class2)
levels(cites1$Class2)

###~~~~~~~~~~~~~~~~~~~~~~###
# Some overall comparisons #
###~~~~~~~~~~~~~~~~~~~~~~###

##Compare total exports of listed species and all selected commodities
#prepare cites data
citesexp <- cites1 %>% select(Exporter, WOE) %>% 
  group_by(Exporter) %>% 
  summarise(Total.1 = sum(WOE)) %>% 
  filter(Exporter %in% c("FR", "DE", "US", "CN", "GB", "IT", "NL", "KR", "JP", "CA"))
citesexp <- citesexp[order(citesexp$Exporter),]

#prepare comtrade data
cmtrdexp <- cmtrd %>% filter(Trade.Flow=="Export") %>%
  select(Reporter.ISO, TradeValue) %>%
  group_by(Reporter.ISO) %>%
  summarise(Total.2 = sum(TradeValue))
vec <- c("CA", "CN", "FR", "IT", "US","DE", "GB", "JP", "KR", "NL")
cmtrdexp$Exporter <- vec
cmtrdexp$Reporter.ISO <- NULL

#merge cites and comtrade into one dataframe
allexp <- merge(cmtrdexp, citesexp, by="Exporter")
allexp <- reshape(allexp, varying=c("Total.1", "Total.2"), direction="long", idvar="Exporter", sep=".")
allexp$time <- as.character(allexp$time)
str(allexp)
allexp$time <- factor(allexp$time, levels = c("1", "2"),
                      labels = c("CITES", "Commodities"))

#make plot
dir.create(paste0(getwd(), "/Figures"))

p <- ggplot(allexp, aes(x=reorder(Exporter, -Total), Total, fill=time, reorder(Exporter, Total)))

png(filename=paste0(getwd(), "/Figures/", "Comtrade CITES Totals comparison.png"), width=8, height=5, res=300, units = "in")

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

dev.off()

##Compare leather, meat, live exports
#prepare cites data
citesexp1 <- cites %>% select(Exporter, WOE, Term) %>% 
  group_by(Exporter, Term) %>% 
  summarise(Total.1 = sum(WOE)) %>%
  filter(Exporter %in% c("FR", "DE", "US", "CN", "GB", "IT", "NL", "KR", "JP", "CA")) %>%
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
                                 labels = c("CN", "DE", "IT", "JP", "US", "CA", "FR", "GB", "KR", "NL"))
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

png(filename=paste0(getwd(), "/Figures/", "Comtrade CITES meat leather live comparison.png"), width=8, height=5, res=300, units = "in")

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

dev.off()

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Chord diagrams showing CITES trade flows #
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

#All CITES shipment-level data between top 10 countries shaped into appropriate format and plotted
citeschord <- cites1 %>% filter(Exporter %in% c("FR", "DE", "US", "CN", "GB", "IT", "NL", "KR", "JP", "CA"),
                                Importer %in% c("FR", "DE", "US", "CN", "GB", "IT", "NL", "KR", "JP", "CA")) %>%
  select(Exporter, Importer, WOE) %>% 
  group_by(Exporter, Importer) %>%
  summarise(Total.1 = sum(WOE))

citeschord[,3]  <- citeschord[,3] / 10000000 

grid.col= c(NL="orange", US="blue", CA="red", IT="green", CN="purple", JP="aquamarine",
            KR="thistle", FR="lightsalmon", DE="yellow", GB="ivory")

dir.create(paste0(getwd(), "/Figures/ChordDiagrams"))
png(filename=paste0(getwd(), "/Figures/ChordDiagrams/", "All Trade.png"), width=5, height=5, res=300, units = "in")
chordDiagram(citeschord, grid.col=grid.col)
title("All CITES Trade")
dev.off()

##Loop to make chord diagrams for each class of species
#Remove class levels not present in selected data before running loop
n <- c(levels(cites1$Class2))
n

#loop
for (i in n) {
  
  classsub <- cites1[cites1$Class2==i,]
  classchord <- classsub %>% filter(Exporter %in% c("FR", "DE", "US", "CN", "GB", "IT", "NL", "KR", "JP", "CA"),
                                    Importer %in% c("FR", "DE", "US", "CN", "GB", "IT", "NL", "KR", "JP", "CA")) %>%
    select(Exporter, Importer, WOE) %>% 
    group_by(Exporter, Importer) %>%
    summarise(Total.1 = sum(WOE))
  
  grid.col = c(NL="orange", US="blue", CA="red", IT="green", CH="purple", JP="aquamarine",
               KR="thistle", FR="lightsalmon", DE="yellow", GB="ivory3")
  
  png(filename=paste0(getwd(), "/Figures/ChordDiagrams/", i, ".png"), width=5, height=5, res=300, units = "in")
  chordDiagram(classchord, grid.col=grid.col)
  title(paste(i))
  
  dev.off()
}

###~~~~~~~~~~~~~~~~~~~~~~~###
# More plots for CITES data #
###~~~~~~~~~~~~~~~~~~~~~~~###
##Plot showing overall trade volume by taxon
one <- cites1 %>% select(Class2, WOE) %>% 
  group_by(Class2) %>%
  summarise(Total.1 = sum(WOE))

#make plot
p <- ggplot(one, aes(x= reorder(Class2, -Total.1), Total.1))

png(filename=paste0(getwd(), "/Figures/", "Total volume by Taxon.png"), width=5, height=5, res=300, units = "in")

p + geom_col() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() +
  theme(text=element_text(family = "Helvetica"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12),
        title = element_text(size=14),
        plot.subtitle = element_text(size = 10)) +
  labs(title="Total CITES trade volume by taxon",
       subtitle = "Plants traded more frequently by orders of magnitude",
       y="Trade Volume (WOE)",
       x="Taxon")

dev.off()

##Plot showing captive vs. wild ratio in each taxon
#prepare data
two <- cites1 %>% filter(Source %in% c("A", "C", "W", "D", "F"))
two$Source2 <- ifelse(two$Source %in% c("A", "C", "D", "F"), "Captive", "Wild")
two <- two %>% select(Class2, Source2, WOE) %>% 
  group_by(Class2, Source2) %>%
  summarise(Total.1 = sum(WOE))

#make plot
p <- ggplot(two, aes(x= reorder(Class2, -Total.1), Total.1, fill=Source2))

png(filename=paste0(getwd(), "/Figures/", "Wild vs Captive Ratio by Taxon.png"), width=5, height=5, res=300, units = "in")

p + geom_bar(position = "fill", stat="identity") +
  theme_classic() +
  theme(text=element_text(family = "Helvetica"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12),
        title = element_text(size=14)) +
  labs(title="CITES Wild vs. Captive Ratio by Taxon",
       subtitle = "In order of overall trade volume",
       y="Percent of Trade",
       x="Taxon")

dev.off()

##Plot purpose of CITES trades by taxon
three <- cites1 %>% filter(Source %in% c("A", "C", "W", "D", "F"))
three$Source2 <- ifelse(three$Source %in% c("A", "C", "D", "F"), "Captive", "Wild")
three <- three %>% select(Class2, Purpose, Source2, WOE) %>% 
  group_by(Class2, Purpose, Source2) %>%
  summarise(Total.1 = sum(WOE))

p <- ggplot(three, aes(x=Class2, y=Total.1, fill=Purpose))

png(filename=paste0(getwd(), "/Figures/", "CITES Trade Purpose by Taxon.png"), width=5, height=5, res=300, units = "in")

p + geom_bar(position = "fill", stat="identity") +
  facet_grid(rows = vars(Source2)) +
  theme_classic() +
  theme(text=element_text(family = "Helvetica"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10),
        title = element_text(size=12)) +
  labs(title="Commercial Trade is Most Common Purpose",
       y="Percent of Trade",
       x="Taxon") +
  scale_fill_discrete(name = "Purpose", 
                      labels = c("Not Specified", "Breeding", "Educational", "Botanical Garden", "Hunting Trophy",
                                 "Law Enforcement", "Medical", "Reintroduction", "Personal", "Circus", "Scientific",
                                 "Commercial", "Zoo"))

dev.off()

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Network analysis of CITES data #
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

##Any warnings that are returned can be ignored
#Node list
exporters <- cites1 %>% distinct(Exporter) %>% rename(label=Exporter)
importers <- cites1 %>% distinct(Importer) %>% rename(label=Importer)
nodes <- full_join(exporters, importers, by = "label")
nodes <- nodes %>% rowid_to_column("id")
nodes$shape <- rep("circle",nrow(nodes))

#Edge list
by_route <- cites1 %>%  
  group_by(Exporter, Importer) %>%
  summarise(weight = sum(WOE)) %>% 
  ungroup()
by_route

edges <- by_route %>% 
  left_join(nodes, by = c("Exporter" = "label")) %>%
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("Importer" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

##Visualize 
library(visNetwork)
library(igraph)

#animated interactive (takes a long time to load)
visNetwork(nodes, edges) 

#static interactive
network_static <- visNetwork(nodes, edges) %>% visIgraphLayout(layout = "layout_with_fr") 
visSave(network_static, file = paste0(getwd(), "/Figures/network_static.html"))









