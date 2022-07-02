
rm(list = ls())   # Remove all objects from environment

# Libraries ---------------------------------------------------------------


library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
#library(writexl)
library(lubridate)
library(rgdal)
library(scales)
#library(ggTimeSeries)




#------------------------------------SAV Data Import------------------------------------------------------------------------------------------------------------------------------------------


#New SAV data file
All_SAV_Data <- read_excel("Data/All SAV Data.xlsx",  sheet = "Data", col_types = c("date",  "text", "text", "text", "text", "numeric",  "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric", 
 "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "text", "text", "text", "numeric",  "text", "text", "numeric", "text")) %>%
mutate(`CELL NAME` = REGION)
str(All_SAV_Data_new)

All_SAV_Data$`CELL NAME` <- All_SAV_Data$`CELL NAME` %>%
str_replace("_", " C") %>% 
str_replace("A1", "A-1") %>% 
str_replace("A2", "A-2") %>% 
str_replace("A34", "A-34") %>% 
str_replace("A5", "A-56")


# Import and transforms spatial Data---------------------------------------------------------------------------------- -----------------------------------------------------


#Import DF of STA cell names
Region_names <-read_excel("STA Outlines/STA Cell Outline Names.xlsx")

#Import outlines of STA cells in DF format 
for(i in seq_along(Region_names[[2]]))
{
  assign(trimws(paste(Region_names[[i,2]])),read_excel(paste("STA Outlines/",Region_names[[i,2]]," .xlsx",sep = "")))  
}


# Create new csv files from shapefiles. Only needs to be run if updates to STA outlines are needed --------

#File Path to File GeoDataBase containing STA Shapefiles
fgdb <- "//ad/DFSroot/data/rsd_sta/STA_GIS/IA._Sources/A_STA.gdb"

#Imports feature class from ESRI File GeoDatabase and and saves to csv   
#Only run if new csv are needed! time consuming step!  CSVs are in project folder !
for(i in seq_along(Region_names[[2]]))
{
  Cell_Outline<- readOGR(dsn=fgdb,layer=Region_names[[i,2]])  
  Cell_Outline <-spTransform(Cell_Outline,CRS("+proj=longlat")) #Convert to Lat/Long Projection
  Cell_Outline <- fortify(Cell_Outline) #allows shapefiles to be plotted in ggplot2 
  write_xlsx(Cell_Outline,path =paste("STA Outlines/",Region_names[[i,2]], ".xlsx"))
}


#---------------------------------Functions needed to transform Data. Run before creating figures or maps----------------------------------------------------------
#Abundance and frequency of SAV in selected work area
veg_abundance_and_frequency <- function(All_SAV_Data,work_area) 
{
  #Creates DF for presence of total SAV at site
  SAV_present <- All_SAV_Data %>%  
  gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,`NAJAS_GUADALUPENSIS`,`NAJAS_MARINA`,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%
  filter(`REGION`==work_area) %>%
  mutate(COVER=as.numeric(COVER)) %>%
  mutate(COVER=ifelse(is.na(COVER),0,COVER)) %>%
  distinct() %>%
  spread(SPECIES,COVER) %>%
  mutate(`SAV Present`= CHARA+CERATOPHYLLUM+HYDRILLA+`NAJAS_GUADALUPENSIS`+`NAJAS_MARINA`+POTAMOGETON+VALLISNERIA+UTRICULARIA) %>%
  mutate(`SAV Present`=ifelse(`SAV Present`>0,1,0)) %>%
  group_by(`Survey Number`) %>%
  summarise(`Frequency of SAV Presence`=sum(`SAV Present`)/n()) %>%
  select(`Survey Number`,`Frequency of SAV Presence`)
  
  #DF of SAV frequency by species
  SAV_Summary_Stats <- All_SAV_Data %>%
  gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,`NAJAS_GUADALUPENSIS`,`NAJAS_MARINA`,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%
  rename( Lat = `LAT_DD`,Long = `LONG_DD`) %>%
  filter(REGION==work_area) %>%
  mutate(COVER=as.numeric(COVER)) %>%
  mutate(COVER=ifelse(COVER==0,NA,COVER)) %>%
  group_by(`Survey Number`,SPECIES) %>%
  summarise(Date=min(DATE),n=n(),Count=sum(!is.na(COVER)),Frequency=sum(!is.na(COVER))/n(),Abundance=sum(COVER,na.rm = TRUE),`Relative SAV Coverage`=(Abundance/3)/n()*100) %>%
  left_join(SAV_present,by=c("Survey Number")) 
  
  return(SAV_Summary_Stats)
}

#Transforms data to long format in selected work area. 
dominant_vegetation <- function(All_SAV_Data,work_area)
{
  SAV_Data_Long <- All_SAV_Data %>%   
    gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,NAJAS_GUADALUPENSIS,NAJAS_MARINA,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%   #Gathers Common SAV Species into long format
    dplyr::select(SITE,LAT_DD,`LONG_DD`,DATE,WATER_YEAR,SEASON,CELL,REGION,SPECIES,COVER,`Survey Number`) %>%   #selects pertinant columns
    mutate(YEAR=year(DATE),MONTH=month(DATE),DAY=day(DATE)) %>%   #adds Year, Month, and Day columns for to use as grouping varaible in analysis later
    rename( Lat = `LAT_DD`,Long = `LONG_DD`) %>%  #rename columns 
    filter(`Lat`!="<Null>") %>%  #remove rows with no spatial information
    filter(`REGION`==work_area) %>%  #Selects work area
    filter(SPECIES!="TOTAL_SAV")%>%   #removes total SAV as this is calcualted from other SAV coverages
    dplyr::select(SITE,Lat,Long,DATE,WATER_YEAR,SEASON,CELL,REGION,SPECIES,COVER,`Survey Number`)%>%  #selects pertinant columns
    mutate(DATE=as.Date(DATE)) %>% #removes time information from date
    group_by(`Survey Number`) %>%   
    mutate(`Display Date`=format(min(DATE),"%Y %b")) %>%   #Creates Display Date, Needed as some surveys take place over multiple days
    group_by(SITE,`Display Date`) %>%  
    mutate(`Total SAV`=sum(COVER,na.rm=TRUE)) %>%  #calculates total SAV from individual species abundance
    mutate(COVER=ifelse(COVER==0,NA,COVER)) %>%  #Converts 0s to NAs otherwise 0s would be used in interpolation map
    mutate(rank=row_number(-COVER)) %>%  #Ranks species by coverage densities at a site and date
    top_n( 1, rank) %>% #Selects highest ranked rows at for each date and site 
    mutate(x=round(as.numeric(`Long`),digits=3))  %>% # define x & y as longitude and latitude with 3 digits
    mutate(y=round(as.numeric(`Lat`),digits=3)) 
  return(SAV_Data_Long)
}

#this function creates identical spatial data frame for every survey, needed for faceting
facet_matcher <- function(SAV_data,SP_data)  
{  
  num_of_facets <-SAV_data %>%  #create DF of distinct facets
  ungroup() %>%
  distinct(`Display Date`,`Survey Number`)
  
  num_of_facets$key=1 #create common key coloumn for binding
  SP_data$key=1
  left_join(num_of_facets,SP_data,by="key")
}

#plots dominant and codominant vegetation by moving coordinantes of codominant vegetation slightly so it can be plotted
codominant_vegetation <- function(All_SAV_Data,work_area)  
{
  SAV_Data_Long <- All_SAV_Data %>%   
    gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,NAJAS_GUADALUPENSIS,NAJAS_MARINA,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%   #Gathers Common SAV Species into long format
    dplyr::select(SITE,LAT_DD,`LONG_DD`,DATE,WATER_YEAR,SEASON,CELL,REGION,SPECIES,COVER,`Survey Number`) %>%   #selects pertinant columns
    mutate(YEAR=year(DATE),MONTH=month(DATE),DAY=day(DATE)) %>%   #adds Year, Month, and Day columns for to use as grouping varaible in analysis later
    rename( Lat = `LAT_DD`,Long = `LONG_DD`) %>%  #rename columns 
    filter(`Lat`!="<Null>") %>%  #remove rows with no spatial information
    filter(`REGION`==work_area) %>%  #Selects work area
    filter(SPECIES!="TOTAL_SAV")%>%   #removes total SAV as this is calcualted from other SAV coverages
    dplyr::select(SITE,Lat,Long,DATE,WATER_YEAR,SEASON,CELL,REGION,SPECIES,COVER,`Survey Number`)%>%  #selects pertinant columns
    mutate(DATE=as.Date(DATE)) %>% #removes time information from date
    group_by(`Survey Number`) %>%   
    mutate(`Display Date`=format(min(DATE),"%Y %b")) %>%   #Creates Display Date, Needed as some surveys take place over multiple days
    group_by(SITE,`Display Date`) %>%  
    mutate(`Total SAV`=sum(COVER,na.rm=TRUE)) %>%  #calculates total SAV from individual species abundance
    mutate(COVER=ifelse(COVER==0,NA,COVER)) %>%  #Converts 0s to NAs otherwise 0s would be used in interpolation map
    mutate(rank=rank(-COVER,ties.method="min")) %>%  #Ranks species by coverage densities at a site and date. Ties are ranked same as lowest ranking
    filter(rank==1) %>%  # Keeps all vegetation that is Dominant or Codominant
    mutate(n=n()) %>%
    mutate(rank2=rank(-COVER,ties.method="first")) %>%  #second ranking. Lowest ranking goes to first value found in vector
    #top_n( 1, rank) %>% #Selects highest ranked rows at for each date and site 
    mutate(x=round(as.numeric(`Long`),digits=3))  %>% # define x & y as longitude and latitude with 3 digits
    mutate(y=round(as.numeric(`Lat`),digits=3)) %>%
    mutate(x=ifelse(n==2,ifelse(rank2==2,x-.002,x+.002),x)) %>% #moves x coordinate slightly of 2 codominant species so they can be plotted together
    mutate(x=ifelse(n==3,ifelse(rank2==2,x-.002,ifelse(rank2==3,x+.002,x)),x)) %>% #moves x for 3 equally ranked species
    mutate(x=ifelse(n==4,ifelse(rank2==2,x-.002,ifelse(rank2==4,x+.002,x)),x)) %>% #moves x for 4 equally ranked species
    mutate(y=ifelse(n==4,ifelse(rank2==1,y-.002,ifelse(rank2==3,y+.002,y)),y))  #moves y for 4 equally ranked species
  return(SAV_Data_Long)
}


#-------------------------------------Creates and saves figures and Maps-------------------------------------------------------------------
#Function creates maps and plots from all regions
#Run user defined functions first
for(i in seq_along(Region_names[[3]]))   
{
  
  assign("cell_name",Region_names[[i,3]])   #creates a variable from the Cell name 
  assign("region",Region_names[[i,1]])   #creates a variable from the region name
  
  
  #Sum of Coverages (Abundances) stacked columns 
  plot_abundance <-ggplot(veg_abundance_and_frequency(All_SAV_Data,region),aes(reorder(as.character(Date,format="%Y %b"),Date),Abundance,fill=SPECIES))+geom_col(position = "stack")+
  scale_fill_viridis(discrete = TRUE,direction = -1)+
  labs(x="Survey Date",y="Abundance",title=paste("SAV Abundance by Species in ",cell_name))+
  theme(panel.background = element_blank(),axis.text.x=element_text(angle=90,hjust=1),axis.line = element_line(colour = "black"))

  ggsave(paste("Figures/",region,"_Abundance.jpg"),plot=plot_abundance) #save abundance plot
  
  #Frequency of Occurance Plot 
  plot_frequency <- ggplot(veg_abundance_and_frequency(All_SAV_Data,region),aes(reorder(as.character(Date,format="%Y %b %d"),Date),Frequency,fill=SPECIES))+geom_col(position = "stack")+
  scale_fill_brewer(type="qual",palette = "Spectral",direction=-1,name ="Species",breaks=c("CERATOPHYLLUM", "CHARA", "HYDRILLA", "NAJAS_GUADALUPENSIS", "NAJAS_MARINA", "POTAMOGETON", "UTRICULARIA", "VALLISNERIA"),
  labels=c("Ceratophyllum", "Chara", "Hydrilla", "Najas guadalupensis", "Najas marina", "Potamogeton", "Utricularia", "Vallisneria"))+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5,size=10,face="bold"),axis.text.y=element_text(size=10,face="bold"),axis.title.x=element_blank(),axis.title.y=element_text(face="bold",size=16),panel.background = element_blank(),
  axis.line = element_line(colour = "black"),plot.title =element_text(size=18,face="bold",hjust=.5),aspect.ratio = 2/3)+
  labs(x="Date",y="Frequency of SAV Occurrence",title=paste("SAV Frequency by Species in",cell_name))
  
  ggsave(paste("Figures/",region,"_Frequency.jpg"),plot=plot_frequency, width = 9, height = 6, units = "in") #save frequency plot
  
  #Frequency of Occurance Plot with total SAV frequency
  plot_frequency_with_total_SAV <- ggplot(veg_abundance_and_frequency(All_SAV_Data,region),aes(reorder(as.character(Date,format="%Y %b %d"),Date),Frequency,fill=SPECIES))+geom_col(position = "stack")+
  geom_point(aes(reorder(as.character(Date,format="%Y %b %d"),Date),`Frequency of SAV Presence`),shape=3,size=3,color="black",show.legend=FALSE)+
  scale_fill_brewer(type="qual",palette = "Spectral",direction=-1,name ="Species",breaks=c("CERATOPHYLLUM", "CHARA", "HYDRILLA", "NAJAS_GUADALUPENSIS", "NAJAS_MARINA", "POTAMOGETON", "UTRICULARIA", "VALLISNERIA"),
  labels=c("Ceratophyllum", "Chara", "Hydrilla", "Najas guadalupensis", "Najas marina", "Potamogeton", "Utricularia", "Vallisneria"))+
  theme(axis.text.x=element_text(angle=90,hjust=0,size=10,face="bold"),axis.text.y=element_text(size=10,face="bold"),axis.title.x=element_blank(),axis.title.y=element_text(face="bold",size=16),panel.background = element_blank(),
  axis.line = element_line(colour = "black"),plot.title =element_text(size=18,face="bold",hjust=.5))+
  labs(x="Date",y="Frequency of SAV Occurrence",title=paste("SAV Frequency by Species in",region))
  
  ggsave(paste("Figures/",region,"_Frequency_with_total_SAV.jpg"),plot=plot_frequency_with_total_SAV) #save frequency plot
  
  #Relative SAV Coverage by Species plot
  plot_Relative_SAV_Coverage <-ggplot(veg_abundance_and_frequency(All_SAV_Data,region),aes(reorder(as.character(Date,format="%Y %b"),Date),`Relative SAV Coverage`,fill=SPECIES))+geom_col(position = "stack")+
  scale_fill_brewer(type="qual",palette = "Spectral",direction=-1,name ="Species",breaks=c("CERATOPHYLLUM", "CHARA", "HYDRILLA", "NAJAS_GUADALUPENSIS", "NAJAS_MARINA", "POTAMOGETON", "UTRICULARIA", "VALLISNERIA"),
  labels=c("Ceratophyllum", "Chara", "Hydrilla", "Najas guadalupensis", "Najas marina", "Potamogeton", "Utricularia", "Vallisneria"))+
  labs(x="Survey Date",y="Relative SAV Coverage (%)",title=paste("Relative SAV Coverage by Species in ",cell_name))+
  theme(axis.text.x=element_text(angle=90,hjust=1,size=10,face="bold"),axis.text.y=element_text(size=10,face="bold"),axis.title.x=element_blank(),axis.title.y=element_text(face="bold",size=16),panel.background = element_blank(),
  axis.line = element_line(colour = "black"),plot.title =element_text(size=18,face="bold",hjust=.3))
  
  ggsave(paste("Figures/",region,"_Relative_SAV.jpg"),plot=plot_Relative_SAV_Coverage) #save Relative SAV Coverage by Species plot
  
  #Time Series Map of dominant and up to 4 codominant species with cell outlines 
  map_codominant_veg <-ggplot(codominant_vegetation(All_SAV_Data,region),aes(x=x, y=y,size=(COVER),fill=SPECIES))+facet_wrap(~reorder(`Display Date`,`Survey Number`))+labs(title=paste("Dominant SAV Over Time in ",cell_name))+
  geom_point(shape=22)+coord_quickmap()+scale_fill_brewer(palette = "Set3",direction=-1)+
  theme(panel.background = element_blank(),axis.ticks = element_blank(),axis.text = element_blank(),axis.title = element_blank()) +
  geom_path(data=facet_matcher(dominant_vegetation(All_SAV_Data,region),get(paste(region,"_11",sep=""))), aes(long,lat),inherit.aes = FALSE)+theme_void()
  
  ggsave(paste("Figures/",region,"_SAV_Over_Time.jpg"),plot=map_codominant_veg) #save dominant vegetation map
  
  #Time Series Map of Total SAV with cell outlines
  map_total_SAV <- ggplot(dominant_vegetation(All_SAV_Data,region),aes(x=x, y=y,size=`Total SAV`,fill=`Total SAV`))+facet_wrap(~reorder(`Display Date`,`Survey Number`))+
  labs(title=paste("Vegetation Over Time in ",cell_name))+
  geom_point(shape=22)+coord_quickmap()+scale_fill_viridis(direction=-1)+
  theme(panel.background = element_blank(),axis.ticks = element_blank(),axis.text = element_blank(),axis.title = element_blank()) +
  geom_path(data=facet_matcher(dominant_vegetation(All_SAV_Data,region),get(paste(region,"_11",sep=""))), aes(long,lat),inherit.aes = FALSE)+theme_void()
  
  ggsave(paste("Figures/",region,"_Total_SAV_Over_Time.jpg"),plot=map_total_SAV) #save total sav map
}


#---------------------------------------Test Code ----------------------------------------------------------------------------------

  SAV_Data_Long <- All_SAV_Data %>%   
    gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,NAJAS_GUADALUPENSIS,NAJAS_MARINA,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%   #Gathers Common SAV Species into long format
    dplyr::select(SITE,LAT_DD,`LONG_DD`,DATE,WATER_YEAR,SEASON,CELL,REGION,SPECIES,COVER,`Survey Number`) %>%   #selects pertinant columns
    mutate(YEAR=year(DATE),MONTH=month(DATE),DAY=day(DATE)) %>%   #adds Year, Month, and Day columns for to use as grouping varaible in analysis later
    rename( Lat = `LAT_DD`,Long = `LONG_DD`) %>%  #rename columns 
    filter(`Lat`!="<Null>") %>%  #remove rows with no spatial information
    filter(`REGION`=="STA1W_3") %>%  #Selects work area
    filter(SPECIES!="TOTAL_SAV")%>%   #removes total SAV as this is calcualted from other SAV coverages
    dplyr::select(SITE,Lat,Long,DATE,WATER_YEAR,SEASON,CELL,REGION,SPECIES,COVER,`Survey Number`)%>%  #selects pertinant columns
    mutate(DATE=as.Date(DATE)) %>% #removes time information from date
    group_by(`Survey Number`) %>%   
    mutate(`Display Date`=format(min(DATE),"%Y %b")) %>%   #Creates Display Date, Needed as some surveys take place over multiple days
    group_by(SITE,`Display Date`) %>%  
    mutate(`Total SAV`=sum(COVER,na.rm=TRUE)) %>%  #calculates total SAV from individual species abundance
    mutate(COVER=ifelse(COVER==0,NA,COVER)) %>%  #Converts 0s to NAs otherwise 0s would be used in interpolation map
    mutate(rank=rank(-COVER,ties.method="min")) %>%  #Ranks species by coverage densities at a site and date. Ties are ranked same as lowest ranking
    filter(rank==1) %>%  # Keeps all vegetation that is Dominant or Codominant
    mutate(n=n()) %>%
    mutate(rank2=rank(-COVER,ties.method="first")) %>%  #second ranking. Lowest ranking goes to first value found in vector
    #top_n( 1, rank) %>% #Selects highest ranked rows at for each date and site 
    mutate(x=round(as.numeric(`Long`),digits=3))  %>% # define x & y as longitude and latitude with 3 digits
    mutate(y=round(as.numeric(`Lat`),digits=3)) %>%
    mutate(x=ifelse(n==2,ifelse(rank2==2,x-.002,x+.002),x)) %>% #moves x coordinate slightly of 2 codominant species so they can be plotted together
    mutate(x=ifelse(n==3,ifelse(rank2==2,x-.002,ifelse(rank2==3,x+.002,x)),x)) %>% #moves x for 3 equally ranked species
    mutate(x=ifelse(n==4,ifelse(rank2==2,x-.002,ifelse(rank2==4,x+.002,x)),x)) %>% #moves x for 4 equally ranked species
    mutate(y=ifelse(n==4,ifelse(rank2==1,y-.002,ifelse(rank2==3,y+.002,y)),y))  #moves y for 4 equally ranked species


#veg_abundance_and_frequency <- function(All_SAV_Data,work_area)   Test the function
  SAV_present <- All_SAV_Data %>%  
    gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,`NAJAS_GUADALUPENSIS`,`NAJAS_MARINA`,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%
    filter(`REGION`=="STA34_1B") %>%
    mutate(COVER=as.numeric(COVER)) %>%
    mutate(COVER=ifelse(is.na(COVER),0,COVER)) %>%
    distinct() %>%
    spread(SPECIES,COVER) %>%
    mutate(`SAV Present`= CHARA+CERATOPHYLLUM+HYDRILLA+`NAJAS_GUADALUPENSIS`+`NAJAS_MARINA`+POTAMOGETON+VALLISNERIA+UTRICULARIA) %>%
    mutate(`SAV Present`=ifelse(`SAV Present`>0,1,0)) %>%
    group_by(`Survey Number`) %>%
    summarise(`Frequency of SAV Presence`=sum(`SAV Present`)/n()) %>%
    select(`Survey Number`,`Frequency of SAV Presence`)
  
  #DF of SAV frequency by species
  SAV_Summary_Stats <- All_SAV_Data %>%
    gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,`NAJAS_GUADALUPENSIS`,`NAJAS_MARINA`,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%
    rename( Lat = `LAT_DD`,Long = `LONG_DD`) %>%
    filter(`REGION`=="STA1W_5B") %>%
    mutate(COVER=as.numeric(COVER)) %>%
    mutate(COVER=ifelse(COVER==0,NA,COVER)) %>%
    group_by(`Survey Number`,SPECIES) %>%
    summarise(Date=min(DATE),n=n(),Count=sum(!is.na(COVER)),Frequency=sum(!is.na(COVER))/n(),Abundance=sum(COVER,na.rm = TRUE),`Relative SAV Coverage`=(Abundance/3)/n()*100) %>%
    left_join(SAV_present,by=c("Survey Number")) 
  









SAV_Summary_Stats <- All_SAV_Data %>%
  gather(SPECIES,COVER,CHARA,CERATOPHYLLUM,HYDRILLA,`NAJAS_GUADALUPENSIS`,`NAJAS_MARINA`,POTAMOGETON,VALLISNERIA,UTRICULARIA) %>%
  rename( Lat = `LAT_DD`,Long = `LONG_DD`) %>%
  mutate(COVER=as.numeric(COVER)) %>%
  mutate(COVER=ifelse(COVER==0,NA,COVER)) %>%
  group_by(REGION,`Survey Number`,SPECIES) %>%
  summarise(Date=min(DATE),n=n(),Count=sum(!is.na(COVER)),Frequency=sum(!is.na(COVER))/n(),Abundance=sum(COVER,na.rm = TRUE),
  `Survey_Numbers min`=min(`Survey Number`),`Survey_Numbers max`=max(`Survey Number`),`Survey mismatch`=`Survey_Numbers max`-`Survey_Numbers min`) 

ggplot(veg_abundance_and_frequency(All_SAV_Data,"STA-1W C3"),aes(reorder(as.character(Date,format="%Y %b %d"),Date),Frequency,fill=SPECIES))+geom_col(position = "stack")+
scale_fill_brewer(type="qual",palette = "Spectral",direction=-1)+
geom_point(aes(reorder(as.character(Date,format="%Y %b %d"),Date),`Frequency of SAV Presence`),shape=3,size=3,color="black",show.legend=FALSE)+
scale_colour_discrete(name  ="Total SAV") +
theme(axis.text.x=element_text(angle=90,hjust=0))+labs(x="Date",y="Frequency",title=paste("SAV Frequency of Occurance by Species in ","STA1W_2B"))+ 
theme(panel.background = element_blank(),axis.text.x=element_text(angle=90,hjust=1),axis.line = element_line(colour = "black"))

#Frequency of Occurance Plot 
plot_frequency <- ggplot(veg_abundance_and_frequency(All_SAV_Data,"STA-2 C3"),aes(reorder(as.character(Date,format="%Y %b %d"),Date),Frequency,fill=SPECIES))+geom_col(position = "stack")+
scale_fill_brewer(type="qual",palette = "Spectral",direction=-1,name ="Species",breaks=c("CERATOPHYLLUM", "CHARA", "HYDRILLA", "NAJAS_GUADALUPENSIS", "NAJAS_MARINA", "POTAMOGETON", "UTRICULARIA", "VALLISNERIA"),
labels=c("Ceratophyllum", "Chara", "Hydrilla", "Najas guadalupensis", "Najas marina", "Potamogeton", "Utricularia", "Vallisneria"))+
theme(axis.text.x=element_text(angle=90,hjust=1,size=10,face="bold"),axis.text.y=element_text(size=10,face="bold"),axis.title.x=element_blank(),axis.title.y=element_text(face="bold",size=16),panel.background = element_blank(),
axis.line = element_line(colour = "black"),plot.title =element_text(size=18,face="bold",hjust=.5))+
labs(x="Date",y="Frequency of SAV Occurrence",title=paste("SAV Frequency by Species in","STA-2 C3"))

ggsave(paste("Figures/",work_area,"_Frequency.jpg"),plot=plot_frequency) #save frequency plot

plot_Relative_SAV_Coverage <-ggplot(veg_abundance_and_frequency(All_SAV_Data,"STA-2 C3"),aes(reorder(as.character(Date,format="%Y %b"),Date),`Relative SAV Coverage`,fill=SPECIES))+geom_col(position = "stack")+
scale_fill_viridis(discrete = TRUE,direction = -1)+
labs(x="Survey Date",y="Relative SAV Coverage",title=paste("Relative SAV Coverage by Species in ","STA-2 C3"))+
theme(panel.background = element_blank(),axis.text.x=element_text(angle=90,hjust=1),axis.line = element_line(colour = "black"))






for(i in seq_along(Region_names[[3]]))   
{
  
  assign("work_area",Region_names[[i,3]])   #creates a variable from the region name 

  #Frequency of Occurance Plot 
  plot_frequency <- ggplot(veg_abundance_and_frequency(All_SAV_Data,work_area),aes(reorder(as.character(Date,format="%Y %b %d"),Date),Frequency,fill=SPECIES))+geom_col(position = "stack")+
    scale_fill_brewer(type="qual",palette = "Spectral",direction=-1,name ="Species",breaks=c("CERATOPHYLLUM", "CHARA", "HYDRILLA", "NAJAS_GUADALUPENSIS", "NAJAS_MARINA", "POTAMOGETON", "UTRICULARIA", "VALLISNERIA"),
                      labels=c("Ceratophyllum", "Chara", "Hydrilla", "Najas guadalupensis", "Najas marina", "Potamogeton", "Utricularia", "Vallisneria"))+
    theme(axis.text.x=element_text(angle=90,hjust=1,size=10,face="bold"),axis.text.y=element_text(size=10,face="bold"),axis.title.x=element_blank(),axis.title.y=element_text(face="bold",size=16),panel.background = element_blank(),
          axis.line = element_line(colour = "black"),plot.title =element_text(size=18,face="bold",hjust=.3))+
    labs(x="Date",y="Frequency",title=paste("SAV Frequency of Occurrence by Species in",work_area))
  
  ggsave(paste("Figures/",work_area,"_Frequency.jpg"),plot=plot_frequency) #save frequency plot
}

