rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(extrafont)
library(geomtextpath)
library(sf)

options(scipen=99999999)

#Set common font for all plots
font <- "Lato"

#Get data by LA across the UK, taking the most recently-available time period
#England & Wales 2021-23
url4 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/drugmisusedeathsbylocalauthority/current/2023localauthorities.xlsx"
temp4 <- tempfile()
temp4 <- curl_download(url=url4, destfile=temp4, quiet=FALSE, mode="wb")

EWLA <- read_excel(temp4, sheet="Table 6", range="A6:F363", col_names = FALSE) %>% 
  mutate(LAname=paste0(if_else(is.na(`...2`), "", `...2`), 
                       if_else(is.na(`...3`), "", `...3`), 
                       if_else(is.na(`...4`), "", `...4`))) %>% 
  select(c(1,7,5,6)) %>% 
  set_names("LAcode", "LAname", "Deaths", "ASMR") %>% 
  mutate(Country=if_else(substr(LAcode,1,1)=="E", "England", "Wales"),
         Deaths=Deaths/3,
         ASMR=as.numeric(ASMR))

#Scotland 2019-23
#Download Scottish data from NRS
url2 <- "https://www.nrscotland.gov.uk/files//statistics/drug-related-deaths/23/drug-related-deaths-23-data.xlsx"
temp2 <- tempfile()
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

ScotLA <- read_excel(temp2, sheet="Table_C4", range="A5:F665") %>% 
  filter(`Five-year period`=="2019 -2023") %>% 
  select(c(1,3,6)) %>% 
  set_names("LAname", "ASMR", "Deaths") %>% 
  mutate(Deaths=Deaths/5,
         ASMR=as.numeric(ASMR),
         Country="Scotland")

#Northern Ireland 2022
url3 <- "https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Drug-related%20deaths%20in%20NI%2C%202012-2022_0.xlsx"
temp3 <- tempfile()
temp3 <- curl_download(url=url3, destfile=temp3, quiet=FALSE, mode="wb")

NILA <- read_excel(temp3, sheet="Table_9b", range="A3:F28") %>% 
  filter(!is.na(`2018`)) %>% 
  mutate(Metric=rep(c("Deaths", "ASMR"), each=12)) %>% 
  filter(!`Number of deaths by LGD` %in% c("Total", "ASMR per 100,000 population by LGD")) %>% 
  select(1,6,7) %>% 
  spread(Metric, `2022`) %>% 
  set_names("LAname", "ASMR", "Deaths") %>% 
  mutate(ASMR=as.numeric(ASMR),
         Deaths=as.numeric(Deaths),
         Country="Northern Ireland",
         LAname=gsub("&", "and", LAname))

#Merge together into full list of all LAs
#Download shapefile of LA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://stg-arcgisazurecdataprod1.az.arcgis.com/exportfiles-1559-23740/Local_Authority_Districts_May_2024_Boundaries_UK_BFC_7298485007179693897.zip?sv=2018-03-28&sr=b&sig=kkrylMGmh360JWetlZ9DBMINRURpX%2Bk150CntKbMs5c%3D&se=2024-11-01T19%3A29%3A51Z&sp=r"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

name <- list.files(temp2, pattern=".shp")
shapefile <- st_read(file.path(temp2, name[1])) %>% 
  rename(LAcode="LAD24CD",
         LAname="LAD24NM")

mapdata <- left_join(shapefile, EWLA %>% select(-LAname), by="LAcode") %>% 
  left_join(NILA, by="LAname") %>% 
  mutate(ASMR=if_else(is.na(ASMR.x), ASMR.y, ASMR.x),
         Deaths=if_else(is.na(Deaths.x), Deaths.y, Deaths.x),
         Country=if_else(is.na(Country.x), Country.y, Country.x)) %>% 
  left_join(ScotLA, by="LAname") %>% 
  mutate(ASMR=if_else(is.na(ASMR.x.x), ASMR.y.y, ASMR.x.x),
         Deaths=if_else(is.na(Deaths.x.x), Deaths.y.y, Deaths.x.x),
         Country=if_else(is.na(Country.x.x), Country.y.y, Country.x.x))

agg_png("Outputs/DRDUKLADotsJitter2.png", units="in", width=6, height=10, res=800, background="#fff7bc")
st_sample(mapdata, size=mapdata$Deaths, type="random") %>% 
  ggplot(aes(geometry=geometry))+
  geom_sf(size=0.3, stroke=0, colour="darkred")+
  theme_void()+
  theme(plot.background=element_rect(fill="#fff7bc", colour="#fff7bc"),
        text=element_text(colour="darkred", family="Lato"),
        plot.title=element_text(size=rel(3)),
        plot.subtitle=element_text(size=rel(1.2)),
        plot.caption=element_text(size=rel(1)),
        plot.caption.position="plot", plot.title.position="plot")+
  labs(title="A national crisis",
       subtitle="Each dot represents one drug misuse death each year in the UK",
       caption="Data from ONS, NRS & NISRA | Plot by @VictimOfMaths")

dev.off()
