#--------------- Data scrapen van rijdendetreinen.nl



#importeren van librarys
library(tidyverse)
library(rvest)
library(dplyr)
library(hms)

#defineren van de datum waarop gescrapt zal worden. 
dates <- format(seq(as.Date("2011-01-04"), as.Date("2019-12-01"), by="days"), format="%d-%m-%Y")

#definieer URL's voor elke datum in het bereik en zorg ervoor dat we ten minste 60 resultaten controleren (als er meer dan 60 resultaten per dag zijn, worden ze niet gezien vanwege de manier waarop de site omgaat met paginering)
urls <- c(str_c("https://www.rijdendetreinen.nl/storingen?lines=&reasons=&date_before=", dates, "&date_after=", dates, "&page=", 1) , str_c("https://www.rijdendetreinen.nl/storingen?lines=&reasons=&date_before=", dates, "&date_after=", dates, "&page=", 2), str_c("https://www.rijdendetreinen.nl/storingen?lines=&reasons=&date_before=", dates, "&date_after=", dates, "&page=", 3))

#functie scrapen van pagina waarbij de data wordt opgeslagen in scrape_page.
scrape_page <- function(url){
  html <- read_html(url)

#filter alle disruptie route 
  disruption_line <- html %>%
    html_nodes(".disruption-line") %>%
    html_text()

#filter alle datums
  date <- url %>% 
    str_extract_all("(?<=e\\=)\\d+\\-\\d+\\-\\d\\d\\d\\d") #Vind een e= gevolgd door een datum en neem alleen de datum

#filter alle begin tijden
  time_stamp_disruption_from <- html %>%
    html_nodes(".timestamp") %>%
    html_text() %>%
    str_extract_all("\\d\\d:\\d\\d(?=\\s{1,20}-)") #Vind cijfer, cijfer, :, cijfer, cijfer gevolg door 1 tot 20 space en dan een min (-) en neem dat mee

#filter alle eind tijden  
  time_stamp_disruption_to <- html %>%
    html_nodes(".timestamp") %>%
    html_text() %>%
    str_extract_all("(?<=-\\s{1,20})\\d\\d:\\d\\d") #vind min (-) gevolgd door 1 tot 20 space en dan cijfer, cijfer, :, cijfer, cijfer en neem dat mee

#filter op type storing  
  content <- html %>%
    html_nodes(".disruption-content") %>%
    html_text() %>%
    substr(30, 200) %>%
    str_extract_all("\\s([a-z]+\\s)+") #vind woorden zonder hoofdletters die omringt zijn met spaces en dit kan 1 of meerdere woorden zijn die samen gevoegd worden

#verwijderen van alle dubbelen    
  return (unique((tibble(route=disruption_line, tijd_tot=time_stamp_disruption_to, tijd_vanaf=time_stamp_disruption_from, typeStoring = content, date = date))))
}

result <- urls %>%
  map(scrape_page)%>% #functie aanroepen scrape_page
  bind_rows() %>% #tabel opbouwen
  unique() #controle op dubbele data

#zet om tot DataFrame
dfresult <-as.data.frame(result)



#--------------- Data importeren van KNMI


#http://projects.knmi.nl/klimatologie/daggegevens/selectie.cgi
#d.m.v. Import Dataset CSV geimporteert als dataset

#hernoemen van geimporteerde data en toewijzen aan dataset KNMI
KNMI <- KNMI_20191201

# V1 = Weerstation nummer 
# V2 = Datum
# V3 = Etmaalgemiddelde temperatuur (in 0.1 graden Celsius)
# V4 = Zonneschijnduur (in 0.1 uur) berekend uit de globale straling (-1 voor <0.05 uur)
# V5 = Etmaalsom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm)


#--------------- Opschonen van dataset rijdendetreinen.nl


#parseren datums
dfresult$date <- as.character(dfresult$date)
dfresult$date <- parse_date(dfresult$date, "%d-%m-%Y")

#parseer de tijden
dfresult$tijd_tot <- as.character(dfresult$tijd_tot)
dfresult$tijd_tot <- parse_time(dfresult$tijd_tot, format="%H:%M")
dfresult$tijd_vanaf <- as.character(dfresult$tijd_vanaf)
dfresult$tijd_vanaf <- parse_time(dfresult$tijd_vanaf, format="%H:%M")

#maak char. van typestoringen en verwijder space
dfresult$typeStoring <- as.character(dfresult$typeStoring)
dfresult$typeStoring <- trimws(dfresult$typeStoring)


#--------------- Opschonen van de dataset KNMI


#parseren datums
KNMI$V2 <- as.character(KNMI$V2)
KNMI$V2 <- parse_date(KNMI$V2, "%Y%m%d")

#temperatuur format veranderen en berekening toepassen
KNMI$V3 = KNMI$V3 * 0.1

#zonnenSchijnduur format veranderen en berekening toepassen
KNMI$V4 = KNMI$V4 * 0.1
KNMI$V4[KNMI$V4<0.05] = 0

#neerslag format veranderen en berekening toepassen
KNMI$V5 = KNMI$V5 * 0.1
KNMI$V5[KNMI$V5<0.05] = 0



#--------------- Samenvoegen van beide datasets

#filteren op alleen de gewenste type storing
aanrijdingpersoon <- dfresult %>% filter(typeStoring == "aanrijding met een persoon")

#aanrijdingpersoon groeperen en aantal toevoegen
aanrijdingpersoon_n <- aanrijdingpersoon %>% group_by(date) %>% count(typeStoring)

#datasets samenvoegen met als key "date" en "V2". 
datasetsSamen <- right_join(aanrijdingpersoon_n, KNMI, by = c("date" = "V2") )


#--------------- Eind dataset opschonen en exp. CSV

#filter op alleen de benodigde kolommen en wijs deze toe
var_name = c("date", "n", "V3", "V4", "V5")
datasetProject <- as.data.frame(datasetsSamen[, (names(datasetsSamen) %in% var_name)])

#NA omzetten naar 0
datasetProject[is.na(datasetProject)] = 0

#Benoemen van kolommen
datasetProject = rename(datasetProject, "Aantal" = "n", "Etmaalgemiddelde temperatuur" = "V3", "Zonneschijnduur" = "V4", "Etmaalsom van de neerslag" = "V5")

#dataset exporteren als CSV
write.csv(datasetProject, "Datasetproject.CSV")




