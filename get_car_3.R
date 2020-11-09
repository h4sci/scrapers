library(tidyverse)
library(rvest)
library(XML)
#library(methods)
library(data.tree)
#library(rjson)
library(jsonlite)


##### NOTE:There are a number of places bugs can arise & should be checked #######################

#          1) When scraping imovel & property id can get issues processing the json script due to internal structures. Currently treating as NA. Shouldn't be issue
#          2a) Dropped observations in html mapping - this is big issue as code & id join is not via matching, simply attaching so if not same length is invalid
#          2b) observation can be identified by error message and removed if necessary but has to be done FOR ALL SCRAPED SECTIONS
#          2c) sometimes you just need to rerun that chunk
#          3) Additionally, data should be validated manually against database to ensure this join was correct


setwd("~/repositories/scrapers") #for  sam
setwd("smb/Volumes/epl/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/CAR/repo") # mac - owen
# setwd("Z:/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/CAR") # for windows

# read data ---------------------------------------------------------------

data2 <- read_delim("PGM_mesogregion.csv", delim = ",", trim_ws = TRUE)

#NOTE: CURRENTLY USE EXTERNAL CODES TO GET DATA. WILL RPLACE WITH A CODE THAT EXTRACTS JSON DATA FROM
#      EVERY CLICK POINT ON MAP http://car.semas.pa.gov.br/#/consulta/mapa 

#clicking on the map runs this http://car.semas.pa.gov.br/site/lotes/wkt?pontoWkt=POINT(-49.7021484375+-2.0210651187669897)

#where the section following "POINT(" contains the lat & the long

# see "point_test.csv" in smb://d/groups/gess/epl/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/CAR/Rscripts
# has lat long pairs as well as a column with it pre-formatted in right way for the 

#will need to create lat long pairs for a grid that covers the whole extent of the map and format them as the URLs above via

# note: this could be done by determining the min/max lat & long - you can see the lat longs on the map link

 LONG <- seq(-59.2710,-45.9623,by=0.01)  #quick attempt
 LAT <- seq(2.4602,-10.1852,by=-0.01)    

# it could also be done by defining origin & creating a grid via something like this https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat
# scale of the grid OR the increment used in seq() will have BIG IMPACT in missing properties
# currently set at 0.01 degree - or around 1.11 km (at the equator)

 #HOWEVER  WLL NEED AN INTERMEDIARY STEP to turn into a grid like this
 x <- expand.grid(LAT,LONG) 
 
 # check it is a real grid,  little slow as pulls PA from internet
 ggplot() + 
   geom_point(data=x,aes(x=Var2,y=Var1)) +
   geom_sf(geobr::read_state(code_state = "PA"),mapping=aes(fill="red"))

 # can then create the urls with the following:
urls_alt <- paste0("http://car.semas.pa.gov.br/site/lotes/wkt?pontoWkt=POINT(",paste(x$Var2,x$Var1,sep="+"),")")

#after this it is the same as before

# NOTE: THIS WILL BE VERY  COMPUTATIONALLY HEAVY, CURRENTLY 1,683,715 clicks. 
#        If we can remove any points outside the boundaries of PA and if we can 
#        optimally choose a point gap that captures all properties with least overlap,
#         will save a lot of energy/time

# scrape pt 1) CAR imovel code & property id  ------------------------------------------------------------------

# prep urls
urls <- data2 %>% 
  pull(COD_IMOVEL) %>% 
  #  str_subset("list?filtro=") %>% 
  str_c("http://car.semas.pa.gov.br/site/consulta/imoveis/CODIGO/list?filtro=", ., "&pagina=1")

#extract the json data (wrapped inside an html node) #note: currently first 10
json <- map(urls[1:50], possibly(function(x){fromJSON(x, simplifyVector = F,simplifyDataFrame = F)}, NULL))

#write function to extract spatial & non-spatial data & convert it to an sf object 
# note: see https://www.jessesadler.com/post/simple-feature-objects/ 

sp_extr <- function(x) {            # IS THIS ROBUST TO RESULTS WITH MULTIPLE PROPERTIES RETURNED???
  #extract non-spatial data         # ALMOST CERTAINLY NOT - COULD ADD FOR LOOP AT THE START TO RUN OVER JSON LISTS
  data <- as.data.frame(t(unlist(x$features[[1]]$properties)))
  
  #extract spatial data and convert it into an array
  g.raw <- x$features[[1]]$geometry$geometries[[1]]$coordinates[[1]][[1]]   
  geom <- array(dim=c(length(g.raw),2))  #empty array
  geom[,1]<- sapply(g.raw,"[[",1)        #first  list item (X coord)
  geom[,2] <-sapply(g.raw,"[[",2)        #second list item (Y coord)
  
  #convert spatial array into an sfg then sfc object
  geom_sfg <-st_linestring(as.matrix(geom))
  geom_sfc <- st_sfc(geom_sfg,crs=4326)
  
  #link spatial data to non-spatial data and return
  data_sp <- st_sf(data,geometry=geom_sfc)
  
  return(data_sp)
}

test <- map(json,sp_extr) %>% reduce(rbind)
test2 <- st_polygonize(test)

processed_u <- na.omit(test2)
ggplot() + geom_sf(processed_u,mapping = aes(fill=as.numeric(id)))
#dim(test2)[1]-dim(processed_u)[1]  #loss of additional 5 obs (7 total)



# scrape pt 2 - domino  ------------------------------------------------------------------

#NOTE: DOMINO CAN HAVE MORE THAN 1 OWNER NOW e.g. http://car.semas.pa.gov.br/site/imovel/417490/dominioFichaResumida
# Nº do Recibo:PA-1502301-561B1A1DDF85493BA305F230B759553C

# prep urls
urls2 <- processed_u %>% 
  pull(id) %>% 
  #str_subset("") %>% 
  str_c("http://car.semas.pa.gov.br/site/imovel/", . ,"/dominioFichaResumida")

# try
html2 <- map(urls2, possibly(read_html, NULL))
html_alt2 <- keep(html2, ~ typeof(.x) == "list")
html.h2 <- html

# extract subparts ----------------------------

# get main body for each
body2 <- map(html_alt2, ~ html_node(., ".table-condensed"))

extract_sections <- function(html, names) {
  names_out <-  html%>%                         # get contents of node
    html_nodes(names) %>%        
    html_text() %>%
    as.data.frame() %>%
    separate(1,c("names","values"),": ")          # split at : into names & values
  return(names_out)
}

# run on all
chunk1 <- map(body2, extract_sections, names = "td")

# compile to df
domino <- map_df(chunk1, spread, names, values)

names(domino) <- c("domino_CPF_CNPJ","domino_nome","domino_tipo")

# just sticking together as operation is sequential & output is same length as input (i.e. successful scrape)
# I THINK possibly() function above could fix this? replace the NULL with NA?
# would be nice if could keep id throughout
domino_c <- data.frame(code = processed_u$codigo,id=processed_u$id,domino) 

# scrape pt 3 - imovel  ------------------------------------------------------------------

#NOTE: IMOVEL HAS OTHER ATTRIBUTES WE ARE DROPPING in other nodes (activities on property, forest area)

# prep urls
urls3 <- processed_u %>% 
  pull(id) %>% 
  #str_subset("") %>% 
  str_c("http://car.semas.pa.gov.br/site/imovel/", . ,"/imovelFichaResumida")

# try
html3 <- map(urls3, possibly(read_html, NULL))
html_alt3 <- keep(html3, ~ typeof(.x) == "list")

# extract subparts ----------------------------

# get main body for each
body3 <- map(html_alt3, ~ html_node(., ".table-condensed"))

# run on all
chunk2 <- map(body3, extract_sections, names = "td")

# compile to df
imovel <- map_df(chunk2, spread, names, values)

names(imovel)       <- c("imovel_CEP","access","Fisc_models","Muni_UF","imovel_nome","imovel_tipo","RURAL_URBANO")

#swap , for  . & convert to numeric
imovel$Fisc_models <- as.numeric(gsub(",", ".", gsub("\\.", "", imovel$Fisc_models)))

#split muni & UF
imovel <- imovel %>%
  separate(Muni_UF,c("Muni","UF"),"/")

imovel_c <- data.frame(code = processed_u$codigo,id=processed_u$id,imovel)

# scrape pt 4- cadastrante  ------------------------------------------------------------------


# prep urls
urls4 <- processed_u %>% 
  pull(id) %>% 
  #str_subset("") %>% 
  str_c("http://car.semas.pa.gov.br/site/imovel/", . ,"/cadastranteFichaResumida")

# try
html4 <- map(urls4, possibly(read_html, NULL))
html_alt4 <- keep(html4, ~ typeof(.x) == "list")
html.h4 <- html4

# extract subparts ----------------------------

# get main body for each
body4 <- map(html_alt4, ~ html_node(., ".table-condensed"))

# run on all
chunk3 <- map(body4, extract_sections, names = "td")

# compile to df
cadastrante <- map_df(chunk3, spread, names, values)

names(cadastrante) <- c("cadastrante_CPF","cadastrante_nome","cadastrante_ART","cadastrante_Profissao")

cadastrante_c <- data.frame(code = processed_u$codigo,id=processed_u$id,cadastrante)

# pt 5? demomnstrativo??? e.g. http://car.semas.pa.gov.br/site/demonstrativo/imovel/PA-1502301-561B1A1DDF85493BA305F230B759553C

# joining

names(processed_u) <- c("id","code","protocolo","area","property_name","geometry")

scrape <- list(processed_u,imovel_c, domino_c, cadastrante_c) %>%
  reduce(left_join, by = c("code", "id"))

write.csv(scrape,"~/Dropbox/Fieldwork_Summer2019/scrape/scraped_data2.csv")

