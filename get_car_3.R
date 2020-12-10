library(tidyverse)
library(rvest)
library(XML)
#library(methods)
library(data.tree)
#library(rjson)
library(jsonlite)
library(geojsonio)
library(sf)
library(httr)
library(ows4R)


#adding a quick attempt at using ows4R
#see https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
car_wms <- "http://car.semas.pa.gov.br/site/geoserver/secar-pa/wms"
url <- parse_url(car_wms)
url$query <- list(service = "WMS",
                  version = "1.3.0",
                  request = "GetCapabilities")
request <- build_url(url)
request

car_client <- OWSClient$new(car_wms,
                            serviceVersion = "1.3.0")

car_client
##### NOTE:There are a number of places bugs can arise & should be checked #######################

#  resolved 1) When scraping imovel & property id can get issues processing the json script due to internal structures. Currently treating as NA. Shouldn't be issue
#           2a) Dropped observations in html mapping - this is big issue as code & id join is not via matching, simply attaching so if not same length is invalid
#           2b) observation can be identified by error message and removed if necessary but has to be done FOR ALL SCRAPED SECTIONS
#           2c) sometimes you just need to rerun that chunk
#                   - potential solution: find a way to append id to every row when scraping, then could left_join but how??
#           3) Additionally, data should be validated manually against database to ensure this join was correct


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

 #get PARÁ 
 PA <- geobr::read_state(code_state = "PA")
 
 #gen lat longs to cover PA
 minmax <-  st_bbox(PA) 
 LONG <- seq(minmax["xmin"],minmax["xmax"],by=0.005) 
 LAT <- seq(minmax["ymin"],minmax["ymax"],by=0.005)  
 
 
 
# can it all just be gotten from this?
 #http://car.semas.pa.gov.br/site/geoserver/secar-pa/wms?SERVICE=WMS&REQUEST=GetCapabilities
 
# it could also be done by defining origin & creating a grid via something like this https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat
# scale of the grid OR the increment used in seq() will have BIG IMPACT in missing properties
# currently set at 0.005 degree - or around 0.66 km (at the equator)
# can assume that smallest property in BR Am would be INCRA plots of 25ha @ 250m*1000m

 #HOWEVER  WLL NEED AN INTERMEDIARY STEP to turn into a grid like this
 latlonggrid <- expand_grid(LONG,LAT) 
 
 # then another to get rid of points outside of PARA 
 test  <- st_multipoint(as.matrix(latlonggrid))
 test2 <- st_sfc(test,crs=st_crs(PA))
 test3 <- st_intersection(test2,PA) #possibly st_within
 test4 <- as.data.frame(st_coordinates(test3))
 
 # check it is a real grid,  little slow as pulls PA from internet
 ggplot() + 
   geom_point(data=test4,aes(x=X,y=Y))+
   geom_sf(PA,mapping=aes(fill="red")) 
   

 # can then create the urls with the following:
urls_alt <- paste0("http://car.semas.pa.gov.br/site/lotes/wkt?pontoWkt=POINT(",paste(test4$X,test4$Y,sep="+"),")")

#after this it is the same as before

# NOTE: THIS WILL BE VERY  COMPUTATIONALLY HEAVY, CURRENTLY 6,734,860 clicks. Ways to improve:
#        Remove any points outside the boundaries of PA via:
#               - turning latlonggrid into a multipoint sf object & clipping to extent of Pará state
#                         - see https://www.jessesadler.com/post/simple-feature-objects/ for how to work with sf objects
#                         - and this for clipping https://stackoverflow.com/questions/56247266/sf-package-extract-clip-and-return-a-polygon
#                         - this discussion useful for how to get the geometries back into dataframe https://github.com/r-spatial/sf/issues/231
#                         - I had some success with the following: as.data.frame(unclass(sf_object$geometry[1])[[1]][[1]][[1]][[1]])
#                           however, that code is written to get lat long from single row of sf object...
#                               - note: will need to set crs to be identical to Pará
#                               - note 2: will need to turn back into dataframe for this code to work
#               - optimally choose a point gap that captures all properties with least overlap, taking into account
#                 both minimum expected property size & corseness of the scale recognised when "clicking" the map
#         will save a lot of energy/time

# scrape pt 1) CAR imovel code & property id  ------------------------------------------------------------------

# prep urls
urls <- data2 %>% 
  pull(COD_IMOVEL) %>% 
  #  str_subset("list?filtro=") %>% 
  str_c("http://car.semas.pa.gov.br/site/consulta/imoveis/CODIGO/list?filtro=", ., "&pagina=1")

urls <- c(urls_alt[2999890:3000010])

#extract the json data (wrapped inside an html node) #note: currently first 10
json <- map(urls, possibly(as.json, NULL))


#this function/code now robust for CAR codes (although returns centroid as well) & geometries, 
#it is also robust for multiple returns

#test with lat long
#x <- geojson_sf(as.json("http://car.semas.pa.gov.br/site/lotes/wkt?pontoWkt=POINT(-46.94938659667969+-2.174084421122967)"))

key_sf <- map(json,possibly(geojson_sf,NULL)) %>% reduce(rbind)

key_sf_u <- unique(key_sf)

#for the lat long urls, need to expand the JSON of coords still held within column "centro"
point <- lapply(key_sf_u$centro,RJSONIO::fromJSON) %>%
  lapply(function(e) list(geo_type = e$type,long=e$coordinates[1],lat=e$coordinates[2])) %>%
  data.table::rbindlist()

processed <- cbind(key_sf_u,point)   #will only work for the lat long urls #potentially should use st_bind_cols
processed$centro<-NULL

#processed_u <- na.omit(processed)
#processed_u <- key_sf_u
ggplot() + geom_sf(processed,mapping=aes(fill=as.numeric(id)))+
  geom_sf(processed,fill=NA,colour="black",mapping=aes())
#dim(processed)[1]-dim(processed_u)[1]  #loss of X obs



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
html_alt2 <- keep(html2, ~ typeof(.x) == "list")  #is this necessary?
#html.h2 <- html

# extract subparts ----------------------------

# get main body for each
body2 <- map(html_alt2, ~ html_nodes(., ".table-condensed")) #changing function from html_node() to html_nodes() makes robust for multiple returns here

#create a function that extracts data for all nodes & indicates which node came from
extract_scts_mult <- function(html,names) {
  l <- list()    
  for (i in 1:length(html)) {
      names_out <-  html[[i]] %>%                         # get contents of node
        html_nodes(names) %>%        
        html_text() %>%
        as.data.frame() %>%
        separate(1,c("name","values"),": ",extra = "merge") %>%    # split at : into names & values
        mutate(names=paste(name,i,sep="_")) %>%
        select(-name)
        l[[i]] <- names_out
      }
      d <- reduce(l,rbind)      #works as no matter how many owners, will capture the under "names" & "values"
      return(d)
  }

#

# run on all
chunk1 <- map(body2, extract_scts_mult, names = "td")

# compile to df
domino <- map_df(chunk1, spread, names, values) #note: not sure this will work with diff lengths of owners

names(domino) <- paste0("dom_",names(domino))

# just sticking together as operation is sequential & output is same length as input (i.e. successful scrape)
# I THINK possibly() function above could fix this? replace the NULL with NA?
# would be nice if could keep id throughout
domino_c <- data.frame(code = processed_u$codigo,id=processed_u$id,domino) 

# scrape pt 3 - imovel  ------------------------------------------------------------------

#NOTE: IMOVEL HAS OTHER ATTRIBUTES WE ARE DROPPING in other nodes (activities on property, forest area)

#can be fixed by taking it node by node, 
#  i.e. extract_sections works great for .table-condensed node1, 
#  but node 2 td only returns the types of LU, easy to fix, just need to add name placeholder
#  for node 3, all info is held in td, which is unfortunate. Fine if always just 2 items, but if longer might need odd & even rule?

# when run all the urls should check if all the nodes are length 3

#test run
#d <- html_nodes(read_html("http://car.semas.pa.gov.br/site/imovel/417490/imovelFichaResumida"), ".table-condensed")

# prep urls
urls3 <- processed_u %>% 
  pull(id) %>% 
  #str_subset("") %>% 
  str_c("http://car.semas.pa.gov.br/site/imovel/", . ,"/imovelFichaResumida")

# try
html3 <- map(urls3, possibly(read_html, NULL))
html_alt3 <- keep(html3, ~ typeof(.x) == "list")

# extract subparts ----------------------------

extract_scts_imov <- function(html, names1="td",names2="th") {
  pt1 <-  html[[1]]%>%                         # get contents of node
    html_nodes(names1) %>%        
    html_text() %>%
    as.data.frame() %>%
    separate(1,c("names","values"),": ",extra = "merge")          # split at : into names & values
  
  values2 <-  html[[2]]%>%                         # get contents of node
    html_nodes(names1) %>%        
    html_text()
  
  if (length(values2)==0) {
    pt2<-NA
  }else{
    pt2 <- data.frame(names=paste0("prod_syst_",1:length(values2)),values=values2)    
    }
  
  names3 <-  html[[3]]%>%                         # get contents of node
    html_nodes(names2) %>%        
    html_text()
  #get values
  values3 <- html[[3]] %>% 
    html_nodes(names1) %>% 
    html_text()
  # compile in tibble
  if (length(values3)==0 |length(names3)==0 ) {
    pt3<-NA
  }else{
    pt3 <- data.frame(names=names3, values=values3)    
  }
  
  if(!is.na(pt3)&&!is.na(pt2)) {
    out <- rbind(pt1,pt2,pt3)
  }else if (!is.na(pt2) && is.na(pt3)) {
    out <- rbind(pt1,pt2) 
  }else if (is.na(pt2) && !is.na(pt3)) {
    out <- rbind(pt1,pt3) 
  } else{
    out <- rbind(pt1)    
    } 
  
  
  return(out)
  
}

# get main body for each
body3 <- map(html_alt3, ~ html_nodes(., ".table-condensed"))

# run on all
chunk2 <- map(body3, extract_scts_imov)

# compile to df
imovel <- map_df(chunk2, spread, names, values)

#names(imovel)       <- c("imovel_CEP","access","Fisc_models","Muni_UF","imovel_nome","imovel_tipo","RURAL_URBANO")

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

# scrape pt 5- demomnstrativo  ------------------------------------------------------------------

#note: missing for some reason

# e.g. http://car.semas.pa.gov.br/site/demonstrativo/imovel/PA-1502301-561B1A1DDF85493BA305F230B759553C
d <- html_nodes(read_html("http://car.semas.pa.gov.br/site/demonstrativo/imovel/PA-1502301-561B1A1DDF85493BA305F230B759553C"),".demonstrativo-section")
dd <-html_nodes(read_html("http://car.semas.pa.gov.br/site/demonstrativo/imovel/PA-1502301-561B1A1DDF85493BA305F230B759553C"),".titulo-situacao") #is there a way to also pull?
ddd <-html_nodes(read_html("http://car.semas.pa.gov.br/site/demonstrativo/imovel/PA-1502301-561B1A1DDF85493BA305F230B759553C"),"#item-sobreposicao-imovel .col-md-7") #is there a way to also pull?
html_text(ddd)

#this works ge
p <- html_text(html_nodes(d[[1]],".item-label"))
pp <- html_text(html_nodes(d[[1]],".col-md-8"))
cbind(p,pp)

y <- html_text(html_nodes(d[[2]],".item-label"))
yy <- html_text(html_nodes(d[[2]],".col-md-9"))
cbind(y,yy)

n <- html_text(html_nodes(d[3:7],".item-label"))
nn <- html_text(html_nodes(d[3:7],".col-md-3"))
cbind(n,nn)

s <- html_text(html_nodes(d[[8]],".item-label"))
ss <- html_text(html_nodes(d[[8]],".col-md-2"))
cbind(s,ss)

o <- html_text(html_nodes(d[[9]],".text-right"))
oo <- html_text(html_nodes(d[[9]],".col-md-2"))
cbind(o,oo)
#turn into a function 

# joining all ---------------------------------------------------------------------------------

names(processed_u) <- c("id","code","protocolo","area","property_name","geometry")

scrape <- list(processed_u,imovel_c, domino_c, cadastrante_c) %>%
  reduce(left_join, by = c("code", "id"))

write.csv(scrape,"~/Dropbox/Fieldwork_Summer2019/scrape/scraped_data2.csv")



