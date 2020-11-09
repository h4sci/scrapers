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


setwd("/Volumes/epl/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/CAR") # mac - owen
setwd("/Volumes/groups/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/CAR") # mac -sam
# setwd("Z:/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/CAR") # for windows

# read data ---------------------------------------------------------------

#data <- read_delim("point_test.csv", delim = ",", trim_ws = TRUE)
data2 <- read_delim("PGM_mesogregion.csv", delim = ",", trim_ws = TRUE)

# scrape pt 1) CAR imovel code & property id  ------------------------------------------------------------------

#first stage should be reading in shapefiles from http://www.car.gov.br/publico/municipios/downloads?sigla=PA
# need to bypass "are you human"

# prep urls
urls <- data2 %>% 
  pull(COD_IMOVEL) %>% 
  #  str_subset("list?filtro=") %>% 
  str_c("http://car.semas.pa.gov.br/site/consulta/imoveis/CODIGO/list?filtro=", ., "&pagina=1")

# prep urls
#urls <- data %>% 
#  pull(STR) %>% 
#  str_subset("wkt?") %>% 
#  str_c("http://car.semas.pa.gov.br/site/lotes/", .)

# try
html <- map(urls[1:10], possibly(read_html, NULL))
html_alt <- keep(html, ~ typeof(.x) == "list")

json <- map(urls[1:10], possibly(fromJSON, NULL))

x<- map(urls[1:10],function(x){fromJSON(x,simplifyVector = F)})
View(x[[1]][[3]][[1]]$geometry$geometries[[1]]$coordinates)
# attempting to pull out the data & spatial 
#st_as_sf(json[[1]]$features$properties,coords=as.data.frame(json[[1]]$features$geometry$geometries[[1]]$coordinates[1]))

# extract -----------------------------------------------------------------

# get main body for each
body <- map(html_alt, ~ html_nodes(., "p"))

y <- data.frame(code = data2$COD_IMOVEL)

for (i in 1:length(html)) {  y[i,"x"] <- html_text(body[[i]])  }      

#NOTE: this stage is subject to loss due to internal data structure (one observation was a list??). 
# Data still generates but missing values need to be dealt with.

yy <- na.omit(y)                   #remove nas
yy <- yy[!grepl("<<",yy$x),]       # remove strings that end in << not in }

dim(y)[1]-dim(yy)[1]  #loss of two observations

x <- lapply(yy$x,fromJSON) # if lapplies fails try as a for loop - will likely fail too but output will be up to failure (can use to identify issue)

#processing each row of data, but only if data was successfully scraped (length x[[i]]> 2) & no duplication of key data ( length(x[[i]][[3]][[3]][[1]] < 2) )

f <- data.frame()
for (i in 1:length(x)) {
  if   (length(x[[i]]) > 2 && length(x[[i]][[3]][[3]][[1]]) < 2 ) { f[i,"id"]          <- x[[i]][[3]][[3]][[1]]}  else     {f[i,"id"]        <- NA} 
  if   (length(x[[i]]) > 2 && length(x[[i]][[3]][[3]][[2]]) < 2 ) { f[i,"codigo"]      <- x[[i]][[3]][[3]][[2]]}  else     {f[i,"codigo"]    <- NA}
  if   (length(x[[i]]) > 2 && length(x[[i]][[3]][[3]][[3]]) < 2 ) { f[i,"protocolo"]   <- x[[i]][[3]][[3]][[3]]}  else     {f[i,"protocolo"] <- NA}
  if   (length(x[[i]]) > 2 && length(x[[i]][[3]][[3]][[4]]) < 2 ) { f[i,"area"]        <- x[[i]][[3]][[3]][[4]]}  else     {f[i,"area"]      <- NA}
  if   (length(x[[i]]) > 2 && length(x[[i]][[3]][[3]][[5]]) < 2 ) { f[i,"nome"]        <- x[[i]][[3]][[3]][[5]]}  else     {f[i,"nome"]      <- NA}
}

processed_u <- na.omit(f)

dim(f)[1]-dim(processed_u)[1]  #loss of additional 5 obs (7 total)

# scrape pt 2 - domino  ------------------------------------------------------------------

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

# test case for first chunk (Dados do Im?vel)
extract_sections <- function(html, names, values) {
  # get names
  names_out <- html %>% 
    html_nodes(names) %>% 
    html_text()
  # get values
  values_out <- html %>% 
    html_nodes(values) %>% 
    html_text()
  # compile in tibble
  tibble(names_out, values_out)
}

# run on all
chunk1 <- map(body2, extract_sections, names = "strong", values = "td")

# compile to df
domino <- map_df(chunk1, spread, names_out, values_out)

names(domino) <- c("domino_CPF_CNPJ","domino_nome","domino_tipo")
domino$domino_CPF_CNPJ <- str_remove(domino$domino_CPF_CNPJ , "CPF/CNPJ: ")
domino$domino_nome <- str_remove(domino$domino_nome , "Nome: ")
domino$domino_tipo <- str_remove(domino$domino_tipo , "Tipo: ")

domino <- domino %>%
  separate(domino_CPF_CNPJ,c("CPF","CNPJ"),"/")

domino_c <- data.frame(code = processed_u$codigo,id=processed_u$id,domino) #just sticking together as operation is sequential & output is same length as input (i.e. successful scrape)

# scrape pt 3 - imovel  ------------------------------------------------------------------

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
chunk2 <- map(body3, extract_sections, names = "strong", values = "td")

# compile to df
imovel <- map_df(chunk2, spread, names_out, values_out)

names(imovel)       <- c("imovel_CEP","access","Fisc_models","Muni_UF","imovel_nome","imovel_tipo","RURAL_URBANO")
imovel$imovel_CEP   <- str_remove(imovel$imovel_CEP , "CEP: ")
imovel$access       <- str_remove(imovel$access , "Descri??o de acesso: ")
imovel$Fisc_models  <- str_remove(imovel$Fisc_models , "M?dulos Fiscais: " )
imovel$imovel_nome  <- str_remove(imovel$imovel_nome , "Nome do im?vel: " )
imovel$Muni_UF      <- str_remove(imovel$Muni_UF , "Munic?pio/UF: " )
imovel$imovel_tipo  <- str_remove(imovel$imovel_tipo , "Tipo: ")
imovel$RURAL_URBANO <- str_remove(imovel$RURAL_URBANO , "Zona de localiza??o: ")

#swap , for  .
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
chunk3 <- map(body4, extract_sections, names = "strong", values = "td")

# compile to df

cadastrante <- map_df(chunk3, spread, names_out, values_out)

names(cadastrante) <- c("cadastrante_CPF","cadastrante_nome","cadastrante_ART","cadastrante_Profissao")
cadastrante$cadastrante_CPF <- str_remove(cadastrante$cadastrante_CPF , "CPF: ")
cadastrante$cadastrante_nome <- str_remove(cadastrante$cadastrante_nome , "Nome: ")
cadastrante$cadastrante_ART <- str_remove(cadastrante$cadastrante_ART , "N?mero de ART: ")
cadastrante$cadastrante_Profissao <- str_remove(cadastrante$cadastrante_Profissao , "Profiss?o: ")

cadastrante_c <- data.frame(code = processed_u$codigo,id=processed_u$id,cadastrante)

# joining

names(processed_u) <- c("id","code","protocolo","area","property_name")

scrape <- list(processed_u,imovel_c, domino_c, cadastrante_c) %>%
  reduce(left_join, by = c("code", "id"))

write.csv(scrape,"~/Dropbox/Fieldwork_Summer2019/scrape/scraped_data2.csv")

