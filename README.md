# scrapers
Web Scraping With R

Project to extract spatial and non-spatial data for rural properties in the Brazilian Amazonian state of Pará from a public repository http://car.semas.pa.gov.br/

Project will create a clear pipeline to regularly extract this data and display it via a shiny app.

1. Run get_car3.R. This will produce a .csv file with the scraped data. 
2. Not yet automated - we reduced the .csv file to a subset of the rows and columns for demonstration.
3. Run shiny. The reduced .csv is used by the script to create an interactive webpage hosted on Github. 
4. 
