####Problema 1####

library(rio)
link="https://github.com/schrodingercase/practicaest2/raw/master/Base1.xlsx"
dataStata=import(link)

#195 obs y 6 var.

###Problema 2###

linkPage="https://en.wikipedia.org/wiki/Index_of_Economic_Freedom" 
linkPath = '//*[@id="mw-content-text"]/div/table[4]'

library(htmltab)
economic = htmltab(doc = linkPage, 
                   which =linkPath) 

str(economic)

#39 obs y 15 var.