library(htmltab)
library(stringr)

##EJERCICIO1

web = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/343.html"
path = '//*[@id="fieldListing"]'
dataCIA = htmltab(doc = web, which =path)

ncol(dataCIA)

Respuesta 1a: Se encuentran 230 observaciones y 2 columnas
Respuesta 1b: Una variable de texto que describe el país y una variable numérica que describe edades.

##EJERCICIO2

dataCIA$promedio = dataCIA$`Median age`

dataCIA$promedio=str_split(string = dataCIA$promedio,
                     pattern = ' ',
                     simplify = T)[,1]
dataCIA$promedio=str_split(string = dataCIA$promedio,
                           pattern = ':',
                           simplify = T)[,2]

dataCIA$promedio = as.numeric(dataCIA$promedio)

min(dataCIA$promedio)
max(dataCIA$promedio)
summary(dataCIA$promedio)

##EJERCICIO3

respuesta: Valor minimo es 14.8, valor maximo es 55.4 y promedio es 31.55




