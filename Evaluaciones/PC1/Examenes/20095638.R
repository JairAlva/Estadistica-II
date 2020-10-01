# PC 1 - JAIR ALVA#
#20095638#

library(rio)
link = "https://github.com/schrodingercase/practicaest2/raw/master/Base1.xlsx"

extraido = import(link)

# La base data extraida tiene 195 elementos (observaciones) y 6 variabes#


#Pregunta 2##

library(htmltab)
linkpagina="https://en.wikipedia.org/wiki/Index_of_Economic_Freedom"
linkpathtabla = "html/table.sortable.wikitable.jquery-tablesorter"

tablapc = htmltab(doc =linkpagina, which =linkpathtabla)
tablapc