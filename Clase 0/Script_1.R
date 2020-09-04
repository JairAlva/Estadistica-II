library(rio)

prueba = import("Data/prueba.xlsx")

table(prueba$pais)

barplot(prueba$pais) #este comando, no funciona, corrígelo
