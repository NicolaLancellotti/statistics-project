## ---- chunk
df <-data.frame(
  economica = c(4.8, 5.1, 3.9, 4.3, 10.8, 4.7, 6.4, 4.1, 3, 4.2, 
                3.4, 2.9, 2.8, 2.2, 1.9, 2.6, 2.2, 1.7, 1.9, 1.9),
  salute = c(17.6, 19.5, 18.2, 18.4, 28.7, 18.7, 17.3, 19.2, 17.1, 16, 
             15.3, 14.6, 18.3, 15.3, 13, 11, 12.1, 10.2, 16.2, 12),
  famiglia = c(36.2, 35.3, 39.5, 35.3, 46.4, 38.5, 37.3, 38.7, 35.6, 35.7, 
               33.7, 31.1, 33.9, 30.6, 24.3, 22.2, 30.5, 28, 30.5, 30.4),
  amici = c(24.5, 25.5, 26.5, 25.5, 35.2, 25.1, 26.8, 28.4, 25.4, 26, 
            23.2, 22.8, 23.8, 19.7, 16.5, 16.2, 21.6, 17.8, 20.1, 21.6), 
  tempoLibero = c(15.4, 15, 16.4, 16, 23.8, 15.4, 16.6, 15.5, 15.5, 17.3, 
                  13.9, 13, 11.9, 12.1, 9.3, 9.3, 10.6, 9.2, 11.6, 10.9)
)

rownames(df) <- c("Piemonte", "Valle d'Aosta", "Liguria", "Lombardia", 
                  "Trentino Alto Adige", "Veneto", "Friuli-Venezia Giulia",
                  "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio",
                  "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata",
                  "Calabria", "Sicilia", "Sardegna")

## ---- chunk2
column.names <- c("Economica", "Salute", "Famiglia", "Amici", "Tempo libero")

rowCount = dim(df)[1]
colCount = dim(df)[2]

## ---- chunk3
classi <- c(0, seq(0.5, 100.5, by = 1))

calcolaFrequenzeRelative = function(x) {
  table(cut(x, breaks = classi, right = FALSE)) / rowCount  
}

frequenzeRelative <- sapply(df, calcolaFrequenzeRelative)

## ---- chunk4
distribuzioneEmpiricaContinua = function(index, lim) {
  x <-frequenzeRelative[, index]
  Fi<-cumsum(x)
  Fi<-c(0,Fi)
  main = paste("Distribuzione empirica continua - Soddisfazione", column.names[index])
  plot(classi, Fi, type = "l",axes = FALSE, 
       main = main,col="red", 
       xlab = " ", ylab = " ",
       xlim = c(0, lim))
  axis(1, classi)
  axis(2, format(Fi, digits = 1), las = 2)
  box()
}