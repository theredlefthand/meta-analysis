library(tidyverse)

ethikcamcsv <- read.csv("data/ethikcam.csv", header = T, sep = ';')

ethikcamcsv %>%
  summarise(Linienvergleich = ethikcamcsv$Linienpost - ethikcamcsv$Linienpre,
            Strichevergleich = ethikcamcsv$Strichepost - ethikcamcsv$Strichepre,
            Pfeilvergleich = ethikcamcsv$Pfeilpost - ethikcamcsv$Pfeilpre,
            Gestrichelterpfeilvergleich = ethikcamcsv$Gestrichelterpfeilpost - ethikcamcsv$Gestrichelterpfeilpre,
            Knotenvergleich = ethikcamcsv$Knotenpost - ethikcamcsv$Knotenpre)
