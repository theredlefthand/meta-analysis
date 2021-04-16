library(tidyverse)

ethikcamcsv <- read.csv("data/ethikcam.csv", header = T, sep = ';')

ethikcomparison <- ethikcamcsv %>%
  summarise(Linienvergleich = ethikcamcsv$Linienpost - ethikcamcsv$Linienpre,
            DurchgezogeneLinienvergleich = ethikcamcsv$DurchgezogeneLinienpost - ethikcamcsv$DurchgezogeneLinienpre,
            Strichevergleich = ethikcamcsv$Strichepost - ethikcamcsv$Strichepre,
            Pfeilvergleich = ethikcamcsv$DurchgezogenePfeilpost - ethikcamcsv$DurchgezogenePfeilpre,
            Gestrichelterpfeilvergleich = ethikcamcsv$Gestrichelterpfeilpost - ethikcamcsv$Gestrichelterpfeilpre,
            Knotenvergleich = ethikcamcsv$Knotenpost - ethikcamcsv$Knotenpre)

ethikcomparison

meanethik <- ethikcomparison %>%
  summarise_if(is.numeric, mean) %>%
  mutate(across(where(is.numeric), round, 2))

meanethik

ethikcamvergleich <- rbind(ethikcomparison, meanethik)

ethikcamvergleich

cor.test(ethikcomparison$Linienvergleich, ethikcomparison$Knotenvergleich)
