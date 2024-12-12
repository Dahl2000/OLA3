library(devtools) 
library(dkstat) 
library(dst) 
library(danstat) 
library(tidyverse) 
library(lubridate) 
library(stats) 
library(ggplot2) 
library(pls)
library(corrplot)

##########################
# OPG. 2.1
####################
#Forbrugerforventninger
{meta_data <- dst_meta("FORV1")
str(meta_data)

# Hent dataen
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# Filtrer data
FORV1_filter <- list(
  INDIKATOR = "*",  # Brug "*" for at inkludere alle indikatorer
  Tid = "*"         # Brug "*" for at inkludere alle tidspunkter
)

FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

# bred format
FORV1Data_wide <- FORV1Data %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

# Sørg for, at TID-kolonnen er i kvartalsformat
FORV1Data_wide$TID <- paste0(format(as.Date(FORV1Data_wide$TID), "%Y"), quarters(as.Date(FORV1Data_wide$TID)))

# Summér alle kolonner gruppevis baseret på samme kvartal
FORV1Data_wide <- aggregate(. ~ TID, data = FORV1Data_wide, FUN = mean, na.rm = TRUE)

start_date <- "2000Q1"
end_date <- "2024Q4"

# Subsetter data mellem start- og slutdatoen
FORV1Data_wide <- subset(FORV1Data_wide, TID >= start_date & TID <= end_date)

# forkort spørgsmåls navne
colnames(FORV1Data_wide)[3:ncol(FORV1Data_wide)] <- c("Familiens økonomi (nu vs. før)",
                                                  "Familiens økonomi (fremtid)",
                                                  "Danmarks økonomi (nu vs. før)",
                                                  "Danmarks økonomi (fremtid)",
                                                  "Større køb (nu)",
                                                  "Priser (nu vs. før)",
                                                  "Priser (fremtid)",
                                                  "Arbejdsløshed (fremtid)",
                                                  "Større køb (fremtid)",
                                                  "Spare op (nu)",
                                                  "Spare op (fremtid)",
                                                  "Økonomisk status (nu)")}

# Realvækst husholdningernes forbrugsudgift
{realforbrug.meta <- dst_meta(table = "NKHC021", lang = "da") 

# vælger filteret på realforbrug dataframe (vil gerne hente totalforbruget, 2020 priser, der er sæssonkorrigeret)
realforbrug.filter <- list( 
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

# Henter data ud fra filteret der er sat ovenover
realforbrug_data <- dst_get_data(table = "NKHC021", query = realforbrug.filter)  
realforbrug_data <- realforbrug_data[, -c(1:3)]  # Fjern de tre første kolonner med metadata, da der er na værdier i value

# Beregning af realvækst
realforbrug_data$Realvækst <- (realforbrug_data$value / dplyr::lag(realforbrug_data$value, 4) - 1) * 100  # Beregn realvækst år-over-år

# udvælger rækker og kolonner for perioden 2000 Q1 til 2024 Q4 (række 41-138)
realforbrug_data_filtreret <- realforbrug_data[c(41:nrow(realforbrug_data)), c("TID", "Realvækst")] 

# tilføj 1 rækker mere så det matcher længden på Forv1data_wide
new_rows <- realforbrug_data_filtreret[100,]
realforbrug_data_filtreret[100,] <- new_rows}

# Nu kan du kombinere datasættene med cbind
kombineret_data <- cbind(FORV1Data_wide[, 1], Realvækst = realforbrug_data_filtreret$Realvækst, FORV1Data_wide[, 2:ncol(FORV1Data_wide)])
colnames(kombineret_data)[1] <- "TID"
rownames(kombineret_data) <- NULL

# Konverter de relevante kolonner til numeriske værdier 
kombineret_data[2:ncol(kombineret_data)] <- lapply(kombineret_data[2:ncol(kombineret_data)], as.numeric) 

# PCA analyse
pcr.fit <- pcr(`Realvækst` ~ 
                 `Familiens økonomi (nu vs. før)` +
                 `Familiens økonomi (fremtid)` +
                 `Danmarks økonomi (nu vs. før)` +
                 `Danmarks økonomi (fremtid)` +
                 `Større køb (nu)` +
                 `Priser (nu vs. før)` +
                 `Priser (fremtid)` +
                 `Arbejdsløshed (fremtid)` +
                 `Større køb (fremtid)` +
                 `Spare op (nu)` +
                 `Spare op (fremtid)` +
                 `Økonomisk status (nu)`,
                 data = kombineret_data, 
                 scale = TRUE,
                 validation = "CV")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.1 <- loadings.pcr.fit[1:12]^2
w.indicators.1
loadings(pcr.fit)

comp1 <- pcr.fit$loadings[,1]
vægte <- data.frame(comp1)

#####################
# OPG. 2.2
#####################

# Korrelation
corrmaxtrix <- kombineret_data[,-c(1,3)]
corrmaxtrix <- corrmaxtrix[-100,]

# Beregn korrelationen mellem hver af X'erne og Y (realvæskt)
correlation_matrix <- round(cor(corrmaxtrix, use = "complete.obs"), digits = 2)

# Ekstraher korrelationen mellem realvæskt og hver af de øvrige variabler
correlation_with_Y <- correlation_matrix[, "Realvækst"]

colnames(correlation_matrix) <- abbreviate(colnames(correlation_matrix), minlength = 5)
rownames(correlation_matrix) <- abbreviate(rownames(correlation_matrix), minlength = 5)

corrplot(correlation_matrix, method = "color", addCoef.col = "black", 
         type = "upper", diag = FALSE, tl.cex = 0.8)

##################
# OPG. 2.3
#################

pred_q4 <- data.frame(kombineret_data[100,-c(1:3)])
pred_q4 <- t(data.frame(apply(pred_q4, 2, as.numeric)))
colnames(pred_q4) <- c("Familiens økonomi (nu vs. før)",
                                                      "Familiens økonomi (fremtid)",
                                                      "Danmarks økonomi (nu vs. før)",
                                                      "Danmarks økonomi (fremtid)",
                                                      "Større køb (nu)",
                                                      "Priser (nu vs. før)",
                                                      "Priser (fremtid)",
                                                      "Arbejdsløshed (fremtid)",
                                                      "Større køb (fremtid)",
                                                      "Spare op (nu)",
                                                      "Spare op (fremtid)",
                                                      "Økonomisk status (nu)")


pca.p <- predict(pcr.fit, pred_q4, ncomp = 1)
pca.p

# PLSR
pls.fit <- plsr(`Realvækst` ~ 
                  `Familiens økonomi (nu vs. før)` +
                  `Familiens økonomi (fremtid)` +
                  `Danmarks økonomi (nu vs. før)` +
                  `Danmarks økonomi (fremtid)` +
                  `Større køb (nu)` +
                  `Priser (nu vs. før)` +
                  `Priser (fremtid)` +
                  `Arbejdsløshed (fremtid)` +
                  `Større køb (fremtid)` +
                  `Spare op (nu)` +
                  `Spare op (fremtid)` +
                  `Økonomisk status (nu)`,
                data = kombineret_data, 
                scale = TRUE,
                validation = "CV")

pls.p <- predict(pls.fit, pred_q4, ncomp = 1)
pls.p

summary(pls.fit)
loadings.pls.fit <- pls.fit$loadings
pls.indicators.1 <- loadings.pls.fit[1:12]^2
pls.indicators.1
loadings(pls.fit)
pls.named_loadings <- setNames(pls.indicators.1, names(pls.fit$terms))
top_5 <- sort(abs(pls.named_loadings), decreasing = TRUE)[1:5]
print(top_5)
# SPG9, SPG1, SPG3, SPG5, SPG2