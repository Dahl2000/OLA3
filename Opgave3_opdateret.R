#3,1
# Load libraries 
library(dst) 
library(ggplot2) 
library(dkstat) 
library(dplyr)
library(caret)
library(pROC)


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

# Familiens økonomi (fremtid) - Danmarks økonomi (nu vs. før) - Større køb (nu)
# Priser (nu vs. før) - Større køb (fremtid) - Spare op (fremtid) - Økonomisk status (nu)

# Dummy variabel
kombineret_data$`op/ned` <- ifelse(kombineret_data$Realvækst > 0, "Op", 
                                              ifelse(realforbrug_data_filtreret$Realvækst < 0, "Ned", "Neutral"))

kombineret_data$dummy <- ifelse(kombineret_data$`op/ned` == "Op", 1,0)

table(kombineret_data$`op/ned`)

# logistisk regression & predict
logi_opg3.1 <- glm(dummy ~ `Forbrugertillidsindikatoren`,
                   family = "binomial",
                   data = kombineret_data)
summary(logi_opg3.1)

forbruger <- data.frame(Forbrugertillidsindikatoren=-9.1)
predict(logi_opg3.1,forbruger, type = "response")
kombineret_data[100, 2] <- 0.5487493 # indsæt predict af realvækst
kombineret_data[100, 17] <- 1 # indsæt dummy af predict

# linær regrerssion
linærQ4 <- lm(Realvækst ~ Forbrugertillidsindikatoren,
              data = kombineret_data)
linærQ4_df <- data.frame(Forbrugertillidsindikatoren=-9.1)
predict(linærQ4, linærQ4_df, type = "response")

#####################
# OPG. 3.2
##################

jul <- data.frame(predict(logi_opg3.1, type = "response"))
jul$prd_direction <- ifelse(jul$predict.logi_opg3.1..type....response.. >= 0.5, 1.0, 0.0)
table(jul$prd_direction)
table(kombineret_data$dummy)

# confusion matrix
con <- data.frame(
  prediction = jul$prd_direction,
  realvækst = kombineret_data$dummy[1:99])
con$prediction <- factor(con$prediction, levels = c(0, 1))
con$realvækst <- factor(con$realvækst, levels = c(0, 1))

confusion <- confusionMatrix(data = con$prediction, reference = con$realvækst)
confusion

# ROC kurve
roc <- roc(kombineret_data$dummy[1:99], jul$predict.logi_opg3.1..type....response..)

roc_sensi <- roc$sensitivities
roc_speci <- roc$specificities

plot(roc_curve, col = "blue", main = "ROC Curve", print.auc = TRUE)
