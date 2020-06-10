#Lê dados contidos em um CSV e transforma para um formato de Lista de Presença
#Utilizar com algoritmo apriori 

options(max.print = 10000000)

players <- '7'

suporte <- 0.2
confianca <- 0.2

#Caminho da pasta de trabalho 
setwd('C:/Users/LanaR/Documents/faculdade/projeto/csvs antigos/')

#Carrega os dados 
data <- read.csv(paste('7wonders', players, 'players.csv', sep = ''), header = TRUE, sep = ';')

#Nome de todas as colunas 
colmap <- colnames(data)

#Nome de todas as linhas 
rowmap <- data$Game.result

#Remove o espaço no nome das colunas
rowmap <- data$Game.result 
rowmap <- gsub(" ", "_", rowmap)

#Filtros para as colunas(por ranking)

#Filtro para os ganhadores 
filter_1st <- grepl("^X1st.*$", colmap)

#filter_2nd <- grepl("^X2nd.*$", colmap)
#filter_3rd <- grepl("^X3rd.*$", colmap)
#filter_4th <- grepl("^X4th.*$", colmap)
#filter_5th <- grepl("^X5th.*$", colmap)
#filter_6th <- grepl("^X6th.*$", colmap)
#filter_7th <- grepl("^X7th.*$", colmap)

#Filtro para os não ganhadores
column_params <- c("^X2nd.*$", "^X3rd.*$", "^X4th.*$", "^X5th.*$", "^X6th.*$", "^X7th.*$")
filter_nowinners <- grepl(paste(column_params, collapse = "|"), colmap)

#Aplicação do filtro nos dados para retornar valores de interesse 
data_1st <- data[filter_1st]
rownames(data_1st) <- data$Game.result

data_nowinners <- data[filter_nowinners]
rownames(data_nowinners) <- data$Game.result

head <- c("n", "Military_Conflicts_Victory", "Military_Conflicts_Defeat", "Treasury_Contents", "Wonder", "Civilian_Structures", "Scientific_Structures", "Commercial_Structures", "Guilds", "n", "n", "n", "n", "n", "n", "Wonder_side_A", "n", "n", "n", "n", "n", "n", "n", "n")

#Matriz transposta e transformação em data.frame
data_1st_t <- as.data.frame(t(data_1st))
colnames(data_1st_t) <- head

data_nowinners_t <- as.data.frame(t(data_nowinners))
colnames(data_nowinners_t) <- head

#Preparação dos dados para associação

#colunas de interesse
head <- c("militar_vitoria", "militar_derrota", "tesouro", "wonder", "civil", "cientifica", "comercial", "guilda", "wonder_sideA", "resultado")

#matriz de presença
data_1st_list_m <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
data_1st_list_1 <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
data_1st_list_3 <- data.frame(matrix(nrow = nrow(data_1st_t), ncol = length(head)))
colnames(data_1st_list_m) <- head
colnames(data_1st_list_1) <- head
colnames(data_1st_list_3) <- head

data_nowinners_list_m <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
data_nowinners_list_1 <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
data_nowinners_list_3 <- data.frame(matrix(nrow = nrow(data_nowinners_t), ncol = length(head)))
colnames(data_nowinners_list_m) <- head
colnames(data_nowinners_list_1) <- head
colnames(data_nowinners_list_3) <- head

#medias das colunas
mediaMilitarVitoria = mean(as.integer(as.character(data_1st_t$Military_Conflicts_Victory)))
mediaMilitarDerrota = mean(as.integer(as.character(data_1st_t$Military_Conflicts_Defeat)))
mediaTesouro        = mean(as.integer(as.character(data_1st_t$Treasury_Contents)))
mediaWonder         = mean(as.integer(as.character(data_1st_t$Wonder)))
mediaCivil          = mean(as.integer(as.character(data_1st_t$Civilian_Structures)))
mediaCientifica     = mean(as.integer(as.character(data_1st_t$Scientific_Structures)))
mediaComercial      = mean(as.integer(as.character(data_1st_t$Commercial_Structures)))
mediaGuilda         = mean(as.integer(as.character(data_1st_t$Guilds)))

#1 quartil das colunas
quartil1MilitarVitoria = summary(as.integer(as.character(data_1st_t$Military_Conflicts_Victory)))[[2]]
quartil1MilitarDerrota = summary(as.integer(as.character(data_1st_t$Military_Conflicts_Defeat)))[[2]]
quartil1Tesouro        = summary(as.integer(as.character(data_1st_t$Treasury_Contents)))[[2]]
quartil1Wonder         = summary(as.integer(as.character(data_1st_t$Wonder)))[[2]]
quartil1Civil          = summary(as.integer(as.character(data_1st_t$Civilian_Structures)))[[2]]
quartil1Cientifica     = summary(as.integer(as.character(data_1st_t$Scientific_Structures)))[[2]]
quartil1Comercial      = summary(as.integer(as.character(data_1st_t$Commercial_Structures)))[[2]]
quartil1Guilda         = summary(as.integer(as.character(data_1st_t$Guilds)))[[2]]

#3 quartil das colunas
quartil3MilitarVitoria = summary(as.integer(as.character(data_1st_t$Military_Conflicts_Victory)))[[5]]
quartil3MilitarDerrota = summary(as.integer(as.character(data_1st_t$Military_Conflicts_Defeat)))[[5]]
quartil3Tesouro        = summary(as.integer(as.character(data_1st_t$Treasury_Contents)))[[5]]
quartil3Wonder         = summary(as.integer(as.character(data_1st_t$Wonder)))[[5]]
quartil3Civil          = summary(as.integer(as.character(data_1st_t$Civilian_Structures)))[[5]]
quartil3Cientifica     = summary(as.integer(as.character(data_1st_t$Scientific_Structures)))[[5]]
quartil3Comercial      = summary(as.integer(as.character(data_1st_t$Commercial_Structures)))[[5]]
quartil3Guilda         = summary(as.integer(as.character(data_1st_t$Guilds)))[[5]]

#discretização e carga dos dados para a matriz de presença

#aqui como só possuímos os dados dos ganhadores então todos recebem 1 nessa coluna (resultado == ganhador)
#os demais receberiam 0 

#media
data_1st_list_m[, "resultado"]  <- 1
data_1st_list_m$militar_vitoria <- as.integer(as.integer(as.character(data_1st_t$Military_Conflicts_Victory)) >= mediaMilitarVitoria)
data_1st_list_m$militar_derrota <- as.integer(as.integer(as.character(data_1st_t$Military_Conflicts_Defeat)) >= mediaMilitarDerrota)
data_1st_list_m$tesouro         <- as.integer(as.integer(as.character(data_1st_t$Treasury_Contents)) >= mediaTesouro)
data_1st_list_m$wonder          <- as.integer(as.integer(as.character(data_1st_t$Wonder)) >= mediaWonder)
data_1st_list_m$civil           <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= mediaCivil)
data_1st_list_m$cientifica      <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= mediaCientifica)
data_1st_list_m$comercial       <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= mediaComercial)
data_1st_list_m$guilda          <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= mediaGuilda)
data_1st_list_m$wonder_sideA    <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list_m[, "resultado"]  <- 0
data_nowinners_list_m$militar_vitoria <- as.integer(as.integer(as.character(data_nowinners_t$Military_Conflicts_Victory)) >= mediaMilitarVitoria)
data_nowinners_list_m$militar_derrota <- as.integer(as.integer(as.character(data_nowinners_t$Military_Conflicts_Defeat)) >= mediaMilitarDerrota)
data_nowinners_list_m$tesouro         <- as.integer(as.integer(as.character(data_nowinners_t$Treasury_Contents)) >= mediaTesouro)
data_nowinners_list_m$wonder          <- as.integer(as.integer(as.character(data_nowinners_t$Wonder)) >= mediaWonder)
data_nowinners_list_m$civil           <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= mediaCivil)
data_nowinners_list_m$cientifica      <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= mediaCientifica)
data_nowinners_list_m$comercial       <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= mediaComercial)
data_nowinners_list_m$guilda          <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= mediaGuilda)
data_nowinners_list_m$wonder_sideA    <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist_media <- rbind(data_1st_list_m, data_nowinners_list_m)

#1 quartil
data_1st_list_1[, "resultado"]  <- 1
data_1st_list_1$militar_vitoria <- as.integer(as.integer(as.character(data_1st_t$Military_Conflicts_Victory)) >= quartil1MilitarVitoria)
data_1st_list_1$militar_derrota <- as.integer(as.integer(as.character(data_1st_t$Military_Conflicts_Defeat)) >= quartil1MilitarDerrota)
data_1st_list_1$tesouro         <- as.integer(as.integer(as.character(data_1st_t$Treasury_Contents)) >= quartil1Tesouro)
data_1st_list_1$wonder          <- as.integer(as.integer(as.character(data_1st_t$Wonder)) >= quartil1Wonder)
data_1st_list_1$civil           <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= quartil1Civil)
data_1st_list_1$cientifica      <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= quartil1Cientifica)
data_1st_list_1$comercial       <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= quartil1Comercial)
data_1st_list_1$guilda          <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= quartil1Guilda)
data_1st_list_1$wonder_sideA    <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list_1[, "resultado"]  <- 0
data_nowinners_list_1$militar_vitoria <- as.integer(as.integer(as.character(data_nowinners_t$Military_Conflicts_Victory)) >= quartil1MilitarVitoria)
data_nowinners_list_1$militar_derrota <- as.integer(as.integer(as.character(data_nowinners_t$Military_Conflicts_Defeat)) >= quartil1MilitarDerrota)
data_nowinners_list_1$tesouro         <- as.integer(as.integer(as.character(data_nowinners_t$Treasury_Contents)) >= quartil1Tesouro)
data_nowinners_list_1$wonder          <- as.integer(as.integer(as.character(data_nowinners_t$Wonder)) >= quartil1Wonder)
data_nowinners_list_1$civil           <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= quartil1Civil)
data_nowinners_list_1$cientifica      <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= quartil1Cientifica)
data_nowinners_list_1$comercial       <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= quartil1Comercial)
data_nowinners_list_1$guilda          <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= quartil1Guilda)
data_nowinners_list_1$wonder_sideA    <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist_1quartil <- rbind(data_1st_list_1, data_nowinners_list_1)

#3 quartil
data_1st_list_3[, "resultado"]  <- 1
data_1st_list_3$militar_vitoria <- as.integer(as.integer(as.character(data_1st_t$Military_Conflicts_Victory)) >= quartil3MilitarVitoria)
data_1st_list_3$militar_derrota <- as.integer(as.integer(as.character(data_1st_t$Military_Conflicts_Defeat)) >= quartil3MilitarDerrota)
data_1st_list_3$tesouro         <- as.integer(as.integer(as.character(data_1st_t$Treasury_Contents)) >= quartil3Tesouro)
data_1st_list_3$wonder          <- as.integer(as.integer(as.character(data_1st_t$Wonder)) >= quartil3Wonder)
data_1st_list_3$civil           <- as.integer(as.integer(as.character(data_1st_t$Civilian_Structures)) >= quartil3Civil)
data_1st_list_3$cientifica      <- as.integer(as.integer(as.character(data_1st_t$Scientific_Structures)) >= quartil3Cientifica)
data_1st_list_3$comercial       <- as.integer(as.integer(as.character(data_1st_t$Commercial_Structures)) >= quartil3Comercial)
data_1st_list_3$guilda          <- as.integer(as.integer(as.character(data_1st_t$Guilds)) >= quartil3Guilda)
data_1st_list_3$wonder_sideA    <- as.integer(data_1st_t$Wonder_side_A == "yes")

data_nowinners_list_3[, "resultado"]  <- 0
data_nowinners_list_3$militar_vitoria <- as.integer(as.integer(as.character(data_nowinners_t$Military_Conflicts_Victory)) >= quartil3MilitarVitoria)
data_nowinners_list_3$militar_derrota <- as.integer(as.integer(as.character(data_nowinners_t$Military_Conflicts_Defeat)) >= quartil3MilitarDerrota)
data_nowinners_list_3$tesouro         <- as.integer(as.integer(as.character(data_nowinners_t$Treasury_Contents)) >= quartil3Tesouro)
data_nowinners_list_3$wonder          <- as.integer(as.integer(as.character(data_nowinners_t$Wonder)) >= quartil3Wonder)
data_nowinners_list_3$civil           <- as.integer(as.integer(as.character(data_nowinners_t$Civilian_Structures)) >= quartil3Civil)
data_nowinners_list_3$cientifica      <- as.integer(as.integer(as.character(data_nowinners_t$Scientific_Structures)) >= quartil3Cientifica)
data_nowinners_list_3$comercial       <- as.integer(as.integer(as.character(data_nowinners_t$Commercial_Structures)) >= quartil3Comercial)
data_nowinners_list_3$guilda          <- as.integer(as.integer(as.character(data_nowinners_t$Guilds)) >= quartil3Guilda)
data_nowinners_list_3$wonder_sideA    <- as.integer(data_nowinners_t$Wonder_side_A == "yes")

#junta tudo para uma matriz completa
full_datalist_3quartil <- rbind(data_1st_list_3, data_nowinners_list_3)

library(arules)

for(i in 1:10){
  full_datalist_media[,i] <- factor(full_datalist_media[,i])
  full_datalist_1quartil[,i] <- factor(full_datalist_1quartil[,i])
  full_datalist_3quartil[,i] <- factor(full_datalist_3quartil[,i])
}

rules_media <- apriori(full_datalist_media, parameter = list(target = "rules", conf = confianca, supp = suporte, minlen = 2))
rules_1quartil <- apriori(full_datalist_1quartil, parameter = list(target = "rules", conf = confianca, supp = suporte, minlen = 2))
rules_3quartil <- apriori(full_datalist_3quartil, parameter = list(target = "rules", conf = confianca, supp = suporte, minlen = 2))

rules_sub_media <- subset(rules_media, subset = rhs %in% "resultado=1" | lhs %in% "resultado=1")
rules_sub_1quartil <- subset(rules_1quartil, subset = rhs %in% "resultado=1" | lhs %in% "resultado=1")
rules_sub_3quartil <- subset(rules_3quartil, subset = rhs %in% "resultado=1" | lhs %in% "resultado=1")

write(rules_media, file = paste("apriori_rules_pontos_media_", players, "players_", suporte, ".csv", sep = ''), sep = ';', quote = FALSE, row.names = FALSE)
write(rules_1quartil, file = paste("apriori_rules_pontos_1quartil_", players, "players_", suporte, ".csv", sep = ''), sep = ';', quote = FALSE, row.names = FALSE)
write(rules_3quartil, file = paste("apriori_rules_pontos_3quartil_", players, "players_", suporte, ".csv", sep = ''), sep = ';', quote = FALSE, row.names = FALSE)

write(rules_sub_media, file = paste("apriori_rules_pontos_sub_media_", players, "players_", suporte, ".csv", sep = ''), sep = ';', quote = FALSE, row.names = FALSE)
write(rules_sub_1quartil, file = paste("apriori_rules_pontos_sub_1quartil_", players, "players_", suporte, ".csv", sep = ''), sep = ';', quote = FALSE, row.names = FALSE)
write(rules_sub_3quartil, file = paste("apriori_rules_pontos_sub_3quartil_", players, "players_", suporte, ".csv", sep = ''), sep = ';', quote = FALSE, row.names = FALSE)
