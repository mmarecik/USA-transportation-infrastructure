rm(list = ls())

# Instalacja bibliotek
install.packages("psych")
install.packages("gridExtra")
install.packages("ggfortify")
install.packages("rgl")
install.packages("MASS")
install.packages("factoextra")
install.packages("clusterSim")
install.packages("scatterplot3d")

# Wczytywanie danych

library(readxl)
data_oryg <- read_excel("data.xlsx")
data <- data_oryg


# Wstepna analiza wykorzystywanych danych

head(data)

library(psych)
describe(data)


# Wspolczynnik zmiennosci

x <- c()
for(i in 2:6){
  x[i-1] = sd(unlist(data[,i]))/mean(unlist(data[,i]))
}

var_coef <- data.frame(variable = colnames(data[,-1]), variance_coef = x)
var_coef


# Wykrywanie outlierów

library(gridExtra)
library(ggplot2)

p1 <- ggplot(data, aes(y=travel_time_to_work)) + 
  geom_boxplot(fill="#9eccfa", alpha=0.8) + 
  labs(y = "Mean travel time to work (minutes) per one commuter") +
  theme_minimal()

p2 <- ggplot(data, aes(y=hours_in_congestion)) + 
  geom_boxplot(fill="#b3cce6", alpha=0.8) + 
  labs(y = "Annual time spent in congestion (in hours)") +
  theme_minimal()

p3 <- ggplot(data, aes(y=fatalities)) + 
  geom_boxplot(fill="#c7ccd1", alpha=0.8) + 
  labs(y = "Fatal car accidents per 100 million vehicle miles traveled") +
  theme_minimal()

p4 <- ggplot(data, aes(y=roads_in_acceptable_conditions)) + 
  geom_boxplot(fill="#a3ccf5", alpha=0.8) + 
  labs(y = "Proportion of road miles in acceptable conditions") +
  theme_minimal()

p5 <- ggplot(data, aes(y=bridges_in_poor_conditions)) + 
  geom_boxplot(fill="#b8cce0", alpha=0.8) + 
  labs(y = "Proportion of bridges in poor conditions") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol=3)

library(dplyr)
top_3_congestion <- data %>%
  top_n(3, hours_in_congestion)

top_3_congestion

grid.arrange(p4, p5, ncol=2)


# Korelacja

library(GGally) 
ggpairs(data[,2:6]) +
  theme(text = element_text(size=20), axis.text = element_text(size=12))


# Average travel time to work by state (USA)

ggplot(data, aes(x=travel_time_to_work)) + 
  geom_histogram(bins=7, fill="#69b3a2", color="#cccccc", alpha=0.8, boundary=10) +
  scale_x_continuous(breaks = seq(min(data[,2])-3, max(data[,2])+3, round((max(data[,2]) - min(data[,2]))/7, 0))) +
  ggtitle("Histogram of average travel time to work by state (USA)") +
  theme_minimal() +
  xlab("Mean travel time to work (minutes)") +
  ylab("Count")

max_travel_time_to_work <- data %>% 
  slice(which.max(travel_time_to_work))
max_travel_time_to_work

min_travel_time_to_work <- data %>% 
  slice(which.min(travel_time_to_work))
min_travel_time_to_work


# Mean travel time to work and hours spent in congestion 

ggplot(data)  + 
  geom_bar(aes(x=reorder(state, hours_in_congestion), y=hours_in_congestion, color = "Mean travel time to work (in minutes)"),stat="identity", fill="#69b3a2", alpha=0.7)+
  geom_line(aes(x=reorder(state, hours_in_congestion), y=travel_time_to_work, group = 1, color = "Annual time spent in traffic jam (in hours)"),
            stat="identity", size=1) + 
  labs(title= "Mean travel time to work (in minutes) vs annual time spent in traffic jam (in hours) by one commuter",
       x="State", y="Annual time spent in congestion (in hours)") +
  theme_minimal() + theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=1)) +
  scale_color_manual(name = "Legend", values = c("Mean travel time to work (in minutes)"="#38947d", "Annual time spent in traffic jam (in hours)" = "#737373"))

max_time_in_congestion <- data %>% 
  slice(which.max(hours_in_congestion))
max_time_in_congestion

cor.test(data$hours_in_congestion, data$travel_time_to_work, method="kendall")


# Fatalities

ggplot(data, aes(x=fatalities)) + 
  geom_histogram(bins=7, fill="#69b3a2", color="#cccccc", alpha=0.8, boundary=10) +
  scale_x_continuous(breaks = seq(0, 2, 0.2)) +
  ggtitle("Histogram of average travel time to work by state (USA)") +
  theme_minimal() +
  xlab("Fatal car accidents per 100 million vehicle miles traveled") +
  ylab("Count")


max_fatalities <- data %>% 
  slice(which.max(fatalities))
max_fatalities

min_fatalities <- data %>% 
  slice(which.min(fatalities))
min_fatalities


# Road miles and bridges in acceptable conditions

max_roads_acceptable <- data %>% 
  slice(which.max(roads_in_acceptable_conditions))
data.frame(max_roads_acceptable)

min_roads_acceptable <- data %>% 
  slice(which.min(roads_in_acceptable_conditions))
data.frame(min_roads_acceptable)

max_poor_bridges <- data %>% 
  slice(which.max(bridges_in_poor_conditions))
data.frame(max_poor_bridges)

min_poor_bridges <- data %>% 
  slice(which.min(bridges_in_poor_conditions))
data.frame(min_poor_bridges)


#####-----------------------------------------------------------------
##### Porzadkowanie liniowe
#####-----------------------------------------------------------------

## Metoda Hellwiga porzadkowania liniowego

# Zamiana zmiennych diagnostycznych na stymulanty

data_stym <- data
data_stym[, 2:4] <- (-1)*data_stym[, 2:4]
data_stym[, 6] <- (-1)*data_stym[, 6]


# Standaryzacja

data_stand <- data_stym

for (i in 2:6){
  data_stand[, i] <- scale(data_stand[ ,i], center = TRUE, scale = TRUE)
}


# Tworzenie wzorca

ideal <- c(max(data_stand$travel_time_to_work),
           max(data_stand$hours_in_congestion), max(data_stand$fatalities),
           max(data_stand$roads_in_acceptable_conditions), max(data_stand$bridges_in_poor_conditions))


# Obliczenie odleglosci obiektow od wzroca

for (i in 1:5){
  data_stand[, i+6] <- (data_stand[, i+1] - ideal[i])^2
}

for (i in 1:nrow(data_stand)){
  data_stand[i, 12] <- sqrt(sum(data_stand[i, 7:11]))
}

# Odleglosc mozliwie daleka

odl_moz_daleka <- mean(unlist(data_stand[, 12])) + 2*sd(unlist(data_stand[, 12]))


# Wartosci miary dla obiektow

for (i in 1:nrow(data_stand)){
  data_stand[i, 13] <- 1 - data_stand[i, 12] / odl_moz_daleka
}

hellwig_final <-data_stand[order(data_stand[ ,13], decreasing=TRUE), -c(2:12)]
colnames(hellwig_final) <- c("state", "hellwig_score")
hellwig_final$hellwig_rank <- c(1:51)
hellwig_final


avg_hellwig_score <- mean(hellwig_final$hellwig_score)
sd_hellwig_score <- sd(hellwig_final$hellwig_score)

# Interpretacja wynikow na podstawie sredniej wartosci wspolczynnika rankingowego

for(i in 1:nrow(hellwig_final)){
  if(hellwig_final[i,2] >= avg_hellwig_score + sd_hellwig_score){
    hellwig_final[i,4] <- 1
  }
  else if(hellwig_final[i,2] >= avg_hellwig_score &&
          hellwig_final[i,2] < avg_hellwig_score + sd_hellwig_score){
    hellwig_final[i,4] <- 2
  }
  else if(hellwig_final[i,2] >= avg_hellwig_score - sd_hellwig_score &&
          hellwig_final[i,2] < avg_hellwig_score ){
    hellwig_final[i,4] <- 3
  }
  else{
    hellwig_final[i,4] <- 4
  }
}

colnames(hellwig_final) <- c("state", "hellwig_score", "hellwig_rank", "hellwig_group")
hellwig_final


### Metoda TOPSIS

# Tworzenie macierzy znormalizowanej

normalized_matrix <- data_stym
x <- c()

for (i in 1:5){
  x[i] <- sqrt(sum(unlist(data_stym[, i+1])^2))
}

for(i in 1:nrow(data_stym)){
  for(j in 2:6){
    normalized_matrix[i, j] <- data_stym[i, j] / x[j-1]
  }
}


# Szukanie wzorca i antywzorca

p_ideal <- c(max(normalized_matrix$travel_time_to_work),
             max(normalized_matrix$hours_in_congestion), max(normalized_matrix$fatalities),
             max(normalized_matrix$roads_in_acceptable_conditions), max(normalized_matrix$bridges_in_poor_conditions))

n_ideal <- c(min(normalized_matrix$travel_time_to_work),
             min(normalized_matrix$hours_in_congestion), min(normalized_matrix$fatalities),
             min(normalized_matrix$roads_in_acceptable_conditions), min(normalized_matrix$bridges_in_poor_conditions))

# Obliczenie odleglosci od wzroca i antywzorca

for (i in 1:5){
  normalized_matrix[,6+i] <- (normalized_matrix[,1+i] - p_ideal[i])^2
}

for (i in 1:5){
  normalized_matrix[,11+i] <- (normalized_matrix[,1+i] - n_ideal[i])^2
}

for (i in 1:nrow(data_stym)){
  normalized_matrix[i,17] <- sqrt(sum(normalized_matrix[i,7:11])) # odleglosci od wzorca
}

for (i in 1:nrow(data_stym)){
  normalized_matrix[i,18] <- sqrt(sum(normalized_matrix[i,12:16])) # odleglosci od antywzorca
}

# Wspolczynnik rankingowy

for (i in 1:nrow(data_stym)){
  normalized_matrix[i,19] <- normalized_matrix[i,18] / (normalized_matrix[i,18] + normalized_matrix[i,17])
}

topsis_final <- normalized_matrix[order(normalized_matrix[ ,19], decreasing=TRUE), -c(2:18)]
colnames(topsis_final) <- c("state", "topsis_score")
topsis_final$topsis_rank <- c(1:51)
topsis_final

avg_topsis_score <- mean(topsis_final$topsis_score)
sd_topsis_score <- sd(topsis_final$topsis_score)


# Interpretacja wynikow na podstawie sredniej wartosci wspolczynnika rankingowego

for(i in 1:nrow(topsis_final)){
  if(topsis_final[i,2] >= avg_topsis_score + sd_topsis_score){
    topsis_final[i,4] <- 1
  }
  else if(topsis_final[i,2] >= avg_topsis_score &&
          topsis_final[i,2] < avg_topsis_score + sd_topsis_score){
    topsis_final[i,4] <- 2
  }
  else if(topsis_final[i,2] >= avg_topsis_score - sd_topsis_score &&
          topsis_final[i,2] < avg_topsis_score ){
    topsis_final[i,4] <- 3
  }
  else{
    topsis_final[i,4] <- 4
  }
}

colnames(topsis_final) <- c("state", "topsis_score", "topsis_rank", "topsis_group")
topsis_final


ordering_final<- merge(hellwig_final,topsis_final,by="state")
ordering_final[order(ordering_final$hellwig_score, decreasing=TRUE),]


same_rank <- 0
same_group <- 0

for(i in 1:length(ordering_final[,1])){
  if(ordering_final[i,3] == ordering_final[i,6]){
    same_rank = same_rank+1
  }
  if(ordering_final[i,4] == ordering_final[i,7]){
    same_group = same_group+1
  }
}

same_rank
same_group

cor(ordering_final[,2], ordering_final[,5], method = "pearson")
cor(ordering_final[,3], ordering_final[,6], method = "spearman")





#####-----------------------------------------------------------------
##### Analiza skupien
#####-----------------------------------------------------------------

## Grupowanie podziałowe

# Standaryacja zmiennych

data_standarized <- data.frame(data)
rownames(data_standarized) <- data_standarized[,1]
data_standarized[,1] <- NULL

data_standarized <- as.data.frame(scale(data_standarized))


# Wizualizacja macierzy odleglosci

library(factoextra)

distance <- get_dist(data_standarized)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


## Metoda k - srednich

# Podzial na 2 grupy

k2 = kmeans(data_standarized, centers=2, nstart = 20)
sort(k2$cluster)


# Usuniecie obserwacji odstajacej

data_standarized_2 <- data_standarized[!(row.names(data_standarized) == "Rhode Island"), ]


# Podzial na 2 grupy

k2 = kmeans(data_standarized_2, centers=2, nstart = 20)
sort(k2$cluster)

clustering_results <- data.frame(data[data$state != "Rhode Island", ])  
rownames(clustering_results) <- clustering_results[,1]
clustering_results[,1] <- NULL


# Porownanie statystyk w 2 grupach

library(psych)
clustering_results$k2 <- as.factor(k2$cluster)
describeBy(clustering_results[,-6], group = clustering_results$k2)

fviz_cluster(k2, data = data_standarized_2) + theme_minimal()


# Podzial na 3 grupy

k3 = kmeans(data_standarized_2, centers=3, nstart = 20)
sort(k3$cluster)


# Porownanie statystyk w 3 grupach

clustering_results$k3 <- as.factor(k3$cluster)
describeBy(clustering_results[,-c(6,7)], group = clustering_results$k3)

fviz_cluster(k3, data = data_standarized_2) + theme_minimal()


# Wykres zmian cakowitej zmeinnosci wewnatrz grup w zaleznosci od ilosci klastrow - metoda lokcia

set.seed(1234)

library(purrr)
kmeans_wss <- map_dbl(2:10, function(i) {
  m <- kmeans(data_standarized, centers = i)
  m$tot.withinss
})

ggplot(data.frame(cluster = 2:10, within.ss = kmeans_wss), aes(cluster, within.ss)) +
  geom_point() + geom_line() + scale_x_continuous(breaks = 1:10) + 
  theme_minimal() + labs(x="Number of clusters", y="The within-sum-of-squares ( withinss )")


# Podzial na 4 grupy

k4 = kmeans(data_standarized_2, centers=4, nstart = 20)
sort(k4$cluster)

# Porownanie statystyk w 4 grupach

clustering_results$k4 <- as.factor(k4$cluster)
describeBy(clustering_results[,-c(6,7,8)], group = clustering_results$k4)

fviz_cluster(k4, data = data_standarized_2) + theme_minimal()


ggplot(clustering_results, aes(x=hours_in_congestion, y=fatalities)) +
  geom_text(aes(label=rownames(clustering_results), color=k2)) + 
  theme_minimal() + theme(legend.position = "None") + 
  labs(x="Annual time spent in congestion (in hours)", 
       y="Fatal car accidents per 100 million vehicle miles traveled")

ggplot(clustering_results, aes(x=hours_in_congestion, y=fatalities)) +
  geom_text(aes(label=rownames(clustering_results), color=k4)) + 
  theme_minimal() + theme(legend.position = "None") + 
  labs(x="Annual time spent in congestion (in hours)", 
       y="Fatal car accidents per 100 million vehicle miles traveled")



### Grupowanie hierarchiczne


# Wyznaczamy odleglosci

dist_eucl <- dist(data_standarized_2, "euclidean")
dist_manh <- dist(data_standarized_2, "manhattan")
dist_mink <- dist(data_standarized_2, "minkowski", p=3)
dist_maxi <- dist(data_standarized_2, "maximum")

ward_eucl <- hclust(dist_eucl, "ward.D")
ward_manh <- hclust(dist_manh, "ward.D")
ward_mink <- hclust(dist_mink, "ward.D")
ward_maxi <- hclust(dist_maxi, "ward.D")

# Dendrogramy

library(ggdendro)
d1 <- ggdendrogram(ward_eucl, theme_dendro = TRUE) + 
  ggtitle("Euclidean distance, Ward method") + 
  labs(x="", y="")
d2 <- ggdendrogram(ward_manh, theme_dendro = TRUE) + 
  ggtitle("Manhattan distance, Ward method") + 
  labs(x="", y="")
d3 <- ggdendrogram(ward_mink, theme_dendro = TRUE) + 
  ggtitle("Minkowski distance,Ward method") + 
  labs(x="", y="")
d4 <- ggdendrogram(ward_maxi, theme_dendro = TRUE) + 
  ggtitle("Maximum distance, Ward method") + 
  labs(x="", y="")

grid.arrange(d1, d2, d3, d4, ncol=2)

# Wybor konkretnej odleglosci i metody

d1


library(dendextend)
dend_ward_eucl <- color_branches(ward_eucl, k=4)

plot(dend_ward_eucl, main="Odleglosc euklidesowa, metoda Warda, 8 klas")


dend_ward_eucl %>% set("branches_lwd", 2) %>% 
  plot(main="Odleglosc euklidesowa, metoda Warda, 8 klas")
abline(h=9, lty=5)


# Analiza wykresu odleglosci

plot(x=1:49, y=ward_eucl$height, type = "S", 
     xlab = "Krok", ylab = "Odleglosc", main="Wykres odleglosci wiazania wzgledem etapow wiazania")

# Wybor liczby skupien - ineksy

library(clusterSim)
indexes <- data.frame(G1=0, G2=0, G3=0, S=0)


for(i in 2:10){
  cluster <- cutree(ward_eucl, k=i)
  g1 <- index.G1(data_standarized, cluster)
  g2 <- index.G2(dist_eucl, cluster)
  g3 <- index.G3(dist_eucl, cluster)
  s <- index.S(dist_eucl, cluster)
  indexes[i,] <- c(g1, g2, g3, s)
}

indexes <- indexes[-1,]
indexes$k <- 2:10
indexes

#G1  
p_1 <- ggplot(data=indexes, aes(x=k, y=G1)) + geom_line() + geom_point() + labs(x="k", y="Index G1 - max")

#G2
p_2 <- ggplot(data=indexes, aes(x=k, y=G2)) + geom_line() + geom_point() + labs(x="k", y="Index G2 - max")

#G3
p_3 <- ggplot(data=indexes, aes(x=k, y=G3)) + geom_line() + geom_point() + labs(x="k", y="Index G3 - min")

#S
p_4 <- ggplot(data=indexes, aes(x=k, y=S)) + geom_line() + geom_point() + labs(x="k", y="Index S - max")


grid.arrange(p_1, p_2, p_3, p_4, ncol=2)





#####-----------------------------------------------------------------
##### Skalowanie wielowymiarowe
#####-----------------------------------------------------------------

# Klasyczne skalowanie wielowymiarowe

# Odleglosc euklidesowa miedzy obiektami

obj_dist <- dist(data_standarized_2)


# Rzutowanie na 2 wymiary

library(ggfortify)
obj_dist <- dist(data_standarized_2)
sww2 <- cmdscale(obj_dist, k = 2)

autoplot(cmdscale(obj_dist, eig = TRUE), 
         label = TRUE, label.size = 4) + 
  labs(title= "", x="Wymair 1", y="Wymiar 2") + theme_minimal()

autoplot(cmdscale(obj_dist, eig = TRUE), label = TRUE, label.size = 4, colour=clustering_results$k4) + 
  labs(title= "", x="Wymair 1", y="Wymiar 2") + theme_minimal()


# Obliczamy wspolczynnik stres

obj_dist2 <- dist(sww2)
stress2 <- sqrt((sum(obj_dist-obj_dist2)^2)/sum(obj_dist^2))
stress2

# Rzutowanie na 3 wymiary
sww3 <- cmdscale(obj_dist, k = 3)
sww3

library("rgl")
plot3d(sww3, xlab="Wymiar 1", ylab="Wymiar 2", zlab="Wymiar 3")+
  text3d(sww3, texts = rownames(sww3))


library("scatterplot3d")
s3d  <- scatterplot3d(sww3, xlab="Wymiar 1", ylab="Wymiar 2", zlab="Wymiar 3",
                      pch = 16, color=clustering_results$k4)
text(s3d$xyz.convert(sww3), labels = rownames(clustering_results), cex = 0.8)

# Obliczamy wspolczynnik stres

obj_dist3 <- dist(sww3)
stress3 <- sqrt((sum(obj_dist-obj_dist3)^2)/sum(obj_dist^2))
stress3


## Metoda skalowania Sammona

# Rzutowanie na 2 wymiary

sammon2 <- sammon(obj_dist, k = 2)
sammon2$points

autoplot(sammon(obj_dist, k = 2), label = TRUE, label.size = 4) + 
  labs(title= "", x="Wymair 1", y="Wymiar 2") + theme_minimal()

autoplot(sammon(obj_dist, k = 2), label = TRUE, label.size = 4, colour=clustering_results$k4) + 
  labs(title= "", x="Wymair 1", y="Wymiar 2") + theme_minimal()


# Wspolczynnik stress

sammon2$stress


# Rzutowanie na 3 wymiary

sammon3 <- sammon(obj_dist, k = 3)
sammon3$points

plot3d(sammon3$points, xlab="Wymiar 1", ylab="Wymiar 2", zlab="Wymiar 3") +
  text3d(sammon3$points, texts = rownames(sammon3$points))

s3d  <- scatterplot3d(sammon3$points, xlab="Wymiar 1", ylab="Wymiar 2", zlab="Wymiar 3",
                      pch = 16, color=clustering_results$k4)
text(s3d$xyz.convert(sammon3$points), labels = rownames(clustering_results), cex = 0.8)


# Wspolczynnik stress

sammon3$stress








