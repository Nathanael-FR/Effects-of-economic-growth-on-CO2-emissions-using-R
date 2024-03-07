setwd("C:/Users/natha/Documents/R_NTU_2023/Propre/")
rm(list=ls())
library(leaflet)
library(sf)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(stringi)
library(rgdal)
library(rgeos)
library(htmltools)
library(ggplot2)
library(readr)
library(gridExtra)
library(zoo)
library(corrplot)
library(lmtest)
library(GGally)
library(car)
library(performance)
library(MASS)
library(stringr)
library(hrbrthemes)
library(vegan)
library(ape)
library(ade4)
library(gclus)
library(qgraph)
library(factoextra)
library(fpc)
library(e1071)
library(tree)
library(rpart)
library(rattle)
library(randomForest)
library(caret)
library(devtools)
library(stats)
library(ggrepel)
library(qgraph)
library(mapview)
library(webshot)
library(factoextra)
library(psych)
library(htmlwidgets)

pib_per_capita_original <- read.csv("PIB_par_hab.csv", sep = ",", skip = 4, header = TRUE)
co2_per_capita_original <- read.csv("CO2_par_hab.csv", sep = ",")
pib_original <- read.csv("PIB.csv", sep = ",", skip = 4, header = TRUE)
world <- st_read("World_Countries_Generalized.shp")

pib_per_capita <- pib_per_capita_original
co2_per_capita <- co2_per_capita_original
pib <- pib_original


#########################################################################################

col = c("COUNTRY","geometry")
world <- world[,col]
col = c("X2021","Country.Code")
pib_per_capita <- pib_per_capita[,col]
col = c("X2021","Country.Code","Country.Name")
pib <- pib[,col]


co2_per_capita_2021 <- co2_per_capita %>%
  filter(TIME == "2021") %>%
  dplyr::select(-INDICATOR, -SUBJECT, -MEASURE, -FREQUENCY, -TIME, -Flag.Codes )


merged_data <- left_join(co2_per_capita_2021, pib_per_capita, by = c("LOCATION" = "Country.Code"))
col <- c("COUNTRY_CODE","CO2_PER_CAPITA","PIB_PER_CAPITA")
merged_data <- left_join(merged_data, pib,  by = c("LOCATION" = "Country.Code"))
col <- c("LOCATION","CO2_PER_CAPITA","GDP_PER_CAPITA","GDP","COUNTRY")
colnames(merged_data) <- col

######################################################################################

merged_data <- merged_data[complete.cases(merged_data[, c("CO2_PER_CAPITA", "GDP_PER_CAPITA")]), ]

plot <- ggplot(merged_data, aes(x = CO2_PER_CAPITA, y = log(GDP_PER_CAPITA), size = GDP, color = CO2_PER_CAPITA)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Relationship between CO2 Emissions per Capita and GDP per Capita (2021)",
       x = "CO2 Emissions per Capita (Tonnes per capita)",
       y = "GDP per Capita (Current $)") +
  theme_minimal()

ggsave("CO2_GDP_Plot.png", plot, width = 10, height = 6, units = "in", dpi = 300)



################################################

geo_date <- left_join(merged_data, world)
geo_sf <- st_as_sf(geo_date)
breaks <- c(0, 3, 6, 10, 15, 20, 24, 35)

geo_sf$CO2_category <- cut(geo_sf$CO2_PER_CAPITA, breaks = breaks)

pal <- colorFactor(
  palette = c("white", "salmon", "indianred3", "red","darkred","black"),
  domain = geo_sf$CO2_category
)


map <- leaflet(data = geo_sf) %>%
  addProviderTiles("Stamen.TonerLite") %>%  
  addPolygons(
    fillColor = ~pal(geo_sf$CO2_category),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "white",
      bringToFront = TRUE
    ),
    label = ~paste("Country: ", geo_sf$COUNTRY, " | CO2 per Capita: ", geo_sf$CO2_PER_CAPITA)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = geo_sf$CO2_category,
    title = "CO2 per capita"
  )

##########################################"""

selected_countries <- c("FRA", "DEU", "GBR","LUX","SAU","TUR")

co2_per_capita_contenders <- co2_per_capita_original %>%
  filter(LOCATION %in% selected_countries) %>%
  dplyr::select(LOCATION, TIME, Value)

pib_per_capita_contenders <- pib_per_capita_original %>%
  filter(Country.Code %in% selected_countries) %>%
  dplyr::select(-Indicator.Name, -Indicator.Code, -Country.Name)

pib_per_capita_contenders <- pib_per_capita_contenders %>%
  pivot_longer(cols = 2:65, names_to = "year", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(year = as.numeric(sub("X", "", year)))

merged_data_contenders <- left_join(co2_per_capita_contenders, pib_per_capita_contenders, by = c("LOCATION" = "Country.Code", "TIME" = "year"))

col = c("LOCATION","TIME","CO2_PER_CAPITA","PIB_PER_CAPITA")
colnames(merged_data_contenders) <- col

merged_data_contenders <- merged_data_contenders %>%
  filter(!is.na(CO2_PER_CAPITA) & !is.na(PIB_PER_CAPITA)) %>%
  group_by(LOCATION) %>%
  filter(TIME >= 1990) %>%
  mutate(CO2_Percent = (CO2_PER_CAPITA - first(CO2_PER_CAPITA)) / first(CO2_PER_CAPITA) * 100,
         PIB_Percent = (PIB_PER_CAPITA - first(PIB_PER_CAPITA)) / first(PIB_PER_CAPITA) * 100)

co2_color <- "#69b3a2"
gdp_color <- rgb(0.2, 0.6, 0.9, 1)

plots_list <- lapply(unique(merged_data_contenders$LOCATION), function(country) {
  country_data <- merged_data_contenders %>% filter(LOCATION == country)
  ggplot(country_data, aes(x = TIME)) +
    geom_line(aes(y = CO2_Percent, color = "CO2 per Capita"), size = 1.2, linetype = "dashed") +
    geom_line(aes(y = PIB_Percent, color = "PIB per Capita"), size = 1.2) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Percentage Change", labels = scales::percent_format(scale = 1)) +
    scale_color_manual(values = c("CO2 per Capita" = "#5A9BD4", "PIB per Capita" = "#FF8C00")) +  # Couleurs moins vives
    labs(title = paste("Percentage Change in CO2 and GPD per Capita over the years -", country),
         color = "") +
    theme_minimal() +
    theme(
      axis.title.y = element_text(color = "black", size = 13),
      legend.title = element_text(size = 20),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
})



##########################################################################################################

exchanges <- read.csv("economics_indicators/exchanges.csv", sep = ",")
hdi <- read.csv("economics_indicators/human-development-index.csv", sep = ",")
inflation_food_nrg <- read.csv("economics_indicators/inflation.csv", sep = ",")
industry_prod <- read.csv("economics_indicators/industry_production.csv", sep = ",")
price_level <- read.csv("economics_indicators/price_lvl_index.csv", sep = ",") 

#########################################################################################

colnames(pib_per_capita) <- c("GDP_PER_CAPITA","LOCATION")

exchanges_2021 <- exchanges %>% filter(TIME == 2021)
hdi_2021 <- hdi %>% filter(Year == 2021)
inflation_2021 <- inflation_food_nrg %>% filter(TIME == 2021)
industry_prod_2021 <- industry_prod %>% filter(TIME == 2021)
price_level_2021 <- price_level %>% filter(TIME == 2021)

price_level_2021 <- price_level_2021 %>% dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -TIME, -SUBJECT)
industry_prod_2021 <- industry_prod_2021 %>% dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -TIME, -SUBJECT)
inflation_2021 <- inflation_2021 %>% dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -TIME)
exchanges_2021 <- exchanges_2021 %>% dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -TIME)
hdi_2021 <- hdi_2021 %>% dplyr::select(-Entity, -Year)

col = c("LOCATION","HDI")
colnames(hdi_2021) <- col
col = c("LOCATION","INDUSTRY_GROWTH")
colnames(industry_prod_2021) <- col
col = c("LOCATION","PRICE_LEVEL")
colnames(price_level_2021) <- col


inflation_2021 <- inflation_2021 %>%
  pivot_wider(names_from = "SUBJECT", values_from = "Value")
exchanges_2021 <- exchanges_2021 %>%
  pivot_wider(names_from = "SUBJECT", values_from = "Value")

merged_eco_factors <- left_join(inflation_2021, exchanges_2021, by = "LOCATION")
merged_eco_factors <- left_join(merged_eco_factors, hdi_2021, by = "LOCATION")
merged_eco_factors <- left_join(merged_eco_factors, industry_prod_2021, by = "LOCATION")
merged_eco_factors <- left_join(merged_eco_factors, price_level_2021, by = "LOCATION" )
merged_eco_factors <- left_join(merged_eco_factors, pib_per_capita, by = "LOCATION" )

merged_eco_factors <- na.omit(merged_eco_factors)

###############################################################################



indicators <- merged_eco_factors[, c("ENRG", "FOOD", "EXP", "IMP", "HDI", "INDUSTRY_GROWTH", "PRICE_LEVEL", "GDP_PER_CAPITA")]
scaled_indicators <- scale(indicators)

correlation_matrix <- cor(scaled_indicators)

corrplot(correlation_matrix, 
         order = "AOE",    
         type = "upper",   
         method = "color", 
         tl.cex = 0.7,     
         tl.col = "black", 
         addCoef.col = "black", 
         col.col = "black", 
         col.lab = "black", 
         bg = "white",     
)

cortest.bartlett(correlation_matrix,n = 35, diag = TRUE)

eigenvalues <- eigen(correlation_matrix)$values
barplot(eigenvalues, main = "Eigenvalues", names.arg = 1:length(eigenvalues))

pca <- rda(scaled_indicators)
summary(pca)
screeplot(pca, bstick = TRUE, npcs = length(pca$CA$eig), main = "PCA")
  
pca_result <- prcomp(scaled_indicators)
fviz_eig(pca_result, addlabels = TRUE)
#########################################################

fviz_nbclust(scaled_indicators, kmeans, method = "silhouette")

#####################################################""""

kmeans_result <- kmeans(scaled_indicators, centers = 4)
merged_eco_factors$Cluster <- as.factor(kmeans_result$cluster)

####################################################

pca_result <- prcomp(scaled_indicators)

pca_data <- as.data.frame(pca_result$x[, 1:2])

kmeans_result <- kmeans(pca_data, centers = 4)

clustered_data <- cbind(pca_data, Cluster = as.factor(kmeans_result$cluster), LOCATION = merged_eco_factors$LOCATION)

cluster_centers <- as.data.frame(kmeans_result$centers[, 1:2])

library(ellipse)

ggplot(clustered_data, aes(x = PC1, y = PC2, color = Cluster, label = LOCATION)) +
  geom_text(size = 4, hjust = 0, vjust = 0) +
  stat_ellipse(geom = "polygon", aes(group = Cluster, fill = Cluster), alpha = 0.25, type = "euclid", linetype = 1) +
  stat_ellipse(aes(group = Cluster), type = "norm", linetype = 2, lwd = 1.2) +
  scale_color_manual(values = c("darkorchid1", "#377EB8", "#4DAF4A","#E41A1C" )) +
  scale_fill_manual(values = c("darkorchid1", "#377EB8", "#4DAF4A","#E41A1C" ), guide = "none") +
  labs(title = "K-means Clustering on PCA.",
       subtitle = "95% Confidence level ellipses (Normal > Euclidian)",
       x = "PC 1",
       y = "PC 2",
       color = "Cluster") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "bottom"  # Position de la légende
  ) +
  guides(color = guide_legend(title = "Cluster", override.aes = list(fill = NA)),  # Pour supprimer la couleur de fond dans la légende
         linetype = guide_legend(title = "Ellipse Type", override.aes = list(size = c(2, 0.5), alpha = c(1, 0.25))))
 
##################################################################################
pca_result_3d <- prcomp(scaled_indicators, ncomp = 3)
pca_data_3d <- as.data.frame(pca_result_3d$x)
kmeans_result_3d <- kmeans(pca_data_3d, centers = 4)
clustered_data_3d <- cbind(pca_data_3d, Cluster = as.factor(kmeans_result_3d$cluster), LOCATION = merged_eco_factors$LOCATION)

library(rgl)
plot3d(clustered_data_3d$PC1, clustered_data_3d$PC2, clustered_data_3d$PC3,
       col = as.factor(kmeans_result_3d$cluster),
       size = 3,
       xlab = "PC1",
       ylab = "PC2",
       zlab = "PC3",
       main = "K-means Clustering on 3D PCA")

text3d(clustered_data_3d$PC1, clustered_data_3d$PC2, clustered_data_3d$PC3,
       text = clustered_data_3d$LOCATION,
       adj = c(-0.5, 0),
       cex = 0.7)

title3d(xlab = "PC1", ylab = "PC2", zlab = "PC3")

legend3d("topright", legend = levels(as.factor(kmeans_result_3d$cluster)), col = 1:4, pch = 16, cex = 0.8, title = "Cluster")

rglwidget()








###########################################################################
create_location_data <- function(location_code) {

    location_exchanges <- exchanges %>%
    filter(LOCATION == location_code) %>%
    dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -LOCATION) %>%
    pivot_wider(names_from = "SUBJECT", values_from = "Value")
  
  location_inflation <- inflation_food_nrg %>%
    filter(LOCATION == location_code) %>%
    dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -LOCATION) %>%
    pivot_wider(names_from = "SUBJECT", values_from = "Value")
  
  
  location_industry <- industry_prod %>%
    filter(LOCATION == location_code) %>%
    dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -LOCATION) %>%
    pivot_wider(names_from = "SUBJECT", values_from = "Value")
  
  location_hdi <- hdi %>%
    dplyr::filter(Code == location_code) %>%
    dplyr::select(-Entity, -Code) 
  
  col = c("TIME","HDI")
  colnames(location_hdi) <- col
  
  location_pib <- pib_per_capita_original %>%
    dplyr::select(-Indicator.Name, -Indicator.Code, -Country.Name) %>%
    pivot_longer(cols = 2:65, names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(sub("X", "", year))) %>%
    dplyr::filter(Country.Code == location_code) %>%
    dplyr::select(-Country.Code)
  
  location_co2 <- co2_per_capita %>%
    dplyr::filter(LOCATION == location_code) %>%
    dplyr::select(-INDICATOR, -MEASURE, -FREQUENCY, -Flag.Codes, -LOCATION) %>%
    pivot_wider(names_from = "SUBJECT", values_from = "Value")
  
  
  col = c("TIME","INDUSTRY_GROWTH")
  colnames(location_industry) <- col
  
  col = c("TIME","PIB_PER_CAPITA")
  colnames(location_pib) <- col
  
  
  location_data <- left_join(location_exchanges, location_inflation, by = "TIME") 
  location_data <- left_join(location_data, location_hdi, by = "TIME") 
  location_data <- left_join(location_data, location_industry, by = "TIME") 
  location_data <- left_join(location_data, location_pib, by = "TIME") 
  location_data <- left_join(location_data, location_co2, by = "TIME") 
  location_data <- na.omit(location_data)
  
  cor_data <- location_data[, -1]
  correlation_matrix <- cor(cor_data)

  return(location_data)
}

location_data <- create_location_data("TUR")

##############################################################################


cor_data <- scale(location_data[, -1])
correlation_matrix <- cor(cor_data)
corrplot(correlation_matrix, 
         order = "AOE", 
         type = "upper",
         method = "color",  # Utiliser des ellipses pour représenter la corrélation
         tl.cex = 0.7,     # Taille du texte sur les axes
         tl.col = "black", # Couleur du texte sur les axes
         addCoef.col = "black", # Couleur du texte des coefficients de corrélation
         col.col = "black", # Couleur de la légende
         col.lab = "black",)


model <- lm(CO2 ~  EXP+HDI+PIB_PER_CAPITA , data = as.data.frame(cor_data))
summary(model)
vif(model)

par(mfrow=c(2,2))
plot(model)

check_model(model) 
check_normality(model) 
check_heteroscedasticity(model)
check_outliers(model)
car::Anova(model)

library(lmtest)
library(skedastic)

lmtest::bptest(model)
lmtest::bptest(model, studentize = FALSE)

shapiro.test(resid(model))
#########################################################################
