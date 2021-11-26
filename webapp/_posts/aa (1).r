#Cargamos las librerias a usar en este proyecto
library(randomForest)
library(highcharter)
library(billboarder)
library(stringr)
library(ggplot2)
library(xgboost)
library(viridis)
library(timetk)
library(tidyr)
library(MASS)
library(dplyr)
library(caret)
library(DMwR)
library(ROSE)
library(glue)
library(pROC)
library(DT)

#Cargando el conjunto de datos
BankChurners <- read.csv("./BankChurners.csv")

# Eliminar columnas que no se utilizarán durante el análisis
BankChurners <- select(BankChurners, -c("CLIENTNUM", "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1", "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2"))

#Observamos las primeras 6 filas de la variable
head(BankChurners)

#Asignamos a otra variable para ver la correlación de todas las variables
BankChurners_cor <- BankChurners

# Reemplazo de los elementos de la variable Attrition_Flag
sub_target <- function(x){
        if(x == "Existing Customer"){
                return(0)
        } else {
                return(1)
        }
}

BankChurners_cor$Attrition_Flag <- sapply(BankChurners_cor$Attrition_Flag, sub_target)

# Reemplazo de los elementos de la variable Attrition_Flag
sub_target1 <- function(x){
  if(x == "Existing.Customer"){
    return(0)
  } else {
    return(1)
  }
}

## Obtención de la matriz de correlaciones con el método de Spearman
cor_spearman <- cor(BankChurners_cor[, sapply(BankChurners_cor, is.numeric)], method = 'spearman')

#Observamos la correkación de las variables
cor_spearman

#Visualizando con un mapa de calor la matriz de correlación con el método pearson
as.matrix(data.frame(cor_spearman)) %>% 
        round(3) %>% #round
        hchart() %>% 
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = "Coeficientes de correlación de Spearman", align = "center") %>% 
        hc_legend(align = "center") %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
        hc_plotOptions(
                series = list(
                        boderWidth = 0,
                        dataLabels = list(enabled = TRUE)))

# Estado civil
BankChurners %>%
        filter(Attrition_Flag == "Attrited Customer") %>%
        count(Marital_Status) %>%
        hchart("treemap", hcaes(x = Marital_Status, value = n, color = n)) %>%
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
        hc_title(text = "Número de cancelaciones por estado civil")

# Ingresos anuales
BankChurners %>%
  filter(Attrition_Flag == "Attrited Customer") %>%
  count(Income_Category) %>%
  hchart("treemap", hcaes(x = Income_Category, value = n, color = n)) %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
  hc_title(text = "Cantidad de cancelaciones por ingreso anual")

# Nivel de educación 
BankChurners %>%
  filter(Attrition_Flag == "Attrited Customer") %>%
  count(Education_Level) %>%
  hchart("treemap", hcaes(x = Education_Level, value = n, color = n)) %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
  hc_title(text = "Número de cancelaciones por nivel educativo")

# Tipo de tarjeta
BankChurners %>%
  filter(Attrition_Flag == "Attrited Customer") %>%
  count(Card_Category) %>%
  hchart("treemap", hcaes(x = Card_Category, value = n, color = n)) %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
  hc_title(text = "Número de cancelaciones por tipo de tarjeta", align = "center")

# Estado civil
BankChurners  %>% group_by(Marital_Status) %>% 
    filter(Attrition_Flag=="Attrited Customer") %>% 
    summarize(NN=n(), percent=round((n()/nrow(.)*100),2))%>% 
    arrange(desc(NN))

# Ingresos anuales
BankChurners  %>% group_by(Income_Category) %>% 
    filter(Attrition_Flag=="Attrited Customer") %>% 
    summarize(NN=n(), percent=round((n()/nrow(.)*100),2))%>% 
    arrange(desc(NN))

# Nivel de educación
BankChurners  %>% group_by(Education_Level) %>% 
    filter(Attrition_Flag=="Attrited Customer") %>% 
    summarize(NN=n(), percent=round((n()/nrow(.)*100),2))%>% 
    arrange(desc(NN))

# Tipo de tarjeta
BankChurners  %>% group_by(Card_Category) %>% 
    filter(Attrition_Flag=="Attrited Customer") %>% 
    summarize(NN=n(), percent=round((n()/nrow(.)*100),2)) %>% 
    arrange(desc(NN))



# Transacciones totales del cliente
BankChurners %>%
  dplyr::select(Total_Trans_Ct, Attrition_Flag) %>%
  group_by(Total_Trans_Ct, Attrition_Flag) %>%
  count(Total_Trans_Ct) %>%
  arrange() %>%
  hchart('areaspline', hcaes(x = Total_Trans_Ct, y = n, group = Attrition_Flag)) %>%
  hc_title(text = "Total de transacciones (últimos 12 meses)", align = "center") %>%
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
  hc_xAxis(labels = list(format = "{value}"), title = list(text = "Total Transactions")) %>%
  hc_colors(c("#8B008B", "#FF4500", "#FC4E07")) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_legend(align = "center") %>%
  hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Total Transactions: ' + this.x)}"))

#Saldo total de la tarjeta de crédito renovable
BankChurners %>%
  dplyr::select(Total_Revolving_Bal, Attrition_Flag) %>%
  group_by(Total_Revolving_Bal, Attrition_Flag) %>%
  count(Total_Revolving_Bal) %>%
  arrange() %>%
  hchart('areaspline', hcaes(x = Total_Revolving_Bal, y = n, group = Attrition_Flag)) %>%
  hc_title(text = "Saldo total de la tarjeta de crédito renovable", align = "center") %>%
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
  hc_xAxis(labels = list(format = "{value}"), title = list(text = "Revolving balance")) %>%
  hc_colors(c("#8B008B", "#FF4500", "#FC4E07")) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_legend(align = "center") %>%
  hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Revolving balance: ' + this.x)}"))

#Tarifa promedio por uso de la tarjeta
BankChurners %>%
  dplyr::select(Avg_Utilization_Ratio, Attrition_Flag) %>%
  group_by(Avg_Utilization_Ratio, Attrition_Flag) %>%
  count(Avg_Utilization_Ratio) %>%
  arrange() %>%
  hchart('areaspline', hcaes(x = Avg_Utilization_Ratio, y = n, group = Attrition_Flag)) %>%
  hc_title(text = "Tarifa promedio por uso de la tarjeta", align = "center") %>%
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
  hc_xAxis(labels = list(format = "{value}"), title = list(text = "Average card usage fee")) %>%
  hc_colors(c("#8B008B", "#FF4500", "#FC4E07")) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_legend(align = "center") %>%
  hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Average card usage fee: ' + this.x)}"))

#Cambio en el recuento de transacciones
BankChurners %>%
  dplyr::select(Total_Ct_Chng_Q4_Q1, Attrition_Flag) %>%
  group_by(Total_Ct_Chng_Q4_Q1, Attrition_Flag) %>%
  count(Total_Ct_Chng_Q4_Q1) %>%
  arrange() %>%
  hchart('areaspline', hcaes(x = Total_Ct_Chng_Q4_Q1, y = n, group = Attrition_Flag)) %>%
  hc_title(text = "Cambio en el recuento de transacciones (Q4 sobre Q1)", align = "center") %>%
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Frequency")) %>% 
  hc_xAxis(labels = list(format = "{value}"), title = list(text = "Change in transaction count (Q4 over Q1)")) %>%
  hc_colors(c("#8B008B", "#FF4500", "#FC4E07")) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_legend(align = "center") %>%
  hc_tooltip(formatter = JS("function(){
                            return (' <br> Frequency: ' + this.y + ' <br> Change in transaction count (Q4 over Q1): ' + this.x)}"))

hcboxplot(outliers = TRUE, x = BankChurners$Total_Trans_Ct, var = BankChurners$Attrition_Flag, name = "Length") %>%
  hc_title(text = "", align = "center") %>%
  hc_yAxis(title = list(text = "Número de transacciones en los últimos 12 meses")) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_legend(align = "center")

# Separating the data
set.seed(333)
intrain      <- createDataPartition(BankChurners$Attrition_Flag, p = 0.7, list = F)
imbal_train  <- BankChurners[intrain, ]

imbal_train %>% 
        hchart('scatter', hcaes(x = Total_Trans_Ct, y = Total_Trans_Amt, group = Attrition_Flag)) %>%
        hc_colors(c("#00AFBB", "#E7B800", "#FC4E07")) %>%
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_legend(align = "center") %>%
  hc_title(text = "Total_Trans_Ct vs Total_Trans_Amt", align = "center")%>%
  hc_tooltip(formatter = JS("function(){
                            return (' <br> Total transaction value (last 12 months): ' + this.y + ' <br> Total transaction count (last 12 months): ' + this.x)}"))


# Función para obtener la frecuencia relativa de cada clase
atr_categorical <- function(df, atr, grupo){

  df <- data.frame(round(prop.table(table(df[atr])) * 100, 2))
  new_df <- data.frame(Class = df$Var1, Percent = df$Freq, Grupo = grupo)
  return(new_df)
}

# Obtener los nombres de las variables cualitativas del conjunto de datos y crear un marco de datos
cols <- colnames(BankChurners[,c(1,3,5,6,7,8)])
df   <- data.frame(Class = character(), Percent = numeric(), Grupo = character())
df1   <- data.frame(Class = character(), Percent = numeric(), Grupo = character())
df2   <- data.frame(Class = character(), Percent = numeric(), Grupo = character())

# Filtrar solo las observaciones de un grupo de clientes específico
df_attritedc <- filter(BankChurners, Attrition_Flag == "Attrited Customer")
df_existingc <- filter(BankChurners, Attrition_Flag == "Existing Customer")

# Attrited Customer
for(coluna in cols){
  df <- rbind(df, data.frame(rbind(atr_categorical(df_attritedc, coluna, "Attrited Customer"))))
  df1 <- rbind(df1, data.frame(rbind(atr_categorical(df_attritedc, coluna, "Attrited Customer"))))
}

# Existing Customer
for(coluna in cols){
  df <- rbind(df, data.frame(rbind(atr_categorical(df_attritedc, coluna, "Attrited Customer"))))
  df2 <- rbind(df2, data.frame(rbind(atr_categorical(df_existingc, coluna, "Existing Customer"))))
}

df1 <- df1[df1$Class != "Attrited Customer" & df1$Class != "Existing Customer", ]
df2 <- df2[df2$Class != "Attrited Customer" & df2$Class != "Existing Customer", ]
df  <- df[df$Class != "Attrited Customer" & df$Class != "Existing Customer", ]

highchart() %>% 
  hc_xAxis(categories = df$Class) %>% 
  hc_add_series(name = "Attrited Customer", data = df1$Percent) %>% 
  hc_add_series(name = "Existing Customer", data = df2$Percent) %>% 
  hc_chart(type = "column",
           options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
  hc_colors(c("#8B008B", "#FF4500", "#FC4E07")) %>%
  hc_add_theme(hc_theme_bloom()) %>% 
  hc_legend(align = "center") %>%
  hc_title(text = "Frecuencia relativa de las características cualitativas de cada grupo de clientes",
    style = list(fontWeight = "bold", fontSize = "25px"), align = "center") %>%
  hc_yAxis(title = list(text = "%"))

gp <- BankChurners %>%
        filter(Attrition_Flag == "Attrited Customer") %>%
        select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category) %>%
        group_by(Gender, Education_Level, Marital_Status, Income_Category, Card_Category) %>%
        count(Attrition_Flag) %>%
        select(Gender, Education_Level, Marital_Status, Income_Category, Card_Category, n) %>%
        arrange(desc(n))

gp$Relative.Frequency <- round(prop.table(gp$n) * 100, 2)
gp$n = NULL

datatable(gp)

# Crear una copia del conjunto de datos
data <- BankChurners

# Aplicando la regla de sturges para obtener el número de intervalos
n <- nrow(data)
k <- round((1 + (10 / 3) * log10(n))) # 14 Grupos

# Creando nuevas columnas con los valores agrupados
cols     <- colnames(data[, sapply(data, is.numeric)])
new_cols <- unlist(lapply(cols, function(x) paste(x, "_f", sep = "")))

# Crear grupos con la función data_binning
data_binning <- function(x, k){
  
  gp <- cut(x, breaks = k, include.lowest = T, ordered_result = T)
  return(gp)
}
data[, new_cols] <- Map(function(x, k) as.factor(data_binning(data[, x], k)), cols, k)

h1 <- hchart(hist(data$Total_Trans_Ct, breaks = 30, plot = F), 
             type = 'histogram', name = 'Total_Trans_Ct') %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
        hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Total_Trans_Ct_f) %>%
        group_by(Total_Trans_Ct_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Total_Trans_Ct_f) %>% 
        hc_add_series(data = h2$n, name = "Total_Trans_Ct_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Avg_Utilization_Ratio, breaks = 30, plot = F), 
             type = 'histogram', name = 'Avg_Utilization_Ratio') %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
        hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Avg_Utilization_Ratio_f) %>%
        group_by(Avg_Utilization_Ratio_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Avg_Utilization_Ratio_f) %>% 
        hc_add_series(data = h2$n, name = "Avg_Utilization_Ratio_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Total_Ct_Chng_Q4_Q1, breaks = 30, plot = F), 
             type = 'histogram', name = 'Total_Ct_Chng_Q4_Q1') %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
        hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Total_Ct_Chng_Q4_Q1_f) %>%
        group_by(Total_Ct_Chng_Q4_Q1_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Total_Ct_Chng_Q4_Q1_f) %>% 
        hc_add_series(data = h2$n, name = "Total_Ct_Chng_Q4_Q1_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Total_Trans_Amt, breaks = 30, plot = F), 
             type = 'histogram', name = 'Total_Trans_Amt') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Total_Trans_Amt_f) %>%
        group_by(Total_Trans_Amt_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Total_Trans_Amt_f) %>% 
        hc_add_series(data = h2$n, name = "Total_Trans_Amt_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Total_Amt_Chng_Q4_Q1, breaks = 30, plot = F), 
             type = 'histogram', name = 'Total_Amt_Chng_Q4_Q1') %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
        hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Total_Amt_Chng_Q4_Q1_f) %>%
        group_by(Total_Amt_Chng_Q4_Q1_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Total_Amt_Chng_Q4_Q1_f) %>% 
        hc_add_series(data = h2$n, name = "Total_Amt_Chng_Q4_Q1_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Avg_Open_To_Buy, breaks = 30, plot = F), 
             type = 'histogram', name = 'Avg_Open_To_Buy') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Avg_Open_To_Buy_f) %>%
        group_by(Avg_Open_To_Buy_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Avg_Open_To_Buy_f) %>% 
        hc_add_series(data = h2$n, name = "Avg_Open_To_Buy_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Total_Revolving_Bal, breaks = 30, plot = F), 
             type = 'histogram', name = 'Total_Revolving_Bal') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Total_Revolving_Bal_f) %>%
        group_by(Total_Revolving_Bal_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Total_Revolving_Bal_f) %>% 
        hc_add_series(data = h2$n, name = "Total_Revolving_Bal_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Credit_Limit, breaks = 30, plot = F), 
             type = 'histogram', name = 'Credit_Limit') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Credit_Limit_f) %>%
        group_by(Credit_Limit_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Credit_Limit_f) %>% 
        hc_add_series(data = h2$n, name = "Credit_Limit_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Contacts_Count_12_mon, breaks = 30, plot = F), 
             type = 'histogram', name = 'Contacts_Count_12_mon') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Contacts_Count_12_mon_f) %>%
        group_by(Contacts_Count_12_mon_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Contacts_Count_12_mon_f) %>% 
        hc_add_series(data = h2$n, name = "Contacts_Count_12_mon_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Months_Inactive_12_mon, breaks = 30, plot = F), 
             type = 'histogram', name = 'Months_Inactive_12_mon') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Months_Inactive_12_mon_f) %>%
        group_by(Months_Inactive_12_mon_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Months_Inactive_12_mon_f) %>% 
        hc_add_series(data = h2$n, name = "Months_Inactive_12_mon_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Total_Relationship_Count, breaks = 30, plot = F), 
             type = 'histogram', name = 'Total_Relationship_Count') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Total_Relationship_Count_f) %>%
        group_by(Total_Relationship_Count_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Total_Relationship_Count_f) %>% 
        hc_add_series(data = h2$n, name = "Total_Relationship_Count_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>% 
        hc_add_theme(hc_theme_smpl())

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Months_on_book, breaks = 30, plot = F), 
             type = 'histogram', name = 'Months_on_book') %>%
      hc_yAxis(title = list(text = "")) %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

h2 <- data %>%
        select(Months_on_book_f) %>%
        group_by(Months_on_book_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Months_on_book_f) %>% 
        hc_add_series(data = h2$n, name = "Months_on_book_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>% 
        hc_add_theme(hc_theme_smpl())

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(data$Dependent_count, breaks = 30, plot = F), 
             type = 'histogram', name = 'Dependent_count') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Dependent_count_f) %>%
        group_by(Dependent_count_f) %>%
        count()

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Dependent_count_f) %>% 
        hc_add_series(data = h2$n, name = "Dependent_count_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>% 
        hc_add_theme(hc_theme_smpl())

hw_grid(h1, h2, ncol = 2)

h1 <- hchart(hist(BankChurners$Customer_Age, breaks = 30, plot = F), 
             type = 'histogram', name = 'Customer_Age') %>%
      hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B")) %>%
      hc_yAxis(title = list(text = ""))

h2 <- data %>%
        select(Customer_Age_f) %>%
        group_by(Customer_Age_f) %>%
        count() 

h2 <- highchart() %>% 
        hc_chart(type ="column", options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = h2$Customer_Age_f) %>% 
        hc_add_series(data = h2$n, name = "Customer_Age_f") %>%
        hc_colors(c("#FF4500", "#FFD700", "#008080", "#006400", "#1C1C1C", "#B8860B"))

hw_grid(h1, h2, ncol = 2)

# Selecting only the variables that will be used for modeling
cols <- c("Attrition_Flag", "Gender", "Education_Level", "Marital_Status", "Income_Category", "Card_Category", new_cols)
data <- data[, cols]

# Creating dummy variables
vars_dummy <- data[, c("Gender", "Marital_Status")]
dummy      <- dummyVars("~.", data = vars_dummy)
dummy_     <- predict(dummy, vars_dummy) 

# Concatenating the new dummy variables in the dataset
data <- dplyr::select(cbind(data, dummy_), -c("Gender", "Marital_Status"))

# Eliminando la columna Attrition_Flag original
data$Attrition_Flag = NULL

#convvirtiendo a factor education level, income_category, card_category
data$Education_Level <- as.factor(data$Education_Level)
data$Income_Category <- as.factor(data$Income_Category)
data$Card_Category <- as.factor(data$Card_Category)

# Transformar las variables independientes al tipo numérico
data[, ] <- sapply(data[, ], as.numeric)

# Class name changes using the original dataset
data$Class <- as.factor(make.names(BankChurners$Attrition_Flag))

# Cambios de nombre de clase usando el conjunto de datos original
set.seed(333)
intrain      <- createDataPartition(data$Class, p = 0.8, list = F)
imbal_train  <- data[intrain, ]
imbal_test   <- data[-intrain, ]

# aplicando - SMOTE
set.seed(333)
smote_train <- SMOTE(Class ~ ., data  = imbal_train)

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# SMOTE
smote_outside <- train(Class ~ ., data = smote_train, 
                       method = "treebag",
                       na.action = na.roughfix,
                       nbagg = 50,
                       metric = "ROC",
                       trControl = ctrl)

# Resumen
smote_outside

h1 = imbal_train %>%
        select(Class) %>%
        group_by(Class) %>% 
        summarise(CNT = n())

h1 <- billboarder() %>% 
        bb_piechart(h1) %>% 
        bb_legend(position = 'right') %>%
        bb_title(text = "Original Data", padding = NULL, position = "center")

h2 = smote_train %>%
        select(Class) %>%
        group_by(Class) %>% 
        summarise(CNT = n())

h2 <- billboarder() %>% 
        bb_piechart(h2) %>% 
        bb_legend(position = 'right') %>%
        bb_title(text = "SMOTE Applied", padding = NULL, position = "center")

hw_grid(h1, h2, ncol = 2)

# Creando una copia de los datos de entrenamiento
smote_train_c       <- smote_train
smote_train_c$Class <- sapply(smote_train$Class, sub_target1)

# Aplicando el algoritmo de random forest para clasificación 
fit <- lm(Class ~ ., method = "rf", data = smote_train_c)

# Creando un DataFrame con información sobre la importancia de cada atributo
best_vars <- data.frame(Var = row.names(varImp(fit)), Import = varImp(fit)$Overall)

# Ver las puntuaciones de las variables
best_vars %>%
  arrange(desc(Import)) %>%
  hchart(type = "bar", hcaes(x = Var, y = round(Import, 2)), borderColor = "black") %>% 
  hc_add_theme(hc_theme_bloom()) %>%
  hc_xAxis(title = list(text = "Variables")) %>%
  hc_yAxis(title = list(text = "Importance"))

# Sembrando una semilla aleatoria
set.seed(333)

# Seleccionar solo variables con importancia> 0,1
variables_for_model <- filter(best_vars, Import >= 0.1)$Var

# Seleccionar solo los atributos de interés para el proceso de entrenamiento y prueba
training <- select(smote_train, c(variables_for_model, "Class"))
testing  <- select(imbal_test,  c(variables_for_model, "Class"))

# Validación cruzada 
control <- trainControl(method = "cv", number = 5)

# Cuadrícula de hiperparámetros de aumento de gradiente extremo
grid <- expand.grid(max_depth        = 17,     # [0, ∞] # 17
                    nrounds          = 172,    # [0, ∞] # 170
                    eta              = 0.4,    # [0, 1] # 0.4
                    gamma            = 0.2,    # [0, ∞] # 0.2
                    colsample_bytree = 0.8,    # (0, 1] # 0.8
                    min_child_weight = 0.6,    # [0, ∞] # 0.6
                    subsample        = 0.8)    # (0, 1] # 0.8

# Entrenamiento de aumento de gradiente extremo
xgbm.tune   = train(
  x         = select(training, -c("Class")),
  y         = training$Class,
  method    = "xgbTree",
  tuneGrid  = grid,
  metric    = "Kappa",
  verbose   = FALSE,
  trControl = control,
  
 )

# Modelo forecasts
previsoes = predict(xgbm.tune, select(testing, -c("Class")))

#Matriz de confusión
confusionMatrix(previsoes, testing$Class)

# Graficado
prev_real <- data.frame(table(previsoes = previsoes, real = testing$Class))

df_confusion            <- data.frame(c1 = prev_real$Freq[1:2], c2 = prev_real$Freq[3:4])
row.names(df_confusion) <- c("Attrited.Customer", "Existing.Customer")
colnames(df_confusion)  <- c("Attrited.Customer", "Existing.Customer")

cm <- as.matrix(df_confusion) %>%
        hchart() %>% 
        hc_add_theme(hc_theme_smpl()) %>%
        hc_title(text = "Matriz de confusión", align = "center") %>% 
        hc_legend(align = "center") %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
        hc_plotOptions(series = list(boderWidth = 0, dataLabels = list(enabled = TRUE))) %>%
        hc_subtitle(text = "Análisis de abandono de clientes", align = "center", 
                    style = list(color = "#2b908f", fontWeight = "bold"))

ac <- billboarder() %>% 
        bb_gaugechart(value = 96.49)  %>%
        bb_title(text = "Puntaje de precisión", padding = 20, position = "center") %>% 
        bb_legend(show = FALSE)

hw_grid(cm, ac, ncol = 1)
