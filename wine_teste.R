if(!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if(!require(daltoolbox, quietly = TRUE)) install.packages("daltoolbox")
if(!require(gridExtra, quietly = TRUE)) install.packages("gridExtra")

library(dplyr)
library(daltoolbox)
library(gridExtra)


url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

wine_data <- read.table(url, sep = ',', col.names =c('Type', 'Alcohol', 'Malic', 'Ash', 
                        'Alcalinity', 'Magnesium', 'Phenols', 
                        'Flavanoids', 'Nonflavanoids',
                        'Proanthocyanins', 'Color', 'Hue', 
                        'Dilution', 'Proline'))


wine_data$Type <- as.character(wine_data$Type)

head(wine_data)

# 1) a) calcular a media e o desvio padrao para todos os atributos
media = wine_data %>%
  summarise(across(.cols =!(Type), .fns = mean))

desvio_padrao = wine_data %>%
  summarise(across(.cols =!(Type), .fns = sd))



#1) b) media e sd para todos os atributos agrupados para tipo de vinho

media_agrupada = wine_data %>%
  group_by(Type) %>%
  summarise(across(.cols = everything(), .fns = mean))


desvio_agrupada = wine_data %>%
  group_by(Type) %>%
  summarise(across(.cols = everything(), .fns = sd))
print(desvio_agrupada)

#1) c) 
plots = list()

for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_density_class(wine_data %>% select(Type, col_name), 'Type', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'), 
                                         alpha = .7)
}

density_plot_grouped = grid.arrange(grobs = plots, ncol = 5)

rm(plots)

#1) d)

plots = list()

for(col_name in colnames(wine_data)[-1]) {
  plots[[col_name]] = plot_boxplot_class(wine_data %>% select(Type, col_name), 'Type', 
                                         label_x = paste(col_name, 'by wine class'), 
                                         colors = c('#0099e5', '#ff4c4c', '#34bf49'))
  
}

boxplot_grouped = grid.arrange(grobs = plots, ncol = 5)

rm(plots)


#1) e)

plots = list()
attrs = colnames(wine_data)[-1]
len = 5 # para gerar menos gráficos

for(i in 1:len) {
  for(j in 1:len) {
    if(i < j) {
      plots[[paste(attrs[i], attrs[j], sep = 'x')]] = plot_scatter(wine_data %>% select(x = attrs[i], 
                                                                                         value = attrs[j], variable = Type),
                                                                   label_x = attrs[i], label_y = attrs[j],
                                                                   colors = c('#0099e5', '#ff4c4c', '#34bf49'))
    }
  }
}

scatter_plot_grouped = grid.arrange(grobs = plots, ncol = 5)
rm(plots)


#2) a)
discretizacao = wine_data %>%
  mutate(across(.cols = !(Type), .fns = function(x) {
    breaks = quantile(x, probs = seq(0, 1, by = 1/3))
    labels = c('Baixo', 'Médio', 'Alto')
    cut(x, breaks, labels, include.lowest = TRUE)
  }))

print(dw_discretized)

#2) b)

cm = categ_mapping('Type')

dw_categ_mapping = cbind(transform(cm, discretizacao), discretizacao[-1])

print(dw_categ_mapping)

install.packages("kmeans")
library(kmeans)


modelo <- cluster_kmeans(k = 3)
data_for_model <- select(wine_data, -Type)
modelo <- fit(modelo, data_for_model)
clu <- cluster(modelo, data_for_model)

calculate_entropy <- function(data) {
  freqs <- table(data) / length(data)
  -sum(freqs * log(freqs + 1e-6))
}

entropy_clusters <- sapply(unique(clu), function(cluster_id) {
  data_in_cluster <- wine_data$Type[clu == cluster_id]
  calculate_entropy(data_in_cluster)
})

entropy_type <- calculate_entropy(wine_data$Type)
print("Entropia para cada grupo gerado pelo K-means:")
print(entropy_clusters)

print("Entropia para a classe 'Type' original:")
print(entropy_type)

#4)

wine_data$Type <- as.factor(wine_data$Type)
wine_data <- as.data.frame(wine_data)

slevels <- levels(wine_data$Type)
model <- cla_mlp("Type", slevels, size=3, decay=0.03)

sr <- sample_random()
sr <- train_test(sr, wine_data)
train <- sr$train
test <- sr$test

model <- fit(model, train)

prediction <- predict(model, test)
predictand <- adjust_class_label(test[,"Type"])
test_eval <- evaluate(model, predictand, prediction)
test_eval$metrics %>% View()

tabela_resultados <- resultados %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

#5)
wine_data_trans <- as(wine_data, "transactions")

rules <- apriori(wine_data_trans, parameter = list(supp = 0.3, conf = 0.8, target = "rules"))
inspect(rules)
rules_a <- as(rules, "data.frame")

plot(rules, engine = "ggplot2", main = NULL) +
  scale_color_gradient2(mid = "blue", high = "red", 
                        midpoint = median(rules_a$lift), limits = c(min(rules_a$lift), max(rules_a$lift))) +
  labs(x = "Supp.", y="Conf.", color = "Lift") +
  theme_classic()



