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




