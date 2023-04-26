library(here)
library(tidyverse)

data = read_csv(here('dados/data01.csv'))

## Tem vazios?

data |>  is.na() |> sum()

data |> head() |> View() # Visualizando as primeiras observações

apply(data, 2, is.numeric) # Todos são numericos?


data_no_na= data |> 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE))) # Completando com a média dos valores

data_no_na |>  is.na() |> sum()


# Quais são os grupos
table(data_no_na$group) |> prop.table()

# Transformando groups in factor
data_no_na$group = as.factor(data_no_na$group)
table(data_no_na$group) |> prop.table()


data_no_na$group = recode_factor(data_no_na$group,'1' = "Alive",'2'="Death")
table(data_no_na$group) |> prop.table()

# Reequilibrando o conjunto de dados

data_to_model = subset(data_no_na, select = -c(ID))
data_to_model = rename(data_to_model, "Class"="group")
data_to_model = as.data.frame(data_to_model) #deve ser dataframe, não tibble
#library(tidymodels)
#library(themis)
library(ROSE)
library(imbalance)

names_data_to_model= str_replace_all( names(data_to_model),"-", "_")  
names_data_to_model =str_replace_all(names_data_to_model," ","_")

names_data_to_model
names(data_to_model) = names_data_to_model

linhas_dominantes=filter(data_to_model, Class == "Alive") |> nrow() 
linhas_para_preencher= nrow(data_to_model) + (nrow(data_to_model) - linhas_dominantes)
linhas_para_preencher

data_balanced_over <- ovun.sample(Class ~ ., data = data_to_model, method = "over",N = linhas_para_preencher)$data
data_balanced_over


#plotComparison(data_to_model,rbind(data_to_model,data_balanced_over), attrs = names(data_to_model)[1:5])

#data_balanced_over_to_model=rbind(data_to_model,data_balanced_over)
#table(data_balanced_over$Class)

#saveRDS(data_balanced_over, here("object/data_to_model.R"))
