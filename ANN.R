# knitr options
knitr::opts_chunk$set(echo = TRUE)

# Load packages

library(knitr)
library(class)
library(gmodels)
library(xtable)

# Artificial Neural Network Classifier
# Obtaining the data:
# We name "data_pca" to the data we are going to work with in this classifier

data_pca = read.csv("PEC2/pcaComponents6.csv")

# We name "pca_classes" to the pca_data classes with which we are going to work

clases_pca = read.csv("PEC2/class6.csv")

# We see that the dataset `data_pca` has a high number of examples and variables. 
# These are too many, so we are going to use only the first eight components of the PCA. 
# These 8 will be the variables that we will use as input to the neural network. 
# Therefore, we must create a new data set with these 8 variablese:

data_pca_8 = data_pca[,1:8]

# We are going to try to do is predict using an artificial neural network algorithm without 
# expression profile belongs to one phenotype or another.

# Data exploration and preparation

str(data_pca_8)

# Neural networks work best when the input data is scaled around zero. If we see very different 
# values we must transform the variables in a range of [0,1] normalizing the data.

# For this we are going to generate our own normalization function:

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Using the `normalize ()` function that we have created and applying it to each column
# with the `lapply ()` function we will obtain the normalization of the entire data set

datos_pca_norm <- as.data.frame(lapply(data_pca_8, normalize))

# To check that everything has gone well:

summary(datos_pca_norm$PC4)

# The variable "phenotype" must be transformed to binary:

variable_fenotipo_1 = clases_pca

f1 = as.vector(variable_fenotipo_1)

f1_binario <- function(x) {
  ifelse(x=="1", 1, 0)
}

f1 = f1_binario(f1)

# We proceed in the same way with the other 3 variables:

variable_fenotipo_2 = clases_pca

f2 = as.vector(variable_fenotipo_2)

f2_binario <- function(x) {
  ifelse(x=="2", 1, 0)
}

f2 = f2_binario(f2)

variable_fenotipo_3 = clases_pca

f3 = as.vector(variable_fenotipo_3)

f3_binario <- function(x) {
  ifelse(x=="3", 1, 0)
}

f3 = f3_binario(f3)

variable_fenotipo_4 = clases_pca

f4 = as.vector(variable_fenotipo_4)


f4_binario <- function(x) {
  ifelse(x=="4", 1, 0)
}

f4 = f4_binario(f4)

# Creation of the training and test dataset:

# Before separating the data into training and 
#test datasets, we must join the examples with their labels:

datos_listos = cbind(datos_pca_norm, f1, f2, f3, f4)

# We also name the variables of the classes ("FNT1", "FNT2", "FNT3", "FNT4")

colnames(datos_listos)[9:12] = c("FNT1","FNT2", "FNT3", "FNT4")

# Now we need to separate the data into two sets: the training set (67%) and the test set (33%). 
# For this we are going to create a vector of random indices:

set.seed(12345)

tasa = 67/100
numero_ejemplos_escogidos = round(nrow(datos_listos)*tasa)
vector_indice = c(1:nrow(datos_listos))
vector_indice_entrenamiento = sample(vector_indice,
                                     replace = FALSE, size = numero_ejemplos_escogidos)

train = datos_listos[vector_indice_entrenamiento,]
test = datos_listos[-vector_indice_entrenamiento,]

# Train the model with the data

install.packages("neuralnet", repos = "http://cran.us.r-project.org")
library(neuralnet)

# One hidden node
set.seed(1234567)

fenotipo_ANN_model_1 = neuralnet(FNT1 + FNT2 + FNT3 + FNT4
                                 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6
                                 + PC7 + PC8, data = train, hidden = 1)

# Three hidden node
set.seed(1234567)

fenotipo_ANN_model_3 = neuralnet(FNT1 + FNT2 + FNT3 + FNT4   
                                 ~ PC1 + PC2 + PC3 + PC4 + PC5 + 
                                   PC6 + PC7 + PC8, data = train, hidden = 3)

# Evaluate the model's performance

install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

# One hidden node
model_results_1 <- compute(fenotipo_ANN_model_1, test[1:8])
head(model_results_1$net.result)

# We will create a function that assigns the phenotype to each sample based on the maximum value of the vector.
FNT <- function(modelo) {
  
  resultado <- c()
  nrow(resultado)
  
  for (v in 1:nrow(modelo)) {
    
    y <- modelo[v,]
    
    replace(y, y==-1, 0)
    
    r <- which(max(y) == y)
    resultado <- append(resultado, 
                        switch (r, "FNT1", "FNT2", "FNT3", "FNT4"))
  }
  return (resultado)
}

#This is the factor that will hold the actual classes of our `test` dataset:
label_test <- as.factor(FNT(test[9:12]))
levels(label_test) <- c("FNT1", "FNT2", "FNT3", "FNT4")

#This is the result of the classification of the data set `test` by our classifier model:
clasificacion_1 <- as.factor(FNT(model_results_1$net.result))
levels(clasificacion_1) <- c("FNT1", "FNT2", "FNT3", "FNT4")

#With the `ConfussionMatrix ()` function from the `caret` package we proceed to evaluate the model:
confussion_matrix_1 <- confusionMatrix(clasificacion_1, label_test)
confussion_matrix_1


# The same for three hidden node:
model_results_3 <- compute(fenotipo_ANN_model_3, test[1:8])

clasificacion_3 <- as.factor(FNT(model_results_3$net.result))

confussion_matrix_3 <- confusionMatrix(clasificacion_3, label_test)
confussion_matrix_3

