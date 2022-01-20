library(dplyr)
library(tidyr)
library(ggplot2)
library(datasets)
library(ggridges)
library(mlbench)
library(GGally)

####################################
## Grouping, Pivoting, Manipulating
####################################

#### Load Data ####
data(iris)

#### view data ####
View(iris)

str(iris)

sapply(iris, class)

summary(iris)

unique(iris$Species)


##### Data Manipulation ######
### Data Types ####
is.character(iris$Species)

is.factor(iris$Species)

iris$species_upper <- toupper(iris$Species)
iris$species_upper <- as.factor(iris$species_upper)
View(iris)



#### Ranking and making new variables ####
iris <- iris %>%
  mutate(Petal.Size = Petal.Length*Petal.Width,
         Sepal.Size = Sepal.Length*Sepal.Width,
         Petal.SizeDenseRank = as.factor(dense_rank(-Petal.Size)),
         Sepal.SizeDenseRank = as.factor(dense_rank(-Sepal.Size)))




## Grouping/Pivoting Data ###
iris_species_stats <- iris %>%
  group_by(Species) %>%
  summarise(count = n(),
            Sepal.SizeMean = mean(Sepal.Size),
            Petal.SizeMean = mean(Petal.Size),
            Sepal.SizeStd = sd(Sepal.Size),
            Petal.SizeStd = sd(Petal.Size),
            Sepal.SizeSE = sd(Sepal.Size)/sqrt(n()),
            Petal.SizeSE = sd(Petal.Size)/sqrt(n()),) 

ggplot(data=iris_species_stats,aes(x=Species,y=Sepal.SizeMean))+
  geom_bar(stat='identity')+
  xlab('Species')+
  ylab('Mean Sepal Size')+
  ggtitle('Sepal Size by Species')



### Pivoting Data ###
# Load Data
data(Titanic)
Titanic_df <- data.frame(Titanic)

#Pivot form tidyr 
titanic_pivot <- Titanic_df %>% # calls titanic data
  group_by(Age,Sex,Survived) %>% # groups the data by age, sex and survived categories
  summarise(freq_new = sum(Freq)) %>% # Takes the sum of the frequency coplumn to aggregate data
  pivot_wider(
    names_from = Survived,
    values_from = c(freq_new)
  ) %>% # pivots survivorship column to make 2 new columns (yes or no)
  mutate(percent_survived = Yes/(Yes+No)) %>% # Makes a new column of percent survived based on the row
  arrange(desc(percent_survived)) # sorts data by percent survived from highest rate to lowest rate


### Melting Data

titanic_unpivoted <- titanic_pivot %>%
  pivot_longer(
    cols = c('Yes','No'),
    names_to = "Survived",
    values_to = "val"
  ) %>%
  select(-percent_survived)



####################################
## Visualizing
####################################
# Scatter Plot
ggplot(iris, aes(x = Sepal.Size, y = Petal.Size)) +
  geom_point(aes(color = factor(Species)))

# Histogram
ggplot(iris, aes(x=Petal.Size, color=Species)) +
  geom_histogram(binwidth = 0.25,alpha=0.8) +
  geom_vline(data = iris_species_stats,aes(xintercept=Petal.SizeMean, color=Species),
             linetype="dashed", size=2)


# Box plot
ggplot(data = iris,
       aes(x = Species, y = Petal.Size)) +
  geom_boxplot()

# Violin Plot
ggplot(data = iris,
       aes(x = Species, y = Petal.Size)) +
  geom_violin()+ 
  stat_summary(fun.y=median, geom="point", size=2, color="red")

# Ridgeline Plot
ggplot(iris, aes(x = Petal.Size, y = Species, color=Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "right")


####################################
## Bayesian Statistics
####################################


# What is the probability I am a I was a child given I survived?
#p(child)
titanic_contengency_table <- Titanic_df %>% # calls titanic data
  group_by(Age,Survived) %>% # groups the data by age, sex and survived categories
  summarise(freq_new = sum(Freq)) %>% # Takes the sum of the frequency coplumn to aggregate data
  pivot_wider(
    names_from = Survived,
    values_from = c(freq_new)
  ) # pivots survivorship column to make 2 new columns (yes or no)

#p(child)
p_child = sum(titanic_contengency_table[1,2:3])/(sum(titanic_contengency_table[1:2,2:3]))
p_child


#p(survived)
p_survive = sum(titanic_contengency_table[1:2,3])/(sum(titanic_contengency_table[1:2,2:3]))
p_survive


#p(survive|child) probabiltiy of surviving given I am a child
p_survived_child = sum(titanic_contengency_table[1,3])/(sum(titanic_contengency_table[1,2:3]))
p_survived_child


# What is the probability I am a I was a child given I survived?
# see slides for bayes theorum
p_ch_survive = (p_survived_child*p_child)/p_survive
p_ch_survive

####################################
## Logistic Regression
####################################
#

data(BreastCancer, package="mlbench")
View(BreastCancer)
bc_df <- BreastCancer
bc_df$binary_class <- as.factor(ifelse(bc_df$Class == "malignant", 1, 0))
bc_df <- bc_df %>%
  select(-Id) %>%
  drop_na()

for(i in 1:9) {
  bc_df[, i] <- as.numeric(bc_df[, i])
}
#from ggally
ggpairs(bc_df)

help(glm)
#m1
model_bc <- glm(binary_class ~ Cl.thickness + Cell.size,data=bc_df, family=binomial)
summary(model_bc)
AIC(model_bc)
BIC(model_bc)

#m2
model_bc <- glm(binary_class ~ Cl.thickness + Cell.size+Cell.shape  ,data=bc_df, family=binomial)
summary(model_bc)
AIC(model_bc)
BIC(model_bc)
