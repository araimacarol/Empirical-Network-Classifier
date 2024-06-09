
#+ Set working directory
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/rcappaw/OneDrive - University of Tasmania/Desktop/R/Workflow/igraphEpi-New")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Loading packages
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



suppressPackageStartupMessages({
  if(!require(Matrix)) {
    install.packages("Matrix")
    library(Matrix)
  }
  if(!require(svglite)) {
    install.packages("svglite")
    library(svglite)
  }

  if(!require(PerformanceAnalytics)) {
    install.packages("PerformanceAnalytics")
    library(PerformanceAnalytics)
  }

  if(!require(corrplot)) {
    install.packages("corrplot")
    library(corrplot)
  }
  if(!require(purrr)) {
    install.packages("purrr")
    library(purrr)
    
  }
  
  if(!require(furrr)) {
    install.packages("furrr")
    library(furrr)
    
  }
  
  if(!require(stringr)) {
    install.packages("stringr")
    library(stringr)
  }
  
  if(!require(here)) {
    install.packages("here")
    library(here)
  }
  if(!require(stats)) {
    install.packages("stats")
    library(stats)
  }
  if(!require(igraph)) {
    install.packages("igraph")
    library(igraph)
  }
  if(!require(tidymodels)) {
    install.packages("tidymodels")
    library(tidymodels)
  }
  if(!require(tidyverse)) {
    install.packages("tidymodels")
    library(tidyverse)
  }
  if(!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(janitor)) {
    install.packages("janitor")
    library(janitor)
  }
  if(!require(vip)) {
    install.packages("vip")
    library(vip)
  }
  if(!require(readxl)) {
    install.packages("readxl")
    library(readxl)
  }
  if(!require(naniar)) {
    install.packages("naniar")
    library(naniar)
  }
  if(!require(furrr)) {
    install.packages("furrr")
    library(furrr)
  }
  
  if(!require(themis)) {
    install.packages("themis")
    library(themis)
  }
  
  if(!require(shapr)) {
    install.packages("shapr")
    library(shapr)
  }
  
  if(!require(Boruta)) {
    install.packages("Boruta")
    library(Boruta)
  }
  if(!require(janitor)) {
    install.packages("janitor")
    library(janitor)
  }
  if(!require(tidyverse)) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  if(!require(tidymodels)) {
    install.packages("tidymodels")
    library(tidymodels)
  }
  if(!require(shapviz)) {
    install.packages("shapviz")
    library(shapviz)
  }
  if(!require(SHAPforxgboost)) {
    install.packages("SHAPforxgboost")
    library(SHAPforxgboost)
  }
  if(!require(mlbench)) {
    install.packages("mlbench")
    library(mlbench)
    
  }
  if(!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(ggpubr)) {
    install.packages("ggpubr")
    library(ggpubr)
  }
  if(!require(cowplot)) {
    install.packages("cowplot")
    library(cowplot)
  }
  if(!require(patchwork)) {
    install.packages("patchwork")
    library(patchwork)
  }
  if(!require(recipes)) {
    install.packages("recipes")
    library(recipes)
  }
  
  if(!require(gridExtra)) {
    install.packages("gridExtra")
    library(gridExtra)
  }
  
  if(!require(finetune)) {
    install.packages("finetune")
    library(finetune)
  }
  
  if(!require(future)) {
    install.packages("future")
    library(future)
  }
  
  if(!require(doFuture)) {
    install.packages("doFuture")
    library(doFuture)
  }
  
  if(!require(doParallel)) {
    install.packages("doParallel")
    library(doParallel)
  }
  
  if(!require(tune)) {
    install.packages("tune")
    library(tune)
  }
  if(!require(Hmisc)) {
    install.packages("Hmisc")
    library(Hmisc)
  }
})

# if needed: install.packages("devtools")
# devtools::install_github("tidymodels/recipes")
# devtools::install_github("tidymodels/tune")
# devtools::install_github("tidymodels/finetune")

###############################################################################################
# Data processing
###############################################################################################

#----read in data
Data=read.csv("model.data.csv",header = T,sep=",")

#----Shuffle----data
Data = Data[sample(1:nrow(Data)), ]##shuffle row indices and randomly re order 


#----Change data frame to tibble (similar to data-frame)
df=as_tibble(Data)
#----count number for each target category
df%>%
  dplyr::count(GraphName,sort=T)#unbalance data

#----Show first n row of the data
df%>%
  slice_head(n=10)
#----view data

###--------Load the janitor package to clean the data---------###
df<-df%>%
  clean_names()

#----glimpse data
df%>%
  glimpse()

#----look at the structure of data
str(df)

#----change categorical variables to factor
df=df%>%
  select(-c(x))%>%
  mutate_if(is.character,factor)

#check for missing data
colSums(is.na(df))

####----Balancing Data-----####
#NOTE: If missing data is less we can omit them from the data frame
#however if they are considerable large, we can impute them using tidymodel
#packages. 

#----check columns where missing data are considerable larger than 1000
names <- modify(df[-1], is.na) %>% 
  colSums() %>%
  tibble(names = colnames(df[-1]), missing_values=.) %>% 
  filter(missing_values < 10000) %>% 
  select(1)

# the missing values from the column in our data set is not much but we will still impute it
df_rec=recipe(graph_name~., data = df)%>%
  step_impute_median(all_numeric_predictors())

saveRDS(df_rec,"df_rec.rds")

df_prep=df_rec%>%prep()
saveRDS(df_prep,"df_prep.rds")

df_juice=df_prep%>%juice
saveRDS(df_juice,"df_rec.rds")

#----count number for each target category
df_juice%>%count(graph_name,sort=T)#balanced data

#check for NA's in the final imputed data
colSums(is.na(df_juice))#no na's

#Imputed Data
df_imputed=df_juice
saveRDS(df_imputed,"df_imputed.rds")
df_imputed=readRDS("df_imputed.rds")


###---Spearmans Correlations of features----
res <- df_imputed%>%
  select(-graph_name)%>%
  cor()

round(res, 2)


res2 <-df_imputed%>%
  select(-graph_name)%>%
  as.matrix()%>%
  rcorr(type=c("pearson","spearman"))

res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P


# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res3=flattenCorrMatrix(res2$r, res2$P)
res3


symnum(res, abbr.colnames = FALSE)


###---Correlation Plot----###
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# # Insignificant correlations are leaved blank
# corrplot(res2$r, type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.01, insig = "blank")

res4 <- df_imputed%>%
  select(-graph_name)%>%
chart.Correlation(histogram=TRUE, pch=19)

res4

# Heat Map
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

#################################################################################################
# Feature Engineering on the whole Data
#################################################################################################

#----Borutal feature selection
set.seed(9532)
boruta.train <- Boruta(graph_name~., data = df_imputed, 
                       doTrace = 2,maxRuns=11)
print(boruta.train)

#----save boruta object
saveRDS(boruta.train,"boruta_train_new.rds")
boruta.train=readRDS("boruta_train_new.rds")

##----renaming variables
new_boruta_column_names=c('Number of nodes','Edges','Mean eccentricity','Mean path length',
                          'Graph energy','Modularity','Diameter','Betweenness centrality',
                          'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                          'Mean degree','Min cut','Fiedler value','Normalized fiedler value',
                          'Closeness centrality','Degree assortativity coeff',
                          'shadowMax','shadowMean','shadowMin' )

#----Plot boruta object showing var importance
plot(boruta.train, xlab = "", xaxt = "n")
colnames(boruta.train$ImpHistory)=new_boruta_column_names

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#----Dealing with the tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#----List of confirmed selected attributes
selected.attr=getSelectedAttributes(final.boruta, withTentative = F)
selected.attr

#----final importance score of each predictor variable with boruta
boruta.df <- attStats(final.boruta)
boruta.df$selected_features <- row.names(boruta.df)   

#----Selecting top N predictors values by group
boruta.df.top <- boruta.df %>%
  select(-c(normHits,decision)) %>% 
  arrange(desc(meanImp),desc(medianImp),desc(minImp),desc(maxImp))%>%
  dplyr::slice(1:5)
boruta.df.top

#----final data after performing boruta feature engineering
boruta.list<-getSelectedAttributes(final.boruta, withTentative = F)
df_data_new<-df_imputed
graph_name<-df_data_new$graph_name
df_data_new<-df_data_new[,names(df_data_new)%in%boruta.list]
df_data_new<-cbind(df_data_new,graph_name)

#----rename nodes
df_data_new=df_data_new%>%
  rename("num_of_nodes"="order")
str(df_data_new)

# df.final=df.final%>%
#   select(-c(X))

#----Final data for the model after feature engineering
saveRDS(df_data_new,"df_data_new.rds")
df_data_new=readRDS("df_data_new.rds")


#----checking for Na's in final data
colSums(is.na(df_data_new))

########################################################################
# End of Feature Engineering 
########################################################################



########################################################################
# Data splitting and Balancing
########################################################################
#----sample and split
df_split_new=df_data_new[sample(1:nrow(df_data_new)), ]%>%
  initial_split(prop = 0.7)
saveRDS(df_split_new,"df_split_new.rds")
df_split_new=readRDS("df_split_new.rds")
#-----training split-----
df_train_new=training(df_split)
saveRDS(df_train_new,"df_train_new.rds")
df_train_new=readRDS("df_train_new.rds")

#-----test split-----
df_test_new=testing(df_split)
saveRDS(df_test_new,"df_test_new.rds")
df_test_new=readRDS("df_test_new.rds")

###----Print number of training and testing set----###
cat("training set:", nrow(df_train_new), "\n",
    "testing set :", nrow(df_test_new), "\n")

############################################################################
######----Unbalanced Data sets----######
df_train_new%>%
  dplyr::count(graph_name,sort=T)#unbalance data

df_test_new%>%
  dplyr::count(graph_name,sort=T)#unbalance data


####----Balance training data with SMOTE----######
set.seed(2938)
df_rec_new=recipe(graph_name~., data = df_train_new)%>%
  step_impute_median(all_numeric_predictors())%>%
  step_smote(graph_name) 


saveRDS(df_rec_new,"df_rec_new.rds")
df_rec_new=readRDS("df_rec_new.rds")


df_prep_new=prep(df_rec_new)
saveRDS(df_prep_new,"df_prep_new.rds")
df_prep_new=readRDS("df_prep_new.rds")

df_bake_new=bake(df_prep_new,new_data=NULL)
saveRDS(df_bake_new,"df_bake_new.rds")
df_bake_new=readRDS("df_bake_new.rds")

#----count number for each target category
df_bake_new%>%count(graph_name,sort=T)#balanced data

#check for NA's in the final imputed data
colSums(is.na(df_bake_new))

###################################
#----Data Partitioning----###
###################################
set.seed(384)

#----full balanced and pre-processed data
final_df_data_new=df_bake_new

saveRDS(final_df_data_new,"final_df_data_new.rds")

final_df_data_new=readRDS("final_df_data_new.rds")

########################################################################
# End of Data Splitting and Balancing
########################################################################





#########################################################################################
# Building first classification  models
#########################################################################################

#----cross validation
df_cv_splits_new <- vfold_cv(final_df_data_new, v = 10)
saveRDS(df_cv_splits_new,"df_cv_splits_new.rds")

#################################################################################################
# Classification Models
#################################################################################################

set.seed(384)
rf.tune.df <- rand_forest(mtry = tune(),
                          trees = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")


# show what will be tuned
rf.tune.df
extract_parameter_set_dials(rf.tune.df)


# Hyperparameter grid
rf.grid <- rf.tune.df %>%
  extract_parameter_set_dials() %>%
  finalize(select(final_df_data_new, -graph_name)) %>%
  dials::grid_max_entropy(size = 20)

# Workflow bundling every step
rf.wflow <- 
  workflow() %>%
  add_model(rf.tune.df)%>%
  add_recipe(df_rec_new)


control_rf = control_race(save_pred = TRUE,
                           verbose_elim = TRUE,
                           pkgs = c("themis"))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Fitting RF Model
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#wflow_list_new <- list(rf.wflow,xgb.wflow)
#grid_list_new <- list(rf.grid,xgb.grid)
set.seed(9843)
trained_rf_model <-
  rf.wflow%>%
  tune_race_anova(
    df_cv_splits_new,
    grid = rf.grid,
    metrics = metric_set(roc_auc,accuracy,mn_log_loss),
    control = control_rf
  )

saveRDS(trained_rf_model,"series_trained_rf_model.rds")


###########################################################################
# RETRAIN RANDOM FOREST BEST MODEL
###########################################################################
TrainedModelslist1=readRDS("series_trained_rf_model.rds")

#----extracting the best model
best.model <- TrainedModelslist1

best.model %>%
  collect_metrics()

###----plot of best metrics
#accuracy
best.model %>%
  show_best(metric = "roc_auc")
autoplot(best.model)

#roc
best.model %>%
  show_best(metric = "roc_auc")

#roc_auc
best_auc <- select_best(best.model,metric = "roc_auc")
best_auc

#----getting metrics of the best model
best.model.specs <-
  best.model %>%
  select_best(metric = "accuracy")

best.model.specs

final_model <-parsnip::rand_forest(mtry = best.model.specs$mtry,
                                   trees =best.model.specs$trees) %>%
  set_engine("ranger") %>%
  set_mode("classification")

##----best workflow
df.best.wflow <- workflow() %>%
  add_recipe(df_rec_new) %>%
  add_model(final_model)

##----final best workflow
df.best.final.wflow<- finalize_workflow(
  df.best.wflow,
  best_auc
)

df.best.final.wflow

###----final fitted RF model----###
final.fitted.model_rf <- last_fit(df.best.final.wflow,
                                  df_split_new,
                                  metrics = metric_set(roc_auc,accuracy,mn_log_loss))

saveRDS(final.fitted.model,"rf_trained_model.rds")

final.fitted.model_rf=readRDS("rf_trained_model.rds")




##################################################################################
#----[2]--Boost---Tree
##################################################################################
# Create xgboost model
xgb_model <- parsnip::boost_tree(
  mtry = tune(),
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  loss_reduction = tune(),
  min_n = tune()
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")



xgb_wflow <-
  workflow() %>%
  add_model(xgb_model)%>%
  add_recipe(df_rec_new) 


xgb_grid <- xgb_model %>%
  hardhat::extract_parameter_set_dials() %>%
  dials::finalize(select(final_df_data_new,-graph_name)) %>%
  dials::grid_max_entropy(size = 20)


control_xgb = control_race(save_pred = TRUE,
                           verbose_elim = TRUE,
                           pkgs = c("themis"))


doParallel::registerDoParallel()

set.seed(3979)
trained_xgb_model_new2 <- 
  xgb_wflow%>%
  tune_race_anova(
    df_cv_splits_new,
    grid = xgb_grid,
    metrics = metric_set(roc_auc,accuracy,mn_log_loss),
    control = control_xgb
  )

saveRDS(trained_xgb_model_new2,"trained_xgb_model_new2.rds")


###########################################################################
# RETRAIN BEST XGBOOST MODEL
###########################################################################

TrainedModelslist=readRDS("trained_xgb_model_new2.rds")


#----extracting the best model
best.model <- TrainedModelslist2

best.model %>%
  collect_metrics()

###----plot of best metrics
#accuracy
best.model %>%
  show_best(metric = "roc_auc")
autoplot(best.model)

#roc
best.model %>%
  show_best(metric = "roc_auc")

#roc_auc
best_auc <- select_best(best.model,metric = "roc_auc")
best_auc

#----getting metrics of the best model
best.model.specs <-
  best.model %>%
  select_best(metric = "accuracy")

best.model.specs


#best model specification
final_model <- boost_tree(mode = "classification", 
                          mtry = best.model.specs$mtry,
                          trees = best.model.specs$trees) %>% 
  set_engine("xgboost")


##----best workflow
df.best.wflow <- workflow() %>%
  add_recipe(df_rec_new) %>%
  add_model(final_model)

##----final best workflow
df.best.final.wflow<- finalize_workflow(
  df.best.wflow,
  best_auc
)

df.best.final.wflow

final.fitted.model_xgb <- last_fit(df.best.final.wflow,
                                   df_split_new,
                                   metrics = metric_set(roc_auc,accuracy,mn_log_loss))

saveRDS(final.fitted.model_xgb,"xgb_final_trained_model.rds")

final.fitted.model = readRDS("xgb_final_trained_model.rds")#upsample
saveRDS(final.fitted.model ,"final.fitted.model.rds")
final.fitted.model = readRDS("final.fitted.model.rds")

#########################################################################################
# Explore final fitted model results (XGBOOST MODEL)
#########################################################################################
##----explore final fitted model
final.fitted.model%>%
collect_metrics()

final.fitted.model%>%
collect_predictions()

# final.fitted.model%>%
#   collect_predictions() %>%
#   roc_curve(graph_name, .pred_class) %>%
#   ggplot(aes(x = 1 - specificity, y = sensitivity)) +
#   geom_line(size = 1.5, color = "midnightblue") +
#   geom_abline(lty = 2, alpha = 0.5,color = "gray50",size = 1.2)

final.fitted.model %>%
  collect_predictions() %>%
  roc_curve(graph_name, c(.pred_ER,.pred_sbm,.pred_SF,.pred_Spatial,.pred_SW)) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(linewidth = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    linewidth = 1.2
    )

#--------extract workflow for final model
extract_final.fitted.model= extract_workflow(final.fitted.model)
extract_final.fitted.model
saveRDS(extract_final.fitted.model ,"extract_final.fitted.model.rds")

extract_final.fitted.model=readRDS("extract_final.fitted.model.rds")

final.model.predictions <- predict(extract_final.fitted.model, 
                                   new_data = df_test_new) %>%
  bind_cols(df_test_new)

final.model.predictions

saveRDS(final.model.predictions,"final.model.predictions.rds")
final.model.predictions=readRDS("final.model.predictions.rds")
#----accuracy of best fitted model
final.model.predictions %>%
#mutate(graph_name = as.factor(graph_name)) %>%
accuracy(graph_name, .pred_class)


##----confusion matrix
final.model.predictions %>%
  conf_mat(graph_name, .pred_class)

final.model.predictions%>%
  conf_mat(truth = graph_name,estimate =.pred_class)%>%
  autoplot(type="heatmap")

###----Statistical summary of confusion matrix----###
Accuracy=yardstick::accuracy(data=final.model.predictions,truth = graph_name,estimate =.pred_class)
Precision=yardstick::precision(data=final.model.predictions,truth = graph_name,estimate =.pred_class)
Recall=yardstick::recall(data=final.model.predictions,truth = graph_name,estimate =.pred_class)


Accuracy %>%
  bind_rows(Precision)%>%
  bind_rows(Recall)




##################################################################################
#----Feature Normalization
################################################################################
df_data_new=readRDS("df_data_new.rds")
df_data_new_numeric <- as_tibble(df_data_new[, sapply(df_data_new, is.numeric)])


# Assuming smax and other necessary values are already computed and stored in the dataframe

# Apply normalizations
# normalized_df <- df_data_new_numeric %>%
#   mutate(
#     mean_eccentr = mean_eccentr/(order - 1),
#     mean_path_length = mean_path_length/(order - 1),
#     graph_energy = graph_energy/(order ^ 1.5),
#     diameter = diameter/(order - 1),
#     betw_centr = betw_centr/(sqrt(order)),
#     transitivity = transitivity/(order-1),  # number of nodes
#     spectral_radius = spectral_radius/(order-1),#suppose its similar to mean path length
#     modularity = modularity / (order-1),  # Assuming node-based normalization
#     eigen_centr = eigen_centr / (sqrt(order)),  # Assuming sqrt order normalization
#     deg_centr = deg_centr / (sqrt(order)),  # Assuming sqrt order normalization
#     mean_degree = mean_degree / (order-1),  # Assuming node-based normalization
#     min_cut = min_cut / (order-1),  # Assuming node-based normalization
#     fiedler_value = fiedler_value / (order-1),  # Assuming node-based normalization
#     normalized_fiedler_value = normalized_fiedler_value / (order-1),  # Assuming node-based normalization
#     closeness_centr = closeness_centr / (sqrt(order)),  # Assuming sqrt order normalization
#     deg_assort_coef = deg_assort_coef / (sqrt(order))  # Assuming sqrt order normalization
#   )
# 
# # Drop original columns if not needed
# View(head(normalized_df))


#graph_name <- df_data_new$graph_name
#df_data_new[, colnames(normalized_df)] <- normalized_df
#df_data_new$graph_name <- graph_name

# View the updated dataframe
#View(head(df_data_new))

# # Apply scaling operation using mutate() and across()
# df_scaled <- df %>%
#   mutate(across(cols_to_normalize, ~ ./order))
# # Step 2: Replace original columns with scaled ones
# df[, cols_to_normalize] <- df_scaled[, cols_to_normalize]



##################################################################################
#----SHAPELY ANALYSIS
################################################################################
df_data_new=readRDS("df_data_new.rds")

####----ER-----
# Filter the desired node sizes
filtered_df_er <- df_data_new %>%
  filter(graph_name=="ER" & 
           order %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtered data by "size"
grouped_df_er <- filtered_df_er %>% group_by(order)

# Sample 200 rows from each group
sampled_df_er <- grouped_df_er %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_er <- ungroup(sampled_df_er)


####----sbm-----
# Filtsbm the desired node sizes
filtered_df_sbm <- df_data_new %>%
  filter(graph_name=="sbm" & 
           order %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtsbmed data by "size"
grouped_df_sbm <- filtered_df_sbm %>% group_by(order)

# Sample 200 rows from each group
sampled_df_sbm <- grouped_df_sbm %>% sample_n(size = 1100, replace = FALSE)

# Remove grouping information
ungrouped_df_sbm <- ungroup(sampled_df_sbm)


####----SW-----
# FiltSW the desired node sizes
filtered_df_SW <- df_data_new %>%
  filter(graph_name=="SW" & 
           order %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtSWed data by "size"
grouped_df_SW <- filtered_df_SW %>% group_by(order)

# Sample 200 rows from each group
sampled_df_SW <- grouped_df_SW %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_SW <- ungroup(sampled_df_SW)

####----SF-----
# FiltSF the desired node sizes
filtered_df_SF <- df_data_new %>%
  filter(graph_name=="SF" & 
           order %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtSFed data by "size"
grouped_df_SF <- filtered_df_SF %>% group_by(order)

# Sample 200 rows from each group
sampled_df_SF <- grouped_df_SF %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_SF <- ungroup(sampled_df_SF)


####----Spatial-----
# FiltSpatial the desired node sizes
filtered_df_Spatial <- df_data_new %>%
  filter(graph_name=="Spatial" & 
           order %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtSpatialed data by "size"
grouped_df_Spatial <- filtered_df_Spatial %>% group_by(order)

# Sample 200 rows from each group
sampled_df_Spatial <- grouped_df_Spatial %>% sample_n(size = 1100, replace = FALSE)

# Remove grouping information
ungrouped_df_Spatial <- ungroup(sampled_df_Spatial)


df.final.new=rbind(ungrouped_df_Spatial,ungrouped_df_er,
                   ungrouped_df_SW,ungrouped_df_SF,
                   ungrouped_df_sbm)

df.final.new%>%dplyr::count(graph_name,sort=T)

saveRDS(df.final.new,"df.final.new.rds")

df.final.new=readRDS("df.final.new.rds")

final_resample_data <- df.final.new#[sample(nrow(df.final), 1000), ]
final_resample_data_prep <- bake(
  df_prep_new, # prep(df.final.rec), 
  has_role("predictor"),
  new_data = final_resample_data, 
  composition = "matrix"
)
head(final_resample_data_prep)

shap2 <- shapviz(extract_fit_engine(final.fitted.model), 
                 X_pred = final_resample_data_prep, 
                 X = final_resample_data)

saveRDS(shap2,"shap.new2.rds")



shap.new=readRDS("shap.new2.rds")
#shap.new=readRDS("shap.interactn.2.rds")

##----renaming variables
new_column_names=c('Nodes','Edges','Mean eccentricity','Mean path length',
                   'Graph energy','Modularity','Diameter','Betweenness centrality',
                   'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                   'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
                   'Closeness centrality','Degree assortativity')#,'graph_name')


names(shap.new)[names(shap.new) == "Class_1"] <- "Erdös-Rényi"
names(shap.new)[names(shap.new) == "Class_2"] <- "Stochastic-Block-Model"
names(shap.new)[names(shap.new) == "Class_3"] <- "Scale-Free"
names(shap.new)[names(shap.new) == "Class_4"] <- "Spatial"
names(shap.new)[names(shap.new) == "Class_5"] <- "Small-World"

colnames(shap.new$`Erdös-Rényi`$X)=new_column_names
colnames(shap.new$`Erdös-Rényi`$S)=new_column_names

colnames(shap.new$`Stochastic-Block-Model`$X)=new_column_names
colnames(shap.new$`Stochastic-Block-Model`$S)=new_column_names

colnames(shap.new$`Scale-Free`$X)=new_column_names
colnames(shap.new$`Scale-Free`$S)=new_column_names

colnames(shap.new$Spatial$X)=new_column_names
colnames(shap.new$Spatial$S)=new_column_names

colnames(shap.new$`Small-World`$X)=new_column_names
colnames(shap.new$`Small-World`$S)=new_column_names


saveRDS(shap.new,"final_shap_new.rds")

final_shap_new=readRDS("final_shap_new.rds")

final_shap=final_shap_new
#sv_importance(final_shap_new, kind = "both")#, show_numbers = TRUE)#var imp plot

########################################################################
###----Erdos Renyi var imp----###
#######################################################################
er_shap_bar.imp_new=sv_importance(final_shap$`Erdös-Rényi`,
                                  kind = 'bar',
                                  show_numbers = F, bee_width = 0.3,
                                  max_display=8L)

saveRDS(er_shap_bar.imp_new,"er_shap_bar.imp_new.rds")

er_shap_imp_new.plot.bar=er_shap_bar.imp_new+
  ggtitle(" ")+
  xlab(" ") +
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

er_shap_imp_new.plot.bar
##------beeswarm plot
er_shap_beeswarm_imp_new=sv_importance(final_shap$`Erdös-Rényi`, 
                                       kind = 'beeswarm',
                                       show_numbers = F,
                                       fill = "#fca50a",
                                       bee_width = 0.3,
                                       max_display=8L)
saveRDS(er_shap_beeswarm_imp_new,"er_shap_beeswarm_imp_new.rds")


er_shap_beeswarm_imp_new.plot=er_shap_beeswarm_imp_new+
  ggtitle("Erdös-Rényi")+
  xlab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        text = element_text(size = 20,family="serif"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-0.1,0,0.5), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

er_shap_beeswarm_imp_new.plot

figure1=er_shap_beeswarm_imp_new.plot+
  theme(legend.position="none")

figure2=er_shap_imp_new.plot.bar

er.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)
#ggsave("comb.varimp1.plot.png", width = 14, height = 7)

# Get the legend from one of the plots
legend <- get_legend(er_shap_beeswarm_imp_new.plot)

# Add the legend to the combined plot
combined_plot_with_legend.er <- plot_grid(er.combine.var.imp,ncol = 1, rel_heights = c(1, 0.2))

combined_plot_with_legend.er
ggsave("er_shap_varimp2.svg", width = 14, height = 7)
ggsave("er_shap_varimp2.pdf", width = 14, height = 7)


########################################################################
###----Stochastic-Block-Model var imp----###
#######################################################################
sbm_shap_bar.imp_new=sv_importance(final_shap$`Stochastic-Block-Model`,
                                   kind = 'bar',
                                   show_numbers = F, bee_width = 0.3,
                                   max_display=8L)

saveRDS(sbm_shap_bar.imp_new,"sbm_shap_bar.imp_new.rds")

sbm_shap_imp_new.plot.bar=sbm_shap_bar.imp_new+
  ggtitle(" ")+
  xlab(" ") +
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#ggsave("sbm_shap_varimp.bar.png", width = 10, height = 10)


##------beeswarm plot
sbm_shap_beeswarm_imp_new=sv_importance(final_shap$`Stochastic-Block-Model`,
                                        kind = 'beeswarm',
                                        show_numbers = F, bee_width = 0.3,
                                        max_display=8L)
saveRDS(sbm_shap_beeswarm_imp_new,"sbm_shap_beeswarm_imp_new.rds")


sbm_shap_beeswarm_imp_new.plot=sbm_shap_beeswarm_imp_new+
  ggtitle("Stochastic-Block-Model")+
  xlab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        text = element_text(size = 20,family="serif"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-0.1,0,1), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(1.5, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

figure1=sbm_shap_beeswarm_imp_new.plot+
  theme(legend.position="none")

figure2=sbm_shap_imp_new.plot.bar

sbm.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)
#ggsave("comb.varimp1.plot.png", width = 14, height = 7)

# Get the legend from one of the plots
legend <- get_legend(sbm_shap_beeswarm_imp_new.plot)

# Add the legend to the combined plot
combined_plot_with_legend.sbm <- plot_grid(sbm.combine.var.imp, 
                                           legend, ncol = 1, rel_heights = c(1, 0.2))

combined_plot_with_legend.sbm
ggsave("sbm_shap_varimp2.svg", width = 14, height = 7)
ggsave("sbm_shap_varimp2.pdf", width = 14, height = 7)

combined_imp1=plot_grid(combined_plot_with_legend.er,
                        combined_plot_with_legend.sbm,
                        nrow = 2)

#ggsave("combined_imp1.png", width = 14, height = 14)
#ggsave("combined_imp1.pdf", width = 14, height = 14)

########################################################################
###----Scale-Free var imp----###
#######################################################################
sf_shap_bar.imp_new=sv_importance(final_shap$`Scale-Free`, kind = 'bar',
                                  show_numbers = F, bee_width = 0.3,
                                  max_display=8L)

saveRDS(sf_shap_bar.imp_new,"sf_shap_bar.imp_new.rds")

sf_shap_imp_new.plot.bar=sf_shap_bar.imp_new+
  ggtitle(" ")+
  xlab(" ") +
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#sf_shap_imp_new.plot.bar=sf_shap_imp_new.plot.bar+plot_layout(widths = 1,heights = 1)

##------beeswarm plot
sf_shap_beeswarm_imp_new=sv_importance(final_shap$`Scale-Free`, kind = 'beeswarm',
                                       show_numbers = F, bee_width = 0.3,
                                       max_display=8L)
saveRDS(sf_shap_beeswarm_imp_new,"sf_shap_beeswarm_imp_new.rds")


sf_shap_beeswarm_imp_new.plot=sf_shap_beeswarm_imp_new+
  ggtitle("Scale-Free")+
  xlab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        text = element_text(size = 20,family="serif"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-0.1,0,1), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

figure1=sf_shap_beeswarm_imp_new.plot+
  theme(legend.position="none")

figure2=sf_shap_imp_new.plot.bar

sf.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)

# Get the legend from one of the plots
legend <- get_legend(sf_shap_beeswarm_imp_new.plot)

# Add the legend to the combined plot
combined_plot_with_legend.sf <- plot_grid(sf.combine.var.imp, ncol = 1, rel_heights = c(1, 0.2))

combined_plot_with_legend.sf=
  combined_plot_with_legend.sf+plot_layout(widths = 1,heights = 1)
ggsave("sf_shap_varimp2.svg", width = 14, height = 7)
ggsave("sf_shap_varimp2.pdf", width = 14, height = 7)

########################################################################
###----Spatial var imp----###
#######################################################################
sp_shap_bar.imp_new=sv_importance(final_shap$Spatial, kind = 'bar',
                                  show_numbers = F, bee_width = 0.3,
                                  max_display=8L)

saveRDS(sp_shap_bar.imp_new,"sp_shap_bar.imp_new.rds")

sp_shap_imp_new.plot.bar=sp_shap_bar.imp_new+
  ggtitle(" ")+
  xlab(" ") +
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


##------beeswarm plot
sp_shap_beeswarm_imp_new=sv_importance(final_shap$`Spatial`, kind = 'beeswarm',
                                       show_numbers = F, bee_width = 0.3,
                                       max_display=8L)
saveRDS(sp_shap_beeswarm_imp_new,"sp_shap_beeswarm_imp_new.rds")


sp_shap_beeswarm_imp_new.plot=sp_shap_beeswarm_imp_new+
  ggtitle("Spatial")+
  xlab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        text = element_text(size = 20,family="serif"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-0.1,0,1), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(1.5, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

figure1=sp_shap_beeswarm_imp_new.plot+
  theme(legend.position="none")

figure2=sp_shap_imp_new.plot.bar

sp.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)

# Get the legend from one of the plots
legend <- get_legend(sp_shap_beeswarm_imp_new.plot)

# Add the legend to the combined plot
combined_plot_with_legend.sp <- plot_grid(sp.combine.var.imp,ncol = 1, rel_heights = c(1, 0.2))

combined_plot_with_legend.sp=combined_plot_with_legend.sp+
  plot_layout(widths = 1,heights = 1)

ggsave("sp_shap_varimp2.svg", width = 14, height = 7)
ggsave("sp_shap_varimp2.pdf", width = 14, height = 7)

########################################################################
###----Small-World var imp----###
#######################################################################
sw_shap_bar.imp_new=sv_importance(final_shap$`Small-World`, kind = 'bar',
                                  show_numbers = F, bee_width = 0.3,
                                  max_display=8L)

saveRDS(sw_shap_bar.imp_new,"sw_shap_bar.imp_new.rds")

sw_shap_imp_new.plot.bar=sw_shap_bar.imp_new+
  ggtitle(" ")+
  xlab(" ") +
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


##------beeswarm plot
sw_shap_beeswarm_imp_new=sv_importance(final_shap$`Small-World`, kind = 'beeswarm',
                                       show_numbers = F, bee_width = 0.3,
                                       max_display=8L)
saveRDS(sw_shap_beeswarm_imp_new,"sw_shap_beeswarm_imp_new.rds")


sw_shap_beeswarm_imp_new.plot=sw_shap_beeswarm_imp_new+
  ggtitle("Small-World")+
  xlab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 20),
        text = element_text(size = 20,family="serif"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-0.1,0,1), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(1.5, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

figure1=sw_shap_beeswarm_imp_new.plot+
  theme(legend.position="none")

figure2=sw_shap_imp_new.plot.bar

sw.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)

# Get the legend from one of the plots
legend <- get_legend(sw_shap_beeswarm_imp_new.plot)

# Add the legend to the combined plot
combined_plot_with_legend.sw <- plot_grid(sw.combine.var.imp, 
                                          legend, ncol = 1, rel_heights = c(1, 0.2))

combined_plot_with_legend.sw=combined_plot_with_legend.sw+plot_layout(widths = 1,heights = 1)

combined_plot_with_legend.sw=combined_plot_with_legend.sw+
  plot_layout(widths = 1,heights = 1)

ggsave("sw_shap_varimp2.svg", width = 14, height = 7)
ggsave("sw_shap_varimp2.pdf", width = 14, height = 7)

# combined_imp2=plot_grid(combined_plot_with_legend.sf,
#                         combined_plot_with_legend.sp,
#                         combined_plot_with_legend.sw,
#                         nrow = 3)
# 
# ggsave("combined_imp22.pdf", width = 14, height = 20, dpi = 50)

##############################################################################
#####-------Graphs of Networks----
#############################################################################
n=200

##----ER Graph----
er.graph= erdos.renyi.game(n,1176,type = c("gnm"),directed = F,loops = F)
er.graph
plot(er.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="orange")#,frame.plot = TRUE)

ggsave("er_plot2.png",height=5,width=6)
ggsave("er_plot.svg",height=5,width=6)
##----SF Graph----
sf.graph = sample_pa(n=n, power=2, m=6, directed=FALSE, algorithm="psumtree")
sf.graph
plot(sf.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="orange")

ggsave("sf_plot2.png",height=5,width=6)
ggsave("sf_plot.svg",height=5,width=6)
##----SW Graph----
sw.graph = sample_smallworld(dim=2, size=sqrt(n), nei=2, p=0.01)
sw.graph
sw.plot=plot(sw.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
             edge.arrow.size=0.5, edge.color="orange")



ggsave("sw_plot2.svg",height=5,width=6)




##----SBM Graph----
sbm.graph=sample_sbm(n, pref.matrix = pm, block.sizes = c(0.4*n, 0.6*n))
sbm.graph
plot(sbm.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="orange")

ggsave("sbm_plot2.svg",height=5,width=6)
##----SP Graph----
sp.graph=fastSpatialNetwork(n = n, r = 0.15, makeConnected=T,keepCellsSeparate=FALSE)
sp.graph

sp=plot(sp.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
        edge.arrow.size=0.5, edge.color="orange")

sp
ggsave("sp_plot2.svg",height=5,width=6)



###############################################################################
# Shap Dependency plot
###############################################################################

###----Modularity shapely depedency plot----###
mod.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Modularity",
                          interactions = FALSE, color_var = NULL)

saveRDS(mod.shap.er,"mod.shap.er.rds")

mod.shap.er=readRDS("mod.shap.er.rds")
mod.shap.er.df=mod.shap.er$data

mod.shap.er.plot=
  ggplot(mod.shap.er.df, aes(x = Modularity, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.er.plot=mod.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
mod.shap.er.plot=mod.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

mod.shap.er.plot


###----Shap variable dependency plot for Modularity----###
mod.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Modularity",
                           interactions = FALSE, color_var = NULL)

saveRDS(mod.shap.sbm,"mod.shap.sbm.rds")

mod.shap.sbm=readRDS("mod.shap.sbm.rds")
mod.shap.sbm.df=mod.shap.sbm$data

mod.shap.sbm.plot=
  ggplot(mod.shap.sbm.df, aes(x = Modularity, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.sbm.plot=mod.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
mod.shap.sbm.plot=mod.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



mod.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Modularity",
                          interactions = FALSE, color_var = NULL)

saveRDS(mod.shap.sf,"mod.shap.sf.rds")

mod.shap.sf=readRDS("mod.shap.sf.rds")
mod.shap.sf.df=mod.shap.sf$data

mod.shap.sf.plot=
  ggplot(mod.shap.sf.df, aes(x = Modularity, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.sf.plot=mod.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
mod.shap.sf.plot=mod.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
mod.shap.sf.plot


###----Shap variable dependency plot for Modularity----###
mod.shap.sp=sv_dependence(final_shap$Spatial, "Modularity",
                          interactions = FALSE, color_var = NULL)

saveRDS(mod.shap.sp,"mod.shap.sp.rds")

mod.shap.sp=readRDS("mod.shap.sp.rds")
mod.shap.sp.df=mod.shap.sp$data

mod.shap.sp.plot=
  ggplot(mod.shap.sp.df, aes(x = Modularity, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.sp.plot=mod.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
mod.shap.sp.plot=mod.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
mod.shap.sp.plot


###----Shap variable dependency plot for Modularity----###
mod.shap.sw=sv_dependence(final_shap$`Small-World`, "Modularity",
                          interactions = FALSE, color_var = NULL)

saveRDS(mod.shap.sw,"mod.shap.sw.rds")

mod.shap.sw=readRDS("mod.shap.sw.rds")
mod.shap.sw.df=mod.shap.sw$data

mod.shap.sw.plot=
  ggplot(mod.shap.sw.df, aes(x = Modularity, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Small-World")+
  xlab(" ")+
  ylab("Modularity")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


#mod.shap.sw.plot=mod.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
mod.shap.sw.plot=mod.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

mod.shap.sw.plot=
  mod.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                      sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

mod.shap.sw.plot


Modularity.figure <- ggarrange(mod.shap.er.plot, mod.shap.sbm.plot, mod.shap.sf.plot,
                               mod.shap.sp.plot,mod.shap.sw.plot,
                               nrow = 1)#,widths=1.5, heights=2)
Modularity.figure=Modularity.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins
# ggtitle("Modularity") +  # Set main title
#  theme(plot.title = element_text(hjust = 0.5,size = 24,family="serif"))  # Center the main title

Modularity.figure

# ggsave("m.png", width=16, height=4)


###----Transitivity shapely depedency plot----###
trans.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Transitivity",
                            interactions = FALSE, color_var = NULL)

saveRDS(trans.shap.er,"trans.shap.er.rds")

trans.shap.er=readRDS("trans.shap.er.rds")
trans.shap.er.df=trans.shap.er$data

trans.shap.er.plot=
  ggplot(trans.shap.er.df, aes(x = Transitivity, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

trans.shap.er.plot=trans.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
trans.shap.er.plot=trans.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

trans.shap.er.plot


###----Shap variable dependency plot for Transitivity----###
trans.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Transitivity",
                             interactions = FALSE, color_var = NULL)

saveRDS(trans.shap.sbm,"trans.shap.sbm.rds")

trans.shap.sbm=readRDS("trans.shap.sbm.rds")
trans.shap.sbm.df=trans.shap.sbm$data

trans.shap.sbm.plot=
  ggplot(trans.shap.sbm.df, aes(x = Transitivity, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

trans.shap.sbm.plot=trans.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
trans.shap.sbm.plot=trans.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



trans.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Transitivity",
                            interactions = FALSE, color_var = NULL)

saveRDS(trans.shap.sf,"trans.shap.sf.rds")

trans.shap.sf=readRDS("trans.shap.sf.rds")
trans.shap.sf.df=trans.shap.sf$data

trans.shap.sf.plot=
  ggplot(trans.shap.sf.df, aes(x = Transitivity, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

trans.shap.sf.plot=trans.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
trans.shap.sf.plot=trans.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
trans.shap.sf.plot


###----Shap variable dependency plot for Transitivity----###
trans.shap.sp=sv_dependence(final_shap$Spatial, "Transitivity",
                            interactions = FALSE, color_var = NULL)

saveRDS(trans.shap.sp,"trans.shap.sp.rds")

trans.shap.sp=readRDS("trans.shap.sp.rds")
trans.shap.sp.df=trans.shap.sp$data

trans.shap.sp.plot=
  ggplot(trans.shap.sp.df, aes(x = Transitivity, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

trans.shap.sp.plot=trans.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
trans.shap.sp.plot=trans.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
trans.shap.sp.plot


###----Shap variable dependency plot for Transitivity----###
trans.shap.sw=sv_dependence(final_shap$`Small-World`, "Transitivity",
                            interactions = FALSE, color_var = NULL)

saveRDS(trans.shap.sw,"trans.shap.sw.rds")

trans.shap.sw=readRDS("trans.shap.sw.rds")
trans.shap.sw.df=trans.shap.sw$data

trans.shap.sw.plot=
  ggplot(trans.shap.sw.df, aes(x = Transitivity, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Transitivity")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


trans.shap.sw.plot=trans.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
trans.shap.sw.plot=trans.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

trans.shap.sw.plot=
  trans.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                        sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

trans.shap.sw.plot


Transitivity.figure <- ggarrange(trans.shap.er.plot, trans.shap.sbm.plot, trans.shap.sf.plot,
                                 trans.shap.sp.plot,trans.shap.sw.plot,
                                 nrow = 1)#,widths=1.5, heights=2)
Transitivity.figure=Transitivity.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Transitivity.figure

ggsave("m2.png", width=18, height=4)


###----Spectral_radius shapely depedency plot----###

spec.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Spectral radius",
                           interactions = FALSE, color_var = NULL)

saveRDS(spec.shap.er,"spec.shap.er.rds")

spec.shap.er=readRDS("spec.shap.er.rds")
spec.shap.er.df=spec.shap.er$data
colnames(spec.shap.er.df)=c("shap","Spectral_radius")

spec.shap.er.plot=
  ggplot(spec.shap.er.df, aes(x = Spectral_radius, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

spec.shap.er.plot=spec.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
spec.shap.er.plot=spec.shap.er.plot+
  scale_x_continuous(breaks = c(0,750))

spec.shap.er.plot


###----Shap variable dependency plot for Spectral radius----###
spec.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Spectral radius",
                            interactions = FALSE, color_var = NULL)

saveRDS(spec.shap.sbm,"spec.shap.sbm.rds")

spec.shap.sbm=readRDS("spec.shap.sbm.rds")
spec.shap.sbm.df=spec.shap.sbm$data
colnames(spec.shap.sbm.df)=c("shap","Spectral_radius")

spec.shap.sbm.plot=
  ggplot(spec.shap.sbm.df, aes(x = Spectral_radius, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

spec.shap.sbm.plot=spec.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
spec.shap.sbm.plot=spec.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,750))



spec.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Spectral radius",
                           interactions = FALSE, color_var = NULL)

saveRDS(spec.shap.sf,"spec.shap.sf.rds")

spec.shap.sf=readRDS("spec.shap.sf.rds")
spec.shap.sf.df=spec.shap.sf$data
colnames(spec.shap.sf.df)=c("shap","Spectral_radius")

spec.shap.sf.plot=
  ggplot(spec.shap.sf.df, aes(x = Spectral_radius, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

spec.shap.sf.plot=spec.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
spec.shap.sf.plot=spec.shap.sf.plot+
  scale_x_continuous(breaks = c(0,750))
spec.shap.sf.plot


###----Shap variable dependency plot for Spectral radius----###
spec.shap.sp=sv_dependence(final_shap$Spatial, "Spectral radius",
                           interactions = FALSE, color_var = NULL)

saveRDS(spec.shap.sp,"spec.shap.sp.rds")

spec.shap.sp=readRDS("spec.shap.sp.rds")
spec.shap.sp.df=spec.shap.sp$data
colnames(spec.shap.sp.df)=c("shap","Spectral_radius")

spec.shap.sp.plot=
  ggplot(spec.shap.sp.df, aes(x = Spectral_radius, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

spec.shap.sp.plot=spec.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
spec.shap.sp.plot=spec.shap.sp.plot+
  scale_x_continuous(breaks = c(0,750))
spec.shap.sp.plot


###----Shap variable dependency plot for Spectral radius----###
spec.shap.sw=sv_dependence(final_shap$`Small-World`, "Spectral radius",
                           interactions = FALSE, color_var = NULL)

saveRDS(spec.shap.sw,"spec.shap.sw.rds")


spec.shap.sw=readRDS("spec.shap.sw.rds")
spec.shap.sw.df=spec.shap.sw$data
colnames(spec.shap.sw.df)=c("shap","Spectral_radius")

spec.shap.sw.plot=
  ggplot(spec.shap.sw.df, aes(x = Spectral_radius, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Spectral radius")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


#spec.shap.sw.plot=spec.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
spec.shap.sw.plot=spec.shap.sw.plot+
  scale_x_continuous(breaks = c(0,750))

spec.shap.sw.plot=
  spec.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                       sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

spec.shap.sw.plot


Spectral_radius.figure <- ggarrange(spec.shap.er.plot, spec.shap.sbm.plot, spec.shap.sf.plot,
                                    spec.shap.sp.plot,spec.shap.sw.plot,
                                    nrow = 1)#,widths=1.5, heights=2)
Spectral_radius.figure=Spectral_radius.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Spectral_radius.figure


# row1 <- plot_grid(mod.shap.er.plot, mod.shap.sbm.plot, mod.shap.sf.plot,
#                   mod.shap.sp.plot, mod.shap.sw.plot, ncol = 3)
# row2=plot_grid(trans.shap.er.plot,trans.shap.sbm.plot, trans.shap.sf.plot,
#                trans.shap.sp.plot,trans.shap.sw.plot,ncol=3)
# 
# row3=plot_grid(spec.shap.er.plot,spec.shap.sbm.plot, spec.shap.sf.plot,
#                spec.shap.sp.plot,spec.shap.sw.plot,ncol=3)
# 
# combinedplot1=ggarrange(row1,row2,row3, nrow = 3)
# ggsave("combinedplot1.png",width=10, height=13)


###----Normalized fiedler value shapely depedency plot----###

nf.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Normalized Fiedler",
                         interactions = FALSE, color_var = NULL)

saveRDS(nf.shap.er,"nf.shap.er.rds")

nf.shap.er=readRDS("nf.shap.er.rds")
nf.shap.er.df=nf.shap.er$data
colnames(nf.shap.er.df)=c("shap","Normalized_Fiedler")

nf.shap.er.plot=
  ggplot(nf.shap.er.df, aes(x = Normalized_Fiedler, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.er.plot=nf.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
nf.shap.er.plot=nf.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

nf.shap.er.plot


###----Shap variable dependency plot for Normalized Fiedler----###
nf.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Normalized Fiedler",
                          interactions = FALSE, color_var = NULL)

saveRDS(nf.shap.sbm,"nf.shap.sbm.rds")

nf.shap.sbm=readRDS("nf.shap.sbm.rds")
nf.shap.sbm.df=nf.shap.sbm$data
colnames(nf.shap.sbm.df)=c("shap","Normalized_Fiedler")

nf.shap.sbm.plot=
  ggplot(nf.shap.sbm.df, aes(x = Normalized_Fiedler, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sbm.plot=nf.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
nf.shap.sbm.plot=nf.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



nf.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Normalized Fiedler",
                         interactions = FALSE, color_var = NULL)

saveRDS(nf.shap.sf,"nf.shap.sf.rds")

nf.shap.sf=readRDS("nf.shap.sf.rds")
nf.shap.sf.df=nf.shap.sf$data
colnames(nf.shap.sf.df)=c("shap","Normalized_Fiedler")

nf.shap.sf.plot=
  ggplot(nf.shap.sf.df, aes(x = Normalized_Fiedler, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sf.plot=nf.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
nf.shap.sf.plot=nf.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
nf.shap.sf.plot


###----Shap variable dependency plot for Normalized Fiedler----###
nf.shap.sp=sv_dependence(final_shap$Spatial, "Normalized Fiedler",
                         interactions = FALSE, color_var = NULL)

saveRDS(nf.shap.sp,"nf.shap.sp.rds")

nf.shap.sp=readRDS("nf.shap.sp.rds")
nf.shap.sp.df=nf.shap.sp$data
colnames(nf.shap.sp.df)=c("shap","Normalized_Fiedler")

nf.shap.sp.plot=
  ggplot(nf.shap.sp.df, aes(x = Normalized_Fiedler, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sp.plot=nf.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
nf.shap.sp.plot=nf.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
nf.shap.sp.plot


###----Shap variable dependency plot for Normalized Fiedler----###
nf.shap.sw=sv_dependence(final_shap$`Small-World`, "Normalized Fiedler",
                         interactions = FALSE, color_var = NULL)

saveRDS(nf.shap.sw,"nf.shap.sw.rds")

nf.shap.sw=readRDS("nf.shap.sw.rds")
nf.shap.sw.df=nf.shap.sw$data
colnames(nf.shap.sw.df)=c("shap","Normalized_Fiedler")

nf.shap.sw.plot=
  ggplot(nf.shap.sw.df, aes(x = Normalized_Fiedler, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("")+
  xlab(" ")+
  ylab("Normalized Fiedler")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#nf.shap.sw.plot=nf.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
nf.shap.sw.plot=nf.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

nf.shap.sw.plot=
  nf.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                     sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

nf.shap.sw.plot


Normalized_Fiedler.figure <- ggarrange(nf.shap.er.plot, nf.shap.sbm.plot, nf.shap.sf.plot,
                                       nf.shap.sp.plot,nf.shap.sw.plot,
                                       nrow = 1)#,widths=1.5, heights=2)
Normalized_Fiedler.figure=Normalized_Fiedler.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Normalized_Fiedler.figure

# ggsave("m3.png", width=18, height=4)



###----Degree assortativity coeff shapely depedency plot----###
deg_assort.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree assortativity",
                                 interactions = FALSE, color_var = NULL)

saveRDS(deg_assort.shap.er,"deg_assort.shap.er.rds")

deg_assort.shap.er=readRDS("deg_assort.shap.er.rds")
deg_assort.shap.er.df=deg_assort.shap.er$data
colnames(deg_assort.shap.er.df)=c("shap","Degree_assortativity")

deg_assort.shap.er.plot=
  ggplot(deg_assort.shap.er.df, aes(x = Degree_assortativity, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.er.plot=deg_assort.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_assort.shap.er.plot=deg_assort.shap.er.plot+
  scale_x_continuous(breaks = c(-1,0))

deg_assort.shap.er.plot


###----Shap variable dependency plot for Degree assortativity----###
deg_assort.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Degree assortativity",
                                  interactions = FALSE, color_var = NULL)

saveRDS(deg_assort.shap.sbm,"deg_assort.shap.sbm.rds")

deg_assort.shap.sbm=readRDS("deg_assort.shap.sbm.rds")
deg_assort.shap.sbm.df=deg_assort.shap.sbm$data
colnames(deg_assort.shap.sbm.df)=c("shap","Degree_assortativity")

deg_assort.shap.sbm.plot=
  ggplot(deg_assort.shap.sbm.df, aes(x = Degree_assortativity, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sbm.plot=deg_assort.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_assort.shap.sbm.plot=deg_assort.shap.sbm.plot+
  scale_x_continuous(breaks = c(-1,0.))



deg_assort.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Degree assortativity",
                                 interactions = FALSE, color_var = NULL)

saveRDS(deg_assort.shap.sf,"deg_assort.shap.sf.rds")

deg_assort.shap.sf=readRDS("deg_assort.shap.sf.rds")
deg_assort.shap.sf.df=deg_assort.shap.sf$data
colnames(deg_assort.shap.sf.df)=c("shap","Degree_assortativity")

deg_assort.shap.sf.plot=
  ggplot(deg_assort.shap.sf.df, aes(x = Degree_assortativity, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sf.plot=deg_assort.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_assort.shap.sf.plot=deg_assort.shap.sf.plot+
  scale_x_continuous(breaks = c(-1,0.))
deg_assort.shap.sf.plot


###----Shap variable dependency plot for Degree assortativity----###
deg_assort.shap.sp=sv_dependence(final_shap$Spatial, "Degree assortativity",
                                 interactions = FALSE, color_var = NULL)

saveRDS(deg_assort.shap.sp,"deg_assort.shap.sp.rds")

deg_assort.shap.sp=readRDS("deg_assort.shap.sp.rds")
deg_assort.shap.sp.df=deg_assort.shap.sp$data
colnames(deg_assort.shap.sp.df)=c("shap","Degree_assortativity")

deg_assort.shap.sp.plot=
  ggplot(deg_assort.shap.sp.df, aes(x = Degree_assortativity, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sp.plot=deg_assort.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_assort.shap.sp.plot=deg_assort.shap.sp.plot+
  scale_x_continuous(breaks = c(-1,0.))
deg_assort.shap.sp.plot


###----Shap variable dependency plot for Degree assortativity----###
deg_assort.shap.sw=sv_dependence(final_shap$`Small-World`, "Degree assortativity",
                                 interactions = FALSE, color_var = NULL)

saveRDS(deg_assort.shap.sw,"deg_assort.shap.sw.rds")


deg_assort.shap.sw=readRDS("deg_assort.shap.sw.rds")
deg_assort.shap.sw.df=deg_assort.shap.sw$data
colnames(deg_assort.shap.sw.df)=c("shap","Degree_assortativity")

deg_assort.shap.sw.plot=
  ggplot(deg_assort.shap.sw.df, aes(x = Degree_assortativity, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Degree assortativity")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.sw.plot=deg_assort.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_assort.shap.sw.plot=deg_assort.shap.sw.plot+
  scale_x_continuous(breaks = c(-1,0))

deg_assort.shap.sw.plot=
  deg_assort.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                             sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

deg_assort.shap.sw.plot


Degree_assortativity.figure <- ggarrange(deg_assort.shap.er.plot, deg_assort.shap.sbm.plot, deg_assort.shap.sf.plot,
                                         deg_assort.shap.sp.plot,deg_assort.shap.sw.plot,
                                         nrow = 1)#,widths=1.5, heights=2)
Degree_assortativity.figure=Degree_assortativity.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Degree_assortativity.figure


###----Degree_centrality shapely depedency plot----###

deg_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree centrality",
                                interactions = FALSE, color_var = NULL)

saveRDS(deg_centr.shap.er,"deg_centr.shap.er.rds")

deg_centr.shap.er=readRDS("deg_centr.shap.er.rds")
deg_centr.shap.er.df=deg_centr.shap.er$data
colnames(deg_centr.shap.er.df)=c("shap","Degree_centrality")

deg_centr.shap.er.plot=
  ggplot(deg_centr.shap.er.df, aes(x = Degree_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.er.plot=deg_centr.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_centr.shap.er.plot=deg_centr.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

deg_centr.shap.er.plot


###----Shap variable dependency plot for Degree centrality----###
deg_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Degree centrality",
                                 interactions = FALSE, color_var = NULL)

saveRDS(deg_centr.shap.sbm,"deg_centr.shap.sbm.rds")

deg_centr.shap.sbm=readRDS("deg_centr.shap.sbm.rds")
deg_centr.shap.sbm.df=deg_centr.shap.sbm$data
colnames(deg_centr.shap.sbm.df)=c("shap","Degree_centrality")

deg_centr.shap.sbm.plot=
  ggplot(deg_centr.shap.sbm.df, aes(x = Degree_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.sbm.plot=deg_centr.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_centr.shap.sbm.plot=deg_centr.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



deg_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Degree centrality",
                                interactions = FALSE, color_var = NULL)

saveRDS(deg_centr.shap.sf,"deg_centr.shap.sf.rds")

deg_centr.shap.sf=readRDS("deg_centr.shap.sf.rds")
deg_centr.shap.sf.df=deg_centr.shap.sf$data
colnames(deg_centr.shap.sf.df)=c("shap","Degree_centrality")

deg_centr.shap.sf.plot=
  ggplot(deg_centr.shap.sf.df, aes(x = Degree_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.sf.plot=deg_centr.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_centr.shap.sf.plot=deg_centr.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
deg_centr.shap.sf.plot


###----Shap variable dependency plot for Degree centrality----###
deg_centr.shap.sp=sv_dependence(final_shap$Spatial, "Degree centrality",
                                interactions = FALSE, color_var = NULL)

saveRDS(deg_centr.shap.sp,"deg_centr.shap.sp.rds")

deg_centr.shap.sp=readRDS("deg_centr.shap.sp.rds")
deg_centr.shap.sp.df=deg_centr.shap.sp$data
colnames(deg_centr.shap.sp.df)=c("shap","Degree_centrality")

deg_centr.shap.sp.plot=
  ggplot(deg_centr.shap.sp.df, aes(x = Degree_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.sp.plot=deg_centr.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_centr.shap.sp.plot=deg_centr.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
deg_centr.shap.sp.plot


###----Shap variable dependency plot for Degree centrality----###
deg_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Degree centrality",
                                interactions = FALSE, color_var = NULL)

saveRDS(deg_centr.shap.sw,"deg_centr.shap.sw.rds")

deg_centr.shap.sw=readRDS("deg_centr.shap.sw.rds")
deg_centr.shap.sw.df=deg_centr.shap.sw$data
colnames(deg_centr.shap.sw.df)=c("shap","Degree_centrality")

deg_centr.shap.sw.plot=
  ggplot(deg_centr.shap.sw.df, aes(x = Degree_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Degree centrality")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#deg_centr.shap.sw.plot=deg_centr.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
deg_centr.shap.sw.plot=deg_centr.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

deg_centr.shap.sw.plot=
  deg_centr.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                            sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

deg_centr.shap.sw.plot


Degree_centrality.figure <- ggarrange(deg_centr.shap.er.plot, deg_centr.shap.sbm.plot, deg_centr.shap.sf.plot,
                                      deg_centr.shap.sp.plot,deg_centr.shap.sw.plot,
                                      nrow = 1)#,widths=1.5, heights=2)
Degree_centrality.figure=Degree_centrality.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Degree_centrality.figure

# ggsave("m4.png", width=18, height=4)

# row3 <- plot_grid(nf.shap.er.plot, nf.shap.sbm.plot, nf.shap.sf.plot,
#                   nf.shap.sp.plot, nf.shap.sw.plot, ncol = 3)
# row4=plot_grid(deg_centr.shap.er.plot,deg_centr.shap.sbm.plot, 
#                deg_centr.shap.sf.plot,
#                deg_centr.shap.sp.plot,deg_centr.shap.sw.plot,ncol=3)
# 
# row5=plot_grid(deg_assort.shap.er.plot,deg_assort.shap.sbm.plot, 
#                deg_assort.shap.sf.plot,
#                deg_assort.shap.sp.plot,deg_assort.shap.sw.plot,ncol=3)
# 
# combinedplot2=ggarrange(row3,row4,row5, nrow = 3)
# ggsave("combinedplot2.png",width=10, height=13)


###----Eigen_centrality shapely depedency plot----###

eign_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Eigen centrality",
                                 interactions = FALSE, color_var = NULL)

saveRDS(eign_centr.shap.er,"eign_centr.shap.er.rds")

eign_centr.shap.er=readRDS("eign_centr.shap.er.rds")
eign_centr.shap.er.df=eign_centr.shap.er$data
colnames(eign_centr.shap.er.df)=c("shap","Eigen_centrality")

eign_centr.shap.er.plot=
  ggplot(eign_centr.shap.er.df, aes(x = Eigen_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

eign_centr.shap.er.plot=eign_centr.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
eign_centr.shap.er.plot=eign_centr.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

eign_centr.shap.er.plot


###----Shap variable dependency plot for Eigen centrality----###
eign_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Eigen centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(eign_centr.shap.sbm,"eign_centr.shap.sbm.rds")

eign_centr.shap.sbm=readRDS("eign_centr.shap.sbm.rds")
eign_centr.shap.sbm.df=eign_centr.shap.sbm$data
colnames(eign_centr.shap.sbm.df)=c("shap","Eigen_centrality")

eign_centr.shap.sbm.plot=
  ggplot(eign_centr.shap.sbm.df, aes(x = Eigen_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

eign_centr.shap.sbm.plot=eign_centr.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
eign_centr.shap.sbm.plot=eign_centr.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



eign_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Eigen centrality",
                                 interactions = FALSE, color_var = NULL)

saveRDS(eign_centr.shap.sf,"eign_centr.shap.sf.rds")

eign_centr.shap.sf=readRDS("eign_centr.shap.sf.rds")
eign_centr.shap.sf.df=eign_centr.shap.sf$data
colnames(eign_centr.shap.sf.df)=c("shap","Eigen_centrality")

eign_centr.shap.sf.plot=
  ggplot(eign_centr.shap.sf.df, aes(x = Eigen_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

eign_centr.shap.sf.plot=eign_centr.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
eign_centr.shap.sf.plot=eign_centr.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
eign_centr.shap.sf.plot


###----Shap variable dependency plot for Eigen centrality----###
eign_centr.shap.sp=sv_dependence(final_shap$Spatial, "Eigen centrality",
                                 interactions = FALSE, color_var = NULL)

saveRDS(eign_centr.shap.sp,"eign_centr.shap.sp.rds")

eign_centr.shap.sp=readRDS("eign_centr.shap.sp.rds")
eign_centr.shap.sp.df=eign_centr.shap.sp$data
colnames(eign_centr.shap.sp.df)=c("shap","Eigen_centrality")

eign_centr.shap.sp.plot=
  ggplot(eign_centr.shap.sp.df, aes(x = Eigen_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

eign_centr.shap.sp.plot=eign_centr.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
eign_centr.shap.sp.plot=eign_centr.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
eign_centr.shap.sp.plot


###----Shap variable dependency plot for Eigen centrality----###
eign_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Eigen centrality",
                                 interactions = FALSE, color_var = NULL)

saveRDS(eign_centr.shap.sw,"eign_centr.shap.sw.rds")

eign_centr.shap.sw=readRDS("eign_centr.shap.sw.rds")
eign_centr.shap.sw.df=eign_centr.shap.sw$data
colnames(eign_centr.shap.sw.df)=c("shap","Eigen_centrality")

eign_centr.shap.sw.plot=
  ggplot(eign_centr.shap.sw.df, aes(x = Eigen_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Eigen centrality")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#eign_centr.shap.sw.plot=eign_centr.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
eign_centr.shap.sw.plot=eign_centr.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

eign_centr.shap.sw.plot=
  eign_centr.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                             sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

eign_centr.shap.sw.plot


Eigen_centrality.figure <- ggarrange(eign_centr.shap.er.plot, eign_centr.shap.sbm.plot, eign_centr.shap.sf.plot,
                                     eign_centr.shap.sp.plot,eign_centr.shap.sw.plot,
                                     nrow = 1)#,widths=1.5, heights=2)
Eigen_centrality.figure=Eigen_centrality.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Eigen_centrality.figure

# ggsave("m5.png", width=18, height=4)



###----Closeness_centrality shapely depedency plot----###
close_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Closeness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(close_centr.shap.er,"close_centr.shap.er.rds")

close_centr.shap.er=readRDS("close_centr.shap.er.rds")
close_centr.shap.er.df=close_centr.shap.er$data
colnames(close_centr.shap.er.df)=c("shap","Closeness_centrality")

close_centr.shap.er.plot=
  ggplot(close_centr.shap.er.df, aes(x = Closeness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

close_centr.shap.er.plot=close_centr.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
close_centr.shap.er.plot=close_centr.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

close_centr.shap.er.plot


###----Shap variable dependency plot for Closeness centrality----###
close_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Closeness centrality",
                                   interactions = FALSE, color_var = NULL)

saveRDS(close_centr.shap.sbm,"close_centr.shap.sbm.rds")

close_centr.shap.sbm=readRDS("close_centr.shap.sbm.rds")
close_centr.shap.sbm.df=close_centr.shap.sbm$data
colnames(close_centr.shap.sbm.df)=c("shap","Closeness_centrality")

close_centr.shap.sbm.plot=
  ggplot(close_centr.shap.sbm.df, aes(x = Closeness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

close_centr.shap.sbm.plot=close_centr.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
close_centr.shap.sbm.plot=close_centr.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



close_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Closeness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(close_centr.shap.sf,"close_centr.shap.sf.rds")

close_centr.shap.sf=readRDS("close_centr.shap.sf.rds")
close_centr.shap.sf.df=close_centr.shap.sf$data
colnames(close_centr.shap.sf.df)=c("shap","Closeness_centrality")

close_centr.shap.sf.plot=
  ggplot(close_centr.shap.sf.df, aes(x = Closeness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

close_centr.shap.sf.plot=close_centr.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
close_centr.shap.sf.plot=close_centr.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
close_centr.shap.sf.plot


###----Shap variable dependency plot for Closeness centrality----###
close_centr.shap.sp=sv_dependence(final_shap$Spatial, "Closeness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(close_centr.shap.sp,"close_centr.shap.sp.rds")

close_centr.shap.sp=readRDS("close_centr.shap.sp.rds")
close_centr.shap.sp.df=close_centr.shap.sp$data
colnames(close_centr.shap.sp.df)=c("shap","Closeness_centrality")

close_centr.shap.sp.plot=
  ggplot(close_centr.shap.sp.df, aes(x = Closeness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

close_centr.shap.sp.plot=close_centr.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
close_centr.shap.sp.plot=close_centr.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
close_centr.shap.sp.plot


###----Shap variable dependency plot for Closeness centrality----###
close_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Closeness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(close_centr.shap.sw,"close_centr.shap.sw.rds")

close_centr.shap.sw=readRDS("close_centr.shap.sw.rds")
close_centr.shap.sw.df=close_centr.shap.sw$data
colnames(close_centr.shap.sw.df)=c("shap","Closeness_centrality")

close_centr.shap.sw.plot=
  ggplot(close_centr.shap.sw.df, aes(x = Closeness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Closeness centrality")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

close_centr.shap.sw.plot=close_centr.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
close_centr.shap.sw.plot=close_centr.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

close_centr.shap.sw.plot=
  close_centr.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                              sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

close_centr.shap.sw.plot


Closeness_centrality.figure <- ggarrange(close_centr.shap.er.plot, close_centr.shap.sbm.plot, close_centr.shap.sf.plot,
                                         close_centr.shap.sp.plot,close_centr.shap.sw.plot,
                                         nrow = 1)#,widths=1.5, heights=2)
Closeness_centrality.figure=Closeness_centrality.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Closeness_centrality.figure

# ggsave("m6.png", width=18, height=4)


###----Betweenness_centrality shapely depedency plot----###

betwn_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Betweenness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(betwn_centr.shap.er,"betwn_centr.shap.er.rds")

betwn_centr.shap.er=readRDS("betwn_centr.shap.er.rds")
betwn_centr.shap.er.df=betwn_centr.shap.er$data
colnames(betwn_centr.shap.er.df)=c("shap","Betweenness_centrality")

betwn_centr.shap.er.plot=
  ggplot(betwn_centr.shap.er.df, aes(x = Betweenness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

betwn_centr.shap.er.plot=betwn_centr.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
betwn_centr.shap.er.plot=betwn_centr.shap.er.plot+
  scale_x_continuous(breaks = c(0,0.5))

betwn_centr.shap.er.plot


###----Shap variable dependency plot for Betweenness centrality----###
betwn_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Betweenness centrality",
                                   interactions = FALSE, color_var = NULL)

saveRDS(betwn_centr.shap.sbm,"betwn_centr.shap.sbm.rds")

betwn_centr.shap.sbm=readRDS("betwn_centr.shap.sbm.rds")
betwn_centr.shap.sbm.df=betwn_centr.shap.sbm$data
colnames(betwn_centr.shap.sbm.df)=c("shap","Betweenness_centrality")

betwn_centr.shap.sbm.plot=
  ggplot(betwn_centr.shap.sbm.df, aes(x = Betweenness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

betwn_centr.shap.sbm.plot=betwn_centr.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
betwn_centr.shap.sbm.plot=betwn_centr.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,0.5))



betwn_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Betweenness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(betwn_centr.shap.sf,"betwn_centr.shap.sf.rds")

betwn_centr.shap.sf=readRDS("betwn_centr.shap.sf.rds")
betwn_centr.shap.sf.df=betwn_centr.shap.sf$data
colnames(betwn_centr.shap.sf.df)=c("shap","Betweenness_centrality")

betwn_centr.shap.sf.plot=
  ggplot(betwn_centr.shap.sf.df, aes(x = Betweenness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

betwn_centr.shap.sf.plot=betwn_centr.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
betwn_centr.shap.sf.plot=betwn_centr.shap.sf.plot+
  scale_x_continuous(breaks = c(0,0.5))
betwn_centr.shap.sf.plot


###----Shap variable dependency plot for Betweenness centrality----###
betwn_centr.shap.sp=sv_dependence(final_shap$Spatial, "Betweenness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(betwn_centr.shap.sp,"betwn_centr.shap.sp.rds")

betwn_centr.shap.sp=readRDS("betwn_centr.shap.sp.rds")
betwn_centr.shap.sp.df=betwn_centr.shap.sp$data
colnames(betwn_centr.shap.sp.df)=c("shap","Betweenness_centrality")

betwn_centr.shap.sp.plot=
  ggplot(betwn_centr.shap.sp.df, aes(x = Betweenness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

betwn_centr.shap.sp.plot=betwn_centr.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
betwn_centr.shap.sp.plot=betwn_centr.shap.sp.plot+
  scale_x_continuous(breaks = c(0,0.5))
betwn_centr.shap.sp.plot


###----Shap variable dependency plot for Betweenness centrality----###
betwn_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Betweenness centrality",
                                  interactions = FALSE, color_var = NULL)

saveRDS(betwn_centr.shap.sw,"betwn_centr.shap.sw.rds")

betwn_centr.shap.sw=readRDS("betwn_centr.shap.sw.rds")
betwn_centr.shap.sw.df=betwn_centr.shap.sw$data
colnames(betwn_centr.shap.sw.df)=c("shap","Betweenness_centrality")

betwn_centr.shap.sw.plot=
  ggplot(betwn_centr.shap.sw.df, aes(x = Betweenness_centrality, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Betweenness centrality")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


#betwn_centr.shap.sw.plot=betwn_centr.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
betwn_centr.shap.sw.plot=betwn_centr.shap.sw.plot+
  scale_x_continuous(breaks = c(0,0.5))

betwn_centr.shap.sw.plot=
  betwn_centr.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                              sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))


betwn_centr.shap.sw.plot


Betweenness_centrality.figure <- ggarrange(betwn_centr.shap.er.plot, betwn_centr.shap.sbm.plot, betwn_centr.shap.sf.plot,
                                           betwn_centr.shap.sp.plot,betwn_centr.shap.sw.plot,
                                           nrow = 1)#,widths=1.5, heights=2)
Betweenness_centrality.figure=Betweenness_centrality.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Betweenness_centrality.figure

#ggsave("m7.png", width=18, height=4)

#ggsave("m7.png", width=18, height=4)




# row6 <- plot_grid(eign_centr.shap.er.plot, eign_centr.shap.sbm.plot, eign_centr.shap.sf.plot,
#                   eign_centr.shap.sp.plot, eign_centr.shap.sw.plot, ncol = 3)
# row7=plot_grid(close_centr.shap.er.plot,close_centr.shap.sbm.plot, 
#                close_centr.shap.sf.plot,
#                close_centr.shap.sp.plot,close_centr.shap.sw.plot,ncol=3)
# 
# row8=plot_grid(betwn_centr.shap.er.plot,betwn_centr.shap.sbm.plot, 
#                betwn_centr.shap.sf.plot,
#                betwn_centr.shap.sp.plot,betwn_centr.shap.sw.plot,ncol=3)
# 
# combinedplot3=ggarrange(row6,row7,row8, nrow = 3)
# ggsave("combinedplot3.png",width=10, height=13)



###----Mean_path_length shapely depedency plot----###

mean_path.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Mean path length",
                                interactions = FALSE, color_var = NULL)

saveRDS(mean_path.shap.er,"mean_path.shap.er.rds")

mean_path.shap.er=readRDS("mean_path.shap.er.rds")
mean_path.shap.er.df=mean_path.shap.er$data
colnames(mean_path.shap.er.df)=c("shap","Mean_path_length")

mean_path.shap.er.plot=
  ggplot(mean_path.shap.er.df, aes(x = Mean_path_length, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_path.shap.er.plot=mean_path.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)

mean_path.shap.er.plot


###----Shap variable dependency plot for Mean path length----###
mean_path.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Mean path length",
                                 interactions = FALSE, color_var = NULL)

saveRDS(mean_path.shap.sbm,"mean_path.shap.sbm.rds")

mean_path.shap.sbm=readRDS("mean_path.shap.sbm.rds")
mean_path.shap.sbm.df=mean_path.shap.sbm$data
colnames(mean_path.shap.sbm.df)=c("shap","Mean_path_length")

mean_path.shap.sbm.plot=
  ggplot(mean_path.shap.sbm.df, aes(x = Mean_path_length, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_path.shap.sbm.plot=mean_path.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)



mean_path.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Mean path length",
                                interactions = FALSE, color_var = NULL)

saveRDS(mean_path.shap.sf,"mean_path.shap.sf.rds")

mean_path.shap.sf=readRDS("mean_path.shap.sf.rds")
mean_path.shap.sf.df=mean_path.shap.sf$data
colnames(mean_path.shap.sf.df)=c("shap","Mean_path_length")

mean_path.shap.sf.plot=
  ggplot(mean_path.shap.sf.df, aes(x = Mean_path_length, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_path.shap.sf.plot=mean_path.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)

mean_path.shap.sf.plot


###----Shap variable dependency plot for Mean path length----###
mean_path.shap.sp=sv_dependence(final_shap$Spatial, "Mean path length",
                                interactions = FALSE, color_var = NULL)

saveRDS(mean_path.shap.sp,"mean_path.shap.sp.rds")

mean_path.shap.sp=readRDS("mean_path.shap.sp.rds")
mean_path.shap.sp.df=mean_path.shap.sp$data
colnames(mean_path.shap.sp.df)=c("shap","Mean_path_length")

mean_path.shap.sp.plot=
  ggplot(mean_path.shap.sp.df, aes(x = Mean_path_length, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_path.shap.sp.plot=mean_path.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)

mean_path.shap.sp.plot


###----Shap variable dependency plot for Mean path length----###
mean_path.shap.sw=sv_dependence(final_shap$`Small-World`, "Mean path length",
                                interactions = FALSE, color_var = NULL)

saveRDS(mean_path.shap.sw,"mean_path.shap.sw.rds")


mean_path.shap.sw=readRDS("mean_path.shap.sw.rds")
mean_path.shap.sw.df=mean_path.shap.sw$data
colnames(mean_path.shap.sw.df)=c("shap","Mean_path_length")

mean_path.shap.sw.plot=
  ggplot(mean_path.shap.sw.df, aes(x = Mean_path_length, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Mean path length")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#mean_path.shap.sw.plot=mean_path.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)

mean_path.shap.sw.plot=
  mean_path.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                            sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

mean_path.shap.sw.plot


Mean_path_length.figure <- ggarrange(mean_path.shap.er.plot, mean_path.shap.sbm.plot, mean_path.shap.sf.plot,
                                     mean_path.shap.sp.plot,mean_path.shap.sw.plot,
                                     nrow = 1)#,widths=1.5, heights=2)
Mean_path_length.figure=Mean_path_length.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Mean_path_length.figure




###----Mean_eccentricity shapely depedency plot----###

mean_eccent.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Mean eccentricity",
                                  interactions = FALSE, color_var = NULL)

saveRDS(mean_eccent.shap.er,"mean_eccent.shap.er.rds")

mean_eccent.shap.er=readRDS("mean_eccent.shap.er.rds")
mean_eccent.shap.er.df=mean_eccent.shap.er$data
colnames(mean_eccent.shap.er.df)=c("shap","Mean_eccentricity")

mean_eccent.shap.er.plot=
  ggplot(mean_eccent.shap.er.df, aes(x = Mean_eccentricity, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_eccent.shap.er.plot=mean_eccent.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
mean_eccent.shap.er.plot=mean_eccent.shap.er.plot+
  scale_x_continuous(breaks = c(0,100))

mean_eccent.shap.er.plot


###----Shap variable dependency plot for Mean eccentricity----###
mean_eccent.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Mean eccentricity",
                                   interactions = FALSE, color_var = NULL)

saveRDS(mean_eccent.shap.sbm,"mean_eccent.shap.sbm.rds")

mean_eccent.shap.sbm=readRDS("mean_eccent.shap.sbm.rds")
mean_eccent.shap.sbm.df=mean_eccent.shap.sbm$data
colnames(mean_eccent.shap.sbm.df)=c("shap","Mean_eccentricity")

mean_eccent.shap.sbm.plot=
  ggplot(mean_eccent.shap.sbm.df, aes(x = Mean_eccentricity, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_eccent.shap.sbm.plot=mean_eccent.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
mean_eccent.shap.sbm.plot=mean_eccent.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,100))



mean_eccent.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Mean eccentricity",
                                  interactions = FALSE, color_var = NULL)

saveRDS(mean_eccent.shap.sf,"mean_eccent.shap.sf.rds")

mean_eccent.shap.sf=readRDS("mean_eccent.shap.sf.rds")
mean_eccent.shap.sf.df=mean_eccent.shap.sf$data
colnames(mean_eccent.shap.sf.df)=c("shap","Mean_eccentricity")

mean_eccent.shap.sf.plot=
  ggplot(mean_eccent.shap.sf.df, aes(x = Mean_eccentricity, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_eccent.shap.sf.plot=mean_eccent.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
mean_eccent.shap.sf.plot=mean_eccent.shap.sf.plot+
  scale_x_continuous(breaks = c(0,100))
mean_eccent.shap.sf.plot


###----Shap variable dependency plot for Mean eccentricity----###
mean_eccent.shap.sp=sv_dependence(final_shap$Spatial, "Mean eccentricity",
                                  interactions = FALSE, color_var = NULL)

saveRDS(mean_eccent.shap.sp,"mean_eccent.shap.sp.rds")

mean_eccent.shap.sp=readRDS("mean_eccent.shap.sp.rds")
mean_eccent.shap.sp.df=mean_eccent.shap.sp$data
colnames(mean_eccent.shap.sp.df)=c("shap","Mean_eccentricity")

mean_eccent.shap.sp.plot=
  ggplot(mean_eccent.shap.sp.df, aes(x = Mean_eccentricity, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_eccent.shap.sp.plot=mean_eccent.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
mean_eccent.shap.sp.plot=mean_eccent.shap.sp.plot+
  scale_x_continuous(breaks = c(0,100))
mean_eccent.shap.sp.plot


###----Shap variable dependency plot for Mean eccentricity----###
mean_eccent.shap.sw=sv_dependence(final_shap$`Small-World`, "Mean eccentricity",
                                  interactions = FALSE, color_var = NULL)

saveRDS(mean_eccent.shap.sw,"mean_eccent.shap.sw.rds")


mean_eccent.shap.sw=readRDS("mean_eccent.shap.sw.rds")
mean_eccent.shap.sw.df=mean_eccent.shap.sw$data
colnames(mean_eccent.shap.sw.df)=c("shap","Mean_eccentricity")

mean_eccent.shap.sw.plot=
  ggplot(mean_eccent.shap.sw.df, aes(x = Mean_eccentricity, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Mean eccentricity")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#mean_eccent.shap.sw.plot=mean_eccent.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
mean_eccent.shap.sw.plot=mean_eccent.shap.sw.plot+
  scale_x_continuous(breaks = c(0,100))

mean_eccent.shap.sw.plot=
  mean_eccent.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                              sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

mean_eccent.shap.sw.plot


Mean_eccentricity.figure <- ggarrange(mean_eccent.shap.er.plot, mean_eccent.shap.sbm.plot, mean_eccent.shap.sf.plot,
                                      mean_eccent.shap.sp.plot,mean_eccent.shap.sw.plot,
                                      nrow = 1)#,widths=1.5, heights=2)
Mean_eccentricity.figure=Mean_eccentricity.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Mean_eccentricity.figure


###----Graph_energy shapely depedency plot----###

graph_energy.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Graph energy",
                                   interactions = FALSE, color_var = NULL)

saveRDS(graph_energy.shap.er,"graph_energy.shap.er.rds")

graph_energy.shap.er=readRDS("graph_energy.shap.er.rds")
graph_energy.shap.er.df=graph_energy.shap.er$data
colnames(graph_energy.shap.er.df)=c("shap","Graph_energy")

graph_energy.shap.er.plot=
  ggplot(graph_energy.shap.er.df, aes(x = Graph_energy, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

graph_energy.shap.er.plot=graph_energy.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
graph_energy.shap.er.plot=graph_energy.shap.er.plot+
  scale_x_continuous(breaks = c(0.0,10000))

graph_energy.shap.er.plot


###----Shap variable dependency plot for Graph energy----###
graph_energy.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Graph energy",
                                    interactions = FALSE, color_var = NULL)

saveRDS(graph_energy.shap.sbm,"graph_energy.shap.sbm.rds")

graph_energy.shap.sbm=readRDS("graph_energy.shap.sbm.rds")
graph_energy.shap.sbm.df=graph_energy.shap.sbm$data
colnames(graph_energy.shap.sbm.df)=c("shap","Graph_energy")

graph_energy.shap.sbm.plot=
  ggplot(graph_energy.shap.sbm.df, aes(x = Graph_energy, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

graph_energy.shap.sbm.plot=graph_energy.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
graph_energy.shap.sbm.plot=graph_energy.shap.sbm.plot+
  scale_x_continuous(breaks = c(0.0,10000))



graph_energy.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Graph energy",
                                   interactions = FALSE, color_var = NULL)

saveRDS(graph_energy.shap.sf,"graph_energy.shap.sf.rds")

graph_energy.shap.sf=readRDS("graph_energy.shap.sf.rds")
graph_energy.shap.sf.df=graph_energy.shap.sf$data
colnames(graph_energy.shap.sf.df)=c("shap","Graph_energy")

graph_energy.shap.sf.plot=
  ggplot(graph_energy.shap.sf.df, aes(x = Graph_energy, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

graph_energy.shap.sf.plot=graph_energy.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
graph_energy.shap.sf.plot=graph_energy.shap.sf.plot+
  scale_x_continuous(breaks = c(0.0,10000))
graph_energy.shap.sf.plot


###----Shap variable dependency plot for Graph energy----###
graph_energy.shap.sp=sv_dependence(final_shap$Spatial, "Graph energy",
                                   interactions = FALSE, color_var = NULL)

saveRDS(graph_energy.shap.sp,"graph_energy.shap.sp.rds")

graph_energy.shap.sp=readRDS("graph_energy.shap.sp.rds")
graph_energy.shap.sp.df=graph_energy.shap.sp$data
colnames(graph_energy.shap.sp.df)=c("shap","Graph_energy")

graph_energy.shap.sp.plot=
  ggplot(graph_energy.shap.sp.df, aes(x = Graph_energy, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

graph_energy.shap.sp.plot=graph_energy.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
graph_energy.shap.sp.plot=graph_energy.shap.sp.plot+
  scale_x_continuous(breaks = c(0.0,10000))
graph_energy.shap.sp.plot


###----Shap variable dependency plot for Graph energy----###
graph_energy.shap.sw=sv_dependence(final_shap$`Small-World`, "Graph energy",
                                   interactions = FALSE, color_var = NULL)

saveRDS(graph_energy.shap.sw,"graph_energy.shap.sw.rds")


graph_energy.shap.sw=readRDS("graph_energy.shap.sw.rds")
graph_energy.shap.sw.df=graph_energy.shap.sw$data
colnames(graph_energy.shap.sw.df)=c("shap","Graph_energy")

graph_energy.shap.sw.plot=
  ggplot(graph_energy.shap.sw.df, aes(x = Graph_energy, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle("Small-World")+
  xlab(" ")+
  ylab("Graph energy")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#graph_energy.shap.sw.plot=graph_energy.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
graph_energy.shap.sw.plot=graph_energy.shap.sw.plot+
  scale_x_continuous(breaks = c(0.0,10000))

graph_energy.shap.sw.plot=
  graph_energy.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                               sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

graph_energy.shap.sw.plot


Graph_energy.figure <- ggarrange(graph_energy.shap.er.plot, graph_energy.shap.sbm.plot, graph_energy.shap.sf.plot,
                                 graph_energy.shap.sp.plot,graph_energy.shap.sw.plot,
                                 nrow = 1)#,widths=1.5, heights=2)
Graph_energy.figure=Graph_energy.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Graph_energy.figure




###----Mean_degree shapely depedency plot----###

Mean_degree.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Mean degree",
                                  interactions = FALSE, color_var = NULL)

saveRDS(Mean_degree.shap.er,"Mean_degree.shap.er.rds")

Mean_degree.shap.er=readRDS("Mean_degree.shap.er.rds")
Mean_degree.shap.er.df=Mean_degree.shap.er$data
colnames(Mean_degree.shap.er.df)=c("shap","Mean_degree")

Mean_degree.shap.er.plot=
  ggplot(Mean_degree.shap.er.df, aes(x = Mean_degree, y = shap)) +
  geom_point(alpha = 0.5,color="violetred3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size = 24),           # Remove x-axis tick labels
        #  axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 24),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

Mean_degree.shap.er.plot=Mean_degree.shap.er.plot+plot_layout(widths = 1.2,heights = 1.2)
Mean_degree.shap.er.plot=Mean_degree.shap.er.plot+
  scale_x_continuous(breaks = c(0,500))

Mean_degree.shap.er.plot


###----Shap variable dependency plot for Mean degree----###
Mean_degree.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Mean degree",
                                   interactions = FALSE, color_var = NULL)

saveRDS(Mean_degree.shap.sbm,"Mean_degree.shap.sbm.rds")

Mean_degree.shap.sbm=readRDS("Mean_degree.shap.sbm.rds")
Mean_degree.shap.sbm.df=Mean_degree.shap.sbm$data
colnames(Mean_degree.shap.sbm.df)=c("shap","Mean_degree")

Mean_degree.shap.sbm.plot=
  ggplot(Mean_degree.shap.sbm.df, aes(x = Mean_degree, y = shap)) +
  geom_point(alpha = 0.5,color="turquoise3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

Mean_degree.shap.sbm.plot=Mean_degree.shap.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
Mean_degree.shap.sbm.plot=Mean_degree.shap.sbm.plot+
  scale_x_continuous(breaks = c(0,500))



Mean_degree.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Mean degree",
                                  interactions = FALSE, color_var = NULL)

saveRDS(Mean_degree.shap.sf,"Mean_degree.shap.sf.rds")

Mean_degree.shap.sf=readRDS("Mean_degree.shap.sf.rds")
Mean_degree.shap.sf.df=Mean_degree.shap.sf$data
colnames(Mean_degree.shap.sf.df)=c("shap","Mean_degree")

Mean_degree.shap.sf.plot=
  ggplot(Mean_degree.shap.sf.df, aes(x = Mean_degree, y = shap)) +
  geom_point(alpha = 0.5,color="tomato3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

Mean_degree.shap.sf.plot=Mean_degree.shap.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
Mean_degree.shap.sf.plot=Mean_degree.shap.sf.plot+
  scale_x_continuous(breaks = c(0,500))
Mean_degree.shap.sf.plot


###----Shap variable dependency plot for Mean degree----###
Mean_degree.shap.sp=sv_dependence(final_shap$Spatial, "Mean degree",
                                  interactions = FALSE, color_var = NULL)

saveRDS(Mean_degree.shap.sp,"Mean_degree.shap.sp.rds")

Mean_degree.shap.sp=readRDS("Mean_degree.shap.sp.rds")
Mean_degree.shap.sp.df=Mean_degree.shap.sp$data
colnames(Mean_degree.shap.sp.df)=c("shap","Mean_degree")

Mean_degree.shap.sp.plot=
  ggplot(Mean_degree.shap.sp.df, aes(x = Mean_degree, y = shap)) +
  geom_point(alpha = 0.5,color="tan3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        #        plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        #axis.text.x = element_text(size = 24),
        axis.text.x = element_text(size=24),           # Remove x-axis tick labels
        axis.text.y = element_text(size = 24),
        #    axis.ticks.x = element_blank(),          # Remove x-axis ticks
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

Mean_degree.shap.sp.plot=Mean_degree.shap.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
Mean_degree.shap.sp.plot=Mean_degree.shap.sp.plot+
  scale_x_continuous(breaks = c(0,500))
Mean_degree.shap.sp.plot


###----Shap variable dependency plot for Mean degree----###
Mean_degree.shap.sw=sv_dependence(final_shap$`Small-World`, "Mean degree",
                                  interactions = FALSE, color_var = NULL)

saveRDS(Mean_degree.shap.sw,"Mean_degree.shap.sw.rds")


Mean_degree.shap.sw=readRDS("Mean_degree.shap.sw.rds")
Mean_degree.shap.sw.df=Mean_degree.shap.sw$data
colnames(Mean_degree.shap.sw.df)=c("shap","Mean_degree")

Mean_degree.shap.sw.plot=
  ggplot(Mean_degree.shap.sw.df, aes(x = Mean_degree, y = shap)) +
  geom_point(alpha = 0.5,color="steelblue3") +
  ylim(-7.5,7.5)+
  geom_smooth(method = "auto", se = FALSE,stat = "smooth",
              position = "identity",color="black")+
  ggtitle(" ")+
  xlab(" ")+
  ylab("Mean degree")+
  theme_bw()+
  theme(plot.title = element_text(size = 24),
        # plot.title.position = "plot",
        text = element_text(size = 24,family="serif"),
        axis.title.y.right = element_text(vjust = +3.4),
        axis.line.y.right = element_blank(),
        axis.text.x = element_text(size=24),  
        axis.text.y = element_text(size = 24),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())



#Mean_degree.shap.sw.plot=Mean_degree.shap.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
Mean_degree.shap.sw.plot=Mean_degree.shap.sw.plot+
  scale_x_continuous(breaks = c(0,500))

Mean_degree.shap.sw.plot=
  Mean_degree.shap.sw.plot+scale_y_continuous(limits = c(-7.5, 7.5),
                                              sec.axis = dup_axis(labels = NULL))+
  theme(axis.ticks.y.right = element_blank(),
        axis.title.y.left = element_blank(),
        axis.text.y.right = element_text(size = 24))

Mean_degree.shap.sw.plot


Mean_degree.figure <- ggarrange(Mean_degree.shap.er.plot, Mean_degree.shap.sbm.plot, Mean_degree.shap.sf.plot,
                                Mean_degree.shap.sp.plot,Mean_degree.shap.sw.plot,
                                nrow = 1)#,widths=1.5, heights=2)
Mean_degree.figure=Mean_degree.figure+
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) #+  # Adjust plot margins

Mean_degree.figure

# row6 <- plot_grid(eign_centr.shap.er.plot, eign_centr.shap.sbm.plot, eign_centr.shap.sf.plot,
#                   eign_centr.shap.sp.plot, eign_centr.shap.sw.plot, ncol = 3)
# row7=plot_grid(close_centr.shap.er.plot,close_centr.shap.sbm.plot, 
#                close_centr.shap.sf.plot,
#                close_centr.shap.sp.plot,close_centr.shap.sw.plot,ncol=3)
# 
# row8=plot_grid(betwn_centr.shap.er.plot,betwn_centr.shap.sbm.plot, 
#                betwn_centr.shap.sf.plot,
#                betwn_centr.shap.sp.plot,betwn_centr.shap.sw.plot,ncol=3)
# 
# combinedplot3=ggarrange(row6,row7,row8, nrow = 3)
# ggsave("combinedplot3.png",width=10, height=13)





# row9 <- plot_grid(mean_path.shap.er.plot, mean_path.shap.sbm.plot, mean_path.shap.sf.plot,
#                   mean_path.shap.sp.plot, mean_path.shap.sw.plot, ncol = 3)
# row10=plot_grid(mean_eccent.shap.er.plot,mean_eccent.shap.sbm.plot, 
#                 mean_eccent.shap.sf.plot,
#                 mean_eccent.shap.sp.plot,mean_eccent.shap.sw.plot,ncol=3)
# 
# row11=plot_grid(graph_energy.shap.er.plot,graph_energy.shap.sbm.plot, 
#                 graph_energy.shap.sf.plot,
#                 graph_energy.shap.sp.plot,graph_energy.shap.sw.plot,ncol=3)
# 
# combinedplot4=ggarrange(row9,row10,row11, nrow = 3)
# ggsave("combinedplot4.png",width=10, height=13)

p11=plot_grid(Modularity.figure,
              Transitivity.figure,
              Normalized_Fiedler.figure,
              Degree_centrality.figure,
              Eigen_centrality.figure,
              Degree_assortativity.figure,
              nrow=6)

ggsave("dependency_plot1.png", width=18, height=20)
#ggsave("dependency_plot1.pdf", width=18, height=26)
#ggsave("dependency_plot1.svg", width=18, height=26)

# p12=plot_grid(Betweenness_centrality.figure,
#               Degree_centrality.figure,
#               Eigen_centrality.figure,
#               nrow=3)


p2=plot_grid(#Degree_assortativity.figure,
  #Spectral_radius.figure,
  Graph_energy.figure,
  Spectral_radius.figure,
  Mean_eccentricity.figure,
  Mean_path_length.figure,
  Mean_degree.figure,
  nrow=5)

ggsave("dependency_plot2.png", width=18, height=16)
#ggsave("dependency_plot2.svg", width=18, height=26)



####--------------------Force and Waterfall plot----------------------------
er.waterfall=
  
sv_waterfall(final_shap$`Erdös-Rényi`, row_id = 10500,
                          size = 18,fill_colors = c("red","blue"),
                          show_connection = T,
                          show_annotation = F,
                          annotation_size = 6,
                          max_display = 7)

er.waterfall=er.waterfall+
  ggtitle("Erdös-Rényi")+
  #xlab(" Graph_energy ")+
  # ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

er.waterfall=er.waterfall+plot_layout(widths = 1.2,heights = 1.2)
ggsave("er.waterfall.png", width = 12, height=10)
ggsave("er.waterfall.pdf", width = 12, height=10)

sbm.waterfall=sv_waterfall(final_shap$`Stochastic-Block-Model`, row_id = 10500,
                           size = 18,fill_colors = c("red","blue"),
                           show_connection = T,
                           show_annotation = F,
                           annotation_size = 6,
                           max_display = 7)

sbm.waterfall=sbm.waterfall+
  ggtitle("Stochastic-Block-Model")+
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

sbm.waterfall=sbm.waterfall+plot_layout(widths = 1.2,heights = 1.2)
sbm.waterfall


ggsave("sbm.waterfall.png", width = 12, height=10)
ggsave("sbm.waterfall.pdf", width = 12, height=10)

sf.waterfall=sv_waterfall(final_shap$`Scale-Free`, row_id = 10500,
                          size = 18,fill_colors = c("red","blue"),
                          show_connection = T,
                          show_annotation = F,
                          annotation_size = 6,
                          max_display = 7)
sf.waterfall=sf.waterfall+
  ggtitle("Scale-Free")+
  xlim(NA,2.4)+
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

sf.waterfall=sf.waterfall+plot_layout(widths = 1.2,heights = 1.2)
ggsave("sf.waterfall.png", width = 12, height=10)
ggsave("sf.waterfall.pdf", width = 12, height=10)

sp.waterfall=sv_waterfall(final_shap$Spatial, row_id = 10500,
                          size = 18,fill_colors = c("red","blue"),
                          show_connection = T,
                          show_annotation = F,
                          annotation_size = 6,
                          max_display = 7)

sp.waterfall=sp.waterfall+
  ggtitle("Spatial")+
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

sp.waterfall=sp.waterfall+plot_layout(widths = 1.2,heights = 1.2)
ggsave("sp.waterfall.png", width = 12, height=10)
ggsave("sp.waterfall.pdf", width = 12, height=10)

sw.waterfall=sv_waterfall(final_shap$`Small-World`, row_id = 10500,
                          size = 18,fill_colors = c("red","blue"),
                          show_connection = T,
                          show_annotation = F,
                          annotation_size = 6,
                          max_display = 7)

sw.waterfall=sw.waterfall+
  ggtitle("Small-World")+
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

sw.waterfall=sw.waterfall+plot_layout(widths = 1.2,heights = 1.2)
ggsave("sw.waterfall.png", width = 12, height=10)
ggsave("sw.waterfall.pdf", width = 12, height=10)

waterfall.plots1=plot_grid(sbm.waterfall,er.waterfall,
                           sw.waterfall,sf.waterfall,
                           sp.waterfall,
                           nrow=3)


ggsave("waterfall.plots1.png", width = 20, height=13)
ggsave("waterfall.plots1.pdf", width = 20, height=13)

# waterfall.plots2=plot_grid(sw.waterfall,sf.waterfall,
#                            sp.waterfall,
#                            nrow=2)
ggsave("waterfall.plots2.png", width = 24.5, height=20)
ggsave("waterfall.plots2.pdf", width = 24.5, height=20)


########################################################################################
# Shap  interaction plots
########################################################################################
final_resample_data <- df.final.new#[sample(nrow(df.final), 1000), ]
final_resample_data_prep <- bake(
  df_prep_new, # prep(df.final.rec), 
  has_role("predictor"),
  new_data = final_resample_data, 
  composition = "matrix"
)
head(final_resample_data_prep)

shap.interactn<- shapviz(extract_fit_engine(final.fitted.model), 
                 X_pred = final_resample_data_prep, 
                 X = final_resample_data,
                 interactions = T)


saveRDS(shap.interactn,"shap.interaction.rds")


shap_interactn=readRDS("shap.interaction.rds")


###----getting shap values----
#shap_interactn_vals=get_shap_values(shap_interactn)
#shap_interactn_vals

names(shap_interactn)[names(shap_interactn) == "Class_1"] <- "Erdös-Rényi"
names(shap_interactn)[names(shap_interactn) == "Class_2"] <- "Stochastic-Block-Model"
names(shap_interactn)[names(shap_interactn) == "Class_3"] <- "Scale-Free"
names(shap_interactn)[names(shap_interactn) == "Class_4"] <- "Spatial"
names(shap_interactn)[names(shap_interactn) == "Class_5"] <- "Small-World"

colnames(shap_interactn$`Erdös-Rényi`$X)=new_column_names
colnames(shap_interactn$`Erdös-Rényi`$S)=new_column_names
colnames(shap_interactn$`Erdös-Rényi`$S_inter)=new_column_names
dimnames(shap_interactn$"Erdös-Rényi"$S_inter)[[3]]=
  c(dimnames(shap_interactn$"Erdös-Rényi"$S_inter)[[2]])

colnames(shap_interactn$`Stochastic-Block-Model`$X)=new_column_names
colnames(shap_interactn$`Stochastic-Block-Model`$S)=new_column_names
colnames(shap_interactn$`Stochastic-Block-Model`$S_inter)=new_column_names
dimnames(shap_interactn$"Stochastic-Block-Model"$S_inter)[[3]]=
  c(dimnames(shap_interactn$"Stochastic-Block-Model"$S_inter)[[2]])


colnames(shap_interactn$`Scale-Free`$X)=new_column_names
colnames(shap_interactn$`Scale-Free`$S)=new_column_names
colnames(shap_interactn$`Scale-Free`$S_inter)=new_column_names
dimnames(shap_interactn$"Scale-Free"$S_inter)[[3]]=
  c(dimnames(shap_interactn$"Scale-Free"$S_inter)[[2]])

colnames(shap_interactn$Spatial$X)=new_column_names
colnames(shap_interactn$Spatial$S)=new_column_names
colnames(shap_interactn$Spatial$S_inter)=new_column_names
dimnames(shap_interactn$"Spatial"$S_inter)[[3]]=
  c(dimnames(shap_interactn$"Spatial"$S_inter)[[2]])

colnames(shap_interactn$`Small-World`$X)=new_column_names
colnames(shap_interactn$`Small-World`$S)=new_column_names
colnames(shap_interactn$`Small-World`$S_inter)=new_column_names
dimnames(shap_interactn$"Small-World"$S_inter)[[3]]=
  c(dimnames(shap_interactn$"Small-World"$S_inter)[[2]])

saveRDS(shap_interactn,"final_interactn_shap.rds")

final_interactn_shap=readRDS("final_interactn_shap.rds")




###################################################################################
# 2D Shapely dependency/interaction plots
##################################################################################
final_interactn_shap=readRDS("final_interactn_shap.rds")

mod.shap.2Ddep=sv_dependence2D(final_interactn_shap$`Stochastic-Block-Model`, 
                               x ="Modularity", 
                               y = c("Mean path length",
                                     "Graph energy",
                                     "Transitivity",
                                     "Degree assortativity",
                                     "Spectral radius",
                                     "Normalized Fiedler"), alpha = 0.2) & 
  ggtitle("Stochastic-Block-Model")&
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) &
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

mod.shap.2Ddep
ggsave("2DSBM.png", width = 16, height = 8)
ggsave("2DSBM.pdf", width = 16, height = 8)


degree.assort.shap.2Ddep=sv_dependence2D(final_interactn_shap$`Stochastic-Block-Model`,
                               x ="Degree assortativity",
                               y = c("Closeness centrality",
                                     "Normalized Fiedler",
                                     "Transitivity",
                                     "Mean path length",
                                     "Graph energy"), alpha = 0.2) &
  ggtitle("Stochastic-Block-Model")&
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) &
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

degree.assort.shap.2Ddep
ggsave("2DSBM2.png", width = 16, height = 8)
ggsave("2DSBM2.pdf", width = 16, height = 8)



trans.shap.2Ddep=sv_dependence2D(final_interactn_shap$Spatial, 
                                 x ="Transitivity", 
                                 y = c("Mean eccentricity",
                                       "Eigen centrality",
                                      # "Degree centrality",
                                       "Degree assortativity",
                                       "Normalized Fiedler"), alpha = 0.2)& 
  ggtitle("Spatial")&
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) &
  scale_color_gradient(low = "blue", high = "red")

trans.shap.2Ddep
ggsave("2DSpatial.png", width = 16, height = 8)
ggsave("2DSpatial.pdf", width = 16, height = 8)

deg.centr.shap.2Ddep=sv_dependence2D(final_interactn_shap$`Scale-Free`, 
                                     x ="Degree centrality", 
                                     y = c("Modularity",
                                           "Eigen centrality",
                                           "Graph energy",
                                           "Degree assortativity",
                                           "Transitivity",
                                           "Spectral radius"), 
                                     alpha = 0.2)& 
  ggtitle("Scale-Free")&
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())  &
  scale_color_gradient(low = "blue", high = "red")

deg.centr.shap.2Ddep
ggsave("2DSF.png", width = 16, height = 8)
ggsave("2DSF.pdf", width = 16, height = 8)


spec.radius.shap.2Ddep=sv_dependence2D(final_interactn_shap$`Small-World`, 
                                       x ="Spectral radius", 
                                       y = c("Mean path length",
                                             "Mean degree",
                                             "Degree centrality",
                                             "Modularity",
                                             "Transitivity",
                                             "Degree assortativity"), 
                                       alpha = 0.2)& 
  ggtitle("Small-World")&
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())  &
  scale_color_gradient(low = "blue", high = "red")

spec.radius.shap.2Ddep

ggsave("2DSW.png", width = 16, height = 8)
ggsave("2DSW.pdf", width = 16, height = 8)

norm.fiedler.shap.2Ddep=sv_dependence2D(final_interactn_shap$`Erdös-Rényi`, 
                                        x ="Normalized Fiedler", 
                                        y = c("Modularity",
                                              "Transitivity",
                                              "Degree centrality",
                                              "Mean degree",
                                              "Spectral radius",
                                              "Mean path length"),
                                        
                                        alpha = 0.2)& 
  ggtitle("Erdös-Rényi")&
  theme_bw()+
  theme(plot.title = element_text(size = 18),
        text = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())  &
  scale_color_gradient(low = "blue", high = "red")

norm.fiedler.shap.2Ddep
ggsave("2DER.png", width = 16, height = 8)
ggsave("2DER.pdf", width = 16, height = 8)


TwoDplot1=plot_grid(mod.shap.2Ddep,
                    trans.shap.2Ddep,
                    nrow=2)

ggsave("TwoDplot1.png", width = 14, height = 12)

TwoDplot2=plot_grid(deg.centr.shap.2Ddep,
                    spec.radius.shap.2Ddep,
                    nrow=2)
ggsave("TwoDplot2.png", width = 14, height = 12)

TwoDplot3=plot_grid(norm.fiedler.shap.2Ddep)
ggsave("TwoDplot3.png", width = 14, height = 6)




############################################################################
#----Violin Plot---
############################################################################
df_data_new=readRDS("df_data_new.rds")

str(df_data_new)
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Normalization function for Z-score scaling
z_score_scaling <- function(x) {
  (x - mean(x)) / sd(x)
}

violin.df=df_data_new
#violin.df=df.final.new

names.violin=c('Number of nodes','Edges','Mean eccentricity',
               'Mean path length',
               'Graph energy','Modularity','Diameter','Betweenness centrality',
               'Transitivity','Spectral radius','Eigen centrality',
               'Degree centrality',
               'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
               'Closeness centrality','Degree assortativity',
               "Graph Name")#,'graph_name')


levels(violin.df$graph_name)=c("Erdös-Rényi","Stochastic-Block-Model",
                               "Scale-Free",
                               "Spatial","Small-World")
colnames(violin.df)=names.violin
#er.df=violin.df#%>%
#filter(graph_name=="Small-World")
violin.df[, 2:18] <- lapply(violin.df[, 2:18], min_max_scaling)


violin.df2=violin.df%>%
  select(c("Mean path length","Transitivity","Modularity",
           "Graph energy","Degree assortativity",
           "Normalized Fiedler",
           "Mean eccentricity","Degree centrality",
           "Eigen centrality","Spectral radius","Fiedler value","Mean degree",
           "Graph Name")
  )

# Melt the data into a long format
data_long <- gather(violin.df2, key = "variable", value = "value",
                    colnames(violin.df2[, 1:12]))


colnames(data_long)=c("Graph_Name","variable","value")
# Create the violin plot
violin.plot=ggplot(data_long, aes(x = variable, y = value)) +
  geom_violin(scale = "width",show.legend = T,
              draw_quantiles = c(.5),aes(fill = Graph_Name))+
  scale_fill_manual(values = c("violetred3", "turquoise3", "tomato3", "tan3", "steelblue3")) +  # Set custom fill colors
  labs(fill = "Graph Names")+  # Label for the legend
  ggtitle(" ")+
  ylab(" ") +
  xlab(" ") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size=18),
        strip.text = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.title = element_text(size = 18, family = "serif"),
        legend.text = element_text(size = 16.5, family = "serif"),
        text = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(1.5, "cm"))
# scale_color_gradient(low = "blue", high = "red")
# Set custom fill colors
# scale_fill_discrete(name = "Graph Name")

violin.plot

ggsave("violin1.png", width=17, height = 9)
ggsave("violin1.pdf", width=17, height = 9)
ggsave("violin1.svg", width=17, height = 9)





#############################################################################
# Hstatistics
#############################################################################



library(hstats)

df_data_new=readRDS("df_data_new.rds")
final.fitted.model = readRDS("final.fitted.model.rds")

df1=df_data_new%>%
  select(-graph_name)

#x = setdiff(colnames(df1), "graph_name")
#model = extract_workflow(final.fitted.model)    
#s = hstats(model, v = x, X = df1, type = "prob")
#plot(s, which = 1:3, normalize = FALSE)

set.seed(1486)
x1 = setdiff(colnames(df_data_new), "graph_name")
model1 = extract_workflow(final.fitted.model) 
H=hstats(model1, v = x1, n_max = 700, 
         X = df_data_new, 
         type = "prob",
         threeway_m = 3L,)

saveRDS(H,"H-stats.rds")
H=readRDS("H-stats.rds")
plot(H, which = 1:3, normalize = TRUE)

# pairwise_plot=h2_pairwise(H, normalize = FALSE, squared = FALSE, top_m = 5, 
#             sort = TRUE,
#             zero = TRUE,
#             plot = TRUE)
# 
# plot(pairwise_plot)
# 
# plot(H, which = 1:3, normalize = F, squared = F, facet_scales = "free_y", ncol = 1)


hstats_old_names <- c('num_of_nodes', 'edges', 'mean_eccentr', 'mean_path_length',
               'graph_energy', 'modularity', 'diameter', 'betw_centr',
               'transitivity', 'spectral_radius', 'eigen_centr', 'deg_centr',
               'mean_degree', 'min_cut', 'fiedler_value', 'normalized_fiedler_value',
               'closeness_centr', 'deg_assort_coef')

hstats_new_names <- c('Nodes', 'Edges', 'Mean eccentricity', 'Mean path length',
               'Graph energy', 'Modularity', 'Diameter', 'Betweenness centrality',
               'Transitivity', 'Spectral radius', 'Eigen centrality', 'Degree centrality',
               'Mean degree', 'Min cut', 'Fiedler value', 'Normalized Fiedler',
               'Closeness centrality', 'Degree assortativity')


network_colors <- c("Erdös-Rényi" = "violetred3", 
                    "Stochastic-Block-Model" = "turquoise3", 
                    "Scale-Free" = "tomato3", 
                    "Spatial" = "tan3", 
                    "Small-World" = "steelblue3")


rownames(H$h2_overall$num) <-hstats_new_names
colnames(H$h2_overall$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
names(H$h2_overall$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")


###----Overall Interactions
hplot=h2_overall(H, normalize = FALSE, squared = FALSE,
                 top_m = 8,zero = T)

saveRDS(hplot,"hstats_Overall.rds")

overall_hplot=plot(hplot)+
  theme_bw()+
  xlab("Values")+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 24,family="serif"),
        axis.text.x = element_text(size = 24,family="serif"),
        axis.text.y = element_text(size = 24,family="serif"),
        axis.title = element_text(size = 24,family="serif"),
        text = element_text(size = 24,family="serif"),
        legend.text = element_text(size = 24,family="serif"),
        legend.title = element_text(size = 24,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

overall_hplot=overall_hplot + scale_fill_manual(name= "Networks", values = network_colors)
overall_hplot

ggsave("hstat_overall2.svg", width = 20,height = 15)
ggsave("hstat_overall2.pdf", width = 20,height = 15)



# Function to replace names
replace_names <- function(old_names, new_names, row_names) {
  for (i in seq_along(old_names)) {
    for (j in seq_along(old_names)) {
      if (i != j) {
        old_pair <- paste0("\\b", old_names[i], ":", old_names[j], "\\b")
        new_pair <- paste0(new_names[i], ":", new_names[j])
        row_names <- str_replace_all(row_names, old_pair, new_pair)
        
        old_pair <- paste0("\\b", old_names[j], ":", old_names[i], "\\b")
        new_pair <- paste0(new_names[j], ":", new_names[i])
        row_names <- str_replace_all(row_names, old_pair, new_pair)
      }
    }
  }
  return(row_names)
}

# Apply the function to update row names
new_row_names <- replace_names(hstats_old_names, hstats_new_names,
                               rownames(H$h2_pairwise$num))

# Update the row names
rownames(H$h2_pairwise$num) <- new_row_names

# Update the row names
rownames(H$h2_pairwise$num) <- new_row_names

colnames(H$h2_pairwise$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
names(H$h2_pairwise$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")



hplot2 <- h2_pairwise(H, normalize = FALSE, squared = FALSE,
                      top_m = 8,zero = T)

saveRDS(hplot2,"hstats_pairwise.rds")

pairwise_hplot <- plot(hplot2)+
  xlab("Values")+
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 24,family="serif"),
        axis.text.x = element_text(size = 24,family="serif"),
        axis.text.y = element_text(size = 24,family="serif"),
        axis.title = element_text(size = 24,family="serif"),
        text = element_text(size = 24,family="serif"),
        legend.text = element_text(size = 24,family="serif"),
        legend.title = element_text(size = 24,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())


pairwise_hplot=pairwise_hplot + scale_fill_manual(name= "Networks", values = network_colors)
pairwise_hplot


ggsave("hstat_pairwise2.svg", width = 20,height = 15)
ggsave("hstat_pairwise2.pdf", width = 20,height = 15)


###----Threeway Interactions--------####
replace_names <- function(old_names, new_names2, row_names) {
  for (i in seq_along(old_names)) {
    for (j in seq_along(old_names)) {
      for (k in seq_along(old_names)) {
        if (i != j & j != k & i != k) {
          old_triplet <- paste(old_names[i], old_names[j], old_names[k], sep = ":")
          new_triplet <- paste(new_names2[i], new_names2[j], new_names2[k], sep = ":")
          row_names <- gsub(old_triplet, new_triplet, row_names)
        }
      }
    }
  }
  return(row_names)
}

# Replace names based on existing row names
new_row_names <- replace_names(hstats_old_names, hstats_new_names, rownames(H$h2_threeway$num))

# Update the row names
rownames(H$h2_threeway$num) <- new_row_names
colnames(H$h2_threeway$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
names(H$h2_threeway$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")


hplot3 <- h2_threeway(H, normalize = FALSE, squared = FALSE,
                      top_m = 8, zero = T)

saveRDS(hplot3,"hstats_threeway.rds")

threeway_hplot <- plot(hplot3) +
  xlab("Values")+
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 24,family="serif"),
        axis.text.x = element_text(size = 24,family="serif"),
        axis.text.y = element_text(size = 24,family="serif"),
        axis.title = element_text(size = 24,family="serif"),
        text = element_text(size = 24,family="serif"),
        legend.text = element_text(size = 24,family="serif"),
        legend.title = element_text(size = 24,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())


threeway_hplot=threeway_hplot + 
  scale_fill_manual(name= "Networks", values = network_colors)

threeway_hplot

ggsave("hstat_threeway.svg", width = 22,height = 15)

ggsave("hstat_threeway.pdf", width = 22,height = 15)







