
#+ Set working directory
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/rcappaw/Desktop/R/Workflow/igraphEpi-New")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Loading packages
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

suppressPackageStartupMessages({
  if(!require(Matrix)) {
    install.packages("Matrix")
    library(Matrix)
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
  if(!require(iml)) {
    install.packages("iml")
    library(iml)
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
  if(!require(recipeselectors)) {
    install.packages("recipeselectors")
    library(recipeselectors)
  }
  
  if(!require(gridExtra)) {
    install.packages("gridExtra")
    library(gridExtra)
  }
  
  if(!require(finetune)) {
    install.packages("finetune")
    library(finetune)
  }
})



###############################################################################################
# Data processing
###############################################################################################

#----read in data
Data=read.csv("model.data.csv",header = T,sep=",")

#----Shuffle----data
Data = Data[sample(1:nrow(Data)), ]##shuffle row indices and randomly re order 

#Data1=write.csv(Data,"model.data.csv")
df_final_meta_data
#----Change data frame to tibble (similar to data-frame)
df=as_tibble(Data)
#----count number for each target category
df%>%
  dplyr::count(GraphName,sort=T)#unbalance data

#----Show first n row of the data
df%>%
  slice_head(n=10)
#----view data
#df%>%View()

###--------Load the janitor package to clean the data---------###
df<-df%>%
  #mutate(GraphName=factor(GraphName,levels=c("ER","sbm","LAT")))%>%
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

set.seed(2938)

df.rec=recipe(graph_name~., data = df)%>%
  step_impute_median(all_numeric_predictors())%>%
  step_smote(graph_name) 
  #step_downsample(graph_name,seed = 7649)

#recipes::step_impute_mode(all_nominal_predictors())%>%
#---step_smote creates a specification of a recipe step 
#---that generate new examples of the minority class using nearest neighbors of these cases.
#--convert categorical variables to factors
#recipes::step_string2factor(all_nominal()) %>%
#-combine low frequency factor levels
#recipes::step_other(all_nominal(), threshold = 0.01) %>%
# remove no variance predictors which provide no predictive information 
#recipes::step_nzv(all_nominal()) %>% #removes near zero variance nominal predictors
#recipes::step_nzv(all_predictors()) %>%#removes near zero variance numeric predictors
#recipes::step_normalize(all_numeric_predictors())%>%#normalize predictors for use of other models
#step_dummy(all_predictors())%>%

saveRDS(df.rec,"df.rec.rds")
df.rec=readRDS("df.rec.rds")


df.prep=df.rec%>%prep()
saveRDS(df.prep,"df.prep.rds")
df.prep=readRDS("df.prep.rds")

df.juice=df.prep%>%juice
saveRDS(df.juice,"df.juice.rds")
df.juice=readRDS("df.juice.rds")

#----count number for each target category
df.juice%>%count(graph_name,sort=T)#balanced data

#check for NA's in the final imputed data
colSums(is.na(df.juice))#no na's

###################################
#----Data Partitioning----###
###################################
set.seed(384)

#----full balanced and pre-processed data for splitting
df.data=df.juice

saveRDS(df.data,"df.data.rds")

df.data=readRDS("df.data.rds")

#----sample and split
df_split=df.data[sample(1:nrow(df.data)), ]%>%
  initial_split(prop = 0.7)

df.train=training(df_split)  
df.test=testing(df_split)
###----Print number of training and testing set----###
cat("training set:", nrow(df.train), "\n",
    "testing set :", nrow(df.test), "\n")

#########################################################################################
# Building first classification  models
#########################################################################################

#----cross validation
df.cv.splits <- vfold_cv(df.train, v = 10)

#----training recipe
df.train.rec <- recipe(graph_name ~ ., data = df.train) 

df.train.prep=df.train.rec%>%prep()

#----training data
df.train.juice=df.train.prep%>%juice()

#################################################################################################
# Feature Engineering
#################################################################################################

#----borutal feature selection
set.seed(9532)
boruta.train <- Boruta(graph_name~., data = df.train, doTrace = 2,maxRuns=11)
print(boruta.train)

#----save boruta object
saveRDS(boruta.train,"boruta.train.upsample.rds")
boruta.train=readRDS("boruta.train.upsample.rds")

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

#----list of confirmed selected attributes
selected.attr=getSelectedAttributes(final.boruta, withTentative = F)
selected.attr

#----final imortance score of each predictor variable with boruta
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
df.final<-df.data
graph_name<-df.final$graph_name
df.final<-df.final[,names(df.final)%in%boruta.list]
df.final<-cbind(df.final,graph_name)

#----check strcuture of final data after feature engineering
#df.final=df.juice
df.final=df.final%>%
  rename("num_of_nodes"="order")
str(df.final)

# df.final=df.final%>%
#   select(-c(X))

saveRDS(df.final,"df.final.rds")
df.final=readRDS("df.final.rds")
#----Final data for the model after feature engineering
#write.csv(df.final,"df.final.csv")
#----checking for Na's in final data
colSums(is.na(df.final))

################################################################
# Pre processing final data for model
################################################################
#----final data recipe
df.final.rec=recipe(graph_name~., data = df.final)
saveRDS(df.final.rec,"df.final.rec.rds")
df.final.rec=readRDS("df.final.rec.rds")

df.final.prep=df.final.rec%>%
  prep()

saveRDS(df.final.prep,"df.final.prep.rds")
df.final.prep=readRDS("df.final.prep.rds")

df.final.juice=df.final.prep%>%
  juice()

saveRDS(df.final.juice,"df.final.juice.rds")
##############################################################################
# MODELS
##############################################################################
set.seed(384)

df.final=readRDS("df.final.rds")
#----final sample and split
df.final.split=df.final[sample(1:nrow(df.final)), ]%>%
  initial_split(prop = 0.7)

saveRDS(df.final.split,"df.final.split.rds")
df.final.split=readRDS("df.final.split.rds")

df.final.train=training(df.final.split) 
saveRDS(df.final.train,"df.final.train.rds")
df.final.train=readRDS("df.final.train.rds")

df.final.test=testing(df.final.split)
saveRDS(df.final.test,"df.final.test.rds")
df.final.test=readRDS("df.final.test.rds")

#----Print number of training and testing set
cat("training set:", nrow(df.final.train), "\n",
    "testing set :", nrow(df.final.test), "\n")

####----cross validation----
df.cv.splits <- vfold_cv(df.final.train, v = 10)
saveRDS(df.cv.splits,"df.cv.splits.rds")
df.cv.splits=readRDS("df.cv.splits.rds")

#################################################################################################
# classification models
#################################################################################################

###+++++++++++++++++++++
#----[1] Random----Forest
###+++++++++++++++++++++
set.seed(384)
rf.tune.df <- rand_forest(mtry = tune(), trees = tune()) %>%
set_engine("ranger") %>%
  set_mode("classification")


# show what will be tuned
rf.tune.df
extract_parameter_set_dials(rf.tune.df)


# Hyperparameter grid
rf.grid <- rf.tune.df %>%
 parameters() %>%
finalize(select(df.final, -graph_name)) %>%
 grid_max_entropy(size = 10)

# Workflow bundling every step
  rf.wflow <- workflow() %>%
  add_recipe(df.final.rec) %>%
  add_model(rf.tune.df)
   
####+++++++++++++++++++++
#----[2]Boost Trees
####+++++++++++++++++++++
xgb.tune.df <- boost_tree(mtry = tune(), tree = tune(),
  learn_rate = tune(), tree_depth = tune()) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

xgb.grid <- xgb.tune.df %>%
parameters() %>%
finalize(select(df.final, -graph_name)) %>%
grid_max_entropy(size = 10)

xgb.wflow <- workflow() %>%
 add_recipe(df.final.rec) %>%
  add_model(xgb.tune.df)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Fitting all Model
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
wflow_list <- list(rf.wflow,xgb.wflow)#,nnet.wflow)
#save trained workflows for the models
saveRDS(wflow_list,"wflow_list.rds")
grid_list <- list(rf.grid,xgb.grid)#,nnet.grid)
#save trained grid list
saveRDS(grid_list,"grid_list.rds")

plan(multisession)

#tuning with tune grid
system.time(trained_models_list<- future_map2(.x = wflow_list,
                        .y = grid_list,
                    ~tune(.x , resamples = df.cv.splits , grid = .y)))

#tuning with racing
library(finetune)
system.time(trained_models_list_anova <- future_map2(.x = wflow_list,
                              .y = grid_list,
                      ~tune_race_anova(.x , resamples = df.cv.splits , grid = .y)))
  
#TrainedModelslist=readRDS("trained_models_list_anova_downsample.rds")
TrainedModelslist=readRDS("trained_models_list_anova_upsample.rds")

TrainedModelslist %>%
map(show_best, metric = "accuracy", n = 1)

#----extracting the best model
best.model <- TrainedModelslist[[2]]

best.model %>%
collect_metrics()

###----plot of best metrics
#accuracy
best.model %>%
show_best(metric = "accuracy")
autoplot(best.model)

#roc
best.model %>%
  show_best(metric = "roc_auc")

#roc_auc
best_auc <- select_best(best.model, "roc_auc")
best_auc


#############################################################################################
# Retrain final best model
#############################################################################################
#----getting metrics of the best model
best.model.specs <-
best.model %>%
select_best(metric = "accuracy")

best.model.specs


#best model specification
final_model <- boost_tree(mode = "classification", mtry = best.model.specs$mtry,
                trees = best.model.specs$trees) %>% 
                set_engine("xgboost")

##----best workflow
df.best.wflow <- workflow() %>%
add_recipe(df.final.rec) %>%
add_model(final_model)

##----final best workflow
df.best.final.wflow<- finalize_workflow(
df.best.wflow,
best_auc
)

df.best.final.wflow

###----final fitted model----###
final.fitted.model <- last_fit(df.best.final.wflow, df.final.split)

#save trained grid list
final.fitted.model = readRDS("final.fitted.model.new.rds")#upsample
# saveRDS(final.fitted.model ,"final.fitted.upsample.model.rds")
# final.fitted.model = readRDS("final.fitted.upsample.model.rds")
#########################################################################################
# Explore final fitted model results
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

#extract workflow fore final model
extract_final.fitted.model= extract_workflow(final.fitted.model)
extract_final.fitted.model
saveRDS(extract_final.fitted.model ,"extract_final.fitted.model.rds")

extract_final.fitted.model=readRDS("extract_final.fitted.model.rds")

final.model.predictions <- predict(extract_final.fitted.model, new_data = df.final.test) %>%
  bind_cols(df.final.test)

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
#----SHAPELY ANALYSIS
################################################################################

####----ER-----
# Filter the desired node sizes
filtered_df_er <- df.final %>%
  filter(graph_name=="ER" & 
           num_of_nodes %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtered data by "size"
grouped_df_er <- filtered_df_er %>% group_by(num_of_nodes)

# Sample 200 rows from each group
sampled_df_er <- grouped_df_er %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_er <- ungroup(sampled_df_er)


####----sbm-----
# Filtsbm the desired node sizes
filtered_df_sbm <- df.final %>%
  filter(graph_name=="sbm" & 
           num_of_nodes %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtsbmed data by "size"
grouped_df_sbm <- filtered_df_sbm %>% group_by(num_of_nodes)

# Sample 200 rows from each group
sampled_df_sbm <- grouped_df_sbm %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_sbm <- ungroup(sampled_df_sbm)


####----SW-----
# FiltSW the desired node sizes
filtered_df_SW <- df.final %>%
  filter(graph_name=="SW" & 
           num_of_nodes %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtSWed data by "size"
grouped_df_SW <- filtered_df_SW %>% group_by(num_of_nodes)

# Sample 200 rows from each group
sampled_df_SW <- grouped_df_SW %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_SW <- ungroup(sampled_df_SW)

####----SF-----
# FiltSF the desired node sizes
filtered_df_SF <- df.final %>%
  filter(graph_name=="SF" & 
           num_of_nodes %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtSFed data by "size"
grouped_df_SF <- filtered_df_SF %>% group_by(num_of_nodes)

# Sample 200 rows from each group
sampled_df_SF <- grouped_df_SF %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_SF <- ungroup(sampled_df_SF)


####----Spatial-----
# FiltSpatial the desired node sizes
filtered_df_Spatial <- df.final %>%
  filter(graph_name=="Spatial" & 
           num_of_nodes %in% c(50, 100, 150, 200, 250, 300, 350, 400, 500, 750, 1000))

# Group the filtSpatialed data by "size"
grouped_df_Spatial <- filtered_df_Spatial %>% group_by(num_of_nodes)

# Sample 200 rows from each group
sampled_df_Spatial <- grouped_df_Spatial %>% sample_n(size = 1000, replace = FALSE)

# Remove grouping information
ungrouped_df_Spatial <- ungroup(sampled_df_Spatial)


df.final.new=rbind(ungrouped_df_Spatial,ungrouped_df_er,
                   ungrouped_df_SW,ungrouped_df_SF,
                   ungrouped_df_sbm)


saveRDS(df.final.new,"df.final.new.rds")

df.final.new=readRDS("df.final.new.rds")

final_resample_data <- df.final.new#[sample(nrow(df.final), 1000), ]
final_resample_data_prep <- bake(
  df.final.prep, # prep(df.final.rec), 
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
er_shap_bar.imp_new=sv_importance(final_shap$`Erdös-Rényi`, kind = 'bar',
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
er_shap_beeswarm_imp_new=sv_importance(final_shap$`Erdös-Rényi`, kind = 'beeswarm',
                                       show_numbers = F, bee_width = 0.3,
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



########################################################################
###----Stochastic-Block-Model var imp----###
#######################################################################
sbm_shap_bar.imp_new=sv_importance(final_shap$`Stochastic-Block-Model`, kind = 'bar',
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
sbm_shap_beeswarm_imp_new=sv_importance(final_shap$`Stochastic-Block-Model`, kind = 'beeswarm',
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


combined_imp2=plot_grid(combined_plot_with_legend.sf,
                        combined_plot_with_legend.sp,
                        combined_plot_with_legend.sw,
                        nrow = 3)

ggsave("combined_imp22.pdf", width = 14, height = 20, dpi = 50)

############-----Graphs----#############
n=200

##----ER Graph----
er.graph= erdos.renyi.game(n,1176,type = c("gnm"),directed = F,loops = F)
er.graph
plot(er.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="gray")#,frame.plot = TRUE)

ggsave("er_plot2.png",height=5,width=6)
ggsave("er_plot.svg",height=5,width=6)
##----SF Graph----
sf.graph = sample_pa(n=n, power=2, m=6, directed=FALSE, algorithm="psumtree")
sf.graph
plot(sf.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="gray")

ggsave("sf_plot2.png",height=5,width=6)
ggsave("sf_plot.svg",height=5,width=6)
##----SW Graph----
sw.graph = sample_smallworld(dim=2, size=sqrt(n), nei=2, p=0.01)
sw.graph
sw.plot=plot(sw.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="gray")



ggsave("sw_plot2.svg",height=5,width=6)




##----SBM Graph----
sbm.graph=sample_sbm(n, pref.matrix = pm, block.sizes = c(0.4*n, 0.6*n))
sbm.graph
plot(sbm.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="gray")

ggsave("sbm_plot2.svg",height=5,width=6)
##----SP Graph----
sp.graph=fastSpatialNetwork(n = n, r = 0.15, makeConnected=T,keepCellsSeparate=FALSE)
sp.graph

sp=plot(sp.graph,vertex.size=4,vertex.label=NA,vertex.color="black", 
     edge.arrow.size=0.5, edge.color="gray")

sp
ggsave("sp_plot2.svg",height=5,width=6)



###############################################################################
# Shap Dependency plot
###############################################################################

###----Modularity shapely depedency plot----###
mod.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Modularity",
                          interactions = FALSE, color_var = NULL)

#saveRDS(mod.shap.er,"mod.shap.er.rds")

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

#saveRDS(mod.shap.sbm,"mod.shap.sbm.rds")

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

#saveRDS(mod.shap.sf,"mod.shap.sf.rds")

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

#saveRDS(mod.shap.sp,"mod.shap.sp.rds")

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

#saveRDS(mod.shap.sw,"mod.shap.sw.rds")

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

ggsave("m.png", width=16, height=4)


###----Transitivity shapely depedency plot----###
trans.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Transitivity",
                            interactions = FALSE, color_var = NULL)

#saveRDS(trans.shap.er,"trans.shap.er.rds")

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

#saveRDS(trans.shap.sbm,"trans.shap.sbm.rds")

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

#saveRDS(trans.shap.sf,"trans.shap.sf.rds")

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

#saveRDS(trans.shap.sp,"trans.shap.sp.rds")

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

#saveRDS(trans.shap.sw,"trans.shap.sw.rds")

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

#saveRDS(spec.shap.er,"spec.shap.er.rds")

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

#saveRDS(spec.shap.sbm,"spec.shap.sbm.rds")

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

#saveRDS(spec.shap.sf,"spec.shap.sf.rds")

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

#saveRDS(spec.shap.sp,"spec.shap.sp.rds")

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

#saveRDS(spec.shap.sw,"spec.shap.sw.rds")


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

#saveRDS(nf.shap.er,"nf.shap.er.rds")

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

#saveRDS(nf.shap.sbm,"nf.shap.sbm.rds")

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

#saveRDS(nf.shap.sf,"nf.shap.sf.rds")

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

#saveRDS(nf.shap.sp,"nf.shap.sp.rds")

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

#saveRDS(nf.shap.sw,"nf.shap.sw.rds")

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

ggsave("m3.png", width=18, height=4)



###----Degree assortativity coeff shapely depedency plot----###
deg_assort.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree assortativity",
                                 interactions = FALSE, color_var = NULL)

#saveRDS(deg_assort.shap.er,"deg_assort.shap.er.rds")

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

#saveRDS(deg_assort.shap.sbm,"deg_assort.shap.sbm.rds")

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

#saveRDS(deg_assort.shap.sf,"deg_assort.shap.sf.rds")

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

#saveRDS(deg_assort.shap.sp,"deg_assort.shap.sp.rds")

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

#saveRDS(deg_assort.shap.sw,"deg_assort.shap.sw.rds")


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

#saveRDS(deg_centr.shap.er,"deg_centr.shap.er.rds")

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

#saveRDS(deg_centr.shap.sbm,"deg_centr.shap.sbm.rds")

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

#saveRDS(deg_centr.shap.sf,"deg_centr.shap.sf.rds")

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

#saveRDS(deg_centr.shap.sp,"deg_centr.shap.sp.rds")

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

#saveRDS(deg_centr.shap.sw,"deg_centr.shap.sw.rds")

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

ggsave("m4.png", width=18, height=4)

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

#saveRDS(eign_centr.shap.er,"eign_centr.shap.er.rds")

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

#saveRDS(eign_centr.shap.sbm,"eign_centr.shap.sbm.rds")

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

#saveRDS(eign_centr.shap.sf,"eign_centr.shap.sf.rds")

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

#saveRDS(eign_centr.shap.sp,"eign_centr.shap.sp.rds")

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

#saveRDS(eign_centr.shap.sw,"eign_centr.shap.sw.rds")

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

ggsave("m5.png", width=18, height=4)



###----Closeness_centrality shapely depedency plot----###
close_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Closeness centrality",
                                  interactions = FALSE, color_var = NULL)

#saveRDS(close_centr.shap.er,"close_centr.shap.er.rds")

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

#saveRDS(close_centr.shap.sbm,"close_centr.shap.sbm.rds")

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

#saveRDS(close_centr.shap.sf,"close_centr.shap.sf.rds")

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

#saveRDS(close_centr.shap.sp,"close_centr.shap.sp.rds")

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

#saveRDS(close_centr.shap.sw,"close_centr.shap.sw.rds")

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

ggsave("m6.png", width=18, height=4)


###----Betweenness_centrality shapely depedency plot----###

betwn_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Betweenness centrality",
                                  interactions = FALSE, color_var = NULL)

#saveRDS(betwn_centr.shap.er,"betwn_centr.shap.er.rds")

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

#saveRDS(betwn_centr.shap.sbm,"betwn_centr.shap.sbm.rds")

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

#saveRDS(betwn_centr.shap.sf,"betwn_centr.shap.sf.rds")

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

#saveRDS(betwn_centr.shap.sp,"betwn_centr.shap.sp.rds")

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

#saveRDS(betwn_centr.shap.sw,"betwn_centr.shap.sw.rds")

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

#saveRDS(mean_path.shap.er,"mean_path.shap.er.rds")

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

#saveRDS(mean_path.shap.sbm,"mean_path.shap.sbm.rds")

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

#saveRDS(mean_path.shap.sf,"mean_path.shap.sf.rds")

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

#saveRDS(mean_path.shap.sp,"mean_path.shap.sp.rds")

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

#saveRDS(mean_path.shap.sw,"mean_path.shap.sw.rds")


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

#saveRDS(mean_eccent.shap.er,"mean_eccent.shap.er.rds")

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

#saveRDS(mean_eccent.shap.sbm,"mean_eccent.shap.sbm.rds")

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

#saveRDS(mean_eccent.shap.sf,"mean_eccent.shap.sf.rds")

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

#saveRDS(mean_eccent.shap.sp,"mean_eccent.shap.sp.rds")

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

#saveRDS(mean_eccent.shap.sw,"mean_eccent.shap.sw.rds")


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

#saveRDS(graph_energy.shap.er,"graph_energy.shap.er.rds")

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

#saveRDS(graph_energy.shap.sbm,"graph_energy.shap.sbm.rds")

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

#saveRDS(graph_energy.shap.sf,"graph_energy.shap.sf.rds")

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

#saveRDS(graph_energy.shap.sp,"graph_energy.shap.sp.rds")

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

#saveRDS(graph_energy.shap.sw,"graph_energy.shap.sw.rds")


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

#saveRDS(Mean_degree.shap.er,"Mean_degree.shap.er.rds")

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

#saveRDS(Mean_degree.shap.sbm,"Mean_degree.shap.sbm.rds")

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

#saveRDS(Mean_degree.shap.sf,"Mean_degree.shap.sf.rds")

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

#saveRDS(Mean_degree.shap.sp,"Mean_degree.shap.sp.rds")

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

#saveRDS(Mean_degree.shap.sw,"Mean_degree.shap.sw.rds")


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
er.waterfall=sv_waterfall(final_shap$`Erdös-Rényi`, row_id = 5000,
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

sbm.waterfall=sv_waterfall(final_shap$`Stochastic-Block-Model`, row_id = 5000,
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
ggsave("sbm.waterfall.png", width = 12, height=10)
ggsave("sbm.waterfall.pdf", width = 12, height=10)

sf.waterfall=sv_waterfall(final_shap$`Scale-Free`, row_id = 5000,
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

sp.waterfall=sv_waterfall(final_shap$Spatial, row_id = 5000,
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

sw.waterfall=sv_waterfall(final_shap$`Small-World`, row_id = 5000,
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
ggsave("mod.2Ddep.png", width = 16, height = 8)



# meanpath.shap.2Ddep=sv_dependence2D(final_interactn_shap$`Stochastic-Block-Model`, 
#                                x ="Mean path length", 
#                                y = c("Mean eccentricity",
#                                      "Degree centrality",
#                                      "Transitivity",
#                                      "Degree assortativity",
#                                      "Spectral radius"), alpha = 0.2) & 
#   ggtitle("Stochastic-Block-Model")&
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) &
#   # legend.position='bottom', legend.direction='horizontal',
#   # legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()
# 
# meanpath.shap.2Ddep
# ggsave("meanpath.2Ddep.png", width = 16, height = 8)



trans.shap.2Ddep=sv_dependence2D(final_interactn_shap$Spatial, 
                                 x ="Transitivity", 
                                 y = c("Mean eccentricity",
                                       "Eigen centrality",
                                       "Degree centrality",
                                       "Degree assortativity",
                                       "Modularity",
                                       "Spectral radius"), alpha = 0.2)& 
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
ggsave("trans.2Ddep.png", width = 16, height = 8)


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
ggsave("deg.centr.2Ddep.png", width = 16, height = 8)


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

ggsave("spec.radius.2Ddep.png", width = 16, height = 8)


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
ggsave("norm.fiedler.2Ddep.png", width = 16, height = 8)


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


########################################################################################
# Shap  interaction plots
########################################################################################
final_resample_data <- df.final#[sample(nrow(df.final), 1000), ]
final_resample_data_prep <- bake(
  df.final.prep, # prep(df.final.rec), 
  has_role("predictor"),
  new_data = final_resample_data, 
  composition = "matrix"
)
#head(final_resample_data_prep)

# shap_interactn_2 <- shapviz(extract_fit_engine(final.fitted.model),
#                           X_pred = final_resample_data_prep, 
#                           X = final_resample_data,
#                           interactions = TRUE)
# saveRDS(shap_interactn_2,"shap_interactn.2.rds")
#shap_interactn=readRDS("shap.new.interaction.rds")
shap.interactn.2 <- shapviz(extract_fit_engine(final.fitted.model), 
                            X_pred = final_resample_data_prep, 
                            X = final_resample_data,
                            interactions = T)
saveRDS(shap.interactn.2,"shap.interactn.2.rds")

#shap_interactn=readRDS("shap_interactn.rds")
shap_interactn=readRDS("shap.interactn.2.rds")


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

###----Shap interaction plot for Erdös-Rényi ----###
##[1] we construct the interaction plot for the most important feature (Normalized_fiedler_value)
## for Erdös-Rényi

trans.top7.shap.interactn.plot=
  sv_interaction(final_interactn_shap$Spatial,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 

trans.top7.shap.interactn.plot
ggsave("trans.top7.shap.interactn.plot.png",width = 30, height = 16)
#ggsave("trans.top7.shap.interactn.plot.pdf",width = 30, height = 16)



mod.top7.shap.interactn.plot=
  sv_interaction(final_interactn_shap$`Stochastic-Block-Model`,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 

mod.top7.shap.interactn.plot
ggsave("mod.top7.shap.interactn.plot.png",width = 30, height = 16)
#ggsave("mod.top7.shap.interactn.plot.pdf",width = 30, height = 16)


nf.top7.shap.interactn.plot=
  sv_interaction(final_interactn_shap$`Erdös-Rényi`,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 

nf.top7.shap.interactn.plot=
  nf.top7.shap.interactn.plot+
  scale_x_continuous(limits = c(-7.5, 7.5), breaks = c(-7.5,0))

ggsave("nf.top7.shap.interactn.plot.png",width = 30, height = 16)
#ggsave("nf.top7.shap.interactn.plot.pdf",width = 30, height = 16)


deg_centr.top7.shap.interactn.plot=
  sv_interaction(final_interactn_shap$`Scale-Free`,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 


deg_centr.top7.shap.interactn.plot
ggsave("deg_centr.top7.shap.interactn.plot.png",width = 30, height = 16)
#ggsave("deg_centr.top7.shap.interactn.plot.pdf",width = 30, height = 16)


spec.top7.shap.interactn.plot=
  sv_interaction(final_interactn_shap$`Small-World`,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 

spec.top7.shap.interactn.plot=
  spec.top7.shap.interactn.plot+
  scale_x_continuous(limits = c(-7.5, 7.5), breaks = c(-7.5,0))

ggsave("spec.top7.shap.interactn.plot.png",width = 30, height = 16)
#ggsave("spec.top7.shap.interactn.plot.pdf",width = 30, height = 16)


####----Violin Plot----####
df.final=readRDS("df.final.rds")
#df.final.new=readRDS("df.final.new.rds")
str(df.final)
min_max_scaling <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Normalization function for Z-score scaling
z_score_scaling <- function(x) {
  (x - mean(x)) / sd(x)
}

violin.df=df.final
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



##########################################################################################
#######====PREDICTION ON NEW DATA WITH BEST XGBOOST MODEL====#####
##########################################################################################

##----Devil data----##
devil.data=read.csv("Devil-data.csv")

devil.data=devil.data%>%
  dplyr::rename("graph_name"="GraphNames")%>%
  dplyr::rename("NumOfNodes"="order")%>%
  dplyr::select(c(graph_name,NumOfNodes,edges,
                  mean_eccentr,mean_path_length,graph_energy,
                  modularity,diameter,betw_centr,transitivity,
                  spectral_radius,eigen_centr,deg_centr,
                  mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
                  closeness_centr,deg_assort_coef))%>%
  mutate_if(is.character,factor)%>%
  clean_names()%>%
  as_tibble()

levels(devil.data$graph_name)=c("Devil_mating","Devil_nonmating")
####----Final preprocessed data frame----for empirical data----##
best.model.predictions <- predict(extract_final.fitted.model, 
                                  new_data = devil.data)%>%
  bind_cols(devil.data)


############----Empirical Data----############################
set.seed(9456)
emp.data=read.csv("GraphFeatOnAllAnimNets.csv")
emp.data%>%
  filter(connected==1)

nrow(emp.data)

#emp.data = emp.data[sample(1:nrow(emp.data)), ]
emp.data=emp.data%>%
  dplyr::rename("graph_name"="GraphNames")%>%
  dplyr::rename("NumOfNodes"="order")%>%
  dplyr::select(c(graph_name,NumOfNodes,edges,
                  mean_eccentr,mean_path_length,graph_energy,
                  modularity,diameter,betw_centr,transitivity,
                  spectral_radius,eigen_centr,deg_centr,
                  mean_degree,minCut,FiedlerValue,Normalized_FiedlerValue,
                  closeness_centr,deg_assort_coef))%>%
  mutate_if(is.character,factor)%>%
  clean_names()%>%
  as_tibble()

colSums(is.na(emp.data))

nrow(emp.data)

emp.data=emp.data%>%
  filter(num_of_nodes>10)

nrow(emp.data)
colSums(is.na(emp.data))

emp.data.rec=recipe(graph_name~., data = emp.data)%>%
  step_impute_median(all_numeric_predictors())
emp.data.prep=emp.data.rec%>%prep()
emp.data.juice=emp.data.prep%>%juice()

saveRDS(emp.data.juice,"final.emp.data.rds")
final.emp.data=readRDS("final.emp.data.rds")
colSums(is.na(final.emp.data))



####----Final preprocessed data frame----for empirical data----##
best.model.predictions <- predict(extract_final.fitted.model, new_data = final.emp.data)%>%
  bind_cols(final.emp.data)

###----data frame for predicted empirical network
df.emp=data.frame(best.model.predictions$graph_name,best.model.predictions$.pred_class)
colnames(df.emp)=c("target","Predicted_classes")  

saveRDS(df.emp,"df.emp.rds")

df.emp=readRDS("df.emp.rds")

write.csv(df.emp,"Predicted-Class-of-Animal-Social-network.csv")
###----Meta data----####
metadata=read.csv("Network_repository_metaData_combined.csv",header=T, sep=",")

metadata=metadata%>%
  # na.omit()%>%
  as_tibble()

nrow(metadata)

metadata=metadata%>%
  #rename("target"="Graph.Name")%>%
  dplyr::select(-c(Source,Citation))

colnames(metadata)
# metadata1=metadata%>%
#   filter(Graph.Name %in% df.emp$target)

df.emp$target=df.emp$target%>%
  as.character()

df.emp$target=gsub("_", "-",df.emp$target)

metadata$Graph.Name=gsub("_", "-", metadata$Graph.Name)

metadata_reordered <- metadata[match(df.emp$target, metadata$Graph.Name),]
metadata_reordered

df_meta_data_new=cbind(metadata_reordered,df.emp)
df.emp.new=df_meta_data_new%>%
  select(c("target","Predicted_classes","Class"))


#graph features and meta data
comb=cbind(final.emp.data,df_meta_data_new)
write.csv(comb,"Reduced-Animal-Social-network.csv")


#df.emp$predicted[df.emp$predicted == 'SF'] <- as.factor('Scale-free')

levels(df.emp.new$Predicted_classes) <- c('Erdös-Rényi', 'Stochastic-Block-Model', 
                                     'Scale-Free', 'Spatial','Small-World')

df.emp.new <- within(df.emp.new, 
                     Predicted_classes <- factor(Predicted_classes, 
                                                 levels=names(sort(table(Predicted_classes), 
                                                                   decreasing=TRUE))))

df.emp.new=df.emp.new%>%
  as_tibble()
 # clean_names())

saveRDS(df.emp.new,"df.emp.new.rds")

df.emp.new=readRDS("df.emp.new.rds")
###----Stack bar plot for muti-nomial model
emp.var.imp.plot= ggplot(df.emp.new, aes(x = Predicted_classes,fill=Class))+
  geom_bar()+
  theme_classic()+
  theme(text = element_text(size = 12,family="serif"),
        legend.text = element_text(size = 12,family="serif"),
        legend.title = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))+
        labs(fill="Class")
emp.var.imp.plot

ggsave("emp.var.imp.svg", width=9,height=6)

######################################################################################
# second classification model with meta data
######################################################################################

#metadata$target=as.factor(metadata$target)
df.emp$target=df.emp$target%>%
  as.character()

df.emp$target=gsub("_", "-",df.emp$target)
#df.emp$target=gsub("-", "_",df.emp$target)
# metadata2=metadata[metadata$target %in% df.emp$target, ]
# metadata2
metadata=metadata%>%
  filter(Graph.Name %in% df.emp$target)

# metadata <- metadata %>%
#   mutate(Data_collection = na_if(Data_collection, ""))%>%
#   mutate(Data_collection = ifelse(is.na(Data_collection), "Unknown", Data_collection))


nrow(metadata)
nrow(df.emp)

metadata_reordered <- metadata[match(df.emp$target, metadata$Graph.Name),]

df_meta_data=cbind(metadata_reordered,df.emp)

write.csv(df_meta_data,"df_meta_data_and_pred_class.csv")
colSums(is.na(df.emp))


df_meta_data=df_meta_data%>%
  dplyr::select(-c("Graph.Name","target"))%>%
  mutate_if(is.character,factor)%>%
  clean_names()

df_meta_data

df_meta_data%>%
  glimpse()
#check for na's
colSums(is.na(df_meta_data))
#checking structure of the data
str(df_meta_data)
#renaming the target variable
df_meta_data=df_meta_data%>%
  # dplyr::rename("Mammal"="Mammal.")%>%
  dplyr::rename("target"="predicted_classes")

str(df_meta_data)

saveRDS(df_meta_data,"df_meta_data.rds")
df_meta_data=readRDS("df_meta_data.rds")

#####################################################################################
# Random forest classification for metadata
#####################################################################################

#Check all levels of each factor of the Categorical vairables (Factors) to understand the variables better
  for (x in names(df_meta_data)) {
    if (is.factor(df_meta_data[[x]]))
      {print(paste (x,":",levels(df_meta_data[[x]])))}
  }

#for one numeric variable
plot(df_meta_data$data_duration_days)

library(missForest)
#lets use missForest function to replace the Missing Values using Random Forest
# NAs will be replaced with the predicted values using the missForest function

df_meta_data=df_meta_data%>%
missForest(maxiter = 1,ntree=100,verbose = TRUE)

str(df_meta_data)


#----The final object is a list.The first element of the list is the original dataset
# with imputed values and the next element is the out of bag error estimates.
# We assign the first element of the list to the actual dataset
df_final_meta_data<-df_meta_data$ximp
#check missing values again
colSums(is.na(df_final_meta_data))
str(df_final_meta_data)

df_final_meta_data

#df_final_meta_data$target=as.factor(as.numeric(df_final_meta_data$target))

str(df_final_meta_data)

saveRDS(df_final_meta_data,"df_final_meta_data.rds")
df_final_meta_data=readRDS("df_final_meta_data.rds")
#----creating training, test and validation set
set.seed(123)
meta.df.split <- df_final_meta_data[sample(1:nrow(df_final_meta_data)),]%>%
initial_split(strata =  target,prop=0.6)

saveRDS(meta.df.split,"meta.df.split.rds")

metadata_train<- training(meta.df.split)
saveRDS(metadata_train,"metadata_train.rds")

metadata_test<- testing(meta.df.split)
saveRDS(metadata_test,"metadata_test.rds")


#check na's for the train and test data
colSums(is.na(metadata_train))
metadata_train
metadata_test

#creating cross validation folds
set.seed(234)

# set.seed(9532)
boruta.train.metadata <- Boruta( target~., data = metadata_train, doTrace = 2,maxRuns=11)

saveRDS(boruta.train.metadata,"boruta.train.metadata.rds")

print(boruta.train.metadata)
lz<-lapply(1:ncol(boruta.train.metadata$ImpHistory),function(i)
  boruta.train.metadata$ImpHistory[is.finite(boruta.train.metadata$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train.metadata$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
at = 1:ncol(boruta.train.metadata$ImpHistory), cex.axis = 0.7)


#----Dealing with the tentative attributes
final.boruta.metadata <- TentativeRoughFix(boruta.train.metadata)
print(final.boruta.metadata)
#----final imortance score of each predictor variable with boruta
boruta.df.metadata <- attStats(final.boruta.metadata)
boruta.df.metadata$selected_features <- row.names(boruta.df.metadata)
boruta.df.metadata$selected_features
#----Selecting top N predictors values by group
boruta.df.metadata.top <- boruta.df.metadata %>%
select(-c(normHits,decision)) %>%
arrange(desc(meanImp),desc(medianImp),desc(minImp),desc(maxImp))%>%
dplyr::slice(1:8)
#----Selecting top N predictors values by group
boruta.df.metadata.top <- boruta.df.metadata %>%
dplyr::select(-c(normHits,decision)) %>%
 arrange(desc(meanImp),desc(medianImp),desc(minImp),desc(maxImp))%>%
  dplyr::slice(1:8)

boruta.df.metadata.top

saveRDS(boruta.df.metadata.top,"boruta.df.metadata.top.rds")
#data to use for random forest model
borlist<-getSelectedAttributes(final.boruta.metadata, withTentative = F)
df_final_meta_data_rf<-df_final_meta_data
target<-df_final_meta_data$target
df_final_meta_data_rf<-df_final_meta_data_rf[,names(df_final_meta_data_rf)%in%borlist]
df_final_meta_data_rf<-cbind(df_final_meta_data_rf,target)
str(df_final_meta_data_rf)
#checking for NA's in data to be used to build the random orest model
colSums(is.na(df_final_meta_data_rf))



##############################################################################
#----Meta data----
##############################################################################
df_final_meta_data=readRDS("df_final_meta_data.rds")
#----creating training, test and validation set
set.seed(123)
meta.df.split <- df_final_meta_data[sample(1:nrow(df_final_meta_data)),]%>%
  initial_split(strata =  target,prop=0.6)

saveRDS(meta.df.split,"meta.df.split.rds")

metadata_train<- training(meta.df.split)
saveRDS(metadata_train,"metadata_train.rds")

metadata_test<- testing(meta.df.split)
saveRDS(metadata_test,"metadata_test.rds")

#check na's for the train and test data
colSums(is.na(metadata_train))
metadata_train
metadata_test

##----Creating the recipe object
metadata.rec.new=recipe(target ~ ., data = metadata_train)%>%
  step_integer(all_nominal_predictors())
#step_dummy(Target, one_hot = T)

saveRDS(metadata.rec.new,"metadata.rec.new.rds")

metadata.prep.new=metadata.rec.new%>%
  prep()

saveRDS(metadata.prep.new,"metadata.prep.new.rds")

metadata.juice.new=juice(metadata.prep.new)

saveRDS(metadata.juice.new,"metadata.juice.new.rds")

colSums(is.na(metadata.juice.new))

#bootstraps(df.final.train, times = 100)
metadata_folds.new <- metadata_train %>% bootstraps(times = 200)
metadata_folds.new


###----model-----
xgb.model.spec<-
  boost_tree(mtry = tune(), tree = tune(),
             learn_rate = tune(), tree_depth = tune()) %>%
  set_engine("xgboost",importance = "permutation") %>%
  set_mode("classification")


#Creating workflow object
xgb.wkflw <-
  workflow() %>%
  add_model(xgb.model.spec) %>%
  add_recipe(metadata.rec.new)


doParallel::registerDoParallel()

set.seed(345)
xgb.res <-
  xgb.wkflw %>% 
  tune_race_anova(metadata_folds.new,
                  grid = 15,
                  control = control_race(save_pred = TRUE)
                  #control = control_grid(save_pred = TRUE)#,
                  #metrics = metric_set(roc_auc)
  )

saveRDS(xgb.res,"xgb.res.new.rds")


####----Model Evaluation----####
xgb.metadata.res.new=xgb.res

xgb.metadata.res.new%>%
  collect_metrics()


#library(here)

#Show best metric
xgb.metadata.res.new %>%
  show_best(metric = "roc_auc")


# ####----Select best tuned parameters for final model-----####
xgb.best_metadata_auc.new <- select_best(xgb.metadata.res.new, "roc_auc")
xgb.best_metadata_auc.new

xgb.final.model.metadata.new <- finalize_model(
  xgb.model.spec,
  xgb.best_metadata_auc.new)
# 

#----The last workflow
xgb.final.metadata.wkflw.new <- workflow() %>%
  add_recipe(metadata.rec.new) %>%
  add_model(xgb.final.model.metadata.new)

set.seed(1273)
#----The last fitted model
xgb.final.metadata.fit.new <-
  xgb.final.metadata.wkflw.new %>%
  last_fit(meta.df.split )


xgb.final.metadata.fit.new%>%
  collect_metrics()

saveRDS(xgb.final.metadata.fit.new,"xgb.final.metadata.fit.new.rds")

xgb.final.metadata.fit.new=readRDS("xgb.final.metadata.fit.new.rds")

xgb.final.metadata.fit.new %>%
  collect_predictions()

xgb.final.metadata.fit.new  %>%
  collect_predictions() %>%
  roc_curve(target, c(`.pred_sbm`,
                      `.pred_ER`,`.pred_SF`,.pred_Spatial,
                      `.pred_SW`)) %>%autoplot()

saveRDS(xgb.final.metadata.fit.new,"xgb.final.metadata.fit.new.rds")
#To make predictions on new data set, we call the saved xgb model eg
load_fitted_xgb_metadata_model.new=readRDS("xgb.final.metadata.fit.new.rds")
#extract workflow from fitted model
xgb.final.metadata.model.new=extract_workflow(xgb.final.metadata.fit.new)
saveRDS(xgb.final.metadata.model.new,"xgb.final.metadata.model.new.rds")
#Prediction on test set
xgb.pred_result_metadata.new=augment(xgb.final.metadata.model.new,metadata_test)

xgb.predValsmetadata.new=xgb.pred_result_metadata.new%>%
  slice_head(n=10)
xgb.predValsmetadata.new

#c----confusion matrix----
#pred_result_metadata%>%
confmatrix.meta1.new=table(xgb.predValsmetadata.new$target,
                           xgb.predValsmetadata.new$.pred_class)
saveRDS(confmatrix.meta1.new,"confmatrix.meta1.new.rds")

confmatrix.meta1.new

########################################################################################
# Shapely analysis for meta data with species
#########################################################################################
metadata_train_new <- metadata_train[sample(nrow(metadata_train)), ]
meta_data_bake <- bake(
  metadata.prep.new, # prep(df.final.rec), 
  has_role("predictor"),
  new_data = metadata_train_new, 
  composition = "matrix"
)
#head(final_resample_data_prep)

meta_shap.new<- shapviz(extract_fit_engine(xgb.final.metadata.fit.new),
                        X_pred = meta_data_bake, 
                        X = metadata_train_new)


saveRDS(meta_shap.new,"meta_shap.new.rds")
meta_shap.new=readRDS("meta_shap.new.rds")



meta_shap_column_names=c("Captive","Edge weight","Interaction type",    
                         "Data collection","Species","Data duration (days)",  
                         "Class","Time resolution (secs)")


names(meta_shap.new)[names(meta_shap.new)=="Class_1"]<-"Erdös-Rényi"
names(meta_shap.new)[names(meta_shap.new) == "Class_1"] <- "Erdös-Rényi"
names(meta_shap.new)[names(meta_shap.new) == "Class_2"] <- "Stochastic-Block-Model"
names(meta_shap.new)[names(meta_shap.new) == "Class_3"] <- "Scale-Free"
names(meta_shap.new)[names(meta_shap.new) == "Class_4"] <- "Spatial"
names(meta_shap.new)[names(meta_shap.new) == "Class_5"] <- "Small-World"

colnames(meta_shap.new$`Erdös-Rényi`$X)=meta_shap_column_names
colnames(meta_shap.new$`Erdös-Rényi`$S)=meta_shap_column_names
#colnames(meta_shap.new$`Erdös-Rényi`$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new$`Stochastic-Block-Model`$X)=meta_shap_column_names
colnames(meta_shap.new$`Stochastic-Block-Model`$S)=meta_shap_column_names
#colnames(meta_shap.new$`Stochastic-Block-Model`$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new$`Scale-Free`$X)=meta_shap_column_names
colnames(meta_shap.new$`Scale-Free`$S)=meta_shap_column_names
#colnames(meta_shap.new$`Scale-Free`$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new$Spatial$X)=meta_shap_column_names
colnames(meta_shap.new$Spatial$S)=meta_shap_column_names
#colnames(meta_shap.new$Spatial$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new$`Small-World`$X)=meta_shap_column_names
colnames(meta_shap.new$`Small-World`$S)=meta_shap_column_names
#colnames(meta_shap.new$`Small-World`$S_inter)=meta_shap.new_column_names
saveRDS(meta_shap.new,"final_meta_shap.new.rds")
final_meta_shap.new=readRDS("final_meta_shap.new.rds")



############################################################
# Shapely variable importance and summary plots
############################################################
###########################################################
sp.meta.shap.imp.bar=sv_importance(final_meta_shap.new$Spatial,
                                   kind = "bar")#,
#show_numbers = TRUE, bee_width = 0.2)
#saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
sp.meta.shap.imp.bar.plot=sp.meta.shap.imp.bar+
  ggtitle(" ")+
  xlab(" ") +
  # xlim(-0.3,0.5)+
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.1), "cm"),#-1.2
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#dev.new(width=50,height=100,noRStudioGD = T)
sp.meta.shap.imp.bar.plot
ggsave("bar1.svg", width=8, height=10)

sp.meta.shap.beeswarm.imp=sv_importance(final_meta_shap.new$Spatial, 
                                        kind = "beeswarm",
                                        bee_width = .5,
                                        bee_adjust = 0.5)#,
#show_numbers = TRUE, bee_width = 0.2)
#saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
sp.meta.shap.imp.beeswarm.plot=sp.meta.shap.beeswarm.imp+
  ggtitle(" ")+
  xlab("SHAP Value")+
   xlim(-3,3)+
  theme_bw()+
  theme(plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-1,0.5,0), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

sp.meta.shap.imp.beeswarm.plot

ggsave("bees.svg", width=12, height=10)

figure1=sp.meta.shap.imp.beeswarm.plot+
  theme(legend.position="none")

figure2=sp.meta.shap.imp.bar.plot
sp.meta.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)
# Get the legend from one of the plots
legend <- get_legend(sp.meta.shap.imp.beeswarm.plot)

# Add the legend to the combined plot
combined_plot_with_legend.sp <- plot_grid(sp.meta.combine.var.imp,
                                          legend, ncol = 1, 
                                          rel_widths = c(3, 1))

combined_plot_with_legend.sp




ggsave("sp_shap_varimp.png", width = 14, height = 7)

combined_plot_with_legend.sp=combined_plot_with_legend.sp+
  plot_layout(widths = 1.2,heights = 1.2)

####################################################################################
# Waterfall plots
####################################################################################
#x=final_meta_shap.new$Spatial
sp.meta.waterfall=sv_waterfall(final_meta_shap.new$Spatial, 
            row_id = 351,
            size = 18,fill_colors = c("red","blue"),
            show_connection = T,
            show_annotation = F,
            annotation_size = 6)

sp.meta.waterfall=sp.meta.waterfall+
  ggtitle(" ")+
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

sp.smeta.waterfall=sp.meta.waterfall+plot_layout(widths = 1.2,heights = 1.2)
sp.meta.waterfall

ggsave("sp.meta.waterfall.svg", width = 12, height=12)
ggsave("sp.meta.waterfall.pdf", width = 12, height=10)

#############################################################################
#Shapely dependency
#############################################################################
###----Species shapely depedency plot----###
species.dep.sp=sv_dependence(final_meta_shap.new$Spatial, 
                             "Species",color_var = NULL)

saveRDS(species.dep.sp,"species.dep.sp.rds")

species.dep.sp=readRDS("species.dep.sp.rds")
species.dep.sp.df=species.dep.sp$data
species.dep.sp.df$Species=gsub("_", " ",species.dep.sp.df$Species)


species.dep.sp.df$Species=sub("^([a-z])", "\\U\\1", species.dep.sp.df$Species, perl = TRUE)

species.dep.sp.df.plot=
  ggplot(species.dep.sp.df, aes(x = reorder(Species,shap), y = shap)) +
  coord_flip()+
  geom_bar(stat = "identity",color = "black", fill = "black")+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Species")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
        strip.text = element_text(size = 17),
        plot.title = element_text(size = 32),
        text = element_text(size = 17,family="serif"),
        axis.text.y = element_text(size = 17),
        axis.text.x = element_text(size = 32),
        axis.title.x = element_text(size = 32),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

species.dep.sp.df.plot=species.dep.sp.df.plot+plot_layout(widths = 1.2,heights = 1.2)
species.dep.sp.df.plot
ggsave("species.dep.svg", width = 12, height=10)

####----shapely dependency for data collection
data_collectn.dep.sp=sv_dependence(final_meta_shap.new$Spatial, 
                                   "Data collection",color_var = NULL)

saveRDS(data_collectn.dep.sp,"data_collectn.dep.sp.rds")

data_collectn.dep.sp=readRDS("data_collectn.dep.sp.rds")
data_collectn.dep.sp.df=data_collectn.dep.sp$data
data_collectn.dep.sp.df$`Data collection`=gsub("_", " ",data_collectn.dep.sp.df$`Data collection`)

data_collectn.dep.sp.df$`Data collection`=
  sub("^([a-z])", "\\U\\1", data_collectn.dep.sp.df$`Data collection`, perl = TRUE)

colnames(data_collectn.dep.sp.df)=c('shap','Data_collection')

data_collectn.dep.sp.df.plot=
  ggplot(data_collectn.dep.sp.df, aes(x = reorder(Data_collection, -shap), y = shap)) +
  geom_bar(stat = "identity",color = "black", fill = "black")+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Data collection")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
        strip.text = element_text(size = 32),
        plot.title = element_text(size = 32),
        text = element_text(size = 32,family="serif"),
        axis.text.y = element_text(size = 32),
        axis.text.x = element_text(size = 32),
        axis.title.x = element_text(size = 32),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_collectn.dep.sp.df.plot=data_collectn.dep.sp.df.plot+plot_layout(widths = 1.2,heights = 1.2)
data_collectn.dep.sp.df.plot
ggsave("data_collectn.dep.svg", width = 12, height=10)
####----shapely dependency for data duration

interact.dep.sp=sv_dependence(final_meta_shap.new$Spatial, 
                              "Interaction type",color_var = NULL)

saveRDS(interact.dep.sp,"interact.dep.sp.rds")

interact.dep.sp=readRDS("interact.dep.sp.rds")
interact.dep.sp.df=interact.dep.sp$data
interact.dep.sp.df$`Interaction type`=gsub("_", " ",interact.dep.sp.df$`Interaction type`)

interact.dep.sp.df$`Interaction type`=
  sub("^([a-z])", "\\U\\1", interact.dep.sp.df$`Interaction type`, perl = TRUE)

colnames(interact.dep.sp.df)=c('shap','interaction_type')

interaction_type.plot=
  ggplot(interact.dep.sp.df, aes(x = reorder(interaction_type, -shap), y = shap)) +
  geom_bar(stat = "identity",color = "black", fill = "black")+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Interaction type")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
    strip.text = element_text(size = 32),
    plot.title = element_text(size = 32),
    text = element_text(size = 32,family="serif"),
    axis.text.y = element_text(size = 32),
    axis.text.x = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())

interaction_type.plot=interaction_type.plot+plot_layout(widths = 1.2,heights = 1.2)
interaction_type.plot

####----shapely dependency for data duration
data_duratn.dep.sp=sv_dependence(final_meta_shap.new$Spatial, 
                                 "Data duration (days)",color_var = NULL)

saveRDS(data_duratn.dep.sp,"data_duratn.dep.sp.rds")

data_duratn.dep.sp=readRDS("data_duratn.dep.sp.rds")
data_duratn.dep.sp.df=data_duratn.dep.sp$data


colnames(data_duratn.dep.sp.df)=c('shap','Data_duration')


data_duratn.dep.sp.df.plot=
  ggplot(data_duratn.dep.sp.df, aes(x = Data_duration, y = shap)) +
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  geom_point()+
  ggtitle("Data duration (days)")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
        strip.text = element_text(size = 32),
        plot.title = element_text(size = 32),
        text = element_text(size = 32,family="serif"),
        axis.text.y = element_text(size = 32),
        axis.text.x = element_text(size = 32),
        axis.title.x = element_text(size = 32),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_duratn.dep.sp.df.plot=data_duratn.dep.sp.df.plot+plot_layout(widths = 1.2,heights = 1.2)
data_duratn.dep.sp.df.plot
ggsave("data_duratn.dep.svg", width = 12, height=10)

####----shapely dependency for data duration
captive.dep.sp=sv_dependence(final_meta_shap.new$Spatial, 
                             "Captive",color_var = NULL)

saveRDS(captive.dep.sp,"captive.dep.sp.rds")

captive.dep.sp=readRDS("captive.dep.sp.rds")
captive.dep.sp.df=captive.dep.sp$data

captive.dep.sp.df$Captive=gsub("_", " ",captive.dep.sp.df$Captive)

sub("^([a-z])", "\\U\\1", captive.dep.sp.df$Captive, perl = TRUE)


captive.dep.sp.df.plot=
  ggplot(captive.dep.sp.df, aes(x = reorder(Captive,-shap), y = shap)) +
  geom_bar(stat = "identity",color = "black", fill = "black")+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Captive")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
        strip.text = element_text(size = 32),
        plot.title = element_text(size = 32),
        text = element_text(size = 32,family="serif"),
        axis.text.y = element_text(size = 32),
        axis.text.x = element_text(size = 32),
        axis.title.x = element_text(size = 32),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

captive.dep.sp.df.plot=captive.dep.sp.df.plot+plot_layout(widths = 0.2,heights = 0.2)
captive.dep.sp.df.plot
ggsave("captive.dep.svg", width = 12, height=10)



meta.dep.plot1=ggarrange(
  species.dep.sp.df.plot,
  ggarrange(data_collectn.dep.sp.df.plot,
            align = "v",widths = c(1,1),
            heights = c(1,1)),
  nrow = 2,
  labels = c("A)","B)"),
  hjust = -1.4,#-3.1,
  vjust = 1.2,
  font.label = list(size = 19, color = "black", face = "bold",
                    family = "serif"))


meta.dep.plot2=ggarrange(
  data_duratn.dep.sp.df.plot,
  ggarrange(captive.dep.sp.df.plot,
            align = "h", heights = c(0.3, 0.5)),
  ncol = 2,
  labels = c("C)","D)"),
  hjust = -1.4,#-3.1,
  vjust = 1,
  font.label = list(size = 19, color = "black", face = "bold",
                    family = "serif"))


metaplot1=plot_grid(meta.dep.plot1,meta.dep.plot2,nrow = 2,rel_heights = c(1.3,0.4,0.2))
metaplot1
ggsave("meta.dep.plot1.png", width = 12, height = 14)



############################################################################
#Shapely interaction
#############################################################################

meta_shap_interactn.new<- shapviz(extract_fit_engine(xgb.final.metadata.fit.new),
                                  X_pred = meta_data_bake, 
                                  X = metadata_train_new,
                                  interactions = TRUE)

# sv_interaction(meta_shap_interactn.new, kind = "no")
saveRDS(meta_shap_interactn.new,"meta_shap_interactn.new.rds")
meta_shap_interactn.new=readRDS("meta_shap_interactn.new.rds")

names(meta_shap_interactn.new)[names(meta_shap_interactn.new) == "Class_1"] <- "Erdös-Rényi"
names(meta_shap_interactn.new)[names(meta_shap_interactn.new) == "Class_2"] <- "Stochastic-Block-Model"
names(meta_shap_interactn.new)[names(meta_shap_interactn.new) == "Class_3"] <- "Scale-Free"
names(meta_shap_interactn.new)[names(meta_shap_interactn.new) == "Class_4"] <- "Spatial"
names(meta_shap_interactn.new)[names(meta_shap_interactn.new) == "Class_5"] <- "Small-World"


colnames(meta_shap_interactn.new$"Erdös-Rényi"$X)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Erdös-Rényi"$S)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Erdös-Rényi"$S_inter)=meta_shap_column_names
dimnames(meta_shap_interactn.new$"Erdös-Rényi"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn.new$"Erdös-Rényi"$S_inter)[[2]])

colnames(meta_shap_interactn.new$"Stochastic-Block-Model"$X)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Stochastic-Block-Model"$S)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Stochastic-Block-Model"$S_inter)=meta_shap_column_names
dimnames(meta_shap_interactn.new$"Stochastic-Block-Model"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn.new$"Stochastic-Block-Model"$S_inter)[[2]])

colnames(meta_shap_interactn.new$"Scale-Free"$X)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Scale-Free"$S)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Scale-Free"$S_inter)=meta_shap_column_names
dimnames(meta_shap_interactn.new$"Scale-Free"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn.new$"Scale-Free"$S_inter)[[2]])

colnames(meta_shap_interactn.new$"Spatial"$X)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Spatial"$S)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Spatial"$S_inter)=meta_shap_column_names
dimnames(meta_shap_interactn.new$"Spatial"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn.new$"Spatial"$S_inter)[[2]])

colnames(meta_shap_interactn.new$"Small-World"$X)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Small-World"$S)=meta_shap_column_names
colnames(meta_shap_interactn.new$"Small-World"$S_inter)=meta_shap_column_names
dimnames(meta_shap_interactn.new$"Small-World"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn.new$"Small-World"$S_inter)[[2]])


final_meta_shap.interactn$Spatial$X$Species=gsub("_", " ",final_meta_shap.interactn$Spatial$X$Species)
sub("^([a-z])", "\\U\\1",final_meta_shap.interactn$Spatial$X$Species, perl = TRUE)

final_meta_shap.interactn$Spatial$X$Captive=gsub("_", " ",final_meta_shap.interactn$Spatial$X$Captive)
sub("^([a-z])", "\\U\\1",final_meta_shap.interactn$Spatial$X$Captive, perl = TRUE)

final_meta_shap.interactn$Spatial$X$`Interaction type`=gsub("_", " ",final_meta_shap.interactn$Spatial$X$`Interaction type`)
sub("^([a-z])", "\\U\\1",final_meta_shap.interactn$Spatial$X$`Interaction type`, perl = TRUE)

final_meta_shap.interactn$Spatial$X$`Data collection`=gsub("_", " ",final_meta_shap.interactn$Spatial$X$`Data collection`)
sub("^([a-z])", "\\U\\1",final_meta_shap.interactn$Spatial$X$`Data collection`, perl = TRUE)

final_meta_shap.interactn$Spatial$X$`Edge weight`=gsub("_", " ",final_meta_shap.interactn$Spatial$X$`Edge weight`)
sub("^([a-z])", "\\U\\1",final_meta_shap.interactn$Spatial$X$`Edge weight`, perl = TRUE)


saveRDS(meta_shap_interactn.new,"final_meta_shap.interactn.rds")

final_meta_shap.interactn=readRDS("final_meta_shap.interactn.rds")


meta.top7.shap.interactn.plot=
  sv_interaction(final_meta_shap.interactn$Spatial,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 

meta.top7.shap.interactn.plot
meta.top7.shap.interactn.plot=meta.top7.shap.interactn.plot+
  scale_x_continuous(limits = c(-2, 1.5), breaks = c(-1,0.0,1))
ggsave("meta.top7.shap.interactn.with.species.plot.png", width = 30, height = 16)

ggsave("meta.top7.shap.interactn.with.species.plot.pdf",
       width = 30, height = 16)

#####----2D interaaction----effects
meta.shap.2Ddep1=sv_dependence2D(final_meta_shap.interactn$Spatial, 
                                 x ="Species", 
                                 y = c("Data duration (days)")) & 
  ggtitle("Species")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()


meta.shap.2Ddep1
ggsave("meta.shap.2Ddep1.svg", width = 18, height=16)
ggsave("meta.2Ddep11.png", width = 18, height=16)

meta.shap.2Ddep2=sv_dependence2D(final_meta_shap.interactn$Spatial, 
                                 x ="Species", 
                                 y = c("Captive")) & 
  ggtitle("Species")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 30, hjust = 1,size=14),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep2
ggsave("meta.shap.2Ddep2.svg", width = 18, height=16)
ggsave("meta.2Ddep12.png", width = 18, height=16)


meta.shap.2Ddep3=sv_dependence2D(final_meta_shap.interactn$Spatial, 
                                 x ="Species", 
                                 y = c("Interaction type")) & 
  ggtitle("Species")&
  coord_flip()&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        #axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
   legend.position='bottom', legend.direction='horizontal',
   legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep3
ggsave("meta.shap.2Ddep3.svg", width = 18, height=16)
ggsave("meta.2Ddep13.png", width = 18, height=16)

meta.shap.2Ddep4=sv_dependence2D(final_meta_shap.interactn$Spatial, 
                                 x ="Species", 
                                 y = c("Data collection"), alpha = 0.2) & 
  ggtitle("Species")&
  coord_flip()&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep4
ggsave("meta.shap.2Ddep4.svg", width = 18, height=16)


meta.shap.2Ddep5=sv_dependence2D(final_meta_shap.interactn$Spatial, 
                                 x ="Data duration (days)", 
                                 y = c("Captive"), alpha = 0.2) & 
  ggtitle("Data duration (days)")&
  coord_flip()&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep5
ggsave("meta.shap.2Ddep5.svg", width = 18, height=16)







meta.shap.2Ddep6=sv_dependence2D(final_meta_shap.interactn$Spatial, 
                                 x ="Data duration (days)", 
                                 y = c("Edge weight"), alpha = 0.2) & 
  ggtitle("Data duration (days)")&
  coord_flip()&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  # legend.position='bottom', legend.direction='horizontal',
  # legend.key.width = unit(0.8, "cm"))+
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep6
ggsave("meta.shap.2Ddep6.svg", width = 18, height=16)








meta.shap.2Ddep=plot_grid(meta.shap.2Ddep1,meta.shap.2Ddep2,
                          meta.shap.2Ddep3,meta.shap.2Ddep4,nrow=2,ncol=2)

ggsave("meta.shap.2Ddep1.png", width = 12, height = 13)




###############################################################################
# Analysing Meta data without species
################################################################################


df_final_meta_data=readRDS("df_final_meta_data.rds")

df_final_meta_data2=df_final_meta_data%>%
  select(-species)

#----creating training, test and validation set
set.seed(123)
meta.df.split2 <- df_final_meta_data2[sample(1:nrow(df_final_meta_data2)),]%>%
  initial_split(strata =  target,prop=0.6)

saveRDS(meta.df.split2,"meta.df.split2.rds")

metadata_train2<- training(meta.df.split2)
saveRDS(metadata_train2,"metadata_train2.rds")

metadata_test2<- testing(meta.df.split2)
saveRDS(metadata_test2,"metadata_test2.rds")

#check na's for the train and test data
colSums(is.na(metadata_train2))
metadata_train2
metadata_test2

##----Creting the recipe object
metadata.rec.new2=recipe(target ~ ., data = metadata_train2)%>%
  step_integer(all_nominal_predictors())
#step_dummy(Target, one_hot = T)

#--compute missing values for numeric predictors
saveRDS(metadata.rec.new2,"metadata.rec.new2.rds")

metadata.prep.new2=metadata.rec.new2%>%
  prep()

saveRDS(metadata.prep.new2,"metadata.prep.new2.rf")

metadata.juice.new2=juice(metadata.prep.new2)

saveRDS(metadata.juice.new2,"metadata.juice.new2")

colSums(is.na(metadata.juice.new2))
#bootstraps(df.final.train, times = 100)
metadata_folds.new2 <- metadata_train2 %>% bootstraps(times = 200)
metadata_folds.new2

###----model-----
xgb.model.spec2<-
  boost_tree(mtry = tune(), tree = tune(),
             learn_rate = tune(), tree_depth = tune()) %>%
  set_engine("xgboost",importance = "permutation") %>%
  set_mode("classification")


#Creating workflow object
xgb.wkflw2<-
  workflow() %>%
  add_model(xgb.model.spec2) %>%
  add_recipe(metadata.rec.new2)

library(finetune)
doParallel::registerDoParallel()

set.seed(345)
xgb.res2 <-
  xgb.wkflw2 %>% 
  tune_race_anova(metadata_folds.new2,
                  grid = 30,
                  control = control_race(save_pred = TRUE)
                  #control = control_grid(save_pred = TRUE)#,
                  #metrics = metric_set(roc_auc)
  )

saveRDS(xgb.res2,"xgb.res2.new.rds")

####----Model Evaluation----####
xgb.metadata.res.new2=xgb.res2

xgb.metadata.res.new2%>%
  collect_metrics()

#Show best metric
xgb.metadata.res.new2 %>%
  show_best(metric = "roc_auc")

# ####----Select best tuned parameters for final model-----####
xgb.best_metadata_auc.new2 <- select_best(xgb.metadata.res.new2, "roc_auc")
xgb.best_metadata_auc.new2

xgb.final.model.metadata.new2 <- finalize_model(
  xgb.model.spec2,
  xgb.best_metadata_auc.new2)

#----The last workflow
xgb.final.metadata.wkflw.new2 <- workflow() %>%
  add_recipe(metadata.rec.new2) %>%
  add_model(xgb.final.model.metadata.new2)

set.seed(1273)
#----The last fitted model
xgb.final.metadata.fit.new2 <-
  xgb.final.metadata.wkflw.new2 %>%
  last_fit(meta.df.split2 )

xgb.final.metadata.fit.new2%>%
  collect_metrics()

saveRDS(xgb.final.metadata.fit.new2,"xgb.final.metadata.fit.new2.rds")

xgb.final.metadata.fit.new2 %>%
  collect_predictions()

xgb.final.metadata.fit.new2  %>%
  collect_predictions() %>%
  roc_curve(target, c(`.pred_sbm`,
                      `.pred_ER`,`.pred_SF`,.pred_Spatial,
                      `.pred_SW`)) %>%autoplot()

saveRDS(xgb.final.metadata.fit.new2,"xgb.final.metadata.fit.new2.rds")
#To make predictions on new data set, we call the saved xgb model eg
load_fitted_xgb_metadata_model.new2=readRDS("xgb.final.metadata.fit.new2.rds")
#extract workflow from fitted model
xgb.final.metadata.model.new2=extract_workflow(xgb.final.metadata.fit.new2)
saveRDS(xgb.final.metadata.model.new2,"xgb.final.metadata.model.new2.rds")
#Prediction on test set
xgb.pred_result_metadata.new2=augment(xgb.final.metadata.model.new2,metadata_test2)

xgb.predValsmetadata.new2=xgb.pred_result_metadata.new2%>%
  slice_head(n=10)
xgb.predValsmetadata.new2

#c----confusion matrix----
#pred_result_metadata%>%
confmatrix.meta1.new2=table(xgb.predValsmetadata.new2$target,
                            xgb.predValsmetadata.new2$.pred_class)
saveRDS(confmatrix.meta1.new2,"confmatrix.meta1.new2.rds")

confmatrix.meta1.new2

########################################################################################
# Shapely analysis for meta data without species
#########################################################################################
metadata_train2_new <- metadata_train2[sample(nrow(metadata_train2)), ]
meta_data_bake2 <- bake(
  metadata.prep.new2, # prep(df.final.rec), 
  has_role("predictor"),
  new_data = metadata_train2_new, 
  composition = "matrix"
)
#head(final_resample_data_prep)

meta_shap.new2<- shapviz(extract_fit_engine(xgb.final.metadata.fit.new2),
                         X_pred = meta_data_bake2, 
                         X = metadata_train2_new)


saveRDS(meta_shap.new2,"meta_shap.new2.rds")
meta_shap.new2=readRDS("meta_shap.new2.rds")



meta_shap_column_names2=c("Captive","Edge weight","Interaction type",    
                          "Data collection","Data duration (days)",  
                          "Class","Time resolution (secs)")


names(meta_shap.new2)[names(meta_shap.new2)=="Class_1"]<-"Erdös-Rényi"
names(meta_shap.new2)[names(meta_shap.new2) == "Class_1"] <- "Erdös-Rényi"
names(meta_shap.new2)[names(meta_shap.new2) == "Class_2"] <- "Stochastic-Block-Model"
names(meta_shap.new2)[names(meta_shap.new2) == "Class_3"] <- "Scale-Free"
names(meta_shap.new2)[names(meta_shap.new2) == "Class_4"] <- "Spatial"
names(meta_shap.new2)[names(meta_shap.new2) == "Class_5"] <- "Small-World"

colnames(meta_shap.new2$`Erdös-Rényi`$X)=meta_shap_column_names2
colnames(meta_shap.new2$`Erdös-Rényi`$S)=meta_shap_column_names2
#colnames(meta_shap.new$`Erdös-Rényi`$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new2$`Stochastic-Block-Model`$X)=meta_shap_column_names2
colnames(meta_shap.new2$`Stochastic-Block-Model`$S)=meta_shap_column_names2
#colnames(meta_shap.new$`Stochastic-Block-Model`$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new2$`Scale-Free`$X)=meta_shap_column_names2
colnames(meta_shap.new2$`Scale-Free`$S)=meta_shap_column_names2
#colnames(meta_shap.new$`Scale-Free`$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new2$Spatial$X)=meta_shap_column_names2
colnames(meta_shap.new2$Spatial$S)=meta_shap_column_names2
#colnames(meta_shap.new$Spatial$S_inter)=meta_shap.new_column_names
colnames(meta_shap.new2$`Small-World`$X)=meta_shap_column_names2
colnames(meta_shap.new2$`Small-World`$S)=meta_shap_column_names2
#colnames(meta_shap.new$`Small-World`$S_inter)=meta_shap.new_column_names
saveRDS(meta_shap.new2,"final_meta_shap.new2.rds")
final_meta_shap.new2=readRDS("final_meta_shap.new2.rds")


############################################################
# Shapely variable importance and summary plots
############################################################
###########################################################
sp.meta.shap.imp.bar2=sv_importance(final_meta_shap.new2$Spatial, kind = "bar",bee_width = 0.3)#,
#show_numbers = TRUE, bee_width = 0.2)
#saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
sp.meta.shap.imp.bar.plot2=sp.meta.shap.imp.bar2+
  ggtitle(" ")+
  xlab(" ") +
  # xlim(-0.3,0.5)+
  theme_bw()+
  theme(axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),  # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.x = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),  # Remove y-axis labels
        axis.ticks.x = element_blank(),  # Remove y-axis ticks
        axis.line = element_blank(),
        plot.margin=unit(c(-1,-1,0.5,-1.1), "cm"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

sp.meta.shap.imp.bar.plot2
ggsave("bar2.svg", width=12, height=10)

sp.meta.shap.beeswarm.imp2=sv_importance(final_meta_shap.new2$Spatial, 
                                         kind = "beeswarm",bee_width = 0.5)#,
#show_numbers = TRUE, bee_width = 0.2)
#saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
sp.meta.shap.imp.beeswarm.plot2=sp.meta.shap.beeswarm.imp2+
  ggtitle("")+
  xlab("SHAP Value")+
  # xlim(-0.3,0.7)+
  theme_bw()+
  theme(plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #        plot.margin=unit(c(1,-1,0.5,0), "cm"),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))+
  scale_color_gradient(low = "blue", high = "red")

sp.meta.shap.imp.beeswarm.plot2
ggsave("bees2.svg", width=12, height=10)

figure1=sp.meta.shap.imp.beeswarm.plot2+
  theme(legend.position="none")

figure2=sp.meta.shap.imp.bar.plot2
sp.meta.combine.var.imp2=plot_grid(figure1, figure2, align = "h", ncol = 2)
# Get the legend from one of the plots
legend <- get_legend(sp.meta.shap.imp.beeswarm.plot2)

# Add the legend to the combined plot
combined_plot_with_legend.sp2 <- plot_grid(sp.meta.combine.var.imp2, 
                                           legend, ncol = 1, rel_heights = c(1, 0.2))

combined_plot_with_legend.sp2

# combined_plot_with_legend.sp2=combined_plot_with_legend.sp2+
#   plot_layout(widths = 1.2,heights = 1.2)

ggsave("sp_shap_varimp2.png", width = 14, height = 7)


###----combine shapely summary plot for spatial with and without species----####
spatial.combine.shapsummary.plot <- ggarrange(combined_plot_with_legend.sp,
                                              ggarrange(combined_plot_with_legend.sp2,
                                                        align = "v",widths = c(1,1),
                                                        heights = c(1,1)),
                                              nrow = 2,
                                              labels = c("A)","B)"),
                                              hjust = -5.9,#-3.1,
                                              vjust = 1.2,
                                              font.label = list(size = 19, color = "black", face = "bold",
                                                                family = "serif"))
spatial.combine.shapsummary.plot
ggsave("spatial.combine.shapsummary.meta.png", width = 14, height = 14)


####################################################################################
# Waterfall plots
####################################################################################
sp.meta.waterfall2=sv_waterfall(final_meta_shap.new2$Spatial, row_id = 150,
                                size = 18,fill_colors = c("red","blue"),
                                show_connection = T,
                                show_annotation = F,
                                annotation_size = 6)

sp.meta.waterfall2=sp.meta.waterfall2+
  ggtitle(" ")+
  #xlab(" Graph_energy ")+
  # xlim(NA,2.24)+
  theme_bw()+
  theme(plot.title = element_text(size = 32),
        text = element_text(size = 32,family="serif"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

sp.meta.waterfall2=sp.meta.waterfall2+plot_layout(widths = 1.2,heights = 1.2)
ggsave("sp.meta.waterfall2.svg", width = 12, height=12)

###----Combined waterfall plot-----
waterfall.meta=ggarrange(sp.meta.waterfall,
                         ggarrange(sp.meta.waterfall2,
                                   align = "h",widths = c(1,1),
                                   heights = c(1,1)),
                         nrow = 1,
                         labels = c("A)","B)"),
                         hjust = -10.9,#-3.1,
                         vjust = 1.2,
                         font.label = list(size = 19, color = "black", face = "bold",
                                           family = "serif"))

waterfall.meta
ggsave("waterfall.meta.png", width = 20, height=6)
ggsave("waterfall.meta.pdf", width = 15, height=6)



###############################################################################
#Shapely dependency plots without species
###############################################################################
data_collectn2.dep.sp=sv_dependence(final_meta_shap.new2$Spatial, 
                                    "Data collection",color_var = NULL)

saveRDS(data_collectn2.dep.sp,"data_collectn2.dep.sp.rds")

data_collectn2.dep.sp=readRDS("data_collectn2.dep.sp.rds")
data_collectn2.dep.sp.df=data_collectn2.dep.sp$data

data_collectn2.dep.sp.df$`Data collection`=gsub("_", " ",
                                                data_collectn2.dep.sp.df$`Data collection`)

data_collectn2.dep.sp.df$`Data collection`=sub("^([a-z])", "\\U\\1",data_collectn2.dep.sp.df$`Data collection`, perl = TRUE)

colnames(data_collectn2.dep.sp.df)=c("shap","Data_collection")

data_collectn2.dep.sp.df.plot=
  ggplot(data_collectn2.dep.sp.df, aes(x = reorder(Data_collection,
                                                   shap), y = shap)) +
  geom_bar(stat = "identity",color = "black", fill = "black")+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Data collection")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
    strip.text = element_text(size = 32),
    plot.title = element_text(size = 32),
    text = element_text(size = 32,family="serif"),
    axis.text.y = element_text(size = 32),
    axis.text.x = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())

data_collectn2.dep.sp.df.plot=data_collectn2.dep.sp.df.plot+plot_layout(widths = 1.2,heights = 1.2)
data_collectn2.dep.sp.df.plot
ggsave("data_collectn2.dep.png", width = 12, height=12)
ggsave("data_collectn2.dep.svg", width = 12, height=12)


####----shapely dependency for data duration
data_duratn2.dep.sp=sv_dependence(final_meta_shap.new2$Spatial, 
                                  "Data duration (days)",color_var = NULL)

saveRDS(data_duratn2.dep.sp,"data_duratn2.dep.sp.rds")

data_duratn2.dep.sp=readRDS("data_duratn2.dep.sp.rds")
data_duratn2.dep.sp.df=data_duratn2.dep.sp$data


colnames(data_duratn2.dep.sp.df)=c('shap','Data_duration')


data_duratn2.dep.sp.df.plot=
  ggplot(data_duratn.dep.sp.df, aes(x = Data_duration, y = shap)) +
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  geom_point()+
  ggtitle("Data duration (days)")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
    strip.text = element_text(size = 32),
    plot.title = element_text(size = 32),
    text = element_text(size = 32,family="serif"),
    axis.text.y = element_text(size = 32),
    axis.text.x = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())

data_duratn2.dep.sp.df.plot=data_duratn2.dep.sp.df.plot+plot_layout(widths = 1.2,heights = 1.2)
data_duratn2.dep.sp.df.plot

ggsave("data_duratn.dep.png", width = 12, height=12)
ggsave("data_duratn.dep.svg", width = 12, height=12)

####----shapely dependency for data duration
edgeweight2.dep.sp=sv_dependence(final_meta_shap.new2$Spatial, 
                                 "Edge weight",color_var = NULL)

saveRDS(edgeweight2.dep.sp,"edgeweight2.dep.sp.rds")

edgeweight2.dep.sp=readRDS("edgeweight2.dep.sp.rds")
edgeweight2.dep.sp.df=edgeweight2.dep.sp$data


edgeweight2.dep.sp.df$`Edge weight`=gsub("_", " ",
                                         edgeweight2.dep.sp.df$`Edge weight`)

edgeweight2.dep.sp.df$`Edge weight`=sub("^([a-z])", "\\U\\1",edgeweight2.dep.sp.df$`Edge weight`, perl = TRUE)


colnames(edgeweight2.dep.sp.df)=c("shap", "Edge_weight")

edgeweight2.dep.sp.df.plot=
  ggplot(edgeweight2.dep.sp.df, aes(x = reorder(Edge_weight,-shap), y = shap)) +
  geom_bar(stat = "identity",color = "black", fill = "black")+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Edge weight")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
    strip.text = element_text(size = 32),
    plot.title = element_text(size = 32),
    text = element_text(size = 32,family="serif"),
    axis.text.y = element_text(size = 32),
    axis.text.x = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())

edgeweight2.dep.sp.df.plot=edgeweight2.dep.sp.df.plot+plot_layout(widths = 0.2,heights = 0.2)
edgeweight2.dep.sp.df.plot
ggsave("edgeweight2.dep.png", width = 12, height=12)
ggsave("edgeweight2.dep.svg", width = 12, height=12)


###----interactn shapely depedency plot----###
timeres2.dep.sp=sv_dependence(final_meta_shap.new2$Spatial, 
                              "Time resolution (secs)",color_var = NULL)

saveRDS(timeres2.dep.sp,"timeres.dep.sp.rds")

timeres2.dep.sp=readRDS("timeres.dep.sp.rds")

timeres2.dep.sp.df=timeres2.dep.sp$data

timeres2.dep.sp.df$`Time resolution (secs)`=gsub("_", " ",timeres2.dep.sp.df$`Time resolution (secs)`)

timeres2.dep.sp.df$`Time resolution (secs)`=sub("^([a-z])", "\\U\\1", timeres2.dep.sp.df$`Time resolution (secs)`, perl = TRUE)

colnames(timeres2.dep.sp.df)=c("shap","time_resolution")

timeres2.dep.sp.df.plot=
  ggplot(timeres2.dep.sp.df, aes(x =reorder(time_resolution,-shap), y = shap)) +
  geom_bar(stat = "identity",color = "black", fill = "black")+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "solid", color = "red")+
  # geom_smooth(method = "auto", se = FALSE,stat = "smooth",
  #             position = "identity",color="red")+
  ggtitle("Time resolution (secs)")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 60, hjust = 1,size=18),
    strip.text = element_text(size = 32),
    plot.title = element_text(size = 32),
    text = element_text(size = 32,family="serif"),
    axis.text.y = element_text(size = 32),
    axis.text.x = element_text(size = 32),
    axis.title.x = element_text(size = 32),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank())

timeres2.dep.sp.df.plot=timeres2.dep.sp.df.plot+plot_layout(widths = 1.2,heights = 1.2)
timeres2.dep.sp.df.plot







meta.dep.plot1=ggarrange(
  interactn.dep.sp.df.plot,
  ggarrange(data_collectn2.dep.sp.df.plot,
            align = "v",widths = c(1,1),
            heights = c(1,1)),
  nrow = 2,
  labels = c("A)","B)"),
  hjust = -1.4,#-3.1,
  vjust = 1.2,
  font.label = list(size = 19, color = "black", face = "bold",
                    family = "serif"))


meta.dep.plot2=ggarrange(
  data_duratn2.dep.sp.df.plot,
  ggarrange(captive2.dep.sp.df.plot,
            align = "h", heights = c(0.3, 0.5)),
  ncol = 2,
  labels = c("C)","D)"),
  hjust = -1.4,#-3.1,
  vjust = 1,
  font.label = list(size = 19, color = "black", face = "bold",
                    family = "serif"))

metaplot2=plot_grid(meta.dep.plot1,meta.dep.plot2,nrow = 2,rel_heights = c(1.3,0.4,0.2))
metaplot2
ggsave("meta.dep.plot2.png", width = 12, height = 14)

############################################################################
#Shapely interaction without species
#############################################################################

meta_shap_interactn2.new<- shapviz(extract_fit_engine(xgb.final.metadata.fit.new2),
                                   X_pred = meta_data_bake2, 
                                   X = metadata_train2_new,
                                   interactions = TRUE)

# sv_interaction(meta_shap_interactn2.new, kind = "no")
saveRDS(meta_shap_interactn2.new,"meta_shap_interactn2.new.rds")
meta_shap_interactn2.new=readRDS("meta_shap_interactn2.new.rds")

names(meta_shap_interactn2.new)[names(meta_shap_interactn2.new) == "Class_1"] <- "Erdös-Rényi"
names(meta_shap_interactn2.new)[names(meta_shap_interactn2.new) == "Class_2"] <- "Stochastic-Block-Model"
names(meta_shap_interactn2.new)[names(meta_shap_interactn2.new) == "Class_3"] <- "Scale-Free"
names(meta_shap_interactn2.new)[names(meta_shap_interactn2.new) == "Class_4"] <- "Spatial"
names(meta_shap_interactn2.new)[names(meta_shap_interactn2.new) == "Class_5"] <- "Small-World"


colnames(meta_shap_interactn2.new$"Erdös-Rényi"$X)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Erdös-Rényi"$S)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Erdös-Rényi"$S_inter)=meta_shap_column_names2
dimnames(meta_shap_interactn2.new$"Erdös-Rényi"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn2.new$"Erdös-Rényi"$S_inter)[[2]])

colnames(meta_shap_interactn2.new$"Stochastic-Block-Model"$X)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Stochastic-Block-Model"$S)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Stochastic-Block-Model"$S_inter)=meta_shap_column_names2
dimnames(meta_shap_interactn2.new$"Stochastic-Block-Model"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn2.new$"Stochastic-Block-Model"$S_inter)[[2]])

colnames(meta_shap_interactn2.new$"Scale-Free"$X)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Scale-Free"$S)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Scale-Free"$S_inter)=meta_shap_column_names2
dimnames(meta_shap_interactn2.new$"Scale-Free"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn2.new$"Scale-Free"$S_inter)[[2]])

colnames(meta_shap_interactn2.new$"Spatial"$X)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Spatial"$S)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Spatial"$S_inter)=meta_shap_column_names2
dimnames(meta_shap_interactn2.new$"Spatial"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn2.new$"Spatial"$S_inter)[[2]])

colnames(meta_shap_interactn2.new$"Small-World"$X)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Small-World"$S)=meta_shap_column_names2
colnames(meta_shap_interactn2.new$"Small-World"$S_inter)=meta_shap_column_names2
dimnames(meta_shap_interactn2.new$"Small-World"$S_inter)[[3]]=
  c(dimnames(meta_shap_interactn2.new$"Small-World"$S_inter)[[2]])


meta_shap_interactn2.new$Spatial$X$`Time resolution (secs)`=gsub("_", " ",meta_shap_interactn2.new$Spatial$X$`Time resolution (secs)`)
sub("^([a-z])", "\\U\\1",meta_shap_interactn2.new$Spatial$X$`Time resolution (secs)`, perl = TRUE)

meta_shap_interactn2.new$Spatial$X$Captive=gsub("_", " ",meta_shap_interactn2.new$Spatial$X$Captive)
sub("^([a-z])", "\\U\\1",meta_shap_interactn2.new$Spatial$X$Captive, perl = TRUE)

meta_shap_interactn2.new$Spatial$X$`Interaction type`=gsub("_", " ",meta_shap_interactn2.new$Spatial$X$`Interaction type`)
sub("^([a-z])", "\\U\\1",meta_shap_interactn2.new$Spatial$X$`Interaction type`, perl = TRUE)

meta_shap_interactn2.new$Spatial$X$`Data collection`=gsub("_", " ",meta_shap_interactn2.new$Spatial$X$`Data collection`)
sub("^([a-z])", "\\U\\1",meta_shap_interactn2.new$Spatial$X$`Data collection`, perl = TRUE)

meta_shap_interactn2.new$Spatial$X$`Edge weight`=gsub("_", " ",meta_shap_interactn2.new$Spatial$X$`Edge weight`)
sub("^([a-z])", "\\U\\1",meta_shap_interactn2.new$Spatial$X$`Edge weight`, perl = TRUE)


saveRDS(meta_shap_interactn2.new,"final_meta_shap.interactn2.rds")

final_meta_shap.interactn2=readRDS("final_meta_shap.interactn2.rds")


meta.top7.shap.interactn2.plot=
  sv_interaction(final_meta_shap.interactn2$Spatial,
                 max_display = 7 , size = 4,
                 alpha = 0.3,
                 bee_width = 0.8,
                 bee_adjust = 0.5)+
  theme_bw()+
  theme(plot.title = element_text(size = 36),
        strip.background =element_rect(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y.left= element_text(angle =0),
        text = element_text(size = 36,family="serif"),
        axis.text.x = element_text(size = 36),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y =element_blank(), 
        #axis.ticks.length.y.left=element_blank(),
        axis.line.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #  panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction='horizontal',
        legend.key.width = unit(4, "cm"))+
  scale_color_gradient(low = "blue", high = "red") 

meta.top7.shap.interactn2.plot
meta.top7.shap.interactn2.plot=meta.top7.shap.interactn2.plot+
  scale_x_continuous(limits = c(-2, 1.5), breaks = c(-1,0.0,1))
ggsave("meta.top7.shap.interactn2.without.species.plot.png",
       width = 30, height = 16)

# ggsave("meta.top7.shap.interactn2.without.species.plot.pdf",
#        width = 30, height = 16)


##########----2D----Interaction Plot
meta.shap.2Ddep1=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Data collection", 
                                 y = c("Captive")) & 
  ggtitle("Data collection")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
   legend.position='bottom', legend.direction='horizontal',
   legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep1
ggsave("datacollectn_and_captive.svg",width=18,height=16)



meta.shap.2Ddep2=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Data collection", 
                                 y = c("Interaction type")) & 
  ggtitle("Data collection")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
  legend.position='bottom', legend.direction='horizontal',
   legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep2
ggsave("datacollectn_and_interactn.svg",width=18,height=16)


meta.shap.2Ddep3=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Data collection", 
                                 y = c("Edge weight")) & 
  ggtitle("Data collection")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
  legend.position='bottom', legend.direction='horizontal',
  legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep3
ggsave("datacollectn_and_edgeweight.svg",width=18,height=16)


meta.shap.2Ddep4=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Data collection", 
                                 y = c("Time resolution (secs)")) & 
  ggtitle("Data collection")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
  legend.position='bottom', legend.direction='horizontal',
   legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep4
ggsave("datacollectn_and_timeressecs.svg",width=18,height=16)




meta.shap.2Ddep5=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Data duration (days)", 
                                 y = c("Captive")) & 
  ggtitle("Data duration (days)")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep5
ggsave("Data duration_and_captive2.svg",width=18,height=16)




meta.shap.2Ddep6=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Data duration (days)", 
                                 y = c("Edge weight")) & 
  ggtitle("Data duration (days)")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep6
ggsave("Data duration_and_edgeweight2.svg",width=18,height=16)



meta.shap.2Ddep7=sv_dependence2D(final_meta_shap.interactn2$Spatial, 
                                 x ="Time resolution (secs)", 
                                 y = c("Captive")) & 
  ggtitle("Time resolution (secs)")&
  geom_hline(yintercept = 0, linetype = "solid", color = "red")&
  coord_flip()&
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=30),
        strip.text = element_text(size = 30),
        plot.title = element_text(size = 30),
        text = element_text(size = 30,family="serif"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position='bottom', legend.direction='horizontal',
        legend.key.width = unit(2, "cm"))&
  scale_color_gradient(low = "blue", high = "red") #& scale_color_continuous()

meta.shap.2Ddep7
ggsave("Timeresolution_captive.svg",width=18,height=16)






meta.shap.2Ddep=plot_grid(meta.shap.2Ddep1,meta.shap.2Ddep2,
                          meta.shap.2Ddep3,meta.shap.2Ddep4,nrow=2,ncol=2)

ggsave("meta.shap.2Ddep2.png", width = 12, height = 12)














library(hstats)

df.final=readRDS("df.final.rds")
final.fitted.model = readRDS("final.fitted.model.new.rds")

df1=df.final%>%
  select(-graph_name)

#x = setdiff(colnames(df1), "graph_name")
#model = extract_workflow(final.fitted.model)    
#s = hstats(model, v = x, X = df1, type = "prob")
#plot(s, which = 1:3, normalize = FALSE)

set.seed(1486)
x1 = setdiff(colnames(df.final), "graph_name")
model1 = extract_workflow(final.fitted.model) 
H=hstats(model1, v = x1, n_max = 30, X = df.final, type = "prob")
saveRDS(H,"H-stats.rds")
H=readRDS("H-stats.rds")
plot(H, which = 1:3, normalize = FALSE)

h2_pairwise(H, normalize = FALSE, squared = FALSE, top_m = 10, plot = TRUE)
plot(H, which = 1:3, normalize = F, squared = F, facet_scales = "free_y", ncol = 1)


###----Overall Interactions
# hplot=plot(H,which = 1, normalize = FALSE, squared = FALSE) +
#   ggtitle("Overall") +
#   
hplot=h2_overall(H, normalize = FALSE, squared = FALSE,
                 top_m = 8, plot = TRUE)+
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 18,family="serif"),
        legend.title = element_text(size = 18,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

hplot + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                      "Scale-Free","Spatial","Small-World"))

ggsave("hstat_overall2.svg", width = 20,height = 15)



###----Pairwise Interactions
# hplot2=plot(H,which = 2, normalize = FALSE, squared = FALSE) +

hplot2=h2_pairwise(H, normalize = FALSE, squared = FALSE,
                   top_m = 10, plot = TRUE)+  
  ggtitle("Pairwise") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 18,family="serif"),
        legend.title = element_text(size = 18,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

hplot2 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                       "Scale-Free","Spatial","Small-World"))

ggsave("hstat_pairwise2.svg", width = 20,height = 15)



###----Pairwise Interactions
hplot3=plot(H,which = 3, normalize = FALSE, squared = FALSE) +
  ggtitle("Threeway") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 14,family="serif"),
        axis.text.x = element_text(size = 14,family="serif"),
        axis.text.y = element_text(size = 14,family="serif"),
        axis.title = element_text(size = 14,family="serif"),
        text = element_text(size = 14,family="serif"),
        legend.text = element_text(size = 14,family="serif"),
        legend.title = element_text(size = 14,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

hplot3 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                       "Scale-Free","Spatial","Small-World"))

ggsave("hstat_threeway.svg", width = 22,height = 15)







##----without species
df_final_meta_data=readRDS("df_final_meta_data.rds")

df_final_meta_data_new=df_final_meta_data%>%
  filter(target=="Spatial")

xgb.final.metadata.fit.new=readRDS("xgb.final.metadata.fit.new.rds")
#extract workflow from fitted model
xgb.final.metadata.model.new=extract_workflow(xgb.final.metadata.fit.new)
set.seed(1486)
m1 = setdiff(colnames(df_final_meta_data), "target")
meta_model1 = extract_workflow(xgb.final.metadata.fit.new) 
meta_interactn1=hstats(meta_model1, v = m1, n_max = 300, X = df_final_meta_data, type = "prob")
saveRDS(meta_interactn1,"meta_interactn1-hstats.rds")
meta_interactn1=readRDS("meta_interactn1-hstats.rds")

h2_pairwise(meta_interactn1, normalize = FALSE, squared = FALSE,
            top_m = 5, plot = TRUE)


###----Model with species: Overall Interactions
# meta_h1_plot1=plot(meta_interactn1, which = 1, normalize = FALSE,
#      squared = F, facet_scales = "free_y", ncol = 1)+

meta_h1_plot1= h2_overall(meta_interactn1, normalize = FALSE, squared = FALSE,
                          top_m = 8, plot = TRUE)  
ggtitle("Overall") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 14,family="serif"),
        legend.title = element_text(size = 14,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

meta_h1_plot1 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                              "Scale-Free","Spatial","Small-World"))

ggsave("hstat_overall_species.svg", width = 20,height = 15)


###----Model with species: Pairwise Interactions
# meta_h1_plot2=plot(meta_interactn1, which = 2, normalize = FALSE,
#                    squared = F, facet_scales = "free_y", ncol = 1)+

meta_h1_plot2= h2_pairwise(meta_interactn1, normalize = FALSE, squared = FALSE,
                           top_m = 10, plot = TRUE)+ 
  
  ggtitle("Pairwise") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 18,family="serif"),
        legend.title = element_text(size = 18,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

meta_h1_plot2 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                              "Scale-Free","Spatial","Small-World"))

ggsave("hstat_pairwise_species2.svg", width = 20,height = 15)


###----Model with species: Threeway Interactions
meta_h1_plot3=plot(meta_interactn1, which = 3, normalize = FALSE,
                   squared = F, facet_scales = "free_y", ncol = 1)+
  ggtitle("Pairwise") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 14,family="serif"),
        legend.title = element_text(size = 14,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

meta_h1_plot3 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                              "Scale-Free","Spatial","Small-World"))

ggsave("hstat_threeway_species.svg", width = 20,height = 15)


##----with species
df_final_meta_data2=df_final_meta_data%>%
  select(-species)

xgb.final.metadata.fit.new2=readRDS("xgb.final.metadata.fit.new2.rds")
#extract workflow from fitted model
xgb.final.metadata.model.new2=extract_workflow(xgb.final.metadata.fit.new2)
set.seed(1486)
m2 = setdiff(colnames(df_final_meta_data2), "target")
meta_model2 = extract_workflow(xgb.final.metadata.fit.new2) 
meta_interactn2=hstats(meta_model2, v = m2, n_max = 300, X = df_final_meta_data2, type = "prob")
saveRDS(meta_interactn2,"meta_interactn2-hstats.rds")
meta_interactn2=readRDS("meta_interactn2-hstats.rds")


###----Model without species: Overall Interactions
meta_h2_plot1=plot(meta_interactn2, which =1, normalize = FALSE,
                   squared = F, facet_scales = "free_y", ncol = 1)+
  ggtitle("Overall") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 18,family="serif"),
        legend.title = element_text(size = 18,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

meta_h2_plot1 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                              "Scale-Free","Spatial","Small-World"))

ggsave("hstat_overall_no_species.svg", width = 20,height = 15)


###----Model without species: Pairwise Interactions
meta_h2_plot2=plot(meta_interactn2, which =2, normalize = FALSE,
                   squared = F, facet_scales = "free_y", ncol = 1)+
  ggtitle("Pairwise") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 18,family="serif"),
        legend.title = element_text(size = 18,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

meta_h2_plot2 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                              "Scale-Free","Spatial","Small-World"))

ggsave("hstat_pairwise_no_species.svg", width = 20,height = 15)



###----Model without species: Threeway Interactions
meta_h2_plot3=plot(meta_interactn2, which =3, normalize = FALSE,
                   squared = F, facet_scales = "free_y", ncol = 1)+
  ggtitle("Threeway") +
  theme_bw()+
  theme(strip.background =element_rect(),
        plot.title = element_text(size = 18,family="serif"),
        axis.text.x = element_text(size = 18,family="serif"),
        axis.text.y = element_text(size = 18,family="serif"),
        axis.title = element_text(size = 18,family="serif"),
        text = element_text(size = 18,family="serif"),
        legend.text = element_text(size = 18,family="serif"),
        legend.title = element_text(size = 18,family="serif"),
        # legend.title = "Networks",
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.y = element_blank(),
        strip.background.x = element_blank())

meta_h2_plot3 + scale_fill_discrete(name= "Networks",labels=c("Erdös-Rényi","Stochastic-Block-Model",
                                                              "Scale-Free","Spatial","Small-World"))

ggsave("hstat_threeway_no_species.svg", width = 20,height = 15)











# strip.text = element_text(size = 17),
#  plot.title = element_text(size = 32),
# text = element_text(size = 17,family="serif"),
#  axis.text.y = element_text(size = 17),
# axis.text.x = element_text(size = 32),
#   axis.title.x = element_text(size = 32),
#    axis.line = element_line(colour = "black"),
#  panel.grid.major = element_blank(),
#  panel.grid.minor = element_blank(),
#  panel.background = element_blank())
#   panel.border = element_blank())
# scale_fill_viridis_d(begin = 0.1, end = 0.9)

#h2_overall(H)
#h2_pairwise(H)

# ice(model1, v = "transitivity", X = df.final, BY = "deg_centr", n_max = 150) |>
#   plot(center = TRUE) +
#   ggtitle("Centered ICE plots")

# Permutation importance 
# perm_importance(model1,v=x1, X = data.matrix(df.final[-19]), y = df.final$graph_name, loss = "mlogloss")
# 
# pd <- partial_dep(model1, v = c("eigen_centr", 
#                                 "deg_centr"), 
#                   X = df1, grid_size = 1000)
#plot(pd)






#x=colnames(df1)

#X_train=data.matrix(df1)


# model=extract_workflow(final.fitted.model)
# fit=model$fit$fit$fit
# 
# set.seed(782)
# system.time(
#   s <- hstats(fit, v = x, X = X_train,type = "response",
#               n_max = 10L,
#               pred_fun = stats::predict,
#               w = NULL,
#               verbose = FALSE)
# )


######testing
# colnames(miami) <- tolower(colnames(miami))
# miami$log_ocean <- log(miami$ocean_dist)
# x <- c("log_ocean", "tot_lvg_area", "lnd_sqfoot", "structure_quality", "age", "month_sold")
# set.seed(1)
# ix <- sample(nrow(miami), 0.8 * nrow(miami))
# 
# y_train <- log(miami$sale_prc[ix])
# y_valid <- log(miami$sale_prc[-ix])
# X_train <- data.matrix(miami[ix, x])
# colnames(X_train)
# X_valid <- data.matrix(miami[-ix, x])
# 
# dtrain <- xgb.DMatrix(X_train, label = y_train)
# dvalid <- xgb.DMatrix(X_valid, label = y_valid)
# 
# # Fit via early stopping
# fit2 <- xgb.train(
#   params = list(learning_rate = 0.15, objective = "reg:squarederror", max_depth = 5),
#   data = dtrain,
#   watchlist = list(valid = dvalid),
#   early_stopping_rounds = 20,
#   nrounds = 1000,
#   callbacks = list(cb.print.evaluation(period = 100))
# )
# 
# fit$feature_names
# 
# set.seed(782)
# system.time(
#   s <- hstats(fit, v = x, X = X_train)
# )
# s
# # Proportion of prediction variability unexplained by main effects of v
# # [1] 0.10
# 
# plot(s)

