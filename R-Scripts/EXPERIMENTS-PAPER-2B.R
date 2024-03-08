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
})

library(iml)
library(themis)
#library(recipeselectors)
library(shapr)
library(Boruta)
library(janitor)
library(tidyverse)
library(tidymodels)
library(shapr)
library(shapviz)
library(SHAPforxgboost)
library(mlbench)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(patchwork)

plan(multisession)


####################################################################
# Read Feature Engineering Data
####################################################################
boruta.train=readRDS("boruta.train.upsample.rds")
#ggsave("boruta_feat_imp.png")
boruta.df.top=readRDS("boruta.df.top.features")
# saveRDS(boruta.df.top,"boruta.df.top.features")
#----final data after performing boruta feature engineering
df.final=readRDS("df.final.rds")
colSums(is.na(df.final))
################################################################
# Pre processing final data for model
################################################################
#----final data recipe
df.final.rec=readRDS("df.final.rec.rds")
df.final.prep=readRDS("df.final.prep.rds")
##############################################################################
# MODELS
##############################################################################
set.seed(384)
#----final sample and split
df.final.split=df.final[sample(1:nrow(df.final)), ]%>%
  initial_split(prop = 0.7)
df.final.split=readRDS("df.final.split.rds")
df.final.train=readRDS("df.final.train.rds")
df.final.test=readRDS("df.final.test.rds")
#----Print number of training and testing set
cat("training set:", nrow(df.final.train), "\n",
    "testing set :", nrow(df.final.test), "\n")
#------cross validation----
df.cv.splits=readRDS("df.cv.splits.rds")

#################################################################
# classification models
#################################################################
#TrainedModelslist=readRDS("trained_models_list_anova_downsample.rds")
TrainedModelslist=readRDS("trained_models_list_anova_upsample.rds")

TrainedModelslist %>%
  map(show_best, metric = "accuracy", n = 1)
#----extracting the best model
best.model <- TrainedModelslist[[2]]
best.model %>%
  collect_metrics()
#----plot of best metrics
#accuracy
best.model %>%
  show_best(metric = "accuracy")
autoplot(best.model)
#----roc
best.model %>%
  show_best(metric = "roc_auc")
#----best roc_auc
best_auc <- select_best(best.model, "roc_auc")
best_auc
#----best model specs
best.model.specs <-
  best.model %>%
  select_best(metric = "accuracy")
best.model.specs

##----Final fitted model----##
final.fitted.model = readRDS("final.fitted.model.new.rds")

extract_workflow(final.fitted.model)#extract workflow
extract.fit.engine=extract_fit_engine(final.fitted.model)

#########################################################################################
# Explore final fitted model results
#########################################################################################
final.fitted.model%>%
  collect_metrics()

final.fitted.model%>%
  collect_predictions()


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

###----Extract final fitted model----###
##----Final fitted model----##
#final.fitted.model = readRDS("final.fitted.model.rds")
final.fitted.model = readRDS("final.fitted.model.rds")
extract_final.fitted.model=readRDS("extract_final.fitted.model.rds")
final.model.predictions=readRDS("final.model.predictions.rds")
final.model.predictions

#----accuracy of best fitted model
final.model.predictions %>%
  accuracy(graph_name, .pred_class)
#------confusion matrix
final.model.predictions %>%
  conf_mat(graph_name, .pred_class)

final.model.predictions%>%
  conf_mat(truth = graph_name,estimate =.pred_class)%>%
  autoplot(type="heatmap")
#-----Statistical summary of confusion matrix----###
Accuracy=yardstick::accuracy(data=final.model.predictions,truth = graph_name,estimate =.pred_class)
Precision=yardstick::precision(data=final.model.predictions,truth = graph_name,estimate =.pred_class)
Recall=yardstick::recall(data=final.model.predictions,truth = graph_name,estimate =.pred_class)
Accuracy%>%
  bind_rows(Precision)%>%
  bind_rows(Recall)

#########################################################################################
# Intepretable machine learning
#########################################################################################
#----Feature importance
feature.importance=readRDS("feature.iml.importance.rds")
features=feature.importance$results$feature
#----Iml plot
iml.var.imp.plot=readRDS("iml.var.imp.plot.rds")
iml.var.imp.plot

#####################################################################################
# LOCAL FEATURE EFFECTS: Accumulated Local Effects (ALE) PLOT
#####################################################################################
# best.model.pred.class=readRDS("best.model.pred.class.rds")
# predictorfunc=readRDS("predictorfunc.rds")
#----Predictor function
predictor=readRDS("predictor.rds")  
##----Interaction betwen all variables
VarInterctns<- Interaction$new(predictorfunc)

VarInterctns+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

VarInterctns
ggsave("VarInterctns.png", width = 10, height = 14)
#interact <- Interaction$new(predictor, feature = "crim")

##----Feature effects for all feature at once
# feat.effects <- FeatureEffects$new(best.model.pred.class)
# feat.effects 

###########################################################################################
# ALE ANALYSIS
############################################################################################
extract_final.fitted.model=readRDS("extract_final.fitted.model.rds")
#model=extract_fit_engine(final.fitted.model) #model
model=extract_final.fitted.model

x.data <- df.final %>%
  dplyr::select(-c(graph_name)) %>%
  as.data.frame()

y.data <- df.final%>%
  dplyr::select(graph_name)

#prediction function
predict.wrapper <- function(model, newdata){
  workflows:::predict.workflow(object = model, new_data = newdata)
}
##The predictor function enables to interrogate or get explanation of the data
predictorfunc <- Predictor$new(
  model =model,
  data =  x.data,
  y = y.data,
  predict.function = predict.wrapper
)

#saveRDS(predictorfunc,"predictorfunc.rds")

predictor <- Predictor$new(predictorfunc$model,
                           data = x.data,
                           type = "prob")

################################################################
#----Modularity Ale Plot----
################################################################
modularity.df=readRDS("modularity.df.rds")#read in data frame

modularity.plot=
  ggplot(modularity.df, aes(x = modularity, 
                            y = scale(.value,center = FALSE, scale = 0.001))) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Modularity") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

modularity.plot

#####################################################################
#----Transitivity Ale Plot----
#####################################################################
transitivity.df=readRDS("transitivity.df.rds")#read in data frame

transitivity.plot=
  ggplot(transitivity.df, aes(x = transitivity, 
                              y = .value)) +
  geom_line(stat = "identity") +
  geom_hline(yintercept=0)+
  #geom_vline(xintercept=0)+
  facet_grid(~.class) +
  #theme(strip.text = element_text(size = 12))+
  xlab("Transitivity") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

transitivity.plot

#####################################################################
#----Normalized_fiedler_value----
#####################################################################
normalized_fiedler_value.df=readRDS("normalized_fiedler_value.df.rds")#read in data frame

#----Normalized fiedler plot
normalized_fiedler_value.plot=
  ggplot(normalized_fiedler_value.df, aes(x = normalized_fiedler_value, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept=0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Normalized fiedler value") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")


normalized_fiedler_value.plot

#####################################################################
#----Degree assortativity plot
#####################################################################
deg_assort_coef.df=readRDS("deg_assort_coef.df.rds")#read in data frame

deg_assort_coef.plot=
  ggplot(deg_assort_coef.df, aes(x = deg_assort_coef, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Degree assortativity coeff") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

deg_assort_coef.plot


#####################################################################
#----degree cenraility plot----####
#####################################################################
deg_centr.df=readRDS("deg_centr.df.rds")#read in data frame

deg_centr.plot=
  ggplot(deg_centr.df, aes(x = deg_centr, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Degree centrality") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

deg_centr.plot

#####################################################################
##----Spectral radius  plot
#####################################################################
spec.radius.df=readRDS("spec.radius.df.rds")#read in data frame

spec.radius.plot=
  ggplot(spec.radius.df, aes(x = spectral_radius, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Spectral radius") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

spec.radius.plot

#####################################################################
##----Eigen centrality plot
#####################################################################
eigen_centr.df=readRDS("eigen_centr.df.rds")#read in data frame

eigen_centr.plot=
  ggplot(eigen_centr.df, aes(x = eigen_centr, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Eigen centrality") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

eigen_centr.plot

#####################################################################
##----Mean eccentricity plot
#####################################################################
mean_eccentr.df=readRDS("mean_eccentr.df.rds")#read in data frame


mean_eccentr.plot=
  ggplot(mean_eccentr.df, aes(x = mean_eccentr, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Mean eccentricity") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

mean_eccentr.plot


#####################################################################
##----Mean path length plot
#####################################################################
mean_path_length.df=readRDS("mean_path_length.df.rds")#read in data frame


mean_path_length.plot=
  ggplot(mean_path_length.df, aes(x = mean_path_length, y = .value)) +
  geom_line(stat = "identity") +
  facet_grid(~.class) +
  geom_hline(yintercept = 0)+
  theme(strip.text = element_text(size = 12))+
  xlab("Mean path length") +
  ylab("ALE")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position="none")

mean_path_length.plot


#######################################################################################
# PARTIAL DEPENDENCY PLOT
#######################################################################################
install.packages("yaImpute")
modularity.pdp.preds <- FeatureEffect$new(predictor, feature = "modularity",
                                      method = "pdp",
                                      grid.size = 30)
saveRDS(modularity.pdp.preds,"modularity.pdp.preds.rds")

transitivity.pdp.preds <- FeatureEffect$new(predictor, feature = "transitivity",
                                          method = "pdp",grid.size = 30)

saveRDS(transitivity.pdp.preds,"transitivity.pdp.preds.rds")

normalized_fiedler.pdp.preds <- FeatureEffect$new(predictor, feature = "normalized_fiedler_value",
                                            method = "pdp",
                                            grid.size = 30)
saveRDS(normalized_fiedler.pdp.preds,"normalized_fiedler.pdp.preds.rds")

deg_assort_coef.pdp.preds <- FeatureEffect$new(predictor,
                                           feature = "deg_assort_coef",
                                           method = "pdp",
                                           grid.size = 30)
saveRDS(deg_assort_coef.pdp.preds,"deg_assort_coef.pdp.preds.rds")

deg_centr.pdp.preds<- FeatureEffect$new(predictor,
                                    feature = "deg_centr",
                                    method = "pdp",
                                    grid.size = 30)
saveRDS(deg_centr.pdp.preds,"deg_centr.pdp.preds.rds")

spec.radius.pdp.preds<- FeatureEffect$new(predictor,
                                      feature = "spectral_radius",
                                      method = "pdp",
                                      grid.size = 30)

saveRDS(spec.radius.pdp.preds,"spec.radius.pdp.preds.rds")

eigen_centr.pdp.preds<- FeatureEffect$new(predictor,
                                      feature = "eigen_centr",
                                      method = "pdp",
                                      grid.size = 30)
saveRDS(eigen_centr.pdp.preds,"eigen_centr.pdp.preds.rds")

mean_eccentr.pdp.preds<- FeatureEffect$new(predictor,
                                       feature = "mean_eccentr",
                                       method = "pdp",
                                       grid.size = 30)
saveRDS(mean_eccentr.pdp.preds,"mean_eccentr.pdp.preds.rds")

mean_path_length.pdp.preds<- FeatureEffect$new(predictor,
                                           feature = "mean_path_length",
                                           method = "pdp",
                                           grid.size = 30)
saveRDS(mean_path_length.pdp.preds,"mean_path_length.pdp.preds.rds")

#######################################################################################
# Shapeley model 
#######################################################################################
doParallel::registerDoParallel()
#shap=readRDS("shap.rds")
final_shap=readRDS("final_shap.rds")

####----Shapely importance for Erdos Renyi
er_shap_imp=readRDS("er_shap_imp.rds")#sv_importance(head(final_shap$`Erdös-Rényi`,n=100L), kind = 'beeswarm')

#er_shap_imp=sv_importance(head(final_shap$`Erdös-Rényi`,n=100L), kind = 'beeswarm')

er_shap_imp=er_shap_imp & scale_color_continuous() &
  theme(legend.position='bottom', legend.direction='horizontal')

er_shap_imp_plot=er_shap_imp+theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
        ggtitle("Erdös-Rényi")+xlab("SHAP Value")
   # xlim(-6, 6)

#er_shap_imp

#ggsave("er_shap_varimp.png", width = 10, height = 10)

####----Shapely importance for SBM
sbm_shap_imp=readRDS("sbm_shap_imp.rds")#sv_importance(final_shap$`Stochastic-Block-Model`, kind = 'beeswarm')+

#sbm_shap_imp=sv_importance(head(final_shap$`Stochastic-Block-Model`,n=100L), kind = 'beeswarm')

sbm_shap_imp=sbm_shap_imp & scale_color_continuous() &
  theme(legend.position='bottom', legend.direction='horizontal') 
 
sbm_shap_imp_plot=sbm_shap_imp+theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle("Stochastic-Block-Model")+
  xlab("SHAP Value")#+
  #xlim(-6, 6)

#ggsave("sbm_shap_varimp.png", width = 10, height = 10)


###----combined erdos renyi and sbm plots
er_sbm =ggarrange(er_shap_imp_plot, sbm_shap_imp_plot, nrow=1,ncol = 2, common.legend = TRUE, legend = "bottom")

er_sbm
ggsave("er_and_sbm_varimp_crunch.png", width = 12, height = 8,bg="white")


####----Shapely importance for Scale Free
sf_shap_imp=readRDS("sf_shap_imp.rds")#sv_importance(final_shap$`Scale-Free`, kind = 'beeswarm')+

sf_shap_imp=sf_shap_imp & scale_color_continuous() &
  theme(legend.position='bottom', legend.direction='horizontal')

sf_shap_imp_plot=sf_shap_imp+theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle("Scale-Free")+
  xlab("SHAP Value")#+
  #xlim(-6, 6)


#ggsave("sf_shap_varimp.png", width = 10, height = 10)

####----Shapely importance for Spatial
sp_shap_imp=readRDS("sp_shap_imp.rds")#sv_importance(final_shap$Spatial, kind = 'beeswarm')+

sp_shap_imp=sp_shap_imp& scale_color_continuous() &
  theme(legend.position='bottom', legend.direction='horizontal')

sp_shap_imp_plot=sp_shap_imp+theme_bw()+
        theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle("Spatial")+
  xlab("SHAP Value")#+
  #xlim(-6, 6)

#ggsave("sp_shap_varimp.png", width = 10, height = 10)


####----Shapely importance for Small world
sw_shap_imp=readRDS("sw_shap_imp.rds")#sv_importance(final_shap$`Small-World`, kind = 'beeswarm')+
 
sw_shap_imp=sw_shap_imp& scale_color_continuous() &
  theme(legend.position='bottom', legend.direction='horizontal')
 
sw_shap_imp_plot=sw_shap_imp+theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none")+ 
      ggtitle("Small-World")+
      xlab("SHAP Value")#+
      #xlim(-6, 6)

#ggsave("sw_shap_varimp.png", width = 10, height = 10)

###----combined scale free, spatial and small world
sf_sp_sw =ggarrange(sf_shap_imp_plot, sp_shap_imp_plot,sw_shap_imp_plot,nrow=2,ncol = 2, common.legend = TRUE, legend = "bottom")
sf_sp_sw

ggsave("sf_sp_sw_varimp.png", width = 10, height = 12,bg="white")


##########################################################################################
# Shapely Dependency plot
###########################################################################################


###----Shap variable dependency plot for Modularity----###

mod.shap.er=readRDS("mod.shap.er.rds")

mod.shap.er=mod.shap.er+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.er=mod.shap.er+plot_layout(widths = 1.2,heights = 1.2)

#mod.shap.er  

mod.shap.sbm=readRDS("mod.shap.sbm.rds")

mod.shap.sbm=mod.shap.sbm+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.sbm=mod.shap.sbm+plot_layout(widths = 1.2,heights = 1.2)


mod.shap.sf=readRDS("mod.shap.sf.rds")
mod.shap.sf=mod.shap.sf+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mod.shap.sf=mod.shap.sf+plot_layout(widths = 1.2,heights = 1.2)

mod.shap.sp=readRDS("mod.shap.sp.rds")

mod.shap.sp=mod.shap.sp+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mod.shap.sp=mod.shap.sp+plot_layout(widths = 1.2,heights = 1.2)


mod.shap.sw=readRDS("mod.shap.sw.rds")

mod.shap.sw=mod.shap.sw+
  ggtitle("Small-World")+
  xlab("Modularity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mod.shap.sw=mod.shap.sw+plot_layout(widths = 1.2,heights = 1.2)

modularity.figure <- ggarrange(mod.shap.er, mod.shap.sbm, mod.shap.sf,
                               mod.shap.sp,mod.shap.sw,
                               ncol = 3, nrow = 2,widths=1.5, heights=2)
modularity.figure
ggsave("mod.dependency.png", width=13, height=7)

###----Transitivity shapely depedency plot----###
trans.shap.er=readRDS("trans.shap.er.rds")

trans.shap.er=trans.shap.er+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

trans.shap.er=trans.shap.er+plot_layout(widths = 1.2,heights = 1.2)


trans.shap.sbm=readRDS("trans.shap.sbm.rds")

trans.shap.sbm=trans.shap.sbm+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


trans.shap.sbm=trans.shap.sbm+plot_layout(widths = 1.2,heights = 1.2)


trans.shap.sf=readRDS("trans.shap.sf.rds")
trans.shap.sf=trans.shap.sf+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())



trans.shap.sf=trans.shap.sf+plot_layout(widths = 1.2,heights = 1.2)


trans.shap.sp=readRDS("trans.shap.sp.rds")

trans.shap.sp=trans.shap.sp+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


trans.shap.sw=readRDS("trans.shap.sw.rds")

trans.shap.sw=trans.shap.sw+
  ggtitle("Small-World")+
  xlab("Transitivity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

trans.shap.sw=trans.shap.sw+plot_layout(widths = 1.2,heights = 1.2)

transitivity.figure <- ggarrange(trans.shap.er, trans.shap.sbm, trans.shap.sf,
                                 trans.shap.sp,trans.shap.sw,
                                 ncol = 3, nrow = 2,widths=4, heights=5)
transitivity.figure
ggsave("transitivity.dependency.png", width=13, height=7)

row1 <- plot_grid(mod.shap.er, mod.shap.sbm, mod.shap.sf, mod.shap.sp, mod.shap.sw, ncol = 3)
row2=plot_grid(trans.shap.er,trans.shap.sbm, trans.shap.sf,
               trans.shap.sp,trans.shap.sw,ncol=3)

combinedplot1=ggarrange(row1,row2, nrow = 2)

ggsave("combinedplot1.png",width=12, height=8)
###----Normalized fiedler value shapely depedency plot----###

nf.shap.er=readRDS("nf.shap.er.rds")

nf.shap.er=nf.shap.er+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.er=nf.shap.er+plot_layout(widths = 1.2,heights = 1.2)


nf.shap.sbm=readRDS("nf.shap.sbm.rds")
  
nf.shap.sbm=nf.shap.sbm+
   ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sbm=nf.shap.sbm+plot_layout(widths = 1.2,heights = 1.2)



nf.shap.sf=readRDS("nf.shap.sf.rds")

nf.shap.sf=nf.shap.sf+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sf=nf.shap.sf+plot_layout(widths = 1.2,heights = 1.2)



nf.shap.sp=readRDS("nf.shap.sp.rds")

nf.shap.sp=nf.shap.sp+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sp=nf.shap.sp+plot_layout(widths = 1.2,heights = 1.2)



nf.shap.sw=readRDS("nf.shap.sw.rds")

nf.shap.sw=nf.shap.sw+
  ggtitle("Small-World")+
  xlab("Normalized fiedler value")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

nf.shap.sw=nf.shap.sw+plot_layout(widths = 1.2,heights = 1.2)

norm_fiedler.figure <- ggarrange(nf.shap.er, nf.shap.sbm, nf.shap.sf,
                                 nf.shap.sp,nf.shap.sw,
                                 ncol = 3, nrow = 2,widths=1.5, heights=2)
norm_fiedler.figure
ggsave("norm_fiedler.dependency.png", width=13, height=7)


###----Degree assortativity coeff shapely depedency plot----###

deg_assort.shap.er=readRDS("deg_assort.shap.er.rds")

deg_assort.shap.er=deg_assort.shap.er+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.er=deg_assort.shap.er+plot_layout(widths = 1.2,heights = 1.2)



deg_assort.shap.sbm=readRDS("deg_assort.shap.sbm.rds")

deg_assort.shap.sbm=deg_assort.shap.sbm+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sbm=deg_assort.shap.sbm+plot_layout(widths = 1.2,heights = 1.2)


deg_assort.shap.sf=readRDS("deg_assort.shap.sf.rds")

deg_assort.shap.sf=deg_assort.shap.sf+  
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sf=deg_assort.shap.sf+plot_layout(widths = 1.2,heights = 1.2)


deg_assort.shap.sp=readRDS("deg_assort.shap.sp.rds")

deg_assort.shap.sp=deg_assort.shap.sp+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sp=deg_assort.shap.sp+plot_layout(widths = 1.2,heights = 1.2)



deg_assort.shap.sw=readRDS("deg_assort.shap.sw.rds")

deg_assort.shap.sw=
  deg_assort.shap.sw+
  ggtitle("Small-World")+
  xlab("Degree assortativity coeff")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.shap.sw=deg_assort.shap.sw+plot_layout(widths = 1.2,heights = 1.2)

deg_assort.figure <- ggarrange(deg_assort.shap.er, deg_assort.shap.sbm, deg_assort.shap.sf,
                               deg_assort.shap.sp,deg_assort.shap.sw,
                               ncol = 3, nrow = 2,widths=4, heights=5)
deg_assort.figure
ggsave("deg_assort.dependency.png", width=13, height=7)

###----Degree centrality shapely depedency plot----###

deg_centr.shap.er=readRDS("deg_centr.shap.er.rds")

deg_centr.shap.er=deg_centr.shap.er+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.er=deg_centr.shap.er+plot_layout(widths = 1.2,heights = 1.2)
#deg_centr.shap.er  

deg_centr.shap.sbm=readRDS("deg_centr.shap.sbm.rds")

deg_centr.shap.sbm=deg_centr.shap.sbm+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.sbm=deg_centr.shap.sbm+plot_layout(widths = 1.2,heights = 1.2)



deg_centr.shap.sf=readRDS("deg_centr.shap.sf.rds")

deg_centr.shap.sf=deg_centr.shap.sf+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())



deg_centr.shap.sp=readRDS("deg_centr.shap.sp.rds")

deg_centr.shap.sp=deg_centr.shap.sp+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.shap.sp=deg_centr.shap.sp+plot_layout(widths = 1.2,heights = 1.2)

 deg_centr.shap.sw=readRDS("deg_centr.shap.sw.rds")
 
 deg_centr.shap.sw=deg_centr.shap.sw+
   ggtitle("Small-World")+
   xlab("Degree centrality")+
   ylab(" ")+
   theme_bw()+
   theme(plot.title = element_text(size = 12),
         text = element_text(size = 12,family="serif"),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12),
         axis.line = element_line(colour = "black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         panel.border = element_blank())
 
 deg_centr.shap.sw=deg_centr.shap.sw+plot_layout(widths = 1.2,heights = 1.2)
 
 deg_centr.figure <- ggarrange(deg_centr.shap.er, deg_centr.shap.sbm, deg_centr.shap.sf,
                               deg_centr.shap.sp,deg_centr.shap.sw,
                               ncol = 3, nrow = 2,widths=4, heights=5)
 deg_centr.figure
 ggsave("deg_centr.dependency.png", width=13, height=7)


row1 <- plot_grid(nf.shap.er, nf.shap.sbm, nf.shap.sf, nf.shap.sp, nf.shap.sw, ncol = 3)
row2=plot_grid(deg_assort.shap.er,deg_assort.shap.sbm, 
               deg_assort.shap.sf,deg_assort.shap.sp,
               deg_assort.shap.sw,ncol=3)
row3=plot_grid(deg_centr.shap.er,deg_centr.shap.sbm, deg_centr.shap.sf,
               deg_centr.shap.sp,deg_centr.shap.sw,ncol=3)

combinedplot2=ggarrange(row1,row2,row3, nrow = 3)

ggsave("combinedplot2.png",width=12, height=12)

 
# combined.plot1.dep=ggarrange(modularity.figure,
#                     transitivity.figure,
#                     deg_assort.figure,nrow = 3)
# 
# combined.plot1.dep
# ggsave("combined.plot1.dep.png",width=15, height=25)
# 
# combined.plot2.dep=ggarrange(norm_fiedler.figure,
#                     deg_centr.figure,
#                     nrow = 2)
# #plot.dep1
# combined.plot2.dep
# ggsave("combined.plot2.dep.png")
# 
# 
# ###----Eigen centrality shapely depedency plot----###
# eigen_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Eigen centrality",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(eigen_centr.shap.er,"eigen_centr.shap.er.rds")
# 
# eigen_centr.shap.er=readRDS("eigen_centr.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #eigen_centr.shap.er  
# 
# eigen_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Eigen centrality",
#                                    interactions = FALSE, color_var = NULL)
# 
# #saveRDS(eigen_centr.shap.sbm,"eigen_centr.shap.sbm.rds")
# eigen_centr.shap.sbm=readRDS("eigen_centr.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-eigen_centrel")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# eigen_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Eigen centrality",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(eigen_centr.shap.sf,"eigen_centr.shap.sf.rds")
# 
# eigen_centr.shap.sf=readRDS("eigen_centr.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# eigen_centr.shap.sp=sv_dependence(final_shap$Spatial, "Eigen centrality",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(eigen_centr.shap.sp,"eigen_centr.shap.sp.rds")
# 
# eigen_centr.shap.sp=readRDS("eigen_centr.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# eigen_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Eigen centrality",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(eigen_centr.shap.sw,"eigen_centr.shap.sw.rds")
# 
# eigen_centr.shap.sw=readRDS("eigen_centr.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Eigen centrality")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# eigen_centr.figure <- ggarrange(eigen_centr.shap.er, eigen_centr.shap.sbm,eigen_centr.shap.sf,
#                                 eigen_centr.shap.sp,eigen_centr.shap.sw,
#                                 ncol = 3, nrow = 2,widths=1.5, heights=2.5)
# 
# ###----Spectral radius shapely depedency plot----###
# spec_radius.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Spectral radius",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(spec_radius.shap.er,"spec_radius.shap.er.rds")
# 
# spec_radius.shap.er=readRDS("spec_radius.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #spec_radius.shap.er  
# 
# spec_radius.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Spectral radius",
#                                    interactions = FALSE, color_var = NULL)
# 
# #saveRDS(spec_radius.shap.sbm,"spec_radius.shap.sbm.rds")
# spec_radius.shap.sbm=readRDS("spec_radius.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-spec_radiusel")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# spec_radius.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Spectral radius",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(spec_radius.shap.sf,"spec_radius.shap.sf.rds")
# 
# spec_radius.shap.sf=readRDS("spec_radius.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# spec_radius.shap.sp=sv_dependence(final_shap$Spatial, "Spectral radius",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(spec_radius.shap.sp,"spec_radius.shap.sp.rds")
# 
# spec_radius.shap.sp=readRDS("spec_radius.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# spec_radius.shap.sw=sv_dependence(final_shap$`Small-World`, "Spectral radius",
#                                   interactions = FALSE, color_var = NULL)
# 
# #saveRDS(spec_radius.shap.sw,"spec_radius.shap.sw.rds")
# 
# spec_radius.shap.sw=readRDS("spec_radius.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Spectral radius")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# spec_radius.figure <- ggarrange(spec_radius.shap.er, spec_radius.shap.sbm,spec_radius.shap.sf,
#                                 spec_radius.shap.sp,spec_radius.shap.sw,
#                                 ncol = 3, nrow = 2,widths=1.5, heights=2.5)
# 
# #----Mean eccentricity shapely depedency plot----###
# mean_eccentr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Mean eccentricity",
#                                    interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_eccentr.shap.er,"mean_eccentr.shap.er.rds")
# 
# mean_eccentr.shap.er=readRDS("mean_eccentr.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #mean_eccentr.shap.er  
# 
# mean_eccentr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Mean eccentricity",
#                                     interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_eccentr.shap.sbm,"mean_eccentr.shap.sbm.rds")
# mean_eccentr.shap.sbm=readRDS("mean_eccentr.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-mean_eccentrel")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_eccentr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Mean eccentricity",
#                                    interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_eccentr.shap.sf,"mean_eccentr.shap.sf.rds")
# 
# mean_eccentr.shap.sf=readRDS("mean_eccentr.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_eccentr.shap.sp=sv_dependence(final_shap$Spatial, "Mean eccentricity",
#                                    interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_eccentr.shap.sp,"mean_eccentr.shap.sp.rds")
# 
# mean_eccentr.shap.sp=readRDS("mean_eccentr.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_eccentr.shap.sw=sv_dependence(final_shap$`Small-World`, "Mean eccentricity",
#                                    interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_eccentr.shap.sw,"mean_eccentr.shap.sw.rds")
# 
# mean_eccentr.shap.sw=readRDS("mean_eccentr.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Mean eccentricity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mean_eccentr.figure <- ggarrange(mean_eccentr.shap.er, mean_eccentr.shap.sbm,mean_eccentr.shap.sf,
#                                  mean_eccentr.shap.sp,mean_eccentr.shap.sw,
#                                  ncol = 3, nrow = 2,widths=1.5, heights=2.5)
# 
# 
# #----Mean path length shapely depedency plot----###
# mean_path_length.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Mean path length",
#                                        interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_path_length.shap.er,"mean_path_length.shap.er.rds")
# 
# mean_path_length.shap.er=readRDS("mean_path_length.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #mean_path_length.shap.er  
# 
# mean_path_length.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Mean path length",
#                                         interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_path_length.shap.sbm,"mean_path_length.shap.sbm.rds")
# mean_path_length.shap.sbm=readRDS("mean_path_length.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-mean_path_lengthel")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_path_length.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Mean path length",
#                                        interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_path_length.shap.sf,"mean_path_length.shap.sf.rds")
# 
# mean_path_length.shap.sf=readRDS("mean_path_length.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_path_length.shap.sp=sv_dependence(final_shap$Spatial, "Mean path length",
#                                        interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_path_length.shap.sp,"mean_path_length.shap.sp.rds")
# 
# mean_path_length.shap.sp=readRDS("mean_path_length.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_path_length.shap.sw=sv_dependence(final_shap$`Small-World`, "Mean path length",
#                                        interactions = FALSE, color_var = NULL)
# 
# #saveRDS(mean_path_length.shap.sw,"mean_path_length.shap.sw.rds")
# 
# mean_path_length.shap.sw=readRDS("mean_path_length.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Mean path length")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mean_path_length.figure <- ggarrange(mean_path_length.shap.er, mean_path_length.shap.sbm,mean_path_length.shap.sf,
#                                      mean_path_length.shap.sp,mean_path_length.shap.sw,
#                                      ncol = 3, nrow = 2,widths=1.5, heights=2.5)


###################################################################################
# 2D Shapely dependency plot
##################################################################################
mod.shap.2Ddep=sv_dependence2D(final_shap$`Stochastic-Block-Model`, 
                               x ="Modularity", 
                               y = c("Mean path length",
                                     "Graph energy",
                                     "Transitivity",
                                     "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) #& scale_color_continuous()

mod.shap.2Ddep#+plot_layout(widths = c(0.8, 0.8))
ggsave("mod.shap.2Ddep.png", width = 8, height = 5)


trans.shap.2Ddep=sv_dependence2D(final_shap$Spatial, 
                                 x ="Transitivity", 
                                 y = c("Mean eccentricity",
                                       "Eigen centrality",
                                       "Normalized fiedler value",
                                       "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) #& scale_color_continuous()

trans.shap.2Ddep#+plot_layout(widths = c(0.8, 0.8))
ggsave("trans.shap.2Ddep.png", width = 8, height = 5)



deg.centr.shap.2Ddep=sv_dependence2D(final_shap$`Scale-Free`, 
                                     x ="Degree centrality", 
                                     y = c("Modularity",
                                           "Eigen centrality",
                                           "Graph energy",
                                           "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) #& scale_color_continuous()

deg.centr.shap.2Ddep
ggsave("deg.centr.shap.2Ddep.png", width = 8, height = 5)

spec.radius.shap.2Ddep=sv_dependence2D(final_shap$`Small-World`, 
                                       x ="Spectral radius", 
                                       y = c("Mean degree",
                                             "Degree centrality",
                                             "Fiedler value",
                                             "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) #& scale_color_continuous()

spec.radius.shap.2Ddep
ggsave("spec.radius.shap.2Ddep.png", width = 8, height = 5)

norm.fiedler.shap.2Ddep=sv_dependence2D(final_shap$`Erdös-Rényi`, 
                                        x ="Normalized fiedler value", 
                                        y = c("Modularity",
                                              "Transitivity",
                                              "Degree centrality",
                                              "Closeness centrality"), alpha = 0.5)& theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()) #& scale_color_continuous()

norm.fiedler.shap.2Ddep
ggsave("norm.fiedler.shap.2Ddep.png", width = 8, height = 5)


##########################################################################
# Visualize multiple predictions with shap values
###########################################################################

###----Shapely force plot----####
er_force=sv_force(final_shap$`Erdös-Rényi`, row_id = 1L,
                  max_display = 6L)
ggsave("er_svforce.png", width = 10, height=4)

sf_force=sv_force(final_shap$`Scale-Free`, row_id = 1)
ggsave("sf_svforce.png", width = 10, height=4)

sbm_force=sv_force(final_shap$`Stochastic-Block-Model`, row_id = 1)
ggsave("sbm_svforce.png", width = 10, height=4)

sp_force=sv_force(final_shap$Spatial, row_id = 1)
ggsave("sp_svforce.png", width = 10, height=4)

sw_force=sv_force(final_shap$`Small-World`, row_id = 1)
ggsave("sp_svforce.png", width = 10, height=4)



###----Shapely waterfall plot----####
er_waterfall=sv_waterfall(final_shap$`Erdös-Rényi`, row_id = 1)
er_waterfall
ggsave("er_svwaterfall.png", width = 10, height=4)

sf_waterfall=sv_waterfall(final_shap$`Scale-Free`, row_id = 1)
ggsave("sf_svwaterfall.png", width = 10, height=4)

sbm_waterfall=sv_waterfall(final_shap$`Stochastic-Block-Model`, row_id = 1)
ggsave("sbm_svwaterfall.png", width = 10, height=4)

sp_waterfall=sv_waterfall(final_shap$Spatial, row_id = 1)
ggsave("sp_svwaterfall.png", width = 10, height=4)

sw_waterfall=sv_waterfall(final_shap$`Small-World`, row_id = 1)
ggsave("sp_svwaterfall.png", width = 10, height=4)

########################################################################
# Shap dependency interaction plot
########################################################################
mod.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
                                    "Modularity",
                                    color_var = "auto", interactions = TRUE)

saveRDS(mod.shap.interactn,"mod.shap.interactn.rds")

mod.shap.interactn.er=readRDS("mod.shap.interactn.er.rds")+ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#mod.shap.interactn.er  

mod.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
                                     "Modularity",
                                     interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sbm,"mod.shap.interactn.sbm.rds")

mod.shap.interactn.sbm=readRDS("mod.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mod.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Modularity",
                                    interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sf,"mod.shap.interactn.sf.rds")

mod.shap.interactn.sf=readRDS("mod.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




mod.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Modularity",
                                    interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sp,"mod.shap.interactn.sp.rds")

mod.shap.interactn.sp=readRDS("mod.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




mod.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Modularity",
                                    interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sw,"mod.shap.interactn.sw.rds")

mod.shap.interactn.sw=readRDS("mod.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Modularity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())



modularity.interactn.figure <- ggarrange(mod.shap.interactn.er, mod.shap.interactn.sbm, mod.shap.interactn.sf,
                                         mod.shap.interactn.sp,mod.shap.interactn.sw,
                                         ncol = 3, nrow = 2,widths=1.5, heights=2)
modularity.interactn.figure

#mod.shap.interactn$labels$title
###----Transitivity shapely depedency plot----###
trans.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
                                      "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.er,"trans.shap.interactn.er.rds")

trans.shap.interactn.er=readRDS("trans.shap.interactn.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#trans.shap.interactn.er  

trans.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-transel`, "Transitivity",
                                       interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sbm,"trans.shap.interactn.sbm.rds")
trans.shap.interactn.sbm=readRDS("trans.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-transel")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


trans.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sf,"trans.shap.interactn.sf.rds")
trans.shap.interactn.sf=readRDS("trans.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




trans.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sp,"trans.shap.interactn.sp.rds")

trans.shap.interactn.sp=readRDS("trans.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




trans.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sw,"trans.shap.interactn.sw.rds")
trans.shap.interactn.sw=readRDS("trans.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Transitivity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

transitivity.figure.interactn <- ggarrange(trans.shap.interactn.er, trans.shap.interactn.sbm, trans.shap.interactn.sf,
                                           trans.shap.interactn.sp,trans.shap.interactn.sw,
                                           ncol = 3, nrow = 2,widths=4, heights=5)
transitivity.figure.interactn

###----Normalized fiedler value shapely depedency plot----###
nf.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.er,"nf.shap.interactn.er.rds")

nf.shap.interactn.er=readRDS("nf.shap.interactn.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#nf.shap.interactn.er  

nf.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-nfel`, "Normalized fiedler value",
                                    interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sbm,"nf.shap.interactn.sbm.rds")
nf.shap.interactn.sbm=readRDS("nf.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-nfel")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


nf.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sf,"nf.shap.interactn.sf.rds")

nf.shap.interactn.sf=readRDS("nf.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


nf.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sp,"nf.shap.interactn.sp.rds")

nf.shap.interactn.sp=readRDS("nf.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


nf.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sw,"nf.shap.interactn.sw.rds")

nf.shap.interactn.sw=readRDS("nf.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Normalized fiedler value")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

norm_fiedler.interactn.figure <- ggarrange(nf.shap.interactn.er, nf.shap.interactn.sbm, nf.shap.interactn.sf,
                                           nf.shap.interactn.sp,nf.shap.interactn.sw,
                                           ncol = 3, nrow = 2,widths=1.5, heights=2)
norm_fiedler.interactn.figure

###----Degree assortativity coeff shapely depedency plot----###
deg_assort.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.er,"deg_assort.shap.interactn.er.rds")

deg_assort.shap.interactn.er=readRDS("deg_assort.shap.interactn.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#deg_assort.shap.interactn.er  

deg_assort.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-deg_assortel`, "Degree assortativity coeff",
                                            interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sbm,"deg_assort.shap.interactn.sbm.rds")
deg_assort.shap.interactn.sbm=readRDS("deg_assort.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-deg_assortel")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sf,"deg_assort.shap.interactn.sf.rds")

deg_assort.shap.interactn.sf=readRDS("deg_assort.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sp,"deg_assort.shap.interactn.sp.rds")

deg_assort.shap.interactn.sp=readRDS("deg_assort.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sw,"deg_assort.shap.interactn.sw.rds")

deg_assort.shap.interactn.sw=readRDS("deg_assort.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Degree assortativity coeff")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.interactn.figure <- ggarrange(deg_assort.shap.interactn.er, deg_assort.shap.interactn.sbm, deg_assort.shap.interactn.sf,
                                         deg_assort.shap.interactn.sp,deg_assort.shap.interactn.sw,
                                         ncol = 3, nrow = 2,widths=4, heights=5)
deg_assort.interactn


mod.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
                                    "Modularity",
                                    color_var = "auto", interactions = TRUE)

saveRDS(mod.shap.interactn,"mod.shap.interactn.rds")

mod.shap.interactn.er=readRDS("mod.shap.interactn.er.rds")+ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#mod.shap.interactn.er  

mod.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
                                     "Modularity",
                                     interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sbm,"mod.shap.interactn.sbm.rds")

mod.shap.interactn.sbm=readRDS("mod.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mod.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Modularity",
                                    interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sf,"mod.shap.interactn.sf.rds")

mod.shap.interactn.sf=readRDS("mod.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




mod.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Modularity",
                                    interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sp,"mod.shap.interactn.sp.rds")

mod.shap.interactn.sp=readRDS("mod.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




mod.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Modularity",
                                    interactions = TRUE, color_var = "auto")

saveRDS(mod.shap.interactn.sw,"mod.shap.interactn.sw.rds")

mod.shap.interactn.sw=readRDS("mod.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Modularity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())



modularity.interactn.figure <- ggarrange(mod.shap.interactn.er, mod.shap.interactn.sbm, mod.shap.interactn.sf,
                                         mod.shap.interactn.sp,mod.shap.interactn.sw,
                                         ncol = 3, nrow = 2,widths=1.5, heights=2)
modularity.interactn.figure

#mod.shap.interactn$labels$title
###----Transitivity shapely depedency plot----###
trans.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
                                      "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.er,"trans.shap.interactn.er.rds")

trans.shap.interactn.er=readRDS("trans.shap.interactn.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#trans.shap.interactn.er  

trans.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Transitivity",
                                       interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sbm,"trans.shap.interactn.sbm.rds")
trans.shap.interactn.sbm=readRDS("trans.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


trans.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sf,"trans.shap.interactn.sf.rds")
trans.shap.interactn.sf=readRDS("trans.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




trans.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sp,"trans.shap.interactn.sp.rds")

trans.shap.interactn.sp=readRDS("trans.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())




trans.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Transitivity",
                                      interactions = TRUE, color_var = "auto")

saveRDS(trans.shap.interactn.sw,"trans.shap.interactn.sw.rds")
trans.shap.interactn.sw=readRDS("trans.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Transitivity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

transitivity.figure.interactn <- ggarrange(trans.shap.interactn.er, trans.shap.interactn.sbm, trans.shap.interactn.sf,
                                           trans.shap.interactn.sp,trans.shap.interactn.sw,
                                           ncol = 3, nrow = 2,widths=4, heights=5)
transitivity.figure.interactn

###----Normalized fiedler value shapely depedency plot----###
nf.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.er,"nf.shap.interactn.er.rds")

nf.shap.interactn.er=readRDS("nf.shap.interactn.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#nf.shap.interactn.er  

nf.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Normalized fiedler value",
                                    interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sbm,"nf.shap.interactn.sbm.rds")
nf.shap.interactn.sbm=readRDS("nf.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


nf.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sf,"nf.shap.interactn.sf.rds")

nf.shap.interactn.sf=readRDS("nf.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


nf.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sp,"nf.shap.interactn.sp.rds")

nf.shap.interactn.sp=readRDS("nf.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


nf.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Normalized fiedler value",
                                   interactions = TRUE, color_var = "auto")

saveRDS(nf.shap.interactn.sw,"nf.shap.interactn.sw.rds")

nf.shap.interactn.sw=readRDS("nf.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Normalized fiedler value")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

norm_fiedler.interactn.figure <- ggarrange(nf.shap.interactn.er, nf.shap.interactn.sbm, nf.shap.interactn.sf,
                                           nf.shap.interactn.sp,nf.shap.interactn.sw,
                                           ncol = 3, nrow = 2,widths=1.5, heights=2)
norm_fiedler.interactn.figure

###----Degree assortativity coeff shapely depedency plot----###
deg_assort.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`,
                                           "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.er,"deg_assort.shap.interactn.er.rds")

deg_assort.shap.interactn.er=readRDS("deg_assort.shap.interactn.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#deg_assort.shap.interactn.er  

deg_assort.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
                                            "Degree assortativity coeff",
                                            interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sbm,"deg_assort.shap.interactn.sbm.rds")
deg_assort.shap.interactn.sbm=readRDS("deg_assort.shap.interactn.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sf,"deg_assort.shap.interactn.sf.rds")

deg_assort.shap.interactn.sf=readRDS("deg_assort.shap.interactn.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sp,"deg_assort.shap.interactn.sp.rds")

deg_assort.shap.interactn.sp=readRDS("deg_assort.shap.interactn.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_assort.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Degree assortativity coeff",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_assort.shap.interactn.sw,"deg_assort.shap.interactn.sw.rds")

deg_assort.shap.interactn.sw=readRDS("deg_assort.shap.interactn.sw.rds")+
  ggtitle("Small-World")+
  xlab("Degree assortativity coeff")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_assort.interactn.figure <- ggarrange(deg_assort.shap.interactn.er, deg_assort.shap.interactn.sbm, deg_assort.shap.interactn.sf,
                                         deg_assort.shap.interactn.sp,deg_assort.shap.interactn.sw,
                                         ncol = 3, nrow = 2,widths=4, heights=5)
deg_assort.interactn


###----Degree centrality shapely depedency plot----###
deg_centr.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Degree centrality",
                                          interactions = TRUE, color_var = "auto")

saveRDS(deg_centr.interactn.shap.er,"deg_centr.interactn.shap.er.rds")

deg_centr.interactn.shap.er=readRDS("deg_centr.interactn.shap.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#deg_centr.interactn.shap.er  

deg_centr.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
                                           "Degree centrality",
                                           interactions = TRUE, color_var = "auto")

saveRDS(deg_centr.interactn.shap.sbm,"deg_centr.interactn.shap.sbm.rds")
deg_centr.interactn.shap.sbm=readRDS("deg_centr.interactn.shap.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_centr.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Degree centrality",
                                          interactions = TRUE, color_var = "auto")

saveRDS(deg_centr.interactn.shap.sf,"deg_centr.interactn.shap.sf.rds")

deg_centr.interactn.shap.sf=readRDS("deg_centr.interactn.shap.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_centr.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Degree centrality",
                                          interactions = TRUE, color_var = "auto")

saveRDS(deg_centr.interactn.shap.sp,"deg_centr.interactn.shap.sp.rds")

deg_centr.interactn.shap.sp=readRDS("deg_centr.interactn.shap.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


deg_centr.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Degree centrality",
                                          interactions = TRUE, color_var = "auto")

saveRDS(deg_centr.interactn.shap.sw,"deg_centr.interactn.shap.sw.rds")

deg_centr.interactn.shap.sw=readRDS("deg_centr.interactn.shap.sw.rds")+
  ggtitle("Small-World")+
  xlab("Degree centrality")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

deg_centr.interactn.figure <- ggarrange(deg_centr.interactn.shap.er, deg_centr.interactn.shap.sbm, deg_centr.interactn.shap.sf,
                                        deg_centr.interactn.shap.sp,deg_centr.interactn.shap.sw,
                                        ncol = 3, nrow = 2,widths=4, heights=5)
deg_centr.interactn.figure


###----Eigen centrality shapely depedency plot----###
eigen_centr.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Eigen centrality",
                                            interactions = TRUE, color_var = "auto")

saveRDS(eigen_centr.interactn.shap.er,"eigen_centr.interactn.shap.er.rds")

eigen_centr.interactn.shap.er=readRDS("eigen_centr.interactn.shap.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#eigen_centr.interactn.shap.er  

eigen_centr.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
                                             "Eigen centrality",
                                             interactions = TRUE, color_var = "auto")

saveRDS(eigen_centr.interactn.shap.sbm,"eigen_centr.interactn.shap.sbm.rds")
eigen_centr.interactn.shap.sbm=readRDS("eigen_centr.interactn.shap.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


eigen_centr.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Eigen centrality",
                                            interactions = TRUE, color_var = "auto")

saveRDS(eigen_centr.interactn.shap.sf,"eigen_centr.interactn.shap.sf.rds")

eigen_centr.interactn.shap.sf=readRDS("eigen_centr.interactn.shap.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


eigen_centr.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Eigen centrality",
                                            interactions = TRUE, color_var = "auto")

saveRDS(eigen_centr.interactn.shap.sp,"eigen_centr.interactn.shap.sp.rds")

eigen_centr.interactn.shap.sp=readRDS("eigen_centr.interactn.shap.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


eigen_centr.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Eigen centrality",
                                            interactions = TRUE, color_var = "auto")

saveRDS(eigen_centr.interactn.shap.sw,"eigen_centr.interactn.shap.sw.rds")

eigen_centr.interactn.shap.sw=readRDS("eigen_centr.interactn.shap.sw.rds")+
  ggtitle("Small-World")+
  xlab("Eigen centrality")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

eigen_centr.interactn.figure <- ggarrange(eigen_centr.interactn.shap.er, eigen_centr.interactn.shap.sbm, eigen_centr.interactn.shap.sf,
                                          eigen_centr.interactn.shap.sp,eigen_centr.interactn.shap.sw,
                                          ncol = 3, nrow = 2,widths=4, heights=5)
eigen_centr.interactn.figure

###----Spectral radius shapely depedency plot----###
spec_radius.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Spectral radius",
                                            interactions = TRUE, color_var = "auto")

saveRDS(spec_radius.interactn.shap.er,"spec_radius.interactn.shap.er.rds")

spec_radius.interactn.shap.er=readRDS("spec_radius.interactn.shap.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#spec_radius.interactn.shap.er  

spec_radius.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Spectral radius",
                                             interactions = TRUE, color_var = "auto")

saveRDS(spec_radius.interactn.shap.sbm,"spec_radius.interactn.shap.sbm.rds")
spec_radius.interactn.shap.sbm=readRDS("spec_radius.interactn.shap.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


spec_radius.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Spectral radius",
                                            interactions = TRUE, color_var = "auto")

saveRDS(spec_radius.interactn.shap.sf,"spec_radius.interactn.shap.sf.rds")

spec_radius.interactn.shap.sf=readRDS("spec_radius.interactn.shap.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


spec_radius.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Spectral radius",
                                            interactions = TRUE, color_var = "auto")

saveRDS(spec_radius.interactn.shap.sp,"spec_radius.interactn.shap.sp.rds")

spec_radius.interactn.shap.sp=readRDS("spec_radius.interactn.shap.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


spec_radius.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Spectral radius",
                                            interactions = TRUE, color_var = "auto")

saveRDS(spec_radius.interactn.shap.sw,"spec_radius.interactn.shap.sw.rds")

spec_radius.interactn.shap.sw=readRDS("spec_radius.interactn.shap.sw.rds")+
  ggtitle("Small-World")+
  xlab("Spectral radius")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

spec_radius.interactn.figure <- ggarrange(spec_radius.interactn.shap.er, spec_radius.interactn.shap.sbm, spec_radius.interactn.shap.sf,
                                          spec_radius.interactn.shap.sp,spec_radius.interactn.shap.sw,
                                          ncol = 3, nrow = 2,widths=4, heights=5)
spec_radius.interactn.figure

###----Mean path length shapely depedency plot----###
mean_path_length.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Mean path length",
                                                 interactions = TRUE, color_var = "auto")

saveRDS(mean_path_length.interactn.shap.er,"mean_path_length.interactn.shap.er.rds")

mean_path_length.interactn.shap.er=readRDS("mean_path_length.interactn.shap.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#mean_path_length.interactn.shap.er  

mean_path_length.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Mean path length",
                                                  interactions = TRUE, color_var = "auto")

saveRDS(mean_path_length.interactn.shap.sbm,"mean_path_length.interactn.shap.sbm.rds")
mean_path_length.interactn.shap.sbm=readRDS("mean_path_length.interactn.shap.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mean_path_length.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Mean path length",
                                                 interactions = TRUE, color_var = "auto")

saveRDS(mean_path_length.interactn.shap.sf,"mean_path_length.interactn.shap.sf.rds")

mean_path_length.interactn.shap.sf=readRDS("mean_path_length.interactn.shap.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mean_path_length.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Mean path length",
                                                 interactions = TRUE, color_var = "auto")

saveRDS(mean_path_length.interactn.shap.sp,"mean_path_length.interactn.shap.sp.rds")

mean_path_length.interactn.shap.sp=readRDS("mean_path_length.interactn.shap.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mean_path_length.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Mean path length",
                                                 interactions = TRUE, color_var = "auto")

saveRDS(mean_path_length.interactn.shap.sw,"mean_path_length.interactn.shap.sw.rds")

mean_path_length.interactn.shap.sw=readRDS("mean_path_length.interactn.shap.sw.rds")+
  ggtitle("Small-World")+
  xlab("Mean path length")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_path_length.interactn.figure <- ggarrange(mean_path_length.interactn.shap.er, mean_path_length.interactn.shap.sbm, mean_path_length.interactn.shap.sf,
                                               mean_path_length.interactn.shap.sp,mean_path_length.interactn.shap.sw,
                                               ncol = 3, nrow = 2,widths=4, heights=5)
mean_path_length.interactn.figure


###----Mean eccentricity shapely depedency plot----###
mean_eccentr.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`,
                                             "Mean eccentricity",
                                             interactions = TRUE, color_var = "auto")

saveRDS(mean_eccentr.interactn.shap.er,"mean_eccentr.interactn.shap.er.rds")

mean_eccentr.interactn.shap.er=readRDS("mean_eccentr.interactn.shap.er.rds")+
  ggtitle("Erdös-Rényi")+
  xlab(" ")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

#mean_eccentr.interactn.shap.er  

mean_eccentr.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Mean eccentricity",
                                              interactions = TRUE, color_var = "auto")

saveRDS(mean_eccentr.interactn.shap.sbm,"mean_eccentr.interactn.shap.sbm.rds")
mean_eccentr.interactn.shap.sbm=readRDS("mean_eccentr.interactn.shap.sbm.rds")+
  ggtitle("Stochastic-Block-Model")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mean_eccentr.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Mean eccentricity",
                                             interactions = TRUE, color_var = "auto")

saveRDS(mean_eccentr.interactn.shap.sf,"mean_eccentr.interactn.shap.sf.rds")

mean_eccentr.interactn.shap.sf=readRDS("mean_eccentr.interactn.shap.sf.rds")+
  ggtitle("Scale-Free")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mean_eccentr.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Mean eccentricity",
                                             interactions = TRUE, color_var = "auto")

saveRDS(mean_eccentr.interactn.shap.sp,"mean_eccentr.interactn.shap.sp.rds")

mean_eccentr.interactn.shap.sp=readRDS("mean_eccentr.interactn.shap.sp.rds")+
  ggtitle("Spatial")+
  xlab(" ")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())


mean_eccentr.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Mean eccentricity",
                                             interactions = TRUE, color_var = "auto")

saveRDS(mean_eccentr.interactn.shap.sw,"mean_eccentr.interactn.shap.sw.rds")

mean_eccentr.interactn.shap.sw=readRDS("mean_eccentr.interactn.shap.sw.rds")+
  ggtitle("Small-World")+
  xlab("Mean eccentricity")+
  ylab(" ")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

mean_eccentr.interactn.figure <- ggarrange(mean_eccentr.interactn.shap.er, mean_eccentr.interactn.shap.sbm, mean_eccentr.interactn.shap.sf,
                                           mean_eccentr.interactn.shap.sp,mean_eccentr.interactn.shap.sw,
                                           ncol = 3, nrow = 2,widths=4, heights=5)
mean_eccentr.interactn.figure


#######################################################
# shap and ale plots combined
#######################################################
mod.combine=ggarrange(modularity.figure,
                      ggarrange(modularity.plot, 
                                align = "v",widths = c(0.5,1)),
                      #   align = "v",
                      nrow = 2,
                      labels = c("A)","B)"),
                      # label.x = 0,
                      # label.y = 1,
                      # heights = c(1.5, 1, 1),
                      # widths = c(.5,.5,.5),
                      hjust = -1.1,#-3.1,
                      vjust = 0.9,
                      font.label = list(size = 12, color = "black", face = "bold",
                                        family = "serif"))
mod.combine

ggsave("mod.combine.png", width = 8, height =8 )

##----Transitivity combined shapely and ale plots-----
trans.combine=ggarrange(transitivity.figure,
                        ggarrange(transitivity.plot, 
                                  align = "v",widths = c(0.5,1)),
                        #   align = "v",
                        nrow = 2,
                        labels = c("A)","B)"),
                        # label.x = 0,
                        # label.y = 1,
                        # heights = c(1.5, 1, 1),
                        # widths = c(.5,.5,.5),
                        hjust = -1.1,#-3.1,
                        vjust = 0.9,
                        transitivity.plot, 
                        align = "v",widths = c(0.5,1))
                        font.label = list(size = 12, color = "black", face = "bold",
                                          family = "serif"))
trans.combine

ggsave("trans.combine.png", width = 8, height = 8)


##----Normalized fiedler combined shapely and ale plots-----
normfiedler.combine=ggarrange(norm_fiedler.figure,
                              ggarrange(normalized_fiedler_value.plot, 
                                        align = "v",widths = c(0.5,1)),
                              #   align = "v",
                              nrow = 2,
                              labels = c("A)","B)"),
                              # label.x = 0,
                              # label.y = 1,
                              # heights = c(1.5, 1, 1),
                              # widths = c(.5,.5,.5),
                              hjust = -1.1,#-3.1,
                              vjust = 0.9,
                              font.label = list(size = 12, color = "black", face = "bold",
                                                family = "serif"))

normfiedler.combine
ggsave("normfiedler.combine.png", width = 8, height = 8)

##----Degree centrality combine shapely and ale plots-----
deg.centr.combine=ggarrange(deg_centr.figure,
                            ggarrange(deg_centr.plot, 
                                      align = "v",widths = c(0.5,1)),
                            #   align = "v",
                            nrow = 2,
                            labels = c("A)","B)"),
                            # label.x = 0,
                            # label.y = 1,
                            # heights = c(1.5, 1, 1),
                            # widths = c(.5,.5,.5),
                            hjust = -1.1,#-3.1,
                            vjust = 0.9,
                            font.label = list(size = 12, color = "black", face = "bold",
                                              family = "serif"))

deg.centr.combine
ggsave("deg.centr.combine.png", width = 8, height = 8)

##----Degree assortativity coeff combined shapely and ale plots-----
deg_assort.combine=ggarrange(deg_assort.figure,
                             ggarrange(deg_assort_coef.plot, 
                                       align = "v",widths = c(0.5,1)),
                             #   align = "v",
                             nrow = 2,
                             labels = c("A)","B)"),
                             # label.x = 0,
                             # label.y = 1,
                             # heights = c(1.5, 1, 1),
                             # widths = c(.5,.5,.5),
                             hjust = -1.1,#-3.1,
                             vjust = 0.9,
                             font.label = list(size = 12, color = "black", face = "bold",
                                               family = "serif"))
deg_assort.combine
ggsave("deg_assort.combine.png", width = 8, height = 8)

##----Eigen centrality combined shapely and ale plots-----
eigen_centr.combine=ggarrange(eigen_centr.figure,
                             ggarrange(eigen_centr.plot, 
                                       align = "v",widths = c(0.5,1)),
                             #   align = "v",
                             nrow = 2,
                             labels = c("A)","B)"),
                             # label.x = 0,
                             # label.y = 1,
                             # heights = c(1.5, 1, 1),
                             # widths = c(.5,.5,.5),
                             hjust = -1.1,#-3.1,
                             vjust = 0.9,
                             font.label = list(size = 12, color = "black", face = "bold",
                                               family = "serif"))
eigen_centr.combine
ggsave("eigen_centr.combine.png", width = 8, height = 8)

##----Spectral radius combine shapely and ale plots-----
spec_radius.combine=ggarrange(spec_radius.figure,
                              ggarrange(spec.radius.plot, 
                                        align = "v",widths = c(0.5,1)),
                              #   align = "v",
                              nrow = 2,
                              labels = c("A)","B)"),
                              # label.x = 0,
                              # label.y = 1,
                              # heights = c(1.5, 1, 1),
                              # widths = c(.5,.5,.5),
                              hjust = -1.1,#-3.1,
                              vjust = 0.9,
                              font.label = list(size = 12, color = "black", face = "bold",
                                                family = "serif"))
spec_radius.combine
ggsave("spec_radius.combine.png", width = 8, height = 8)

##----Mean eccentricity combined shapely and ale plots-----
mean_eccentr.combine=ggarrange(mean_eccentr.figure,
                              ggarrange(mean_eccentr.plot, 
                                        align = "v",widths = c(0.5,1)),
                              #   align = "v",
                              nrow = 2,
                              labels = c("A)","B)"),
                              # label.x = 0,
                              # label.y = 1,
                              # heights = c(1.5, 1, 1),
                              # widths = c(.5,.5,.5),
                              hjust = -1.1,#-3.1,
                              vjust = 0.9,
                              font.label = list(size = 12, color = "black", face = "bold",
                                                family = "serif"))
mean_eccentr.combine
ggsave("mean_eccentr.combine.png", width = 8, height = 8)


##----Mean eccentricity combined shapely and ale plots-----
mean_path_length.combine=ggarrange(mean_path_length.figure,
                               ggarrange(mean_path_length.plot, 
                                         align = "v",widths = c(0.5,1)),
                               #   align = "v",
                               nrow = 2,
                               labels = c("A)","B)"),
                               # label.x = 0,
                               # label.y = 1,
                               # heights = c(1.5, 1, 1),
                               # widths = c(.5,.5,.5),
                               hjust = -1.1,#-3.1,
                               vjust = 0.9,
                               font.label = list(size = 12, color = "black", face = "bold",
                                                 family = "serif"))
mean_path_length.combine
ggsave("mean_path_length.combine.png", width = 8, height = 8)


#######################################################################################
# Explain single prediction with local model: LIME MODEL
#######################################################################################
#For analyzing how models make individual predictions
best.model.lime <- LocalModel$new(predictor, x.interest = x.data[1,])
best.model.lime %>% plot()

##########################################################################################
#######====PREDICTION ON NEW DATA WITH BEST XGBOOST MODEL====#####
##########################################################################################

##----Empirical Data----##
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
final.emp.data=emp.data.juice
colSums(is.na(final.emp.data))

####----Final preprocessed data frame----for empirical data----##
best.model.predictions <- predict(extract_final.fitted.model, new_data = final.emp.data)%>%
  bind_cols(final.emp.data)

###----data frame for predicted empirical network
df.emp=data.frame(best.model.predictions$graph_name,best.model.predictions$.pred_class)
colnames(df.emp)=c("target","Predicted_classes")  
df.emp

#df.emp$predicted[df.emp$predicted == 'SF'] <- as.factor('Scale-free')

levels(df.emp$Predicted_classes) <- c('Erdös-Rényi', 'Stochastic-Block-Model', 
                                      'Scale-Free', 'Spatial','Small-World')
df.emp=df.emp%>%
  as_tibble()
# clean_names()

df.emp

## set the levels in order we want
df.emp <- within(df.emp, 
                 Predicted_classes <- factor(Predicted_classes, 
                                      levels=names(sort(table(Predicted_classes), 
                                                        decreasing=TRUE))))

###----Stack bar plot for muti-nomial model
emp.var.imp.plot= ggplot(df.emp, aes(x = Predicted_classes,fill = target))+
  geom_bar(binwidth=1)+
  theme_classic()+
  theme(text = element_text(size = 15,family="serif"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position="none")
emp.var.imp.plot

ggsave("emp.var.imp.png", width=9,height=6)

######################################################################################
# second classification model with meta data
######################################################################################

# df_final_meta_data=readRDS("df_final_meta_data.rds")
# 
# 
# FinalTrainedMetadata=readRDS("FinalTrainedMetadata.rds")
# rf.final.metadata.fit=readRDS("rf_final_metadata_fit.rds")
# df_final_meta_data=readRDS("df_final_meta_data.rds")
# metadata.rec=readRDS("metadata.rec.rds")
# metadata.prep.rf=readRDS("metadata.prep.rf")
# metadata.juice.rf=readRDS("metadata.juice.rf")
# FinalTrainedMetadata=readRDS("FinalTrainedMetadata.rds")
# ##----Tuned fitted first model
# rf.res=readRDS("rf.res.rds")
# rf.metadata.res=readRDS("rf_metadata_res.rds")
# 
# #Show best metric
# rf.metadata.res %>%
#   show_best(metric = "roc_auc")
# 
# #Show best metric
# rf.metadata.res %>%
#   show_best(metric = "roc_auc")
# 
# #Shows best estimated tuned parameters
# autoplot(rf.metadata.res)
# #collect results
# rf.final.metadata.fit%>%
#   collect_metrics()
# 
# rf.final.metadata.fit=readRDS("rf.final.metadata.fit.rds")
# 
# #Collect prediction
# rf.final.metadata.fit %>%
#   collect_predictions()
# 
# rf.final.metadata.fit %>%
#   collect_predictions() %>%
#   roc_curve(target, c(`.pred_1`,
#                       `.pred_2`,`.pred_3`,.pred_4,
#                       `.pred_5`)) %>%autoplot()
# 
# final.metadata.model=readRDS("final.metadata.model.rds")
# 
# #Prediction on test set
# pred_result_metadata=augment(final.metadata.model,last_metadata_test)
# predValsmetadata=pred_result_metadata%>%
#   slice_head(n=10)
# predValsmetadata
# ##Confusion Matrix
# pred_result_metadata%>%
#   conf_mat(truth = target,estimate =.pred_class)
# 
# 
# numeric_metadata=last_metadata_df#%>%
#        #filter(target==1)%>%
#        mutate_all(as.numeric)
#           
# 
# model.metadata=ranger(target ~ ., data = last_metadata_df)#extract_fit_engine(rf.final.metadata.fit)
# 
# meta_model_unified <- ranger.unify(model.metadata, numeric_metadata)
# 
# meta.tree=treeshap(meta_model_unified,numeric_metadata)
# meta.tree$shaps
# plot_contribution(meta.tree)
# 
# # Assuming `df` is your data frame and `columns` is a vector of column names to convert to numeric
# 
# # Convert selected columns to numeric
# df[columns] <- sapply(df[columns], as.numeric)

# names(meta.shap.vals)[names(meta.shap.vals) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.vals)[names(meta.shap.vals) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.vals)[names(meta.shap.vals) == "Class_3"] <- "Scale-Free"
# names(meta.shap.vals)[names(meta.shap.vals) == "Class_4"] <- "Spatial"
# names(meta.shap.vals)[names(meta.shap.vals) == "Class_5"] <- "Small-World"
# colnames(meta.shap.vals$`Erdös-Rényi`)=meta_shap_column_names
# colnames(meta.shap.vals$`Stochastic-Block-Model`)=meta_shap_column_names
# colnames(meta.shap.vals$`Scale-Free`)=meta_shap_column_names
# colnames(meta.shap.vals$Spatial)=meta_shap_column_names
# colnames(meta.shap.vals$`Small-World`)=meta_shap_column_names
# meta.shap.vals
# 
# meta.shap.feat=get_feature_values(meta_shap)
# names(meta.shap.feat)[names(meta.shap.feat) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.feat)[names(meta.shap.feat) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.feat)[names(meta.shap.feat) == "Class_3"] <- "Scale-Free"
# names(meta.shap.feat)[names(meta.shap.feat) == "Class_4"] <- "Spatial"
# names(meta.shap.feat)[names(meta.shap.feat) == "Class_5"] <- "Small-World"
# colnames(meta.shap.feat$`Erdös-Rényi`)=meta_shap_column_names
# colnames(meta.shap.feat$`Stochastic-Block-Model`)=meta_shap_column_names
# colnames(meta.shap.feat$`Scale-Free`)=meta_shap_column_names
# colnames(meta.shap.feat$Spatial)=meta_shap_column_names
# colnames(meta.shap.feat$`Small-World`)=meta_shap_column_names
# meta.shap.feat
# 
# 
# meta.shap.baseline=get_baseline(meta_shap)
# names(meta.shap.baseline)[names(meta.shap.baseline) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.baseline)[names(meta.shap.baseline) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.baseline)[names(meta.shap.baseline) == "Class_3"] <- "Scale-Free"
# names(meta.shap.baseline)[names(meta.shap.baseline) == "Class_4"] <- "Spatial"
# names(meta.shap.baseline)[names(meta.shap.baseline) == "Class_5"] <- "Small-World"
# 
# 
# meta.shap.interactions=get_shap_interactions(meta_shap)
# names(meta.shap.interactions)[names(meta.shap.interactions) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.interactions)[names(meta.shap.interactions) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.interactions)[names(meta.shap.interactions) == "Class_3"] <- "Scale-Free"
# names(meta.shap.interactions)[names(meta.shap.interactions) == "Class_4"] <- "Spatial"
# names(meta.shap.interactions)[names(meta.shap.interactions) == "Class_5"] <- "Small-World"
# colnames(meta.shap.interactions$`Erdös-Rényi`)=meta_shap_column_names
# colnames(meta.shap.interactions$`Stochastic-Block-Model`)=meta_shap_column_names
# colnames(meta.shap.interactions$`Scale-Free`)=meta_shap_column_names
# colnames(meta.shap.interactions$Spatial)=meta_shap_column_names
# colnames(meta.shap.interactions$`Small-World`)=meta_shap_column_names
# meta.shap.interactions
# 
# sp.meta.shap=shapviz(meta.shap.vals$Spatial,meta.shap.feat$Spatial,
#                      meta.shap.baseline$Spatial)
# 
# er.meta.shap=shapviz(meta.shap.vals$`Erdös-Rényi`,meta.shap.feat$`Erdös-Rényi`,
#                      meta.shap.baseline$`Erdös-Rényi`)
# 
# sbm.meta.shap=shapviz(meta.shap.vals$`Stochastic-Block-Model`,
#                       meta.shap.feat$`Stochastic-Block-Model`,
#                       meta.shap.baseline$`Stochastic-Block-Model`)
# sw.meta.shap=shapviz(meta.shap.vals$`Small-World`,meta.shap.feat$`Small-World`,
#                      meta.shap.baseline$`Small-World`)
# sf.meta.shap=shapviz(meta.shap.vals$`Scale-Free`,meta.shap.feat$`Scale-Free`,
#                      meta.shap.baseline$`Scale-Free`)
# 
# final_meta_shap <- mshapviz(c('Erdös-Rényi' = er.meta.shap,
#                               'Stochastic-Block-Model'=sbm.meta.shap,
#                               'Scale-Free'= sf.meta.shap,
#                               'Spatial'=sp.meta.shap,
#                               'Small-World'=sw.meta.shap))
#final_meta_shap$Spatial, kind = 'beeswarm'
###----Shap variable dependency plot for time resolution----###
time_resolution_very_fine.shap=sv_dependence(final_meta_shap$Spatial, 
                                             c("Time_resolution_secs (very_fine)"),
                                             interactions = F, color_var = NULL)
saveRDS(time_resolution_very_fine.shap,"time_resolution_very_fine.shap.rds")

time_resolution_very_fine.plot=time_resolution_very_fine.shap+ggtitle("Shap dependency plot for Spatial")+
  geom_line()+
  xlab(" Time_resolution_secs (very_fine)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

time_resolution_very_fine.plot
ggsave("time_resolution_very_fine1.png", width = 5,height=4)



Interaction_type_spb.shap=sv_dependence(final_meta_shap$Spatial, 
                                        c("Interaction_type (social_projection_bipartite)"),
                                        interactions = F, color_var = NULL)

saveRDS(Interaction_type_spb.shap,"Interaction_type_spb.shap.rds")
Interaction_type_spb.plot=Interaction_type_spb.shap+
  ggtitle(" ")+
  geom_line()+
  xlab(" Interaction_type (social_projection_bipartite)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

Interaction_type_spb.plot
ggsave("Interaction_type_spb1.png", width = 5,height=4)


###----Shap variable dependency plot for captive----###
class_mammalia.shap1=sv_dependence(final_meta_shap$Spatial, 
                                   c("Class (mammalia)"),
                                   interactions = F, color_var = NULL)

saveRDS(class_mammalia.shap1,"class_mammalia.shap1.rds")
class_mammalia.plot=class_mammalia.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Class (mammalia))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

class_mammalia.plot
ggsave("class_mammalia.shap1.png", width = 5,height=4)

data_collection_focal_sampling.shap1=sv_dependence(final_meta_shap$Spatial, 
                                                   c("Data_collection (focal_sampling)"),
                                                   interactions = F, color_var = NULL)

saveRDS(data_collection_focal_sampling.shap1,"data_collection_focal_sampling.shap1.rds")
data_collection_focal_sampling.plot=data_collection_focal_sampling.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Data_collection (focal_sampling))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_collection_focal_sampling.plot
ggsave("data_collection_focal_sampling.shap1.png", width = 5,height=4)


time_resolution_secs_inter.shap1=sv_dependence(final_meta_shap$Spatial, 
                                               c("Time_resolution_secs (intermediate)"),
                                               interactions = F, color_var = NULL)

saveRDS(time_resolution_secs_inter.shap1,"time_resolution_secs_inter.shap1.rds")
time_resolution_secs_inter.plot=time_resolution_secs_inter.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Time_resolution_secs (intermediate))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

time_resolution_secs_inter.plot
ggsave("time_resolution_secs_inter.shap1.png", width = 5,height=4)




interaction_type_grooming.shap1=sv_dependence(final_meta_shap$Spatial, 
                                              c("Interaction_type (grooming)"),
                                              interactions = F, color_var = NULL)

saveRDS(interaction_type_grooming.shap1,"interaction_type_grooming.shap1.rds")

interaction_type_grooming.plot=interaction_type_grooming.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Interaction_type (grooming))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

interaction_type_grooming.plot
ggsave("interaction_type_grooming.shap1.png", width = 5,height=4)



species_camponotus_fellah.shap1=sv_dependence(final_meta_shap$Spatial, 
                                              c("Data_collection (survey_scan)"),
                                              interactions = F, color_var = NULL)

saveRDS(species_camponotus_fellah.shap1,"species_camponotus_fellah.shap1.rds")

species_camponotus_fellah.plot=species_camponotus_fellah.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Data_collection (survey_scan))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

species_camponotus_fellah.plot
ggsave("species_camponotus_fellah.shap1.png", width = 5,height=4)





data_collection_survey_scan.shap1=sv_dependence(final_meta_shap$Spatial, 
                                                c("Data_collection (survey_scan)"),
                                                interactions = F, color_var = NULL)

saveRDS(data_collection_survey_scan.shap1,"data_collection_survey_scan.shap1.rds")


data_collection_survey_scan.plot=data_collection_survey_scan.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Data_collection (survey_scan))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_collection_survey_scan.plot
ggsave("data_collection_survey_scan.plot.png", width = 5,height=4)


data_duration_days.shap1=sv_dependence(final_meta_shap$Spatial, 
                                       c("Data_duration (days)"),
                                       interactions = F, color_var = NULL)

saveRDS(data_duration_days.shap1,"data_duration_days.shap1.rds")


data_duration_days.plot=data_duration_days.shap1+
  ggtitle(" ")+
  geom_line()+
  xlab(" Data_duration (days))")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_duration_days.plot
ggsave("data_duration_days.plot.png", width = 5,height=4)






#################################################################################
# Shap Dependency interaction plots
#################################################################################
#final_meta_shap$Spatial, kind = 'beeswarm'
###----Shap variable dependency plot for data duration days----###

sv_interaction(final_meta_shap$Spatial, kind = "no")

sv_interaction(x, max_display = 2, size = 3)

data_duration.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                          c("data_duration_days"),
                                          color_var = "auto")+
  ggtitle("Shap dependency plot for Spatial")+
  geom_line()+
  xlab(" data_duration_days")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_duration.shap.interact
saveRDS(data_duration.shap.interact,"data_duration.shap.interact.rds")

###----Shap variable dependency plot for data collection----###
data_collection.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                            c("data_collection_video"),
                                            color_var = "auto")+
  ggtitle("Shap dependency plot for Spatial")+
  geom_line()+
  xlab(" data_collection_video")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_collection.shap.interact



###----Shap variable dependency plot for captive----###
captive_No.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                       c("captive_No"),
                                       color_var = "auto")+
  ggtitle("Shap dependency plot for Spatial")+
  geom_line()+
  xlab(" captive (no)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

captive_No.shap.interact

###----Shap variable dependency plot for Modularity----###
species_Macaca_fuscata.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                                   c("species_Macaca_fuscata"),
                                                   color_var = "auto")+
  ggtitle("Shap dependency plot for Spatial")+
  geom_line()+
  xlab(" species (macaca_fuscata)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

species_Macaca_fuscata.shap.interact


###----Shap variable dependency plot for data collection----###
data_collection_focal_sampling.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                                           c("data_collection_focal_sampling"),
                                                           color_var = "auto")+
  ggtitle("Shap dependency plot for Spatial")+
  geom_line()+
  xlab(" data_collection (focal_sampling)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

data_collection_focal_sampling.shap.interact



###----Shap variable dependency plot for species----###
species_Microtus_agrestis.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                                      c("species_Microtus_agrestis"),
                                                      color_var = "auto")+
  ggtitle(" ")+
  geom_line()+
  xlab(" species (microtus agrestis)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

species_Microtus_agrestis.shap.interact


captive_Yes.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                        c("captive_Yes"),
                                        color_var = "auto")+
  ggtitle("")+
  geom_line()+
  xlab(" captive (yes)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

captive_Yes.shap.interact


interaction_type_Social_projection_bipartite.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                                                         c("interaction_type_Social_projection_bipartite"),
                                                                         color_var = "auto")+
  ggtitle("")+
  geom_line()+
  xlab(" interaction_type (Social_projection_bipartite)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

interaction_type_Social_projection_bipartite.shap.interact


class_Mammalia.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                           c("class_Mammalia"),
                                           color_var = "auto")+
  ggtitle("")+
  geom_line()+
  xlab("class (mammalia)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

class_Mammalia.shap.interact


time_resolution_secs_Very_fine.shap.interact=sv_dependence(final_meta_shap$Spatial, 
                                                           c("time_resolution_secs_Very_fine"),
                                                           color_var = "auto")+
  ggtitle("")+
  geom_line()+
  xlab("time_resolution_secs (Very_fine)")+
  ylab("SHAP Value")+
  theme_bw()+
  theme(plot.title = element_text(size = 12),
        text = element_text(size = 12,family="serif"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())

time_resolution_secs_Very_fine.shap.interact




####################################################################################
# ALE AND SHAPELY  ANALYSIS
####################################################################################
#######################################################################################
# # Creating the model -one hot encoding
# ########################################################################################
# ##----Creting the recipe object
# metadata.rec=recipe(target ~ ., data = metadata_train)%>%
#   step_dummy(all_nominal_predictors(),one_hot = T)
# #step_dummy(Target, one_hot = T)
# 
# #--compute missing values for numeric predictors
# saveRDS(metadata.rec,"metadata.rec.rds")
# 
# metadata.prep.rf=metadata.rec%>%
#   prep()
# 
# saveRDS(metadata.prep.rf,"metadata.prep.rf")
# 
# metadata.juice.rf=juice(metadata.prep.rf)
# 
# saveRDS(metadata.juice.rf,"metadata.juice.rf")
# 
# colSums(is.na(metadata.juice.rf))
# 
# last_metadata_df=metadata.juice.rf
# saveRDS(last_metadata_df,"last_metadata_df.rds")
# 
# colnames(metadata.juice.rf)
# #----creating final training, test and validation set
# #last_metadata_df=FinalTrainedMetadata
# set.seed(123)
# last_meta.df.split <- last_metadata_df[sample(1:nrow(last_metadata_df)),]%>%
#   initial_split(strata =  target,prop=0.7)
# 
# saveRDS(last_meta.df.split,"last_meta.df.split.rds")
# 
# last_metadata_train<- training(last_meta.df.split)
# saveRDS(last_metadata_train,"last_metadata_train.rds")
# 
# last_metadata_test<- testing(last_meta.df.split)
# saveRDS(last_metadata_test,"last_metadata_test.rds")
# 
# colnames(last_metadata_train)
# 
# last_metadata_rec=recipe(target ~ ., data = last_metadata_train)
# saveRDS(last_metadata_rec,"last_metadata_rec.rds")
# 
# #bootstraps(df.final.train, times = 100)
# last_metadata_folds <- last_metadata_train %>% bootstraps(times = 100)
# last_metadata_folds
# saveRDS(last_metadata_folds,"last_metadata_folds.rds")
# 
# ###----model-----
# rf.model.spec <-
#   rand_forest(
#     mtry = tune(),
#     min_n = tune(),
#     trees = 1000)%>%
#   set_engine("ranger",importance = "permutation") %>%
#   set_mode("classification")
# 
# #Creating workflow object
# rf.wkflw <-
#   workflow() %>%
#   add_model(rf.model.spec) %>%
#   add_recipe(last_metadata_rec)
# 
# library(finetune)
# doParallel::registerDoParallel()
# 
# set.seed(345)
# rf.res <-
#   rf.wkflw %>%
#   tune_race_anova(last_metadata_folds,
#                   grid = 25,
#                   control = control_race(save_pred = TRUE)
#                   #control = control_grid(save_pred = TRUE)#,
#                   #metrics = metric_set(roc_auc)
#   )
# 
# saveRDS(rf.res,"rf.res.rds")
# 
# #ERROR:tune_race_anova() not handling very similar metrics well
# # when you get the error it means that your metric is too close to the same for every possible model.
# # and that finetune may not be handling the metrics being the same well,
# #and is erroring in a confusing way.
# # This is due to error  coming from tune:::pulley() and it is
# #removing all the candidates at some step? Because they are too similar?
# #It results in a pretty confusing error.
# # In simple terms not enough sample to do racing
# 
# ####----Model Evaluation----####
# rf.metadata.res=rf.res
# rf.metadata.res%>%
#   collect_metrics()
# 
# library(here)
# #save trained RF classifier
# saveRDS(rf.metadata.res,"rf_metadata_res.rds")
# rf.metadata.res=readRDS("rf_metadata_res.rds")
# #Show best metric
# rf.metadata.res %>%
#   show_best(metric = "roc_auc")
# #Shows best estimated tuned parameters
# autoplot(rf.metadata.res)
# 
# rf.metadata.res %>%
#   collect_metrics() %>%
#   filter(.metric == "roc_auc") %>%
#   select(mean, min_n, mtry) %>%
#   pivot_longer(min_n:mtry,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")
# 
# rf.metadata.res %>%
#   collect_metrics() %>%
#   dplyr::filter(.metric == "roc_auc") %>%
#   dplyr::select(mean, min_n, mtry) %>%
#   pivot_longer(min_n:mtry,
#                values_to = "value",
#                names_to = "parameter")%>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")
# #Viewing best results
# rf.metadata.res %>%
#   collect_metrics() %>%
#   dplyr::filter(.metric == "roc_auc") %>%
#   mutate(min_n = factor(min_n)) %>%
#   ggplot(aes(mtry, mean, color = min_n)) +
#   geom_line(alpha = 0.5, linewidth = 1.5) +
#   geom_point() +
#   labs(y = "AUC")
# 
# ####----Select best tuned parameters for final model-----####
# best_metadata_auc <- select_best(rf.metadata.res, "roc_auc")
# best_metadata_auc
# #sabeRDS()
# 
# rf.final.model.metadata <- finalize_model(
#   rf.model.spec,
#   best_metadata_auc)
# 
# 
# # rf.final.model.metadata
# # final.model.metadata%>%
# # set_engine("ranger", importance = "permutation") %>%
# # fit(Targets ~ .,data = metadata.prep.rf) %>%
# #   vip(geom = "point")
# 
# rf.final.model.metadata%>%
#   set_engine("ranger", importance = "permutation") %>%
#   fit(target ~ .,data = last_metadata_df) %>%
#   vip(geom = "col")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# 
# 
# 
# ####################################################
# # var imp for multiple sim of meta data
# ####################################################
# # 
# # best.meta.model.specs <-
# #   rf.metadata.res %>%
# #   select_best(metric = "accuracy")
# 
# #----best model specification
# # metadata_final_model <- rand_forest(mode = "classification", mtry = best.meta.model.specs$mtry,
# #                                     min_n=best.meta.model.specs$min_n,trees = 1000) %>% 
# #   set_engine("ranger",importance = "impurity")
# 
# 
# # metafolds=bootstraps(metadata_train, times = 100)
# #  
# #  get_rfmeta_imp <- function(x) {
# #    x %>% 
# #      extract_fit_parsnip() %>% 
# #      vip::vi()
# #  }
# # 
# #  
# #  rfmeta_ctrl_imp <- control_grid(extract = get_rfmeta_imp)
# # 
# # 
# # varimp.model.metadata=metadata_final_model %>%
# #      finalize_model(select_best(rf.metadata.res))%>%
# #      set_engine("ranger",importance="permutation")
# # 
# # varimp.res.metadata=workflow() %>%
# #     add_recipe(metadata.rec) %>% 
# #      add_model(varimp.model.metadata)%>% 
# #      fit_resamples(metafolds, control = rfmeta_ctrl_imp)
# 
# #######################################################################################
# 
# #----The last workflow
# rf.final.metadata.wkflw <- workflow() %>%
#   add_recipe(last_metadata_rec) %>%
#   add_model(rf.final.model.metadata)
# 
# # rf.final.metadata.fit <- 
# #   rf.final.metadata.wkflw %>%
# #   fit_resamples(metadata_folds)
# 
# #----The last fitted model
# rf.final.metadata.fit <-
#   rf.final.metadata.wkflw %>%
#   last_fit(last_meta.df.split)
# 
# saveRDS(rf.final.metadata.fit,"rf.final.metadata.fit.rds")
# 
# rf.final.metadata.fit %>% 
#   extract_fit_parsnip()%>%
#   vip(geom = "col")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# ggsave("metadata.varimp.png",width = 10,height = 5)
# ###----Save----Trained----RF----last----model----###
# #save trained RF classifier
# saveRDS(rf.final.metadata.fit  ,"rf_final_metadata_fit.rds")
# 
# rf.final.metadata.fit=readRDS(("rf.final.metadata.fit.rds"))
# 
# #collect results
# rf.final.metadata.fit%>%
#   collect_metrics()
# 
# #Collect prediction
# rf.final.metadata.fit %>%
#   collect_predictions()
# 
# rf.final.metadata.fit %>%
#   collect_predictions() %>%
#   roc_curve(target, c(`.pred_1`,
#                       `.pred_2`,`.pred_3`,.pred_4,
#                       `.pred_5`)) %>%autoplot()
# 
# ###########################################################################################
# #saveRDS(rf.final.metadata.fit,"rf.final.metadata.fit.rds")
# #To make predictions on new data set, we call the saved RF model eg
# load_fitted_rf_metadata_model=readRDS("rf.final.metadata.fit.rds")
# #extract workflow fro fitted model
# final.metadata.model=extract_workflow(load_fitted_rf_metadata_model)
# saveRDS(final.metadata.model,"final.metadata.model.rds")
# #Prediction on test set
# pred_result_metadata=augment(final.metadata.model,last_metadata_test)
# 
# predValsmetadata=pred_result_metadata%>%
#   slice_head(n=10)
# predValsmetadata
# 
# 
# #sort(pred_result_metadata$Target)
# #sort(pred_result_metadata$.pred_class)
# ##Confusion Matrix
# #pred_result_metadata%>%
# table(pred_result_metadata$target,pred_result_metadata$.pred_class)
# #conf_mat(truth = Target,estimate =.pred_class)
# 
# ##############################################################################################
# # LOCAL FEATURE EFFECTS FOR METADATA: Accumulated Local Effects (ALE) PLOT
# ##############################################################################################
# extract_final.fitted.metadatamodel= extract_workflow(rf.final.metadata.fit)
# extract_final.fitted.metadatamodel
# 
# #feature set
# meta.features <- df_final_meta_data %>%
#   dplyr::select(-target)%>%
#   as.data.frame()
# #target variable
# #df.final.test[sample(1:nrow(df.final.test)), ]
# 
# meta.target <- df_final_meta_data %>%
#   dplyr::select(target)
# #prediction function
# meta.predict.wrapper <- function(model, newdata){
#   workflows:::predict.workflow(object = model, new_data = newdata)
# }
# 
# ##The predictor function enables to interrogate or get explanation of the data
# metadata_predictorfunc <- Predictor$new(
#   model =extract_final.fitted.metadatamodel,
#   data = meta.features,
#   y = meta.target,
#   predict.fun = meta.predict.wrapper
# )
# 
# saveRDS(metadata_predictorfunc,"metadata_predictorfunc.rds")
# 
# metadata_predictorfunc=readRDS("metadata_predictorfunc.rds")  
# #----FEATURE IMPORTANCE
# meta.feature.importance <- FeatureImp$new(metadata_predictorfunc, loss = "ce")
# 
# meta.feature.importance
# 
# 
# metadata.pred.class <- Predictor$new(metadata_predictorfunc$model,
#                                      data = meta.features,
#                                      type = "prob")
# 
# metadata.pred.class
# 
# 
# 
# ###############################################################################
# # Ale plots for meta data
# ###############################################################################
# #captive edge_weight interaction_type data_collection species
# ###----Fetaure effects for top 5 selected variables/predictors----###
# #Species
# species.preds.ale <- FeatureEffect$new(metadata.pred.class,
#                                        feature = "species")
# species.preds.pdp <- FeatureEffect$new(metadata.pred.class,
#                                        feature = "species",method="pdp")
# 
# species.ale.df=species.preds.ale$results
# species.pdp.df=species.preds.pdp$results
# # Rename the ".class" categories
# species.ale.df <-  species.ale.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# species.pdp.df <-  species.pdp.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# 
# # View the modified dataframe
# species.ale.df=species.ale.df%>%
#   filter(.class=="Spatial")
# 
# species.pdp.df=species.pdp.df%>%
#   filter(.class=="Spatial")
# 
# species.ale.df <- within(species.ale.df, 
#                          .value <-sort(.value,decreasing=TRUE))
# 
# species.pdp.df <- within(species.pdp.df, 
#                          .value <-sort(.value,decreasing=TRUE))
# 
# #species.df <- rbind(species.pdp.df, species.ale.df)
# # colnames(species.pdp.df)[colnames(species.pdp.df) == ".type"] <- "method"
# # colnames(species.ale.df)[colnames(species.ale.df) == ".type"] <- "method"
# #----Species plot
# species.ale.plot=ggplot(species.ale.df, aes(x = species, y = .value)) +
#   geom_bar(stat = "identity",fill = "maroon4")+
#   labs(x = "Species", y = "ale") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1,size=12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# species.ale.plot
# 
# species.pdp.plot=ggplot(species.pdp.df, aes(x = species, y = .value)) +
#   geom_bar(stat = "identity",fill = "grey67")+
#   labs( x="",y = "pdp",title = "Spatial") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# species.pdp.plot
# 
# species_plot=ggarrange(species.pdp.plot,species.ale.plot,
#                        nrow = 2,
#                        labels = c("A)","B)"),
#                        hjust = -0.7,
#                        vjust = 1,
#                        font.label = list(size = 12, color = "black", face = "bold",
#                                          family = "serif",
#                                          common.legend=T,legend.position="bottom"))
# 
# ggsave("species_plot.png", width = 10, height=8)
# 
# 
# 
# captive.preds.ale <- FeatureEffect$new(metadata.pred.class,
#                                        feature = "captive")
# captive.preds.pdp <- FeatureEffect$new(metadata.pred.class,
#                                        feature = "captive",method="pdp")
# 
# captive.ale.df=captive.preds.ale$results
# captive.pdp.df=captive.preds.pdp$results
# # Rename the ".class" categories
# captive.ale.df <-  captive.ale.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# captive.pdp.df <-  captive.pdp.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# 
# # View the modified dataframe
# captive.ale.df=captive.ale.df%>%
#   filter(.class=="Spatial")
# 
# captive.pdp.df=captive.pdp.df%>%
#   filter(.class=="Spatial")
# 
# 
# captive.ale.df <- within(captive.ale.df, 
#                          captive <-sort(captive,decreasing=TRUE))
# 
# captive.pdp.df <- within(captive.pdp.df, 
#                          captive <-sort(captive,decreasing=TRUE))
# 
# 
# #----captive plot
# captive.ale.plot=ggplot(captive.ale.df, aes(x = captive, y = .value)) +
#   geom_bar(stat = "identity",fill = "maroon4")+
#   labs(x = "captive", y = "ale") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# captive.ale.plot
# 
# captive.pdp.plot=ggplot(captive.pdp.df, aes(x = captive, y = .value)) +
#   geom_bar(stat = "identity",fill = "grey67")+
#   labs( x="",y = "pdp",title = "Spatial") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# captive.pdp.plot
# 
# captive_plot=ggarrange(captive.pdp.plot,captive.ale.plot,
#                        nrow = 2,
#                        labels = c("A)","B)"),
#                        hjust = -0.7,
#                        vjust = 1,
#                        font.label = list(size = 12, color = "black", face = "bold",
#                                          family = "serif",
#                                          common.legend=T,legend.position="bottom"))
# 
# ggsave("captive_plot.png", width = 10, height=8)
# 
# 
# class.preds.ale <- FeatureEffect$new(metadata.pred.class,
#                                      feature = "class")
# class.preds.pdp <- FeatureEffect$new(metadata.pred.class,
#                                      feature = "class",method="pdp")
# 
# class.ale.df=class.preds.ale$results
# class.pdp.df=class.preds.pdp$results
# # Rename the ".class" categories
# class.ale.df <-  class.ale.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# class.pdp.df <-  class.pdp.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# 
# # View the modified dataframe
# class.ale.df=class.ale.df%>%
#   filter(.class=="Spatial")
# 
# class.pdp.df=class.pdp.df%>%
#   filter(.class=="Spatial")
# 
# 
# class.ale.df <- within(class.ale.df, 
#                        class <-sort(class,decreasing=TRUE))
# 
# class.pdp.df <- within(class.pdp.df, 
#                        class <-sort(class,decreasing=TRUE))
# 
# 
# #----class plot
# class.ale.plot=ggplot(class.ale.df, aes(x = class, y = .value)) +
#   geom_bar(stat = "identity",fill = "maroon4")+
#   labs(x = "Class", y = "ale") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# class.ale.plot
# 
# class.pdp.plot=ggplot(class.pdp.df, aes(x = class, y = .value)) +
#   geom_bar(stat = "identity",fill = "grey67")+
#   labs( x="",y = "pdp",title = "Spatial") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# class.pdp.plot
# 
# class_plot=ggarrange(class.pdp.plot,class.ale.plot,
#                      nrow = 2,
#                      labels = c("A)","B)"),
#                      hjust = -0.7,
#                      vjust = 1,
#                      font.label = list(size = 12, color = "black", face = "bold",
#                                        family = "serif",
#                                        common.legend=T,legend.position="bottom"))
# 
# ggsave("class_plot.png", width = 10, height=8)
# 
# 
# interaction_type.preds.ale <- FeatureEffect$new(metadata.pred.class,
#                                                 feature = "interaction_type")
# interaction_type.preds.pdp <- FeatureEffect$new(metadata.pred.class,
#                                                 feature = "interaction_type",method="pdp")
# 
# interaction_type.ale.df=interaction_type.preds.ale$results
# interaction_type.pdp.df=interaction_type.preds.pdp$results
# # Rename the ".class" categories
# interaction_type.ale.df <-  interaction_type.ale.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# interaction_type.pdp.df <-  interaction_type.pdp.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# 
# # View the modified dataframe
# interaction_type.ale.df=interaction_type.ale.df%>%
#   filter(.class=="Spatial")
# 
# interaction_type.pdp.df=interaction_type.pdp.df%>%
#   filter(.class=="Spatial")
# 
# 
# interaction_type.ale.df <- within(interaction_type.ale.df, 
#                                   class <-sort(interaction_type,decreasing=TRUE))
# 
# interaction_type.pdp.df <- within(interaction_type.pdp.df, 
#                                   class <-sort(interaction_type,decreasing=TRUE))
# 
# 
# #----class plot
# interaction_type.ale.plot=ggplot(interaction_type.ale.df, aes(x = interaction_type, y = .value)) +
#   geom_bar(stat = "identity",fill = "maroon4")+
#   labs(x = "Interaction type", y = "ale") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# interaction_type.ale.plot
# 
# interaction_type.pdp.plot=ggplot(interaction_type.pdp.df, aes(x = class, y = .value)) +
#   geom_bar(stat = "identity",fill = "grey67")+
#   labs( x="",y = "pdp",title = "Spatial") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# interaction_type.pdp.plot
# 
# interaction_type_plot=ggarrange(interaction_type.pdp.plot,interaction_type.ale.plot,
#                                 nrow = 2,
#                                 labels = c("A)","B)"),
#                                 hjust = -0.7,
#                                 vjust = 1,
#                                 font.label = list(size = 12, color = "black", face = "bold",
#                                                   family = "serif",
#                                                   common.legend=T,legend.position="bottom"))
# 
# ggsave("interaction_type_plot.png", width = 10, height=8)
# 
# data_collection.preds.ale <- FeatureEffect$new(metadata.pred.class,
#                                                feature = "data_collection")
# data_collection.preds.pdp <- FeatureEffect$new(metadata.pred.class,
#                                                feature = "data_collection",method="pdp")
# 
# data_collection.ale.df=data_collection.preds.ale$results
# data_collection.pdp.df=data_collection.preds.pdp$results
# # Rename the ".class" categories
# data_collection.ale.df <-  data_collection.ale.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# data_collection.pdp.df <-  data_collection.pdp.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# 
# # View the modified dataframe
# data_collection.ale.df=data_collection.ale.df%>%
#   filter(.class=="Spatial")
# 
# data_collection.pdp.df=data_collection.pdp.df%>%
#   filter(.class=="Spatial")
# 
# 
# data_collection.ale.df <- within(data_collection.ale.df, 
#                                  class <-sort(data_collection,decreasing=TRUE))
# 
# data_collection.pdp.df <- within(data_collection.pdp.df, 
#                                  class <-sort(data_collection,decreasing=TRUE))
# 
# 
# #----class plot
# data_collection.ale.plot=ggplot(data_collection.ale.df, aes(x = data_collection, y = .value)) +
#   geom_bar(stat = "identity",fill = "maroon4")+
#   labs(x = "Data collection", y = "ale") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# data_collection.ale.plot
# 
# data_collection.pdp.plot=ggplot(data_collection.pdp.df, aes(x = class, y = .value)) +
#   geom_bar(stat = "identity",fill = "grey67")+
#   labs( x="",y = "pdp",title = "Spatial") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# data_collection.pdp.plot
# 
# data_collection_plot=ggarrange(data_collection.pdp.plot,data_collection.ale.plot,
#                                nrow = 2,
#                                labels = c("A)","B)"),
#                                hjust = -0.7,
#                                vjust = 1,
#                                font.label = list(size = 12, color = "black", face = "bold",
#                                                  family = "serif",
#                                                  common.legend=T,legend.position="bottom"))
# 
# ggsave("data_collection_plot.png", width = 10, height=8)
# 
# 
# time_resolution_secs.preds.ale <- FeatureEffect$new(metadata.pred.class,
#                                                     feature = "time_resolution_secs")
# time_resolution_secs.preds.pdp <- FeatureEffect$new(metadata.pred.class,
#                                                     feature = "time_resolution_secs",method="pdp")
# 
# time_resolution_secs.ale.df=time_resolution_secs.preds.ale$results
# time_resolution_secs.pdp.df=time_resolution_secs.preds.pdp$results
# # Rename the ".class" categories
# time_resolution_secs.ale.df <-  time_resolution_secs.ale.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# time_resolution_secs.pdp.df <-  time_resolution_secs.pdp.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_Erdös-Rényi" ~ "Erdös-Rényi",
#     .class == ".pred_Stochastic-Block-Model" ~ "Stochastic-Block-Model",
#     .class == ".pred_Scale-Free" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_Small-World" ~ "Small-World"
#   ))
# 
# 
# # View the modified dataframe
# time_resolution_secs.ale.df=time_resolution_secs.ale.df%>%
#   filter(.class=="Spatial")
# 
# time_resolution_secs.pdp.df=time_resolution_secs.pdp.df%>%
#   filter(.class=="Spatial")
# 
# 
# time_resolution_secs.ale.df <- within(time_resolution_secs.ale.df, 
#                                       class <-sort(time_resolution_secs,decreasing=TRUE))
# 
# time_resolution_secs.pdp.df <- within(time_resolution_secs.pdp.df, 
#                                       class <-sort(time_resolution_secs,decreasing=TRUE))
# 
# 
# #----class plot
# time_resolution_secs.ale.plot=ggplot(time_resolution_secs.ale.df, aes(x = time_resolution_secs, y = .value)) +
#   geom_bar(stat = "identity",fill = "maroon4")+
#   labs(x = "Time resolution seconds", y = "ale") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# time_resolution_secs.ale.plot
# 
# time_resolution_secs.pdp.plot=ggplot(time_resolution_secs.pdp.df, aes(x = class, y = .value)) +
#   geom_bar(stat = "identity",fill = "grey67")+
#   labs( x="",y = "pdp",title = "Spatial") +
#   theme(strip.text = element_text(size = 12))+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.text = element_text(size = 12))
# 
# time_resolution_secs.pdp.plot
# 
# time_resolution_secs_plot=ggarrange(time_resolution_secs.pdp.plot,time_resolution_secs.ale.plot,
#                                     nrow = 2,
#                                     labels = c("A)","B)"),
#                                     hjust = -0.7,
#                                     vjust = 1,
#                                     font.label = list(size = 12, color = "black", face = "bold",
#                                                       family = "serif",
#                                                       common.legend=T,legend.position="bottom"))
# 
# ggsave("time_resolution_secs_plot.png", width = 10, height=8)
# 
# 
# 
# 
# 
# ########################################################################################
# # Shap Dependency  interaction plots for meta data
# ########################################################################################
# 
# 
# #####################################
# # Shapeley model 
# #####################################
# meta.model.shapley <- Shapley$new(metadata.pred.class, x.interest = meta.features [1, ])
# meta.model.shapley$plot()
# 
# meta.model.shapley$explain(x.interest = meta.features[2, ])
# meta.model.shapley$plot()
# 
# 
# 
# ##----renaming variables
# new_meta_column_names=c("Captive", "Edge weight", "Interaction type", "Data collection",
#                         "Species", "Data duration days", "Class","Time resolution secs",
#                         "target")
# 
# colnames(df_final_meta_data)=new_meta_column_names
# # str(df.final)
# #df.final=df.final.juice
# #----new receipe
# # df.new.rec=recipe(graph_name~., data = df.final)
# # df.new.prep=df.new.rec%>%
# #   prep()
# 
# #meta_final_resample_data <- df_final_meta_data#[sample(nrow(df.final), 1000), ]
# #meta_final_resample_data_prep=
# # meta_final_resample_data_prep <- bake(
# #   metadata.prep.rf, # prep(df.final.rec), 
# #   has_role("predictor"),
# #   new_data = meta_final_resample_data, 
# #   composition = "matrix"
# # )
# head(df_final_meta_data)
# 
# 
# ##----convert categorical factors with one-hot encoding----
# metadata.onehot=mlr::createDummyFeatures(df_final_meta_data %>%
#                                            select(-target))
# 
# Targets=df_final_meta_data$target
# metadata.encoded=cbind(metadata.onehot,Targets)
# 
# 
# #model <- ranger::ranger(Targets ~ ., data = metadata.encoded)
# 
# #model_unified <- ranger.unify(model, metadata.encoded)
# #extract_fit_engine(rf.final.metadata.fit)
# 
# metadata_encoded_prep <- bake(
#   metadata.prep.rf, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = metadata.juice.rf, 
#   composition = "matrix"
# )
# 
# 
# shap.metadata <- shapr(extract_fit_engine(rf.final.metadata.fit), metadata.juice.rf,
#                        interactions = TRUE)
# 
# saveRDS(shap.metadata,"shap.metadata.rds")
# shap.metadata=readRDS("shap.metadata.rds")
# 
# ###----getting shap values----
# shap_vals=get_shap_values(shap)
# shap_vals
# 
# 
# # Specify the font size and family
# # font_size <- 12
# # font_family <- "serif"
# # 
# # # Create an expression with the desired font properties
# # my_expression <- bquote(atop(.(my_string), list(font(size = .(font_size), family = .(font_family)))))
# # bquote(atop(.(x), list(font(size = .(font_size), family = .(font_family)))))
# # x="Erdös-Rényi"
# # expression(atop(textstyle("Erdös-Rényi"), font = 12, family = "Arial"))
# # Renaming elements in the list
# 
# names(shap_vals)[names(shap_vals) == "Class_1"] <- "Erdös-Rényi"
# names(shap_vals)[names(shap_vals) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_vals)[names(shap_vals) == "Class_3"] <- "Scale-Free"
# names(shap_vals)[names(shap_vals) == "Class_4"] <- "Spatial"
# names(shap_vals)[names(shap_vals) == "Class_5"] <- "Small-World"
# colnames(shap_vals$`Erdös-Rényi`)=new_column_names
# colnames(shap_vals$`Stochastic-Block-Model`)=new_column_names
# colnames(shap_vals$`Scale-Free`)=new_column_names
# colnames(shap_vals$Spatial)=new_column_names
# colnames(shap_vals$`Small-World`)=new_column_names
# shap_vals
# 
# 
# 
# shap_feat=get_feature_values(shap)
# names(shap_feat)[names(shap_feat) == "Class_1"] <- "Erdös-Rényi"
# names(shap_feat)[names(shap_feat) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_feat)[names(shap_feat) == "Class_3"] <- "Scale-Free"
# names(shap_feat)[names(shap_feat) == "Class_4"] <- "Spatial"
# names(shap_feat)[names(shap_feat) == "Class_5"] <- "Small-World"
# colnames(shap_feat$`Erdös-Rényi`)=new_column_names
# colnames(shap_feat$`Stochastic-Block-Model`)=new_column_names
# colnames(shap_feat$`Scale-Free`)=new_column_names
# colnames(shap_feat$Spatial)=new_column_names
# colnames(shap_feat$`Small-World`)=new_column_names
# shap_feat
# 
# 
# shap_baseline=get_baseline(shap)
# names(shap_baseline)[names(shap_baseline) == "Class_1"] <- "Erdös-Rényi"
# names(shap_baseline)[names(shap_baseline) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_baseline)[names(shap_baseline) == "Class_3"] <- "Scale-Free"
# names(shap_baseline)[names(shap_baseline) == "Class_4"] <- "Spatial"
# names(shap_baseline)[names(shap_baseline) == "Class_5"] <- "Small-World"
# 
# 
# 
# er.shap=shapviz(shap_vals$`Erdös-Rényi`,shap_feat$`Erdös-Rényi`,shap_baseline$`Erdös-Rényi`)
# sbm.shap=shapviz(shap_vals$`Stochastic-Block-Model`,shap_feat$`Stochastic-Block-Model`,
#                  shap_baseline$`Stochastic-Block-Model`)
# sf.shap=shapviz(shap_vals$`Scale-Free`,shap_feat$`Scale-Free`,
#                 shap_baseline$`Scale-Free`)
# sp.shap=shapviz(shap_vals$Spatial,shap_feat$Spatial,
#                 shap_baseline$Spatial)
# sw.shap=shapviz(shap_vals$`Small-World`,shap_feat$`Small-World`,
#                 shap_baseline$`Small-World`)
# 
# 
# final_shap <- mshapviz(c('Erdös-Rényi' = er.shap, 'Stochastic-Block-Model'=sbm.shap,
#                          'Scale-Free'= sf.shap,
#                          'Spatial'=sp.shap,
#                          'Small-World'=sw.shap))
# 
# 
# #shap.plot.summary(shap_vals)
# # final_shap+ theme(
# #   text = element_text(size = 12)  # Set the desired font size (e.g., 12)
# # )
# size=12
# family="serif"
# 
# 
# 
# sv_importance(final_shap, kind = "both")#, show_numbers = TRUE)#var imp plot
# sv_importance(final_shap, kind = 'beeswarm')
# ###----Erdos Renyi var imp----###
# er_shap_imp=sv_importance(final_shap$`Erdös-Rényi`, kind = 'beeswarm')+
#   ggtitle("Erdös-Rényi")+
#   xlab("SHAP Value") +
#   xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# er_shap_imp=er_shap_imp & scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# er_shap_imp
# ggsave("er_shap_varimp.png", width = 10, height = 10)
# 
# ###----SBM var imp----###
# sbm_shap_imp=sv_importance(final_shap$`Stochastic-Block-Model`, 
#                            kind = 'beeswarm')+
#   ggtitle("Stochastic-Block-Model")+
#   xlab("SHAP Value") +
#   xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# 
# sbm_shap_imp=sbm_shap_imp & scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# 
# ggsave("sbm_shap_varimp.png", width = 10, height = 10)
# 
# ###----combined erdos renyi and sbm plots
# er_sbm =ggarrange(er_shap_imp, sbm_shap_imp, nrow=1,ncol = 2, common.legend = TRUE, legend = "bottom")
# er_sbm
# 
# ggsave("er_and_sbm_varimp.png", width = 10, height = 6)
# 
# # er_sbm =er_shap_imp+sbm_shap_imp+
# #   plot_layout(nrow = 1)+
# #   theme(legend.direction = "horizontal",  # Set legend direction to horizontal
# #         legend.position = "bottom")
# 
# 
# ###----SF var imp----###
# sf_shap_imp=sv_importance(final_shap$`Scale-Free`, kind = 'beeswarm')+
#   ggtitle("Scale-Free")+
#   xlab("SHAP Value") +
#   xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# sf_shap_imp=sf_shap_imp & scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# ggsave("sf_shap_varimp.png", width = 10, height = 10)
# 
# 
# ###----Spatial var imp----###
# sp_shap_imp=sv_importance(final_shap$Spatial, kind = 'beeswarm')+
#   ggtitle("Spatial")+
#   xlab("SHAP Value") +
#   xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# sp_shap_imp
# sp_shap_imp=sp_shap_imp& scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# ggsave("sp_shap_varimp.png", width = 10, height = 10)
# ###----Spatial var imp----###
# sw_shap_imp=sv_importance(final_shap$`Small-World`, kind = 'beeswarm')+
#   ggtitle("Small-World")+
#   xlab("SHAP Value") +
#   xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position="none")
# 
# 
# sw_shap_imp
# sw_shap_imp=sw_shap_imp& scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# ggsave("sw_shap_varimp.png", width = 10, height = 10)
# 
# 
# ###----combined erdos renyi and sbm plots
# sf_sp_sw =ggarrange(sf_shap_imp, sp_shap_imp,sw_shap_imp,nrow=2,ncol = 2, common.legend = TRUE, legend = "bottom")
# sf_sp_sw
# 
# ggsave("sf_sp_sw_varimp.png", width = 10, height = 12)
# 
# 
# 
# #guides(color = guide_colorbar(direction = "horizontal"))
# 
# # er_sbm <- er_shap_imp + sbm_shap_imp & scale_color_continuous() &
# #   theme(legend.position='bottom', legend.direction='horizontal')
# 
# 
# 
# #guides(fill = guide_legend(reverse = TRUE)) & theme(legend.position='bottom', legend.direction='horizontal')
# #er_sbm + plot_layout(guides = "collect")
# 
# 
# # er_sbm=ggarrange(er_shap_imp,sbm_shap_imp,
# #           nrow=1,common.legend = TRUE, legend = "bottom")+
# # guides(fill = guide_legend(reverse = TRUE))
# #      
# # coord_flip() +
# # theme(legend.position='bottom', legend.direction='horizontal')
# 
# # all_imp=ggarrange(er_sbm,sf_sp,
# #           sw_shap_imp,
# #           nrow = 3,
# #           common.legend = TRUE, legend = "right")
# # 
# # all_imp
# 
# ############################################################
# # Shap Dependency plot
# ############################################################
# 
# ###----Shap variable dependency plot----###
# mod.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Modularity",
#                           interactions = FALSE, color_var = NULL)+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.er  
# 
# mod.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Modularity",
#                            interactions = FALSE, color_var = NULL)+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.sbm  
# 
# 
# mod.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Modularity",
#                           interactions = FALSE, color_var = NULL)+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.sf
# 
# 
# mod.shap.sp=sv_dependence(final_shap$Spatial, "Modularity",
#                           interactions = FALSE, color_var = NULL)+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.sp
# 
# 
# mod.shap.sw=sv_dependence(final_shap$`Small-World`, "Modularity",
#                           interactions = FALSE, color_var = NULL)+
#   ggtitle("Small-World")+
#   xlab("Modularity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.sw
# 
# modularity.figure <- ggarrange(mod.shap.er, mod.shap.sbm, mod.shap.sf,
#                                mod.shap.sp,mod.shap.sw,
#                                ncol = 3, nrow = 2,widths=1.5, heights=2)
# modularity.figure
# 
# #mod.shap$labels$title
# ###----Transitivity shapely depedency plot----###
# trans.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Transitivity",
#                             interactions = FALSE, color_var = NULL)+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.er  
# 
# trans.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Transitivity",
#                              interactions = FALSE, color_var = NULL)+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.sbm  
# 
# 
# trans.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Transitivity",
#                             interactions = FALSE, color_var = NULL)+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.sf
# 
# 
# trans.shap.sp=sv_dependence(final_shap$Spatial, "Transitivity",
#                             interactions = FALSE, color_var = NULL)+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.sp
# 
# 
# trans.shap.sw=sv_dependence(final_shap$`Small-World`, "Transitivity",
#                             interactions = FALSE, color_var = NULL)+
#   ggtitle("Small-World")+
#   xlab("Transitivity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.sw
# 
# transitivity.figure <- ggarrange(trans.shap.er, trans.shap.sbm, trans.shap.sf,
#                                  trans.shap.sp,trans.shap.sw,
#                                  ncol = 3, nrow = 2,widths=4, heights=5)
# transitivity.figure
# 
# ###----Normalized fielder shapely dependency plot----###
# nf.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.er  
# 
# nf.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Normalized fiedler value",
#                           interactions = FALSE, color_var = NULL)+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sbm  
# 
# 
# nf.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sf
# 
# 
# nf.shap.sp=sv_dependence(final_shap$Spatial, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sp
# 
# 
# nf.shap.sw=sv_dependence(final_shap$`Small-World`, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)+
#   ggtitle("Small-World")+
#   xlab("Normalized fiedler value")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sw
# 
# norm_fiedler.figure <- ggarrange(nf.shap.er, nf.shap.sbm, nf.shap.sf,
#                                  nf.shap.sp,nf.shap.sw,
#                                  ncol = 3, nrow = 2,widths=1.5, heights=2)
# norm_fiedler.figure
# 
# ###----Degree assortativity shapely dependency plot----###
# deg_assort.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.er  
# 
# deg_assort.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Degree assortativity coeff",
#                                   interactions = FALSE, color_var = NULL)+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sbm  
# 
# 
# deg_assort.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sf
# 
# 
# deg_assort.shap.sp=sv_dependence(final_shap$Spatial, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sp
# 
# 
# deg_assort.shap.sw=sv_dependence(final_shap$`Small-World`, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)+
#   ggtitle("Small-World")+
#   xlab("Degree assortativity coeff")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sw
# 
# deg_assort.figure <- ggarrange(deg_assort.shap.er, deg_assort.shap.sbm, deg_assort.shap.sf,
#                                deg_assort.shap.sp,deg_assort.shap.sw,
#                                ncol = 3, nrow = 2,widths=4, heights=5)
# deg_assort.figure
# 
# ###----Degree assortativity shapely dependency plot----###
# deg_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.er  
# 
# deg_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Degree centrality",
#                                  interactions = FALSE, color_var = NULL)+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sbm  
# 
# 
# deg_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sf
# 
# 
# deg_centr.shap.sp=sv_dependence(final_shap$Spatial, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sp
# 
# 
# deg_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)+
#   ggtitle("Small-World")+
#   xlab("Degree centrality")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sw
# 
# deg_centr.figure <- ggarrange(deg_centr.shap.er, deg_centr.shap.sbm, deg_centr.shap.sf,
#                               deg_centr.shap.sp,deg_centr.shap.sw,
#                               ncol = 3, nrow = 2,widths=4, heights=5)
# deg_centr.figure
# 
# 
# 
# #######################################################
# # shap and ale plots combined
# #######################################################
# mod.combine=ggarrange(modularity.figure,
#                       ggarrange(modularity.plot, 
#                                 align = "v",widths = c(0.5,1)),
#                       #   align = "v",
#                       nrow = 2,
#                       labels = c("A)","B)"),
#                       # label.x = 0,
#                       # label.y = 1,
#                       # heights = c(1.5, 1, 1),
#                       # widths = c(.5,.5,.5),
#                       hjust = -1.1,#-3.1,
#                       vjust = 0.9,
#                       font.label = list(size = 12, color = "black", face = "bold",
#                                         family = "serif"))
# mod.combine
# 
# ggsave("mod.combine.png", width = 4, height =4 )
# 
# trans.combine=ggarrange(transitivity.figure,
#                         ggarrange(transitivity.plot, 
#                                   align = "v",widths = c(0.5,1)),
#                         #   align = "v",
#                         nrow = 2,
#                         labels = c("A)","B)"),
#                         # label.x = 0,
#                         # label.y = 1,
#                         # heights = c(1.5, 1, 1),
#                         # widths = c(.5,.5,.5),
#                         hjust = -1.1,#-3.1,
#                         vjust = 0.9,
#                         font.label = list(size = 12, color = "black", face = "bold",
#                                           family = "serif"))
# trans.combine
# 
# ggsave("trans.combine.png", width = 8, height = 8)
# 
# normfiedler.combine=ggarrange(norm_fiedler.figure,
#                               ggarrange(normalized_fiedler_value.plot, 
#                                         align = "v",widths = c(0.5,1)),
#                               #   align = "v",
#                               nrow = 2,
#                               labels = c("A)","B)"),
#                               # label.x = 0,
#                               # label.y = 1,
#                               # heights = c(1.5, 1, 1),
#                               # widths = c(.5,.5,.5),
#                               hjust = -1.1,#-3.1,
#                               vjust = 0.9,
#                               font.label = list(size = 12, color = "black", face = "bold",
#                                                 family = "serif"))
# 
# normfiedler.combine
# ggsave("normfiedler.combine.png", width = 8, height = 8)
# 
# deg.centr.combine=ggarrange(deg_centr.figure,
#                             ggarrange(deg_centr.plot, 
#                                       align = "v",widths = c(0.5,1)),
#                             #   align = "v",
#                             nrow = 2,
#                             labels = c("A)","B)"),
#                             # label.x = 0,
#                             # label.y = 1,
#                             # heights = c(1.5, 1, 1),
#                             # widths = c(.5,.5,.5),
#                             hjust = -1.1,#-3.1,
#                             vjust = 0.9,
#                             font.label = list(size = 12, color = "black", face = "bold",
#                                               family = "serif"))
# 
# deg.centr.combine
# ggsave("deg.centr.combine.png", width = 8, height = 8)
# 
# 
# deg_assort.combine=ggarrange(deg_assort.figure,
#                              ggarrange(deg_assort_coef.plot, 
#                                        align = "v",widths = c(0.5,1)),
#                              #   align = "v",
#                              nrow = 2,
#                              labels = c("A)","B)"),
#                              # label.x = 0,
#                              # label.y = 1,
#                              # heights = c(1.5, 1, 1),
#                              # widths = c(.5,.5,.5),
#                              hjust = -1.1,#-3.1,
#                              vjust = 0.9,
#                              font.label = list(size = 12, color = "black", face = "bold",
#                                                family = "serif"))
# 
# deg_assort.combine
# ggsave("deg_assort.combine.png", width = 8, height = 8)


##################################################################################
#################################################################################
#----SHAPELY
##################################################################################
#################################################################################
#########################################################################################
# variable importance for 100 simulations
# #########################################################################################
# folds=bootstraps(df.final.train, times = 100)
# 
# #folds
# 
# get_xgb_imp <- function(x) {
#   x %>% 
#     extract_fit_parsnip() %>% 
#     vip::vi()
# }
# 
# ctrl_imp <- control_grid(extract = get_xgb_imp)
# 
# model=final_model
# data=df.final.train
# 
# varimp.model=model%>%
#   finalize_model(select_best(best.model))%>%
#   set_engine("xgboost",importance="permutation")
# 
# varimp.res=workflow() %>%
#   add_recipe(df.final.rec) %>% 
#   add_model(varimp.model)%>% 
#   fit_resamples(folds, control = ctrl_imp)
# 
# varimp.res
# #save trained models
# saveRDS(varimp.res,"varimp.res.rds")
# 
# varimp.res=readRDS("varimp.res.rds")
# 
# 
# rename_var_imp=
#   varimp.res %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts) %>%
#   group_by(Variable) 
# 
# 
# ##----renaming variables
# rename_var_imp$Variable=factor(rename_var_imp$Variable)
# 
# levels(rename_var_imp$Variable) <- c('Betweenness centrality','Closeness centrality',
#                                      'Degree assortativity coeff',
#                                      'Degree centrality','Diameter','Edges','Eigen centrality',
#                                      'Fiedler value','Graph energy','Mean degree','Mean eccentricity',
#                                      'Mean path length','Min cut','Modularity','Normalized fiedler value',
#                                      'Number of nodes','Spectral radius',
#                                      'Transitivity')
# 
# rename_var_imp$Variable=as.character(rename_var_imp$Variable) 
# 
# # rename_var_imp_data <- rename_var_imp
# # rename_var_imp_data$Variable<- sapply(rename_var_imp_data$Variable,  # Replace values in certain columns
# #                               function(x) replace(x, x %in% val_repl, repl))
# 
# sim.var.imp.plot=rename_var_imp%>%
#   summarise(Mean = scale(mean(Importance),center = FALSE, scale = 0.1),
#             Variance = scale(sd(Importance),center = FALSE, scale = 0.1)) %>%
#   slice_max(Mean, n = 18) %>%
#   ggplot(aes(Mean, reorder(Variable, Mean))) +
#   geom_crossbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
#   labs(x = "Variable importance", y = NULL)+
#   theme_classic()+
#   theme(text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# # theme(text = element_text(size = 15,family="serif"),
# #       axis.text.x = element_text(size = 12),
# #       axis.text.y = element_text(size = 12),
# #       axis.title.x = element_text(size = 12),
# #       axis.title.y = element_text(size = 12))+
# # theme_classic()
# 
# sim.var.imp.plot
# ggsave("sim.var.imp.plot.png", width = 10, height = 8)
# #ggsave("mtcars.pdf", width = 20, height = 20, units = "cm")
# 
# 
# #########################################################################################
# # Intepretable machine learning
# #########################################################################################
# #feature set
# features <- df.final.test %>%
#   dplyr::select(-graph_name)
# 
# saveRDS(features,"features.rds")
# #target variable
# #df.final.test[sample(1:nrow(df.final.test)), ]
# 
# target <- df.final.test %>%
#   # mutate(job_search = as.factor(job_search)) %>%
#   dplyr::select(graph_name)
# 
# saveRDS(target,"target.rds")  
# 
# #prediction function
# predict.wrapper <- function(model, newdata){
#   workflows:::predict.workflow(object = model, new_data = newdata)
# }
# 
# saveRDS(predict.wrapper,"predict.wrapper.rds")   
# ##The predictor function enables to interrogate or get explanation of the data
# predictorfunc <- Predictor$new(
#   model =extract_final.fitted.model,
#   data = features,
#   y = target,
#   predict.function = predict.wrapper
# )
# 
# saveRDS(predictorfunc,"predictorfunc.rds")
# 
# predictorfunc=readRDS("predictorfunc.rds")  
# #----FEATURE IMPORTANCE
# feature.importance <- FeatureImp$new(predictorfunc, loss = "ce")
# 
# feature.importance
# saveRDS(feature.importance,"feature.iml.importance.rds")
# 
# feature.importance=readRDS("feature.iml.importance.rds")
# #ggsave("iml.var.imp.plot.png", width = 10, height = 8)
# features=feature.importance$results$feature
# 
# rep_str=c('modularity' ='Modularity',
#           'normalized_fiedler_value'='Normalized fiedler value',
#           'transitivity' = 'Transitivity',
#           'deg_centr'='Degree centrality',
#           'deg_assort_coef'='Degree assortativity coeff',
#           'eigen_centr'='Eigen centrality',
#           'spectral_radius'='Spectral radius',
#           'mean_path_length'='Mean path length',
#           'fiedler_value'='Fiedler value',
#           'mean_degree'='Mean degree',
#           'graph_energy'='Graph energy',
#           'min_cut'='Min cut',
#           'closeness_centr'='Closeness centrality',
#           'mean_eccentr'='Mean eccentricity',
#           'edges'='Edges',
#           'betw_centr'='Betweenness centrality',
#           'num_of_nodes'='Number of nodes',
#           'diameter'='Diameter')
# feature.importance$results$feature=str_replace_all(feature.importance$results$feature,
#                                                    rep_str)
# varimp.df=feature.importance$results
# 
# sorted_data <- varimp.df[order(varimp.df$importance), ]
# iml.var.imp.plot=ggplot(varimp.df, aes(y = reorder(feature,importance), x=importance))+
#   geom_bar(stat = "identity")+
#   theme_classic()+
#   theme(text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")+
#   ylab("Features")+
#   xlab("Variable importance")   
# 
# iml.var.imp.plot
# saveRDS(iml.var.imp.plot,"iml.var.imp.plot.rds")
# 
# ggsave("iml.var.imp.plot.png", width = 10, height = 8)
# 
# theme_set(theme_pubr())
# iml.var.imp.plot
# sim.var.imp.plot
# 
# figure <- ggarrange(iml.var.imp.plot, sim.var.imp.plot,
#                     labels = c("A", "B"),
#                     ncol = 2, nrow = 1)
# 
# # theme(axis.text.y =element_text(angle = 90, vjust = 0.5, hjust = 0.1))
# 
# figure
# 
# ###----Variable importance plots-----
# ## Variable importanc from 'iml' and 'xgb-importance'
# ggarrange(iml.var.imp.plot, sim.var.imp.plot, ncol = 2,
#           labels = c("A)","B)"),
#           label.x = 0,
#           label.y = 1,
#           hjust = -8.1,#-3.1,
#           vjust = 1.0,
#           font.label = list(size = 12, color = "black", face = "bold",
#                             family = "serif"))
# 
# 
# 
# ###----Combining multiple plots----###
# # ggarrange(
# #   emp.var.imp.plot,# First row with line plot
# #   # Second row with box and dot plots
# #   ggarrange(iml.var.imp.plot, sim.var.imp.plot, ncol = 2,
# #             labels = c("B)","C)"),
# #             label.x = 0,
# #             label.y = 1,
# #             hjust = -8.1,#-3.1,
# #             vjust = 0.4,
# #             font.label = list(size = 12, color = "black", face = "bold",
# #                           family = "serif")), 
# #   nrow = 2, 
# #   labels = "A)",       # Label of the line plot
# #   font.label = list(size = 12, color = "black", face = "bold",
# #                     family = "serif")
# #   )
# 
# ##########################################################
# # LOCAL FEATURE EFFECTS: Accumulated Local Effects (ALE) PLOT
# ##########################################################
# x.data <- df.final %>%
#   dplyr::select(-c(graph_name)) %>%
#   as.data.frame()
# 
# best.model.pred.class <- Predictor$new(predictorfunc$model,
#                                        data = x.data,
#                                        type = "prob")
# 
# saveRDS(best.model.pred.class,"best.model.pred.class.rds")
# #theme_set(theme_pubr())
# #theme_set(theme_classic2())
# ###----Fetaure effects for top 5 selected variables/predictors----###
# #modularity
# modularity.preds <- FeatureEffect$new(best.model.pred.class,
#                                       feature = "modularity",
#                                       grid.size = 30)
# 
# modularity.df=modularity.preds$results
# # Rename the ".class" categories
# modularity.df <-  modularity.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(modularity.df,"modularity.df.rds")
# 
# #scale(.value,center = FALSE, scale = 0.1),
# #----Modularity plot
# modularity.plot=
#   ggplot(modularity.df, aes(x = modularity, 
#                             y = scale(.value,center = FALSE, scale = 0.001))) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Modularity") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# modularity.plot
# # modularity.df$.class <- gsub("\\.pred_", "",  modularity.df$.class)
# # modularity.df$.class <- gsub("_", "-",  modularity.df$.class)
# # 
# # modularity.df$.class[ modularity.df$.class == ".pred_ER"] <- "Erdös-Rényi"
# # df$.class[df$.class == "sbm"] <- "Stochastic-block-model"
# # df$.class[df$.class == "SF"] <- "Scale-Free"
# # df$.class[df$.class == "Spatial"] <- "Spatial"
# 
# # modularity_plot=modularity.preds%>%
# #                    plot()
# 
# 
# ###----Transitivity----###
# transitivity.preds<- FeatureEffect$new(best.model.pred.class,
#                                        feature = "transitivity",
#                                        grid.size = 30)
# transitivity.df=transitivity.preds$results
# # Rename the ".class" categories
# transitivity.df <-  transitivity.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(transitivity.df,"transitivity.df.rds")
# 
# #----Transitivity plot
# transitivity.plot=
#   ggplot(transitivity.df, aes(x = transitivity, 
#                               y = .value)) +
#   geom_line(stat = "identity") +
#   geom_hline(yintercept=0)+
#   #geom_vline(xintercept=0)+
#   facet_grid(~.class) +
#   #theme(strip.text = element_text(size = 12))+
#   xlab("Transitivity") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# transitivity.plot
# 
# # transitivity_plot=transitivity.preds%>%
# #                 plot()
# 
# ###----Normalized_fiedler_value
# normalized_fiedler_value.preds<- FeatureEffect$new(best.model.pred.class,
#                                                    feature = "normalized_fiedler_value",
#                                                    grid.size = 30)
# 
# normalized_fiedler_value.df=normalized_fiedler_value.preds$results
# # Rename the ".class" categories
# normalized_fiedler_value.df <-  normalized_fiedler_value.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(normalized_fiedler_value.df,"normalized_fiedler_value.df.rds")
# 
# #----Normalized fiedler plot
# normalized_fiedler_value.plot=
#   ggplot(normalized_fiedler_value.df, aes(x = normalized_fiedler_value, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept=0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Normalized fiedler value") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# 
# normalized_fiedler_value.plot
# 
# # normalized_fiedler_value_plot=normalized_fiedler_value.preds%>%
# #                     plot()
# 
# ###----degree assortativity----###
# deg_assort_coef.preds <- FeatureEffect$new(best.model.pred.class,
#                                            feature = "deg_assort_coef",
#                                            grid.size = 30)
# 
# deg_assort_coef.df=deg_assort_coef.preds$results
# # Rename the ".class" categories
# deg_assort_coef.df <-  deg_assort_coef.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(deg_assort_coef.df,"deg_assort_coef.df.rds")
# 
# #----Degree assortativity plot
# deg_assort_coef.plot=
#   ggplot(deg_assort_coef.df, aes(x = deg_assort_coef, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Degree assortativity coeff") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# deg_assort_coef.plot
# 
# # deg_assort_coef_plot=deg_assort_coef.preds%>%
# #                     plot()
# 
# ###----deg_centr variable
# deg_centr.preds<- FeatureEffect$new(best.model.pred.class,
#                                     feature = "deg_centr",
#                                     grid.size = 30)
# 
# deg_centr.df=deg_centr.preds$results
# # Rename the ".class" categories
# deg_centr.df <-  deg_centr.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(deg_centr.df,"deg_centr.df.rds")
# 
# 
# ####----degree cenraility plot----####
# deg_centr.plot=
#   ggplot(deg_centr.df, aes(x = deg_centr, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Degree centrality") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# deg_centr.plot
# 
# ####----Spectral radius variable----####
# spec.radius.preds<- FeatureEffect$new(best.model.pred.class,
#                                       feature = "spectral_radius",
#                                       grid.size = 30)
# 
# spec.radius.df=spec.radius.preds$results
# # Rename the ".class" categories
# spec.radius.df <-  spec.radius.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(spec.radius.df,"spec.radius.df.rds")
# 
# #----degree cenraility plot
# spec.radius.plot=
#   ggplot(spec.radius.df, aes(x = spectral_radius, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Spectral radius") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# spec.radius.plot
# 
# ####----Eigen centrality variable----####
# eigen_centr.preds<- FeatureEffect$new(best.model.pred.class,
#                                       feature = "eigen_centr",
#                                       grid.size = 30)
# 
# eigen_centr.df=eigen_centr.preds$results
# # Rename the ".class" categories
# eigen_centr.df <-  eigen_centr.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(eigen_centr.df,"eigen_centr.df.rds")
# 
# ###----eigen cenraility plot
# eigen_centr.plot=
#   ggplot(eigen_centr.df, aes(x = eigen_centr, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Eigen centrality") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# eigen_centr.plot
# 
# 
# ####----Mean eccentricity variable----####
# mean_eccentr.preds<- FeatureEffect$new(best.model.pred.class,
#                                        feature = "mean_eccentr",
#                                        grid.size = 30)
# 
# mean_eccentr.df=mean_eccentr.preds$results
# # Rename the ".class" categories
# mean_eccentr.df <-  mean_eccentr.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(mean_eccentr.df,"mean_eccentr.df.rds")
# 
# 
# ###----mean eccentricity plot
# mean_eccentr.plot=
#   ggplot(mean_eccentr.df, aes(x = mean_eccentr, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Mean eccentricity") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# mean_eccentr.plot
# 
# ####----Mean path length variable----####
# mean_path_length.preds<- FeatureEffect$new(best.model.pred.class,
#                                            feature = "mean_path_length",
#                                            grid.size = 30)
# 
# mean_path_length.df=mean_path_length.preds$results
# # Rename the ".class" categories
# mean_path_length.df <-  mean_path_length.df %>%
#   mutate(.class = case_when(
#     .class == ".pred_ER" ~ "Erdös-Rényi",
#     .class == ".pred_sbm" ~ "Stochastic-Block-Model",
#     .class == ".pred_SF" ~ "Scale-Free",
#     .class == ".pred_Spatial" ~ "Spatial",
#     .class == ".pred_SW" ~ "Small-World"
#   ))
# 
# # View the modified dataframe
# saveRDS(mean_path_length.df,"mean_path_length.df.rds")
# 
# 
# mean_path_length.plot=
#   ggplot(mean_eccentr.df, aes(x = mean_path_length, y = .value)) +
#   geom_line(stat = "identity") +
#   facet_grid(~.class) +
#   geom_hline(yintercept = 0)+
#   theme(strip.text = element_text(size = 12))+
#   xlab("Mean path length") +
#   ylab("ALE")+
#   theme_bw()+
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_rect(colour="white", fill="white"),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# mean_path_length.plot
# 
# 
# # deg_centr_plot=deg_centr.preds%>%
# #                   plot()
# 
# #Remove plots to plot new ones
# deg_centr.preds$set.feature("rm")
# ##----Interaction betwen all variables
# VarInterctns<- Interaction$new(best.model.pred.class)
# ##----Feature effects for all feature at once
# feat.effects <- FeatureEffects$new(best.model.pred.class)
# feat.effects 
# 
# #######################################################################################
# # Shapeley model 
# #######################################################################################
# #best.model.shapley <- Shapley$new(best.model.pred.class, x.interest = x.data[1, ])
# #best.model.shapley$plot()
# 
# #best.model.shapley$explain(x.interest = x.data[10, ])
# #best.model.shapley$plot()
# 
# 
# ##----renaming variables
# new_column_names=c('Number of nodes','Edges','Mean eccentricity','Mean path length',
#                    'Graph energy','Modularity','Diameter','Betweenness centrality',
#                    'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
#                    'Mean degree','Min cut','Fiedler value','Normalized fiedler value',
#                    'Closeness centrality','Degree assortativity coeff')#,'graph_name')
# 
# 
# #colnames(df.final)=new_column_names
# # str(df.final)
# #df.final=df.final.juice
# #----new receipe
# # df.new.rec=recipe(graph_name~., data = df.final)
# # df.new.prep=df.new.rec%>%
# #   prep()
# 
# final_resample_data <- df.final#[sample(nrow(df.final), 1000), ]
# final_resample_data_prep <- bake(
#   df.final.prep, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = final_resample_data, 
#   composition = "matrix"
# )
# head(final_resample_dataprep)
# 
# shap <- shapviz(extract_fit_engine(final.fitted.model), 
#                 X_pred = final_resample_data_prep, 
#                 X = final_resample_data)
# saveRDS(shap,"shap.rds")
# shap=readRDS("shap.rds")
# 
# ###----getting shap values----
# shap_vals=get_shap_values(shap)
# shap_vals
# 
# 
# # Specify the font size and family
# # font_size <- 12
# # font_family <- "serif"
# # 
# # # Create an expression with the desired font properties
# # my_expression <- bquote(atop(.(my_string), list(font(size = .(font_size), family = .(font_family)))))
# # bquote(atop(.(x), list(font(size = .(font_size), family = .(font_family)))))
# # x="Erdös-Rényi"
# # expression(atop(textstyle("Erdös-Rényi"), font = 12, family = "Arial"))
# # Renaming elements in the list
# 
# names(shap_vals)[names(shap_vals) == "Class_1"] <- "Erdös-Rényi"
# names(shap_vals)[names(shap_vals) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_vals)[names(shap_vals) == "Class_3"] <- "Scale-Free"
# names(shap_vals)[names(shap_vals) == "Class_4"] <- "Spatial"
# names(shap_vals)[names(shap_vals) == "Class_5"] <- "Small-World"
# colnames(shap_vals$`Erdös-Rényi`)=new_column_names
# colnames(shap_vals$`Stochastic-Block-Model`)=new_column_names
# colnames(shap_vals$`Scale-Free`)=new_column_names
# colnames(shap_vals$Spatial)=new_column_names
# colnames(shap_vals$`Small-World`)=new_column_names
# #shap_vals
# 
# 
# 
# shap_feat=get_feature_values(shap)
# names(shap_feat)[names(shap_feat) == "Class_1"] <- "Erdös-Rényi"
# names(shap_feat)[names(shap_feat) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_feat)[names(shap_feat) == "Class_3"] <- "Scale-Free"
# names(shap_feat)[names(shap_feat) == "Class_4"] <- "Spatial"
# names(shap_feat)[names(shap_feat) == "Class_5"] <- "Small-World"
# colnames(shap_feat$`Erdös-Rényi`)=new_column_names
# colnames(shap_feat$`Stochastic-Block-Model`)=new_column_names
# colnames(shap_feat$`Scale-Free`)=new_column_names
# colnames(shap_feat$Spatial)=new_column_names
# colnames(shap_feat$`Small-World`)=new_column_names
# #shap_feat
# 
# 
# shap_baseline=get_baseline(shap)
# names(shap_baseline)[names(shap_baseline) == "Class_1"] <- "Erdös-Rényi"
# names(shap_baseline)[names(shap_baseline) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_baseline)[names(shap_baseline) == "Class_3"] <- "Scale-Free"
# names(shap_baseline)[names(shap_baseline) == "Class_4"] <- "Spatial"
# names(shap_baseline)[names(shap_baseline) == "Class_5"] <- "Small-World"
# 
# 
# 
# er.shap=shapviz(shap_vals$`Erdös-Rényi`,shap_feat$`Erdös-Rényi`,shap_baseline$`Erdös-Rényi`)
# sbm.shap=shapviz(shap_vals$`Stochastic-Block-Model`,shap_feat$`Stochastic-Block-Model`,
#                  shap_baseline$`Stochastic-Block-Model`)
# sf.shap=shapviz(shap_vals$`Scale-Free`,shap_feat$`Scale-Free`,
#                 shap_baseline$`Scale-Free`)
# sp.shap=shapviz(shap_vals$Spatial,shap_feat$Spatial,
#                 shap_baseline$Spatial)
# sw.shap=shapviz(shap_vals$`Small-World`,shap_feat$`Small-World`,
#                 shap_baseline$`Small-World`)
# 
# 
# final_shap <- mshapviz(c('Erdös-Rényi' = er.shap, 'Stochastic-Block-Model'=sbm.shap,
#                          'Scale-Free'= sf.shap,
#                          'Spatial'=sp.shap,
#                          'Small-World'=sw.shap))
# 
# saveRDS(final_shap,"final_shap.rds")
# 
# final_shap=readRDS("final_shap.rds")
# #shap.plot.summary(shap_vals)
# # final_shap+ theme(
# #   text = element_text(size = 12)  # Set the desired font size (e.g., 12)
# # )
# #size=12
# #family="serif"
# 
# 
# 
# sv_importance(final_shap, kind = "both")#, show_numbers = TRUE)#var imp plot
# sv_importance(final_shap, kind = 'beeswarm')
# ###----Erdos Renyi var imp----###
# er_shap_imp=sv_importance(final_shap$`Erdös-Rényi`, kind = 'both',
#                           show_numbers = TRUE, bee_width = 0.4)
# saveRDS(er_shap_imp,"er_shap_imp.rds")
# 
# er_shap_imp.plot=er_shap_imp+
#   ggtitle("Erdös-Rényi")+
#   xlab("SHAP Value") +
#   #xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# er.shap.plot=er_shap_imp.plot& scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# ggsave("er_shap_varimp.png", width = 10, height = 10)
# 
# ###----SBM var imp----###
# sbm_shap_imp=sv_importance(final_shap$`Stochastic-Block-Model`, 
#                            kind = 'both',show_numbers = TRUE, bee_width = 0.4)
# saveRDS(sbm_shap_imp,"sbm_shap_imp.rds")
# 
# sbm_shap_imp.plot=sbm_shap_imp+
#   ggtitle("Stochastic-Block-Model")+
#   xlab("SHAP Value") +
#   #xlim(-6, 6)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# 
# sbm.shap.imp.plot=sbm_shap_imp.plot & scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# ggsave("sbm_shap_varimp.png", width = 10, height = 10)
# 
# ###----combined erdos renyi and sbm plots
# er_sbm =ggarrange(er.shap.plot, sbm.shap.imp.plot, nrow=1,ncol = 2, common.legend = TRUE, legend = "bottom")
# er_sbm
# ggsave("er_and_sbm_varimp.png", width = 10, height = 6)
# 
# # er_sbm =er_shap_imp+sbm_shap_imp+
# #   plot_layout(nrow = 1)+
# #   theme(legend.direction = "horizontal",  # Set legend direction to horizontal
# #         legend.position = "bottom")
# 
# 
# ###----SF var imp----###
# sf_shap_imp=sv_importance(final_shap$`Scale-Free`, kind = 'both',
#                           show_numbers = TRUE, bee_width = 0.4)
# saveRDS(sf_shap_imp,"sf_shap_imp.rds")
# 
# sf_shap_imp.plot=sf_shap_imp+
#   ggtitle("Scale-Free")+
#   xlab("SHAP Value") +
#   xlim(-14, 11)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# 
# sf.shap.imp.plot=sf_shap_imp.plot & scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# sf.shap.imp.plot
# ggsave("sf_shap_varimp.png", width = 10, height = 10)
# 
# 
# ###----Spatial var imp----###
# sp_shap_imp=sv_importance(final_shap$Spatial, kind = 'both',
#                           show_numbers = TRUE, bee_width = 0.4)
# saveRDS(sp_shap_imp,"sp_shap_imp.rds")
# 
# sp_shap_imp.plot=sp_shap_imp+
#   ggtitle("Spatial")+
#   xlab("SHAP Value") +
#   xlim(-14, 11)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# #sp_shap_imp
# sp.shap.imp.plot=sp_shap_imp.plot& scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# sp.shap.imp.plot
# ggsave("sp_shap_varimp.png", width = 10, height = 10)
# 
# ###----Small world var imp----###
# sw_shap_imp=sv_importance(final_shap$`Small-World`, kind = 'both',
#                           show_numbers = TRUE, bee_width = 0.4)
# 
# saveRDS(sw_shap_imp,"sw_shap_imp.rds")
# sw_shap_imp.plot=sw_shap_imp+ggtitle("Small-World")+
#   xlab("SHAP Value") +
#   xlim(-14, 11)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position="none")
# 
# #sw_shap_imp
# sw.shap.imp.plot=sw_shap_imp.plot& scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# sw.shap.imp.plot
# ggsave("sw_shap_varimp.png", width = 10, height = 10)
# 
# 
# ###----combined erdos renyi and sbm plots
# sf_sp_sw =ggarrange(sf.shap.imp.plot, sp.shap.imp.plot,sw.shap.imp.plot,nrow=2,ncol = 2, common.legend = TRUE, legend = "bottom")
# sf_sp_sw
# ggsave("sf_sp_sw_varimp.png", width = 10, height = 12)
# 
# 
# 
# #guides(color = guide_colorbar(direction = "horizontal"))
# 
# # er_sbm <- er_shap_imp + sbm_shap_imp & scale_color_continuous() &
# #   theme(legend.position='bottom', legend.direction='horizontal')
# 
# 
# 
# #guides(fill = guide_legend(reverse = TRUE)) & theme(legend.position='bottom', legend.direction='horizontal')
# #er_sbm + plot_layout(guides = "collect")
# 
# 
# # er_sbm=ggarrange(er_shap_imp,sbm_shap_imp,
# #           nrow=1,common.legend = TRUE, legend = "bottom")+
# # guides(fill = guide_legend(reverse = TRUE))
# #      
# # coord_flip() +
# # theme(legend.position='bottom', legend.direction='horizontal')
# 
# # all_imp=ggarrange(er_sbm,sf_sp,
# #           sw_shap_imp,
# #           nrow = 3,
# #           common.legend = TRUE, legend = "right")
# # 
# # all_imp
# 
# ###############################################################################
# # Shap Dependency plot
# ###############################################################################
# 
# ###----Shap variable dependency plot for Modularity----###
# mod.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Modularity",
#                           interactions = FALSE, color_var = NULL)
# 
# saveRDS(mod.shap.er,"mod.shap.er.rds")
# 
# mod.shap.er=readRDS("mod.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.er=mod.shap.er+plot_layout(widths = 1.2,heights = 1.0)
# 
# #mod.shap.er  
# 
# mod.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Modularity",
#                            interactions = FALSE, color_var = NULL)
# 
# saveRDS(mod.shap.sbm,"mod.shap.sbm.rds")
# 
# mod.shap.sbm=readRDS("mod.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.sbm=mod.shap.sbm+plot_layout(widths = 1.2,heights = 1.0)
# 
# mod.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Modularity",
#                           interactions = FALSE, color_var = NULL)
# 
# saveRDS(mod.shap.sf,"mod.shap.sf.rds")
# 
# mod.shap.sf=readRDS("mod.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mod.shap.sf=mod.shap.sf+plot_layout(widths = 1.2,heights = 1.0)
# 
# mod.shap.sp=sv_dependence(final_shap$Spatial, "Modularity",
#                           interactions = FALSE, color_var = NULL)
# 
# saveRDS(mod.shap.sp,"mod.shap.sp.rds")
# 
# mod.shap.sp=readRDS("mod.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mod.shap.sp=mod.shap.sp+plot_layout(widths = 1.2,heights = 1.0)
# 
# mod.shap.sw=sv_dependence(final_shap$`Small-World`, "Modularity",
#                           interactions = FALSE, color_var = NULL)
# 
# saveRDS(mod.shap.sw,"mod.shap.sw.rds")
# 
# mod.shap.sw=readRDS("mod.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Modularity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mod.shap.sw=mod.shap.sw+plot_layout(widths = 1.2,heights = 1.0)
# 
# modularity.figure <- ggarrange(mod.shap.er, mod.shap.sbm, mod.shap.sf,
#                                mod.shap.sp,mod.shap.sw,
#                                ncol = 3, nrow = 2,widths=1.5, heights=2)
# modularity.figure
# ggsave("mod.dependency.png", width=13, height=7)
# 
# #mod.shap$labels$title
# ###----Transitivity shapely depedency plot----###
# trans.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Transitivity",
#                             interactions = FALSE, color_var = NULL)
# 
# saveRDS(trans.shap.er,"trans.shap.er.rds")
# 
# trans.shap.er=readRDS("trans.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.er=trans.shap.er+plot_layout(widths = 1.2,heights = 1.0)
# 
# trans.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Transitivity",
#                              interactions = FALSE, color_var = NULL)
# 
# saveRDS(trans.shap.sbm,"trans.shap.sbm.rds")
# trans.shap.sbm=readRDS("trans.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# trans.shap.sbm=trans.shap.sbm+plot_layout(widths = 1.2,heights = 1.0)
# 
# trans.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Transitivity",
#                             interactions = FALSE, color_var = NULL)
# 
# saveRDS(trans.shap.sf,"trans.shap.sf.rds")
# trans.shap.sf=readRDS("trans.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# trans.shap.sf=trans.shap.sf+plot_layout(widths = 1.2,heights = 1.0)
# 
# trans.shap.sp=sv_dependence(final_shap$Spatial, "Transitivity",
#                             interactions = FALSE, color_var = NULL)
# 
# saveRDS(trans.shap.sp,"trans.shap.sp.rds")
# 
# trans.shap.sp=readRDS("trans.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# trans.shap.sp=trans.shap.sp+plot_layout(widths = 1.2,heights = 1.0)
# 
# trans.shap.sw=sv_dependence(final_shap$`Small-World`, "Transitivity",
#                             interactions = FALSE, color_var = NULL)
# 
# saveRDS(trans.shap.sw,"trans.shap.sw.rds")
# trans.shap.sw=readRDS("trans.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Transitivity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# trans.shap.sw=trans.shap.sw+plot_layout(widths = 1.2,heights = 1.2)
# 
# transitivity.figure <- ggarrange(trans.shap.er, trans.shap.sbm, trans.shap.sf,
#                                  trans.shap.sp,trans.shap.sw,
#                                  ncol = 3, nrow = 2,widths=4, heights=5)
# transitivity.figure
# 
# ggsave("transitivity.dependency.png", width=13, height=7)
# ###----Normalized fiedler value shapely depedency plot----###
# nf.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)
# 
# saveRDS(nf.shap.er,"nf.shap.er.rds")
# 
# nf.shap.er=readRDS("nf.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.er=nf.shap.er+plot_layout(widths = 1.2,heights = 1.0)
# 
# nf.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Normalized fiedler value",
#                           interactions = FALSE, color_var = NULL)
# 
# saveRDS(nf.shap.sbm,"nf.shap.sbm.rds")
# nf.shap.sbm=readRDS("nf.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sbm=nf.shap.sbm+plot_layout(widths = 1.2,heights = 1.0)
# 
# nf.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)
# 
# saveRDS(nf.shap.sf,"nf.shap.sf.rds")
# 
# nf.shap.sf=readRDS("nf.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sf=nf.shap.sf+plot_layout(widths = 1.2,heights = 1.0)
# 
# nf.shap.sp=sv_dependence(final_shap$Spatial, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)
# 
# saveRDS(nf.shap.sp,"nf.shap.sp.rds")
# 
# nf.shap.sp=readRDS("nf.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sp=nf.shap.sp+plot_layout(widths = 1.2,heights = 1.0)
# 
# nf.shap.sw=sv_dependence(final_shap$`Small-World`, "Normalized fiedler value",
#                          interactions = FALSE, color_var = NULL)
# 
# saveRDS(nf.shap.sw,"nf.shap.sw.rds")
# 
# nf.shap.sw=readRDS("nf.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Normalized fiedler value")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# nf.shap.sw=nf.shap.sw+plot_layout(widths = 1.2,heights = 1.0)
# 
# norm_fiedler.figure <- ggarrange(nf.shap.er, nf.shap.sbm, nf.shap.sf,
#                                  nf.shap.sp,nf.shap.sw,
#                                  ncol = 3, nrow = 2,widths=1.5, heights=2)
# norm_fiedler.figure
# ggsave("norm_fiedler.dependency.png", width=13, height=7)
# 
# 
# ###----Degree assortativity coeff shapely depedency plot----###
# deg_assort.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_assort.shap.er,"deg_assort.shap.er.rds")
# 
# deg_assort.shap.er=readRDS("deg_assort.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.er=deg_assort.shap.er+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_assort.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Degree assortativity coeff",
#                                   interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_assort.shap.sbm,"deg_assort.shap.sbm.rds")
# deg_assort.shap.sbm=readRDS("deg_assort.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sbm=deg_assort.shap.sbm+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_assort.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_assort.shap.sf,"deg_assort.shap.sf.rds")
# 
# deg_assort.shap.sf=readRDS("deg_assort.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sf=deg_assort.shap.sf+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_assort.shap.sp=sv_dependence(final_shap$Spatial, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_assort.shap.sp,"deg_assort.shap.sp.rds")
# 
# deg_assort.shap.sp=readRDS("deg_assort.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sp=deg_assort.shap.sp+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_assort.shap.sw=sv_dependence(final_shap$`Small-World`, "Degree assortativity coeff",
#                                  interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_assort.shap.sw,"deg_assort.shap.sw.rds")
# 
# deg_assort.shap.sw=readRDS("deg_assort.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Degree assortativity coeff")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.shap.sw=deg_assort.shap.sw+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_assort.figure <- ggarrange(deg_assort.shap.er, deg_assort.shap.sbm, deg_assort.shap.sf,
#                                deg_assort.shap.sp,deg_assort.shap.sw,
#                                ncol = 3, nrow = 2,widths=4, heights=5)
# deg_assort.figure
# ggsave("deg_assort.dependency.png", width=13, height=7)
# ###----Degree assortativity shapely dependency plot----###
# ###----Degree centrality shapely depedency plot----###
# deg_centr.shap.er=sv_dependence(final_shap$`Erdös-Rényi`, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_centr.shap.er,"deg_centr.shap.er.rds")
# 
# deg_centr.shap.er=readRDS("deg_centr.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.er=deg_centr.shap.er+plot_layout(widths = 1.2,heights = 1.0)
# #deg_centr.shap.er  
# 
# deg_centr.shap.sbm=sv_dependence(final_shap$`Stochastic-Block-Model`, "Degree centrality",
#                                  interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_centr.shap.sbm,"deg_centr.shap.sbm.rds")
# deg_centr.shap.sbm=readRDS("deg_centr.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sbm=deg_centr.shap.sbm+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_centr.shap.sf=sv_dependence(final_shap$`Scale-Free`, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_centr.shap.sf,"deg_centr.shap.sf.rds")
# 
# deg_centr.shap.sf=readRDS("deg_centr.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sf=deg_centr.shap.sf+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_centr.shap.sp=sv_dependence(final_shap$Spatial, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_centr.shap.sp,"deg_centr.shap.sp.rds")
# 
# deg_centr.shap.sp=readRDS("deg_centr.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sp=deg_centr.shap.sp+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_centr.shap.sw=sv_dependence(final_shap$`Small-World`, "Degree centrality",
#                                 interactions = FALSE, color_var = NULL)
# 
# saveRDS(deg_centr.shap.sw,"deg_centr.shap.sw.rds")
# 
# deg_centr.shap.sw=readRDS("deg_centr.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Degree centrality")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.shap.sw=deg_centr.shap.sw+plot_layout(widths = 1.2,heights = 1.0)
# 
# deg_centr.figure <- ggarrange(deg_centr.shap.er, deg_centr.shap.sbm, deg_centr.shap.sf,
#                               deg_centr.shap.sp,deg_centr.shap.sw,
#                               ncol = 3, nrow = 2,widths=4, heights=5)
# deg_centr.figure
# ggsave("deg_centr.dependency.png", width=13, height=7)
# 
# 
# plot1.dep=ggarrange(modularity.figure,
#                     transitivity.figure,
#                     deg_assort.figure,nrow = 3)
# 
# plot1.dep
# ggsave("plot1.dep.png")
# 
# plot2.dep=ggarrange(norm_fiedler.figure,
#                     deg_centr.figure,
#                     nrow = 2)
# #plot.dep1
# plot2.dep
# ggsave("plot2.dep.png")
# 
# ###################################################################################
# # 2D Shapely dependency plot
# ##################################################################################
# mod.shap.2Ddep=sv_dependence2D(final_shap$`Stochastic-Block-Model`, 
#                                x ="Modularity", 
#                                y = c("Mean path length",
#                                      "Graph energy",
#                                      "Transitivity",
#                                      "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# mod.shap.2Ddep#+plot_layout(widths = c(0.8, 0.8))
# ggsave("mod.shap.2Ddep.png", width = 8, height = 5)
# 
# 
# trans.shap.2Ddep=sv_dependence2D(final_shap$Spatial, 
#                                  x ="Transitivity", 
#                                  y = c("Mean eccentricity",
#                                        "Eigen centrality",
#                                        "Normalized fiedler value",
#                                        "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# trans.shap.2Ddep#+plot_layout(widths = c(0.8, 0.8))
# ggsave("trans.shap.2Ddep.png", width = 8, height = 5)
# 
# 
# 
# deg.centr.shap.2Ddep=sv_dependence2D(final_shap$`Scale-Free`, 
#                                      x ="Degree centrality", 
#                                      y = c("Modularity",
#                                            "Eigen centrality",
#                                            "Graph energy",
#                                            "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# deg.centr.shap.2Ddep
# ggsave("deg.centr.shap.2Ddep.png", width = 8, height = 5)
# 
# spec.radius.shap.2Ddep=sv_dependence2D(final_shap$`Small-World`, 
#                                        x ="Spectral radius", 
#                                        y = c("Mean degree",
#                                              "Degree centrality",
#                                              "Fiedler value",
#                                              "Degree assortativity coeff"), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# spec.radius.shap.2Ddep
# ggsave("spec.radius.shap.2Ddep.png", width = 8, height = 5)
# 
# norm.fiedler.shap.2Ddep=sv_dependence2D(final_shap$`Erdös-Rényi`, 
#                                         x ="Normalized fiedler value", 
#                                         y = c("Modularity",
#                                               "Transitivity",
#                                               "Degree centrality",
#                                               "Closeness centrality"), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# norm.fiedler.shap.2Ddep
# ggsave("norm.fiedler.shap.2Ddep.png", width = 8, height = 5)
# 
# ########################################################################################
# # Shap  interaction plots
# ########################################################################################
# final_resample_data <- df.final#[sample(nrow(df.final), 1000), ]
# final_resample_data_prep <- bake(
#   df.final.prep, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = final_resample_data, 
#   composition = "matrix"
# )
# #head(final_resample_data_prep)
# 
# shap_interactn <- shapviz(extract_fit_engine(final.fitted.model),
#                           X_pred = final_resample_data_prep, 
#                           X = final_resample_data,
#                           interactions = TRUE)
# 
# saveRDS(shap_interactn,"shap_interactn.rds")
# shap_interactn=readRDS("shap_interactn.rds")
# 
# 
# ###----getting shap values----
# shap_interactn_vals=get_shap_values(shap_interactn)
# shap_interactn_vals
# 
# names(shap_interactn_vals)[names(shap_interactn_vals) == "Class_1"] <- "Erdös-Rényi"
# names(shap_interactn_vals)[names(shap_interactn_vals) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_interactn_vals)[names(shap_interactn_vals) == "Class_3"] <- "Scale-Free"
# names(shap_interactn_vals)[names(shap_interactn_vals) == "Class_4"] <- "Spatial"
# names(shap_interactn_vals)[names(shap_interactn_vals) == "Class_5"] <- "Small-World"
# colnames(shap_interactn_vals$`Erdös-Rényi`)=new_column_names
# colnames(shap_interactn_vals$`Stochastic-Block-Model`)=new_column_names
# colnames(shap_interactn_vals$`Scale-Free`)=new_column_names
# colnames(shap_interactn_vals$Spatial)=new_column_names
# colnames(shap_interactn_vals$`Small-World`)=new_column_names
# shap_interactn_vals
# 
# 
# 
# shap_interactn_feat=get_feature_values(shap_interactn)
# names(shap_interactn_feat)[names(shap_interactn_feat) == "Class_1"] <- "Erdös-Rényi"
# names(shap_interactn_feat)[names(shap_interactn_feat) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_interactn_feat)[names(shap_interactn_feat) == "Class_3"] <- "Scale-Free"
# names(shap_interactn_feat)[names(shap_interactn_feat) == "Class_4"] <- "Spatial"
# names(shap_interactn_feat)[names(shap_interactn_feat) == "Class_5"] <- "Small-World"
# colnames(shap_interactn_feat$`Erdös-Rényi`)=new_column_names
# colnames(shap_interactn_feat$`Stochastic-Block-Model`)=new_column_names
# colnames(shap_interactn_feat$`Scale-Free`)=new_column_names
# colnames(shap_interactn_feat$Spatial)=new_column_names
# colnames(shap_interactn_feat$`Small-World`)=new_column_names
# shap_interactn_feat
# 
# 
# 
# shap_interactn_baseline=get_baseline(shap_interactn)
# names(shap_interactn_baseline)[names(shap_interactn_baseline) == "Class_1"] <- "Erdös-Rényi"
# names(shap_interactn_baseline)[names(shap_interactn_baseline) == "Class_2"] <- "Stochastic-Block-Model"
# names(shap_interactn_baseline)[names(shap_interactn_baseline) == "Class_3"] <- "Scale-Free"
# names(shap_interactn_baseline)[names(shap_interactn_baseline) == "Class_4"] <- "Spatial"
# names(shap_interactn_baseline)[names(shap_interactn_baseline) == "Class_5"] <- "Small-World"
# 
# 
# 
# er.interactn.shap=shapviz(shap_interactn_vals$`Erdös-Rényi`,shap_interactn_feat$`Erdös-Rényi`,shap_interactn_baseline$`Erdös-Rényi`)
# sbm.interactn.shap=shapviz(shap_interactn_vals$`Stochastic-Block-Model`,shap_interactn_feat$`Stochastic-Block-Model`,
#                            shap_interactn_baseline$`Stochastic-Block-Model`)
# sf.interactn.shap=shapviz(shap_interactn_vals$`Scale-Free`,shap_interactn_feat$`Scale-Free`,
#                           shap_interactn_baseline$`Scale-Free`)
# sp.interactn.shap=shapviz(shap_interactn_vals$Spatial,shap_interactn_feat$Spatial,
#                           shap_interactn_baseline$Spatial)
# sw.interactn.shap=shapviz(shap_interactn_vals$`Small-World`,shap_interactn_feat$`Small-World`,
#                           shap_interactn_baseline$`Small-World`)
# 
# 
# final_interactn_shap <- mshapviz(c('Erdös-Rényi' = er.interactn.shap,
#                                    'Stochastic-Block-Model'=sbm.interactn.shap,
#                                    'Scale-Free'= sf.interactn.shap,
#                                    'Spatial'=sp.interactn.shap,
#                                    'Small-World'=sw.interactn.shap))
# 
# saveRDS(final_interactn_shap,"final_interactn_shap.rds")
# 
# final_interactn_shap=readRDS("final_interactn_shap.rds")
# 
# 
# 
# ###----Shap variable dependency interaction plot for Modularity----###
# mod.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
#                                     "Modularity",
#                                     color_var = "auto", interactions = TRUE)
# 
# saveRDS(mod.shap.interactn,"mod.shap.interactn.rds")
# 
# mod.shap.interactn.er=readRDS("mod.shap.interactn.er.rds")+ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #mod.shap.interactn.er  
# 
# mod.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
#                                      "Modularity",
#                                      interactions = TRUE, color_var = "auto")
# 
# saveRDS(mod.shap.interactn.sbm,"mod.shap.interactn.sbm.rds")
# 
# mod.shap.interactn.sbm=readRDS("mod.shap.interactn.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mod.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Modularity",
#                                     interactions = TRUE, color_var = "auto")
# 
# saveRDS(mod.shap.interactn.sf,"mod.shap.interactn.sf.rds")
# 
# mod.shap.interactn.sf=readRDS("mod.shap.interactn.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# 
# mod.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Modularity",
#                                     interactions = TRUE, color_var = "auto")
# 
# saveRDS(mod.shap.interactn.sp,"mod.shap.interactn.sp.rds")
# 
# mod.shap.interactn.sp=readRDS("mod.shap.interactn.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# 
# mod.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Modularity",
#                                     interactions = TRUE, color_var = "auto")
# 
# saveRDS(mod.shap.interactn.sw,"mod.shap.interactn.sw.rds")
# 
# mod.shap.interactn.sw=readRDS("mod.shap.interactn.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Modularity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# modularity.interactn.figure <- ggarrange(mod.shap.interactn.er, mod.shap.interactn.sbm, mod.shap.interactn.sf,
#                                          mod.shap.interactn.sp,mod.shap.interactn.sw,
#                                          ncol = 3, nrow = 2,widths=1.5, heights=2)
# modularity.interactn.figure
# 
# #mod.shap.interactn$labels$title
# ###----Transitivity shapely depedency plot----###
# trans.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
#                                       "Transitivity",
#                                       interactions = TRUE, color_var = "auto")
# 
# saveRDS(trans.shap.interactn.er,"trans.shap.interactn.er.rds")
# 
# trans.shap.interactn.er=readRDS("trans.shap.interactn.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #trans.shap.interactn.er  
# 
# trans.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Transitivity",
#                                        interactions = TRUE, color_var = "auto")
# 
# saveRDS(trans.shap.interactn.sbm,"trans.shap.interactn.sbm.rds")
# trans.shap.interactn.sbm=readRDS("trans.shap.interactn.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# trans.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Transitivity",
#                                       interactions = TRUE, color_var = "auto")
# 
# saveRDS(trans.shap.interactn.sf,"trans.shap.interactn.sf.rds")
# trans.shap.interactn.sf=readRDS("trans.shap.interactn.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# 
# trans.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Transitivity",
#                                       interactions = TRUE, color_var = "auto")
# 
# saveRDS(trans.shap.interactn.sp,"trans.shap.interactn.sp.rds")
# 
# trans.shap.interactn.sp=readRDS("trans.shap.interactn.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# 
# trans.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Transitivity",
#                                       interactions = TRUE, color_var = "auto")
# 
# saveRDS(trans.shap.interactn.sw,"trans.shap.interactn.sw.rds")
# trans.shap.interactn.sw=readRDS("trans.shap.interactn.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Transitivity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# transitivity.figure.interactn <- ggarrange(trans.shap.interactn.er, trans.shap.interactn.sbm, trans.shap.interactn.sf,
#                                            trans.shap.interactn.sp,trans.shap.interactn.sw,
#                                            ncol = 3, nrow = 2,widths=4, heights=5)
# transitivity.figure.interactn
# 
# ###----Normalized fiedler value shapely depedency plot----###
# nf.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Normalized fiedler value",
#                                    interactions = TRUE, color_var = "auto")
# 
# saveRDS(nf.shap.interactn.er,"nf.shap.interactn.er.rds")
# 
# nf.shap.interactn.er=readRDS("nf.shap.interactn.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #nf.shap.interactn.er  
# 
# nf.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Normalized fiedler value",
#                                     interactions = TRUE, color_var = "auto")
# 
# saveRDS(nf.shap.interactn.sbm,"nf.shap.interactn.sbm.rds")
# nf.shap.interactn.sbm=readRDS("nf.shap.interactn.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# nf.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Normalized fiedler value",
#                                    interactions = TRUE, color_var = "auto")
# 
# saveRDS(nf.shap.interactn.sf,"nf.shap.interactn.sf.rds")
# 
# nf.shap.interactn.sf=readRDS("nf.shap.interactn.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# nf.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Normalized fiedler value",
#                                    interactions = TRUE, color_var = "auto")
# 
# saveRDS(nf.shap.interactn.sp,"nf.shap.interactn.sp.rds")
# 
# nf.shap.interactn.sp=readRDS("nf.shap.interactn.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# nf.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Normalized fiedler value",
#                                    interactions = TRUE, color_var = "auto")
# 
# saveRDS(nf.shap.interactn.sw,"nf.shap.interactn.sw.rds")
# 
# nf.shap.interactn.sw=readRDS("nf.shap.interactn.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Normalized fiedler value")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# norm_fiedler.interactn.figure <- ggarrange(nf.shap.interactn.er, nf.shap.interactn.sbm, nf.shap.interactn.sf,
#                                            nf.shap.interactn.sp,nf.shap.interactn.sw,
#                                            ncol = 3, nrow = 2,widths=1.5, heights=2)
# norm_fiedler.interactn.figure
# 
# ###----Degree assortativity coeff shapely depedency plot----###
# deg_assort.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`,
#                                            "Degree assortativity coeff",
#                                            interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_assort.shap.interactn.er,"deg_assort.shap.interactn.er.rds")
# 
# deg_assort.shap.interactn.er=readRDS("deg_assort.shap.interactn.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #deg_assort.shap.interactn.er  
# 
# deg_assort.shap.interactn.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
#                                             "Degree assortativity coeff",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_assort.shap.interactn.sbm,"deg_assort.shap.interactn.sbm.rds")
# deg_assort.shap.interactn.sbm=readRDS("deg_assort.shap.interactn.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# deg_assort.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Degree assortativity coeff",
#                                            interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_assort.shap.interactn.sf,"deg_assort.shap.interactn.sf.rds")
# 
# deg_assort.shap.interactn.sf=readRDS("deg_assort.shap.interactn.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# deg_assort.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, "Degree assortativity coeff",
#                                            interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_assort.shap.interactn.sp,"deg_assort.shap.interactn.sp.rds")
# 
# deg_assort.shap.interactn.sp=readRDS("deg_assort.shap.interactn.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# deg_assort.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, "Degree assortativity coeff",
#                                            interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_assort.shap.interactn.sw,"deg_assort.shap.interactn.sw.rds")
# 
# deg_assort.shap.interactn.sw=readRDS("deg_assort.shap.interactn.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Degree assortativity coeff")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_assort.interactn.figure <- ggarrange(deg_assort.shap.interactn.er, deg_assort.shap.interactn.sbm, deg_assort.shap.interactn.sf,
#                                          deg_assort.shap.interactn.sp,deg_assort.shap.interactn.sw,
#                                          ncol = 3, nrow = 2,widths=4, heights=5)
# deg_assort.interactn
# 
# 
# ###----Degree centrality shapely depedency plot----###
# deg_centr.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Degree centrality",
#                                           interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_centr.interactn.shap.er,"deg_centr.interactn.shap.er.rds")
# 
# deg_centr.interactn.shap.er=readRDS("deg_centr.interactn.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #deg_centr.interactn.shap.er  
# 
# deg_centr.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
#                                            "Degree centrality",
#                                            interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_centr.interactn.shap.sbm,"deg_centr.interactn.shap.sbm.rds")
# deg_centr.interactn.shap.sbm=readRDS("deg_centr.interactn.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# deg_centr.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Degree centrality",
#                                           interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_centr.interactn.shap.sf,"deg_centr.interactn.shap.sf.rds")
# 
# deg_centr.interactn.shap.sf=readRDS("deg_centr.interactn.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# deg_centr.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Degree centrality",
#                                           interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_centr.interactn.shap.sp,"deg_centr.interactn.shap.sp.rds")
# 
# deg_centr.interactn.shap.sp=readRDS("deg_centr.interactn.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# deg_centr.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Degree centrality",
#                                           interactions = TRUE, color_var = "auto")
# 
# saveRDS(deg_centr.interactn.shap.sw,"deg_centr.interactn.shap.sw.rds")
# 
# deg_centr.interactn.shap.sw=readRDS("deg_centr.interactn.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Degree centrality")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# deg_centr.interactn.figure <- ggarrange(deg_centr.interactn.shap.er, deg_centr.interactn.shap.sbm, deg_centr.interactn.shap.sf,
#                                         deg_centr.interactn.shap.sp,deg_centr.interactn.shap.sw,
#                                         ncol = 3, nrow = 2,widths=4, heights=5)
# deg_centr.interactn.figure
# 
# 
# ###----Eigen centrality shapely depedency plot----###
# eigen_centr.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Eigen centrality",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(eigen_centr.interactn.shap.er,"eigen_centr.interactn.shap.er.rds")
# 
# eigen_centr.interactn.shap.er=readRDS("eigen_centr.interactn.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #eigen_centr.interactn.shap.er  
# 
# eigen_centr.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
#                                              "Eigen centrality",
#                                              interactions = TRUE, color_var = "auto")
# 
# saveRDS(eigen_centr.interactn.shap.sbm,"eigen_centr.interactn.shap.sbm.rds")
# eigen_centr.interactn.shap.sbm=readRDS("eigen_centr.interactn.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# eigen_centr.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Eigen centrality",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(eigen_centr.interactn.shap.sf,"eigen_centr.interactn.shap.sf.rds")
# 
# eigen_centr.interactn.shap.sf=readRDS("eigen_centr.interactn.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# eigen_centr.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Eigen centrality",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(eigen_centr.interactn.shap.sp,"eigen_centr.interactn.shap.sp.rds")
# 
# eigen_centr.interactn.shap.sp=readRDS("eigen_centr.interactn.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# eigen_centr.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Eigen centrality",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(eigen_centr.interactn.shap.sw,"eigen_centr.interactn.shap.sw.rds")
# 
# eigen_centr.interactn.shap.sw=readRDS("eigen_centr.interactn.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Eigen centrality")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# eigen_centr.interactn.figure <- ggarrange(eigen_centr.interactn.shap.er, eigen_centr.interactn.shap.sbm, eigen_centr.interactn.shap.sf,
#                                           eigen_centr.interactn.shap.sp,eigen_centr.interactn.shap.sw,
#                                           ncol = 3, nrow = 2,widths=4, heights=5)
# eigen_centr.interactn.figure
# 
# ###----Spectral radius shapely depedency plot----###
# spec_radius.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Spectral radius",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(spec_radius.interactn.shap.er,"spec_radius.interactn.shap.er.rds")
# 
# spec_radius.interactn.shap.er=readRDS("spec_radius.interactn.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #spec_radius.interactn.shap.er  
# 
# spec_radius.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Spectral radius",
#                                              interactions = TRUE, color_var = "auto")
# 
# saveRDS(spec_radius.interactn.shap.sbm,"spec_radius.interactn.shap.sbm.rds")
# spec_radius.interactn.shap.sbm=readRDS("spec_radius.interactn.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# spec_radius.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Spectral radius",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(spec_radius.interactn.shap.sf,"spec_radius.interactn.shap.sf.rds")
# 
# spec_radius.interactn.shap.sf=readRDS("spec_radius.interactn.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# spec_radius.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Spectral radius",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(spec_radius.interactn.shap.sp,"spec_radius.interactn.shap.sp.rds")
# 
# spec_radius.interactn.shap.sp=readRDS("spec_radius.interactn.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# spec_radius.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Spectral radius",
#                                             interactions = TRUE, color_var = "auto")
# 
# saveRDS(spec_radius.interactn.shap.sw,"spec_radius.interactn.shap.sw.rds")
# 
# spec_radius.interactn.shap.sw=readRDS("spec_radius.interactn.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Spectral radius")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# spec_radius.interactn.figure <- ggarrange(spec_radius.interactn.shap.er, spec_radius.interactn.shap.sbm, spec_radius.interactn.shap.sf,
#                                           spec_radius.interactn.shap.sp,spec_radius.interactn.shap.sw,
#                                           ncol = 3, nrow = 2,widths=4, heights=5)
# spec_radius.interactn.figure
# 
# ###----Mean path length shapely depedency plot----###
# mean_path_length.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, "Mean path length",
#                                                  interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_path_length.interactn.shap.er,"mean_path_length.interactn.shap.er.rds")
# 
# mean_path_length.interactn.shap.er=readRDS("mean_path_length.interactn.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #mean_path_length.interactn.shap.er  
# 
# mean_path_length.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Mean path length",
#                                                   interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_path_length.interactn.shap.sbm,"mean_path_length.interactn.shap.sbm.rds")
# mean_path_length.interactn.shap.sbm=readRDS("mean_path_length.interactn.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_path_length.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Mean path length",
#                                                  interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_path_length.interactn.shap.sf,"mean_path_length.interactn.shap.sf.rds")
# 
# mean_path_length.interactn.shap.sf=readRDS("mean_path_length.interactn.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_path_length.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Mean path length",
#                                                  interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_path_length.interactn.shap.sp,"mean_path_length.interactn.shap.sp.rds")
# 
# mean_path_length.interactn.shap.sp=readRDS("mean_path_length.interactn.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_path_length.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Mean path length",
#                                                  interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_path_length.interactn.shap.sw,"mean_path_length.interactn.shap.sw.rds")
# 
# mean_path_length.interactn.shap.sw=readRDS("mean_path_length.interactn.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Mean path length")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mean_path_length.interactn.figure <- ggarrange(mean_path_length.interactn.shap.er, mean_path_length.interactn.shap.sbm, mean_path_length.interactn.shap.sf,
#                                                mean_path_length.interactn.shap.sp,mean_path_length.interactn.shap.sw,
#                                                ncol = 3, nrow = 2,widths=4, heights=5)
# mean_path_length.interactn.figure
# 
# 
# ###----Mean eccentricity shapely depedency plot----###
# mean_eccentr.interactn.shap.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`,
#                                              "Mean eccentricity",
#                                              interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_eccentr.interactn.shap.er,"mean_eccentr.interactn.shap.er.rds")
# 
# mean_eccentr.interactn.shap.er=readRDS("mean_eccentr.interactn.shap.er.rds")+
#   ggtitle("Erdös-Rényi")+
#   xlab(" ")+
#   ylab("SHAP Value")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# #mean_eccentr.interactn.shap.er  
# 
# mean_eccentr.interactn.shap.sbm=sv_dependence(final_interactn_shap$`Stochastic-Block-Model`, "Mean eccentricity",
#                                               interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_eccentr.interactn.shap.sbm,"mean_eccentr.interactn.shap.sbm.rds")
# mean_eccentr.interactn.shap.sbm=readRDS("mean_eccentr.interactn.shap.sbm.rds")+
#   ggtitle("Stochastic-Block-Model")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_eccentr.interactn.shap.sf=sv_dependence(final_interactn_shap$`Scale-Free`, "Mean eccentricity",
#                                              interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_eccentr.interactn.shap.sf,"mean_eccentr.interactn.shap.sf.rds")
# 
# mean_eccentr.interactn.shap.sf=readRDS("mean_eccentr.interactn.shap.sf.rds")+
#   ggtitle("Scale-Free")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_eccentr.interactn.shap.sp=sv_dependence(final_interactn_shap$Spatial, "Mean eccentricity",
#                                              interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_eccentr.interactn.shap.sp,"mean_eccentr.interactn.shap.sp.rds")
# 
# mean_eccentr.interactn.shap.sp=readRDS("mean_eccentr.interactn.shap.sp.rds")+
#   ggtitle("Spatial")+
#   xlab(" ")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# mean_eccentr.interactn.shap.sw=sv_dependence(final_interactn_shap$`Small-World`, "Mean eccentricity",
#                                              interactions = TRUE, color_var = "auto")
# 
# saveRDS(mean_eccentr.interactn.shap.sw,"mean_eccentr.interactn.shap.sw.rds")
# 
# mean_eccentr.interactn.shap.sw=readRDS("mean_eccentr.interactn.shap.sw.rds")+
#   ggtitle("Small-World")+
#   xlab("Mean eccentricity")+
#   ylab(" ")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# mean_eccentr.interactn.figure <- ggarrange(mean_eccentr.interactn.shap.er, mean_eccentr.interactn.shap.sbm, mean_eccentr.interactn.shap.sf,
#                                            mean_eccentr.interactn.shap.sp,mean_eccentr.interactn.shap.sw,
#                                            ncol = 3, nrow = 2,widths=4, heights=5)
# mean_eccentr.interactn.figure
# 
# 
# #######################################################
# # shap and ale plots combined
# #######################################################
# mod.combine=ggarrange(modularity.figure,
#                       ggarrange(modularity.plot, 
#                                 align = "v",widths = c(0.5,1)),
#                       #   align = "v",
#                       nrow = 2,
#                       labels = c("A)","B)"),
#                       # label.x = 0,
#                       # label.y = 1,
#                       # heights = c(1.5, 1, 1),
#                       # widths = c(.5,.5,.5),
#                       hjust = -1.1,#-3.1,
#                       vjust = 0.9,
#                       font.label = list(size = 12, color = "black", face = "bold",
#                                         family = "serif"))
# mod.combine
# 
# ggsave("mod.combine.png", width = 4, height =4 )
# 
# trans.combine=ggarrange(transitivity.figure,
#                         ggarrange(transitivity.plot, 
#                                   align = "v",widths = c(0.5,1)),
#                         #   align = "v",
#                         nrow = 2,
#                         labels = c("A)","B)"),
#                         # label.x = 0,
#                         # label.y = 1,
#                         # heights = c(1.5, 1, 1),
#                         # widths = c(.5,.5,.5),
#                         hjust = -1.1,#-3.1,
#                         vjust = 0.9,
#                         font.label = list(size = 12, color = "black", face = "bold",
#                                           family = "serif"))
# trans.combine
# 
# ggsave("trans.combine.png", width = 8, height = 8)
# 
# normfiedler.combine=ggarrange(norm_fiedler.figure,
#                               ggarrange(normalized_fiedler_value.plot, 
#                                         align = "v",widths = c(0.5,1)),
#                               #   align = "v",
#                               nrow = 2,
#                               labels = c("A)","B)"),
#                               # label.x = 0,
#                               # label.y = 1,
#                               # heights = c(1.5, 1, 1),
#                               # widths = c(.5,.5,.5),
#                               hjust = -1.1,#-3.1,
#                               vjust = 0.9,
#                               font.label = list(size = 12, color = "black", face = "bold",
#                                                 family = "serif"))
# 
# normfiedler.combine
# ggsave("normfiedler.combine.png", width = 8, height = 8)
# 
# deg.centr.combine=ggarrange(deg_centr.figure,
#                             ggarrange(deg_centr.plot, 
#                                       align = "v",widths = c(0.5,1)),
#                             #   align = "v",
#                             nrow = 2,
#                             labels = c("A)","B)"),
#                             # label.x = 0,
#                             # label.y = 1,
#                             # heights = c(1.5, 1, 1),
#                             # widths = c(.5,.5,.5),
#                             hjust = -1.1,#-3.1,
#                             vjust = 0.9,
#                             font.label = list(size = 12, color = "black", face = "bold",
#                                               family = "serif"))
# 
# deg.centr.combine
# ggsave("deg.centr.combine.png", width = 8, height = 8)
# 
# 
# deg_assort.combine=ggarrange(deg_assort.figure,
#                              ggarrange(deg_assort_coef.plot, 
#                                        align = "v",widths = c(0.5,1)),
#                              #   align = "v",
#                              nrow = 2,
#                              labels = c("A)","B)"),
#                              # label.x = 0,
#                              # label.y = 1,
#                              # heights = c(1.5, 1, 1),
#                              # widths = c(.5,.5,.5),
#                              hjust = -1.1,#-3.1,
#                              vjust = 0.9,
#                              font.label = list(size = 12, color = "black", face = "bold",
#                                                family = "serif"))
# 
# deg_assort.combine
# ggsave("deg_assort.combine.png", width = 8, height = 8)
# 
# 
# 
# 
# 
# 
# # sv_dependence(final_shap, "Spectral radius",interactions = FALSE, color_var = NULL)
# # sv_dependence(final_shap, "Mean degree",interactions = FALSE, color_var = NULL)
# # sv_dependence(final_shap, "Eigen centrality",interactions = FALSE, color_var = NULL)
# # sv_dependence(final_shap, "Betweenness centrality",interactions = FALSE, color_var = NULL)
# 
# 
# x=sv_force(final_shap, row_id = 1)
# ggsave("svforce.png", width = 30, height=30)
# sv_waterfall(final_shap, row_id = 1)
# ggsave("svwaterfall.png", width = 30, height=30)
# 
# 
# #Preparing data for shap
# # dm_shap <-
# #   shap.prep(
# #     xgb_model = extract_fit_engine(final.fitted.model),
# #     X_train = bake(df.final.prep,
# #                    has_role("predictor"),
# #                    new_data = NULL,
# #                    composition = "matrix"
# #     ))
# # 
# # #Shap summary
# # shap.plot.summary(dm_shap)
# 
# 
# #######################################################################################
# # Explain single prediction with local model: LIME MODEL
# #######################################################################################
# #For analyzing how models make individual predictions
# best.model.lime <- LocalModel$new(best.model.pred.class, x.interest = x.data[5,])
# best.model.lime %>% plot()
# 
# 
# #For analyzing how models make individual predictions 
# # rf_lime <- LocalModel$new(rf.predictor, x.interest = x.data[1,])
# # rf_lime %>% plot()
# # 
# # 
# # xgb_lime <- LocalModel$new(xgb.predictor, x.interest = x.data[1,])
# # xgb_lime %>% plot()

















####################################################################################
#----Metadata----
###################################################################################
######################
# df_final_meta_data<-df_final_meta_data_rf
# saveRDS(df_final_meta_data,"df_final_meta_data.rds")
# 
# 
# ######################################################################################
# # Creating the model -one hot encoding
# ########################################################################################
# df_final_meta_data=readRDS("df_final_meta_data.rds")
# #----creating training, test and validation set
# set.seed(123)
# meta.df.split <- df_final_meta_data[sample(1:nrow(df_final_meta_data)),]%>%
#   initial_split(strata =  target,prop=0.6)
# 
# saveRDS(meta.df.split,"meta.df.split.rds")
# 
# metadata_train<- training(meta.df.split)
# saveRDS(metadata_train,"metadata_train.rds")
# 
# metadata_test<- testing(meta.df.split)
# saveRDS(metadata_test,"metadata_test.rds")
# 
# 
# #check na's for the train and test data
# colSums(is.na(metadata_train))
# metadata_train
# metadata_test
# 
# ##----Creting the recipe object
# metadata.rec=recipe(target ~ ., data = metadata_train)%>%
#   step_dummy(all_nominal_predictors(),one_hot = T)
# #step_dummy(Target, one_hot = T)
# 
# #--compute missing values for numeric predictors
# saveRDS(metadata.rec,"metadata.rec.rds")
# 
# metadata.prep=metadata.rec%>%
#   prep()
# 
# saveRDS(metadata.prep,"metadata.prep.rf")
# 
# metadata.juice=juice(metadata.prep)
# 
# saveRDS(metadata.juice,"metadata.juice")
# 
# colSums(is.na(metadata.juice))
# 
# x.meta=metadata.juice%>%
#   select(-target)
# 
# y.meta=metadata.juice%>%
#   select(target)
# 
# x.meta.feat=x.meta%>%
#   mutate_all(as.numeric)
# 
# last_metadata_df=cbind(y.meta,x.meta.feat)
# 
# saveRDS(last_metadata_df,"last_metadata_df.rds")
# 
# str(last_metadata_df)
# 
# #----creating final training, test and validation set
# #last_metadata_df=FinalTrainedMetadata
# set.seed(123)
# last_meta.df.split <- last_metadata_df[sample(1:nrow(last_metadata_df)),]%>%
#   initial_split(strata =  target,prop=0.6)
# 
# saveRDS(last_meta.df.split,"last_meta.df.split.rds")
# 
# last_metadata_train<- training(last_meta.df.split)
# saveRDS(last_metadata_train,"last_metadata_train.rds")
# 
# last_metadata_test<- testing(last_meta.df.split)
# saveRDS(last_metadata_test,"last_metadata_test.rds")
# last_metadata_test=readRDS("last_metadata_test.rds")
# colnames(last_metadata_train)
# 
# last_metadata_rec=recipe(target ~ ., data = last_metadata_train)
# saveRDS(last_metadata_rec,"last_metadata_rec.rds")
# 
# last_metadata_prep=last_metadata_rec%>%prep()
# saveRDS(last_metadata_prep,"last_metadata_prep.rds")
# 
# last_metadata_juice=last_metadata_prep%>%juice()
# saveRDS(last_metadata_juice,"last_metadata_juice.rds")
# 
# 
# #bootstraps(df.final.train, times = 100)
# last_metadata_folds <- last_metadata_train %>% bootstraps(times = 100)
# last_metadata_folds
# saveRDS(last_metadata_folds,"last_metadata_folds.rds")
# 
# 
# ###----model-----
# xgb.model.spec <-
#   boost_tree(mtry = tune(), tree = tune(),
#              learn_rate = tune(), tree_depth = tune()) %>%
#   set_engine("xgboost",importance = "permutation") %>%
#   set_mode("classification")
# 
# 
# #Creating workflow object
# xgb.wkflw <-
#   workflow() %>%
#   add_model(xgb.model.spec) %>%
#   add_recipe(last_metadata_rec)
# 
# library(finetune)
# doParallel::registerDoParallel()
# 
# set.seed(345)
# xgb.res <-
#   xgb.wkflw %>% 
#   tune_race_anova(last_metadata_folds,
#                   grid = 25,
#                   control = control_race(save_pred = TRUE)
#                   #control = control_grid(save_pred = TRUE)#,
#                   #metrics = metric_set(roc_auc)
#   )
# 
# saveRDS(xgb.res,"xgb.res.rds")
# 
# 
# ####----Model Evaluation----####
# xgb.metadata.res=xgb.res
# 
# xgb.metadata.res%>%
#   collect_metrics()
# 
# library(here)
# #save trained RF classifier
# saveRDS(xgb.metadata.res,"xgb_metadata_res.rds")
# #Show best metric
# xgb.metadata.res %>%
#   show_best(metric = "roc_auc")
# #Shows best estimated tuned parameters
# autoplot(xgb.metadata.res)
# 
# # xgb.metadata.res %>%
# #   collect_metrics() %>%
# #   filter(.metric == "roc_auc") %>%
# #   select(mean, tree_depth, mtry) %>%
# #   pivot_longer(tree_depth:mtry,
# #                values_to = "value",
# #                names_to = "parameter"
# #   ) %>%
# #   ggplot(aes(value, mean, color = parameter)) +
# #   geom_point(show.legend = FALSE) +
# #   facet_wrap(~parameter, scales = "free_x") +
# #   labs(x = NULL, y = "AUC")
# # 
# # xgb.metadata.res %>%
# #   collect_metrics() %>%
# #   dplyr::filter(.metric == "roc_auc") %>%
# #   dplyr::select(mean, tree_depth, mtry) %>%
# #   pivot_longer(tree_depth:mtry,
# #                values_to = "value",
# #                names_to = "parameter")%>%
# #   ggplot(aes(value, mean, color = parameter)) +
# #   geom_point(show.legend = FALSE) +
# #   facet_wrap(~parameter, scales = "free_x") +
# #   labs(x = NULL, y = "AUC")
# # #Viewing best results
# # xgb.metadata.res %>%
# #   collect_metrics() %>%
# #   dplyr::filter(.metric == "roc_auc") %>%
# #   mutate(tree_depth = factor(tree_depth)) %>%
# #   ggplot(aes(mtry, mean, color = tree_depth)) +
# #   geom_line(alpha = 0.5, linewidth = 1.5) +
# #   geom_point() +
# #   labs(y = "AUC")
# # 
# # ####----Select best tuned parameters for final model-----####
# xgb.best_metadata_auc <- select_best(xgb.metadata.res, "roc_auc")
# xgb.best_metadata_auc
# 
# xgb.final.model.metadata <- finalize_model(
#   xgb.model.spec,
#   xgb.best_metadata_auc)
# # 
# 
# #----The last workflow
# xgb.final.metadata.wkflw <- workflow() %>%
#   add_recipe(last_metadata_rec) %>%
#   add_model(xgb.final.model.metadata)
# 
# #----The last fitted model
# xgb.final.metadata.fit <-
#   xgb.final.metadata.wkflw %>%
#   last_fit(last_meta.df.split)
# 
# 
# xgb.final.metadata.fit%>%
#   collect_metrics()
# 
# saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# 
# ##############################################################
# # Variable Importance
# #############################################################
# set.seed(539)
# meta.folds=last_metadata_folds#bootstraps(last_metadata_train, times = 100)
# 
# meta.get_xgb_imp <- function(x) {
#   x %>% 
#     extract_fit_parsnip() %>% 
#     vip::vi()
# }
# 
# meta.ctrl_imp<- control_grid(extract = meta.get_xgb_imp)
# 
# xgb.best.model.specs <-
#   xgb.metadata.res%>%
#   select_best(metric = "accuracy")
# 
# 
# meta.varimp.model=xgb.final.model.metadata%>%
#   finalize_model(select_best(xgb.metadata.res))%>%
#   set_engine("xgboost",importance="permutation")
# 
# meta.varimp.res=workflow() %>%
#   add_recipe(last_metadata_rec) %>% 
#   add_model(meta.varimp.model)%>% 
#   fit_resamples(meta.folds, control = meta.ctrl_imp)
# 
# meta.varimp.res
# #save trained models
# #saveRDS(varimp.res,"varimp.res.rds")
# 
# #varimp.res=readRDS("varimp.res.rds")
# 
# 
# rename_var_imp.meta=
#   meta.varimp.res%>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts) %>%
#   group_by(Variable) 
# 
# 
# ##----renaming variables
# rename_var_imp.meta$Variable=factor(rename_var_imp.meta$Variable)
# 
# levels(rename_var_imp.meta$Variable)
# 
# #meta_df_new_column_names=last_metadata_df%>%
# #                               clean_names()#case = "upper_camel")
# 
# #meta_df_new=last_metadata_df
# new.colname.meta.data=c("Captive (no)","Captive (semi_ranging)",
#                         "Captive (yes)","Class (aves)", 
#                         "Class (insecta)" , "Class (mammalia)",
#                         "Class (reptilia)","Data_collection (focal_sampling)",
#                         "Data_collection (logger)", "Data_collection (mark_recapture)",
#                         "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#                         "Data_collection (unknown)", "Data_collection (video)",  
#                         "Data_duration (days)","Edge_weight (duration)",                        
#                         "Edge_weight (frequency)","Edge_weight (half_weight_index)",               
#                         "Edge_weight (simple_ratio_index)","Edge_weight (twice_weight_index)",              
#                         "Edge_weight (udoi)","Edge_weight (unweighted)",
#                         "Interaction_type (dominance)","Interaction_type (foraging)",
#                         "Interaction_type (grooming)", "Interaction_type (group_membership)",                 
#                         "Interaction_type (physical_contact)","Interaction_type (social_projection_bipartite)",           
#                         "Interaction_type (spatial_proximity)","Species (bolitotherus_cornutus)",
#                         "Species (bos_taurus)","Species (branta_leucopsis)",  
#                         "Species (camponotus_fellah)", "Species (crocuta_crocuta)",
#                         "Species (giraffa_camelopardalis)","Species (gopherus_agassizii)",
#                         "Species (haemorhous_mexicanus)","Species (hirundo_rustica)" ,
#                         "Species (macaca_fuscata)", "Species (macaca_mulatta)",
#                         "Species (macropus_giganteus)","Species (meles_meles)",
#                         "Species (microtus_agrestis)", "Species (mirounga_angustirostris)", 
#                         "Species (pan_troglodytes)", "Species (papio_cynocephalus)",                    
#                         "Species (philetairus_socius)", "Species (procyon_lotor)",                 
#                         "Species (tursiops_truncatus)","Species (zonotrichia_atricapilla)", 
#                         "Time_resolution_secs (coarse)","Time_resolution_secs (fine)",
#                         "Time_resolution_secs (focal_follow)","Time_resolution_secs (intermediate)",
#                         "Time_resolution_secs (very_fine)")
# 
# 
# 
# #levels(rename_var_imp$Variable)=clean_names(case = "upper_camel")
# 
# levels(rename_var_imp.meta$Variable)=new.colname.meta.data
# rename_var_imp.meta$Variable=as.character(rename_var_imp.meta$Variable) 
# 
# sim.var.imp.plot.meta=rename_var_imp.meta%>%
#   summarise(Mean = scale(mean(Importance),center = FALSE, scale = 0.1),
#             Variance = scale(sd(Importance),center = FALSE, scale = 0.1)) %>%
#   slice_max(Mean, n = 15) %>%
#   ggplot(aes(Mean, reorder(Variable, Mean))) +
#   #  xlim(-0.1,3)+
#   geom_crossbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
#   labs(x = " ", y = NULL)+
#   theme_classic()+
#   theme(text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# sim.var.imp.plot.meta
# ggsave("sim.var.imp.plot.meta.png",width = 7,height = 5)
# #############################################################################
# # xgb.final.metadata.fit %>%
# #   extract_fit_parsnip()%>%
# #   vip(geom = "col")+
# #   theme_bw()+
# #   theme(plot.title = element_text(size = 12),
# #         text = element_text(size = 12,family="serif"),
# #         axis.text.x = element_text(size = 12),
# #         axis.text.y = element_text(size = 12),
# #         axis.line = element_line(colour = "black"),
# #         panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         panel.border = element_blank())
# # 
# # ggsave("xgb.metadata.varimp.png",width = 10,height = 5)
# ###----Save----Trained----RF----last----model----###
# 
# 
# #Collect prediction
# xgb.final.metadata.fit %>%
#   collect_predictions()
# 
# xgb.final.metadata.fit %>%
#   collect_predictions() %>%
#   roc_curve(target, c(.pred_sbm,
#                       .pred_ER,.pred_SF,.pred_Spatial,
#                       .pred_SW)) %>%autoplot()
# 
# saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# #To make predictions on new data set, we call the saved xgb model eg
# load_fitted_xgb_metadata_model=readRDS("xgb.final.metadata.fit.rds")
# #extract workflow from fitted model
# xgb.final.metadata.model=extract_workflow(xgb.final.metadata.fit)
# saveRDS(xgb.final.metadata.model,"xgb.final.metadata.model.rds")
# #Prediction on test set
# xgb.pred_result_metadata=augment(xgb.final.metadata.model,last_metadata_test)
# 
# xgb.predValsmetadata=xgb.pred_result_metadata%>%
#   slice_head(n=10)
# xgb.predValsmetadata
# 
# #c----confusion matrix----
# #pred_result_metadata%>%
# confmatrix.meta1=table(xgb.predValsmetadata$target,xgb.predValsmetadata$.pred_class)
# confmatrix.meta1
# saveRDS(confmatrix.meta1,"confmatrix.meta1.rds")
# 
# 
# ########################################################################################
# # Shapely analysis for meta data with species
# #########################################################################################
# meta_data_bake <- bake(
#   last_metadata_prep, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = last_metadata_df, 
#   composition = "matrix"
# )
# #head(final_resample_data_prep)
# 
# meta_shap<- shapviz(extract_fit_engine(xgb.final.metadata.fit),
#                     X_pred = meta_data_bake, 
#                     X = last_metadata_df)
# 
# # sv_interaction(meta_shap, kind = "no")
# 
# saveRDS(meta_shap,"meta_shap.rds")
# meta_shap=readRDS("meta_shap.rds")
# 
# ##----renaming variables
# meta_shap_column_names=
#   c("Data_duration (days)","Captive (no)",
#     "Captive (semi_ranging)","Captive (yes)",
#     "Edge_weight (duration)","Edge_weight (frequency)",
#     "Edge_weight (half_weight_index)","Edge_weight (simple_ratio_index)",               
#     "Edge_weight (twice_weight_index)", "Edge_weight (udoi)",         
#     "Edge_weight (unweighted)","Interaction_type (dominance)",
#     "Interaction_type (foraging)", "Interaction_type (grooming)",                  
#     "Interaction_type (group_membership)","Interaction_type (physical_contact)",           
#     "Interaction_type (social_projection_bipartite)","Interaction_type (spatial_proximity)",
#     "Interaction_type (trophallaxis)","Data_collection (focal_sampling)",
#     "Data_collection (logger)", "Data_collection (mark_recapture)",
#     "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#     "Data_collection (unknown)", "Data_collection (video)", 
#     "Species (acanthiza_sp)", "Species_Ateles (hybridus)",
#     "Species (bison_bison)" ,"Species (bolitotherus_cornutus)",            
#     "Species (bos_taurus)","Species (branta_leucopsis)",  
#     "Species (camponotus_fellah)","Species (camponotus_pennsylvanicus)",
#     "Species (Canis_familiaris)","Species (crocuta_crocuta)",
#     "Species (desmodus_rotundus)","Species (elephas_maximus)",
#     "Species (equus_grevyi)" ,"Species (gallus_gallus)",
#     "Species (giraffa_camelopardalis)","Species (gopherus_agassizii)" ,
#     "Species (haemorhous_mexicanus)","Species (hirundo_rustica)" ,
#     "Species (macaca_fuscata)", "Species (macaca_mulatta)",
#     "Species (macropus_giganteus)","Species (meles_meles)",
#     "Species (microtus_agrestis)","Species (mirounga_angustirostris)",
#     "Species (myotis_sodalis)","Species (ovis_canadensis)",
#     "Species (pan_troglodytes)", "Species (papio_cynocephalus)",                    
#     "Species (philetairus_socius)", "Species (procyon_lotor)",                 
#     "Species (tiliqua_rugosa)", "Species (tursiops_truncatus)",
#     "Species (zonotrichia_atricapilla)","Class (aves)",
#     "Class (insecta)" , "Class (mammalia)",
#     "Class (reptilia)","Time_resolution_secs (coarse)",
#     "Time_resolution_secs (fine)","Time_resolution_secs (focal_follow)",
#     "Time_resolution_secs (intermediate)","Time_resolution_secs (very_fine)")
# 
# 
# names(meta_shap)[names(meta_shap)=="Class_1"]<-"Erdös-Rényi"
# names(meta_shap)[names(meta_shap) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap)[names(meta_shap) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap)[names(meta_shap) == "Class_3"] <- "Scale-Free"
# names(meta_shap)[names(meta_shap) == "Class_4"] <- "Spatial"
# names(meta_shap)[names(meta_shap) == "Class_5"] <- "Small-World"
# 
# meta_shap
# 
# colnames(meta_shap$`Erdös-Rényi`$X)=meta_shap_column_names
# colnames(meta_shap$`Erdös-Rényi`$S)=meta_shap_column_names
# #colnames(meta_shap$`Erdös-Rényi`$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$`Stochastic-Block-Model`$X)=meta_shap_column_names
# colnames(meta_shap$`Stochastic-Block-Model`$S)=meta_shap_column_names
# #colnames(meta_shap$`Stochastic-Block-Model`$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$`Scale-Free`$X)=meta_shap_column_names
# colnames(meta_shap$`Scale-Free`$S)=meta_shap_column_names
# #colnames(meta_shap$`Scale-Free`$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$Spatial$X)=meta_shap_column_names
# colnames(meta_shap$Spatial$S)=meta_shap_column_names
# #colnames(meta_shap$Spatial$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$`Small-World`$X)=meta_shap_column_names
# colnames(meta_shap$`Small-World`$S)=meta_shap_column_names
# #colnames(meta_shap$`Small-World`$S_inter)=meta_shap_column_names
# 
# 
# saveRDS(meta_shap,"final_meta_shap.rds")
# 
# final_meta_shap=readRDS("final_meta_shap.rds")
# 
# #sv_dependence(shp_i, v = "color", color_var = "cut", interactions = TRUE)
# #sv_interaction(final_meta_shap, kind = "no")
# 
# ############################################################
# # Shapely variable importance and summary plots
# ############################################################
# sp.meta.shap.imp=sv_importance(final_meta_shap$Spatial, kind = "both",bee_width = 0.3)#,
# #show_numbers = TRUE, bee_width = 0.2)
# saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
# sp.meta.shap.imp.plot=sp.meta.shap.imp+ggtitle(" ")+
#   xlab(" ") +
#   xlim(NA,2.5)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position = "none") & scale_color_continuous()
# 
# sp.meta.shap.imp.plot
# 
# ggsave("spatial.shap.summary.plot1.png", width = 7,height=4)
# 
# 
# 
# #sv_importance(final_meta_shap$Spatial, kind = 'beeswarm')
# ###----Erdos Renyi var imp----###
# # sp.meta.shap.imp=sv_importance(final_meta_shap$Spatial, kind = 'beeswarm')+
# #   ggtitle("Shap summary plot for Spatial ")+
# #   xlab("SHAP Value") +
# #   theme_bw()+
# #   theme(plot.title = element_text(size = 12),
# #         text = element_text(size = 12,family="serif"),
# #         axis.text.x = element_text(size = 12),
# #         axis.text.y = element_text(size = 12),
# #         axis.line = element_line(colour = "black"),
# #         panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank())
# 
# #ggsave("sp.meta.shap.imp.png", width = 7,height=4)
# 
# #saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
# 
# ############################################################
# # Shap Dependency plot
# ############################################################
# top.meta.feats=c("Interaction_type (physical_contact)",
#                  "Captive (no)","Time_resolution_secs (very_fine)",
#                  "Data_collection (video)","Species (camponotus_fellah)",
#                  "Data_duration (days)")
# ###----shapely dependency plot----
# metashap.dependency.plot1=sv_dependence(final_meta_shap$Spatial, v =top.meta.feats,color_var = NULL ) &
#   theme_gray(base_size = 9)& theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) 
# 
# metashap.dependency.plot1
# 
# ###----2D dependency plot
# metashap.2Ddependency.plot1=sv_dependence2D(final_meta_shap$Spatial, 
#                                             x ="Species (camponotus_fellah)", 
#                                             y = c("Data_duration (days)","Captive (no)",
#                                                   "Time_resolution_secs (very_fine)",
#                                                   "Interaction_type (physical_contact)"
#                                             ), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) & scale_color_continuous()
# 
# metashap.2Ddependency.plot1
# 
# 
# ###----combine shapely summary plot for spatial----####
# spatial.combine.shapdepend.plot1 <- ggarrange(metashap.dependency.plot1,
#                                               ggarrange(metashap.2Ddependency.plot1,
#                                                         align = "v",widths = c(0.5,1)),
#                                               nrow = 2,
#                                               labels = c("A)","B)"),
#                                               hjust = -2.6,#-3.1,
#                                               vjust = 1.1,
#                                               font.label = list(size = 12, color = "black", face = "bold",
#                                                                 family = "serif"))
# spatial.combine.shapdepend.plot1
# ggsave("spatial.combine.shapdepend.plot1.png", width = 13, height = 14)
# 
# ###############################################################################
# # shapely  interactions
# ###############################################################################
# meta_shap_interactn<- shapviz(extract_fit_engine(xgb.final.metadata.fit),
#                               X_pred = meta_data_bake, 
#                               X = last_metadata_df,
#                               interactions = TRUE)
# 
# 
# 
# # sv_interaction(meta_shap_interactn, kind = "no")
# saveRDS(meta_shap_interactn,"meta_shap_interactn.rds")
# meta_shap_interactn=readRDS("meta_shap_interactn.rds")
# 
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_3"] <- "Scale-Free"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_4"] <- "Spatial"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_5"] <- "Small-World"
# 
# 
# colnames(meta_shap_interactn$"Erdös-Rényi"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Erdös-Rényi"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Erdös-Rényi"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Erdös-Rényi"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Erdös-Rényi"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Stochastic-Block-Model"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Stochastic-Block-Model"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Stochastic-Block-Model"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Stochastic-Block-Model"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Stochastic-Block-Model"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Scale-Free"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Scale-Free"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Scale-Free"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Scale-Free"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Scale-Free"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Spatial"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Spatial"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Spatial"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Spatial"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Spatial"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Small-World"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Small-World"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Small-World"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Small-World"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Small-World"$S_inter)[[2]])
# 
# 
# saveRDS(meta_shap_interactn,"final_meta_shap.interactn.rds")
# 
# final_meta_shap.interactn=readRDS("final_meta_shap.interactn.rds")
# 
# ##----Interaction for "time_resolution_secs_Very_fine"
# sv_dependence(final_meta_shap.interactn$Spatial, "Time_resolution_secs (very_fine)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "social projections bipartite"
# sv_dependence(final_meta_shap.interactn$Spatial,
#               "Interaction_type (social_projection_bipartite)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "data collection focal sampling"
# sv_dependence(final_meta_shap.interactn$Spatial,
#               "Data_collection (focal_sampling)",
#               color_var = "auto", interactions = TRUE)
# 
# ##----Interaction for "time resolution intermediate"
# sv_dependence(final_meta_shap.interactn$Class_4, 
#               "time_resolution_secs_Intermediate",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "class_Mammalia"
# sv_dependence(final_meta_shap.interactn$Spatial, "Class (mammalia)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "interaction_type_Grooming"
# sv_dependence(final_meta_shap.interactn$Spatial, "Interaction_type (grooming)",
#               color_var = "auto", interactions = TRUE)
# 
# ##----Interaction for "species_Camponotus_fellah"
# sv_dependence(final_meta_shap.interactn$Spatial, "Species (camponotus_fellah)",
#               color_var = "auto", interactions = TRUE)
# 
# ##----Interaction for "interaction_type_Grooming"
# sv_dependence(final_meta_shap.interactn$Spatial, "Data_duration (days)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ####----Interaction for multiple plots at once
# sample.meta.feats=c("Species (camponotus_fellah)",
#                     "Time_resolution_secs (very_fine)",
#                     "Interaction_type (physical_contact)",
#                     "Captive (no)",
#                     #  "Data_collection (focal_sampling)",
#                     #  "Time_resolution_secs (intermediate)",
#                     "Data_duration (days)")
# #"interaction_type_Grooming"
# 
# meta.ineractn1.plot=sv_dependence(final_meta_shap.interactn$Spatial,
#                                   v = "Species (camponotus_fellah)", 
#                                   color_var = sample.meta.feats, interactions = TRUE) & theme_bw()+
#   theme(plot.title = element_text(size = 34),
#         text = element_text(size = 34,family="serif"),
#         axis.text.x = element_text(size = 34),
#         axis.text.y = element_text(size = 34),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# meta.ineractn1.plot+plot_layout(widths = c(1.7, 1.7))
# ggsave("shap.spatial.interaction1.png", width = 29, height=20)
# 
# #########################################################################################
# # Analysis without species
# #########################################################################################
# df_final_meta_data2=df_final_meta_data%>%
#   select(-species)
# 
# saveRDS(df_final_meta_data2,"df_final_meta_data2.rds")
# #----creating training, test and validation set
# set.seed(123)
# meta.df.split2 <- df_final_meta_data2[sample(1:nrow(df_final_meta_data2)),]%>%
#   initial_split(strata =  target,prop=0.6)
# 
# #saveRDS(meta.df.split,"meta.df.split.rds")
# 
# metadata_train2<- training(meta.df.split2)
# #saveRDS(metadata_train,"metadata_train.rds")
# metadata_test2<- testing(meta.df.split2)
# #saveRDS(metadata_test,"metadata_test.rds")
# 
# 
# #check na's for the train and test data
# colSums(is.na(metadata_train2))
# metadata_train2
# metadata_test2
# 
# ##----Creting the recipe object
# metadata.rec2=recipe(target ~ ., data = metadata_train2)%>%
#   step_dummy(all_nominal_predictors(),one_hot = T)
# #step_dummy(Target, one_hot = T)
# 
# #--compute missing values for numeric predictors
# #saveRDS(metadata.rec,"metadata.rec.rds")
# 
# metadata.prep2=metadata.rec2%>%
#   prep()
# 
# #saveRDS(metadata.prep,"metadata.prep.rf")
# 
# metadata.juice2=juice(metadata.prep2)
# 
# #saveRDS(metadata.juice,"metadata.juice")
# 
# colSums(is.na(metadata.juice2))
# 
# x.meta2=metadata.juice2%>%
#   select(-target)
# 
# y.meta2=metadata.juice2%>%
#   select(target)
# 
# x.meta.feat2=x.meta2%>%
#   mutate_all(as.numeric)
# 
# last_metadata_df2=cbind(y.meta2,x.meta.feat2)
# colnames(last_metadata_df2)
# 
# saveRDS(last_metadata_df2,"last_metadata_df2.rds")
# 
# set.seed(123)
# last_meta.df.split2 <- last_metadata_df2[sample(1:nrow(last_metadata_df2)),]%>%
#   initial_split(strata =  target,prop=0.6)
# 
# saveRDS(last_meta.df.split2,"last_meta.df.split2.rds")
# #saveRDS(last_meta.df.split2,"last_meta.df.split2.rds")
# 
# last_metadata_train2<- training(last_meta.df.split2)
# saveRDS(last_metadata_train2,"last_metadata_train2.rds")
# 
# last_metadata_test2<- testing(last_meta.df.split2)
# saveRDS(last_metadata_test2,"last_metadata_test2.rds")
# 
# last_metadata_rec2=recipe(target ~ ., data = last_metadata_train2)
# saveRDS(last_metadata_rec2,"last_metadata_rec2.rds")
# 
# last_metadata_prep2=last_metadata_rec2%>%prep()
# saveRDS(last_metadata_prep2,"last_metadata_prep2.rds")
# 
# last_metadata_juice2=last_metadata_prep2%>%juice()
# saveRDS(last_metadata_juice2,"last_metadata_juice2.rds")
# 
# 
# #bootstraps(df.final.train, times = 100)
# last_metadata_folds2 <- last_metadata_train2 %>% bootstraps(times = 100)
# 
# saveRDS(last_metadata_folds2,"last_metadata_folds2.rds")
# 
# #saveRDS(last_metadata_df,"last_metadata_df.rds")
# ###----model-----
# xgb.model.spec2 <-
#   boost_tree(mtry = tune(), tree = tune(),
#              learn_rate = tune(), tree_depth = tune()) %>%
#   set_engine("xgboost",importance = "permutation") %>%
#   set_mode("classification")
# 
# 
# #Creating workflow object
# 
# xgb.wkflw2 <-
#   workflow() %>%
#   add_model(xgb.model.spec2) %>%
#   add_recipe(last_metadata_rec2)
# 
# library(finetune)
# doParallel::registerDoParallel()
# 
# set.seed(345)
# xgb.res2 <-
#   xgb.wkflw2 %>% 
#   tune_race_anova(last_metadata_folds2,
#                   grid = 25,
#                   control = control_race(save_pred = TRUE)
#                   #control = control_grid(save_pred = TRUE)#,
#                   #metrics = metric_set(roc_auc)
#   )
# 
# saveRDS(xgb.res2,"xgb.res2.rds")
# 
# ####----Model Evaluation----####
# xgb.metadata.res2=xgb.res2
# 
# xgb.metadata.res2%>%
#   collect_metrics()
# 
# xgb.metadata.res2%>%
#   collect_predictions
# 
# xgb.metadata.res2 %>%
#   show_best(metric = "roc_auc")
# #Shows best estimated tuned parameters
# #autoplot(xgb.metadata.res2)
# 
# 
# # ####----Select best tuned parameters for final model-----####
# xgb.best_metadata_auc2 <- select_best(xgb.metadata.res2, "roc_auc")
# xgb.best_metadata_auc2
# 
# xgb.final.model.metadata2 <- finalize_model(
#   xgb.model.spec2,
#   xgb.best_metadata_auc2)
# 
# #----The last workflow
# xgb.final.metadata.wkflw2 <- workflow() %>%
#   add_recipe(last_metadata_rec2) %>%
#   add_model(xgb.final.model.metadata2)
# 
# #----The last fitted model
# xgb.final.metadata.fit2 <-
#   xgb.final.metadata.wkflw2 %>%
#   last_fit(last_meta.df.split2)
# 
# saveRDS(xgb.final.metadata.fit2,"xgb.final.metadata.fit2.rds")
# 
# 
# xgb.final.metadata.fit2%>%
#   collect_metrics()
# 
# xgb.final.metadata.fit2 %>%
#   extract_fit_parsnip()%>%
#   vip(geom = "col")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# #Collect prediction
# xgb.final.metadata.fit2 %>%
#   collect_predictions()
# 
# xgb.final.metadata.fit2 %>%
#   collect_predictions() %>%
#   roc_curve(target, c(.pred_ER,.pred_sbm,
#                       .pred_SF,.pred_Spatial,.pred_SW)) %>%autoplot()
# 
# #saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# #To make predictions on new data set, we call the saved xgb model eg
# load_fitted_xgb_metadata_model2=readRDS("xgb.final.metadata.fit2.rds")
# #extract workflow from fitted model
# xgb.final.metadata.model2=extract_workflow(xgb.final.metadata.fit2)
# saveRDS(xgb.final.metadata.model2,"xgb.final.metadata.model2.rds")
# #Prediction on test set
# xgb.pred_result_metadata2=augment(xgb.final.metadata.model2,last_metadata_test2)
# 
# xgb.predValsmetadata2=xgb.pred_result_metadata2%>%
#   slice_head(n=10)
# xgb.predValsmetadata2
# 
# #c----confusion matrix----
# #pred_result_metadata%>%
# confmatrix.meta2=table(xgb.predValsmetadata2$target,xgb.predValsmetadata2$.pred_class)
# confmatrix.meta2
# saveRDS(confmatrix.meta2,"confmatrix.meta2.rds")
# 
# 
# 
# 
# 
# 
# ### var importance for multiple simulations
# set.seed(95321)
# meta.folds2=bootstraps(last_metadata_train2, times = 100)
# 
# #folds
# 
# meta.get_xgb_imp2 <- function(x) {
#   x %>% 
#     extract_fit_parsnip() %>% 
#     vip::vi()
# }
# 
# meta.ctrl_imp2<- control_grid(extract = meta.get_xgb_imp2)
# 
# xgb.best.model.specs2 <-
#   xgb.metadata.res2 %>%
#   select_best(metric = "accuracy")
# 
# meta.varimp.model2=xgb.final.model.metadata2%>%
#   finalize_model(select_best(xgb.metadata.res2))%>%
#   set_engine("xgboost",importance="permutation")
# 
# meta.varimp.res2=workflow() %>%
#   add_recipe(last_metadata_rec2) %>% 
#   add_model(meta.varimp.model2)%>% 
#   fit_resamples(meta.folds2, control = meta.ctrl_imp2)
# 
# meta.varimp.res2
# #save trained models
# #saveRDS(varimp.res,"varimp.res.rds")
# 
# #varimp.res=readRDS("varimp.res.rds")
# 
# 
# rename_var_imp.meta2=
#   meta.varimp.res2 %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts) %>%
#   group_by(Variable) 
# 
# 
# ##----renaming variables
# rename_var_imp.meta2$Variable=factor(rename_var_imp.meta2$Variable)
# 
# #meta_df_new=last_metadata_df
# new.colname.meta.data2=c("Captive (no)","Captive (semi_ranging)",
#                          "Captive (yes)","Class (aves)", 
#                          "Class (insecta)" , "Class (mammalia)",
#                          "Class (reptilia)","Data_collection (focal_sampling)",
#                          "Data_collection (logger)", "Data_collection (mark_recapture)",
#                          "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#                          "Data_collection (unknown)", "Data_collection (video)",  
#                          "Data_duration (days)","Edge_weight (duration)",                        
#                          "Edge_weight (frequency)","Edge_weight (half_weight_index)",               
#                          "Edge_weight (simple_ratio_index)","Edge_weight (twice_weight_index)",
#                          "Edge_weight (udoi)","Edge_weight (unweighted)",
#                          "Interaction_type (dominance)","Interaction_type (foraging)",
#                          "Interaction_type (grooming)", "Interaction_type (group_membership)",                 
#                          "Interaction_type (physical_contact)","Interaction_type (social_projection_bipartite)",           
#                          "Interaction_type (spatial_proximity)","Interaction_type(trophallaxis)",
#                          "Time_resolution_secs (coarse)","Time_resolution_secs (fine)",
#                          "Time_resolution_secs (focal_follow)", "Time_resolution_secs (intermediate)",
#                          "Time_resolution_secs (very_fine)")
# 
# 
# levels(rename_var_imp.meta2$Variable)=new.colname.meta.data2
# rename_var_imp.meta2$Variable=as.character(rename_var_imp.meta2$Variable) 
# 
# sim.var.imp.plot.meta2=rename_var_imp.meta2%>%
#   summarise(Mean = scale(mean(Importance),center = FALSE, scale = 0.1),
#             Variance = scale(sd(Importance),center = FALSE, scale = 0.1)) %>%
#   slice_max(Mean, n = 18) %>%
#   ggplot(aes(Mean, reorder(Variable, Mean))) +
#   geom_crossbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
#   labs(x = "Variable importance", y = NULL)+
#   theme_classic()+
#   theme(text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# sim.var.imp.plot.meta2
# 
# ggsave("sim.var.imp.meta2.plot.png", width = 7, height = 5)
# 
# ######----combine metadata variable importance plot-----####
# meta.figure <- ggarrange(sim.var.imp.plot.meta,
#                          ggarrange(sim.var.imp.plot.meta2,
#                                    align = "v",widths = c(0.5,1)),
#                          nrow = 2,
#                          labels = c("A)","B)"),
#                          hjust = -5.9,#-3.1,
#                          vjust = 0.9,
#                          font.label = list(size = 12, color = "black", face = "bold",
#                                            family = "serif"))
# 
# meta.figure
# ggsave("meta.figure.png", width = 10, height = 8)
# 
# 
# 
# ########################################################################################
# # Shapely analysis for meta data without species 
# #########################################################################################
# meta_data_bake2 <- bake(
#   last_metadata_prep2, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = last_metadata_df2, 
#   composition = "matrix"
# )
# #head(final_resample_data_prep)
# 
# ##----No interactions first
# meta_shap2<- shapviz(extract_fit_engine(xgb.final.metadata.fit2),
#                      X_pred = meta_data_bake2, 
#                      X = last_metadata_df2)
# 
# 
# saveRDS(meta_shap2,"meta_shap2.rds")
# meta_shap2=readRDS("meta_shap2.rds")
# 
# ###----getting shap values----
# meta.shap.vals2=get_shap_values(meta_shap2)
# meta.shap.vals2
# 
# ##----renaming variables
# #meta_shap_column_names2=colnames(meta.shap.vals2$Class_1)
# meta_shap_column_names2=
#   c("Data_duration (days)","Captive (no)",
#     "Captive (semi_ranging)","Captive (yes)",
#     "Edge_weight (duration)", "Edge_weight (frequency)",
#     "Edge_weight (half_weight_index)","Edge_weight (simple_ratio_index)",
#     "Edge_weight (twice_weight_index)","Edge_weight (udoi)",
#     "Edge_weight (unweighted)","Interaction_type (dominance)",
#     "Interaction_type (foraging)","Interaction_type (grooming)",
#     "Interaction_type (group_membership)","Interaction_type (physical_contact)",
#     "Interaction_type (social_projection_bipartite)", "Interaction_type (spatial_proximity)",
#     "Interaction_type(trophallaxis)","Data_collection (focal_sampling)",
#     "Data_collection (logger)", "Data_collection (mark_recapture)",
#     "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#     "Data_collection (unknown)", "Data_collection (video)",  
#     "Class (aves)", "Class (insecta)" ,
#     "Class (mammalia)","Class (reptilia)",
#     "Time_resolution_secs (coarse)","Time_resolution_secs (fine)",
#     "Time_resolution_secs (focal_follow)", "Time_resolution_secs (intermediate)",
#     "Time_resolution_secs (very_fine)")
# 
# 
# names(meta.shap.vals2)[names(meta.shap.vals2) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.vals2)[names(meta.shap.vals2) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.vals2)[names(meta.shap.vals2) == "Class_3"] <- "Scale-Free"
# names(meta.shap.vals2)[names(meta.shap.vals2) == "Class_4"] <- "Spatial"
# names(meta.shap.vals2)[names(meta.shap.vals2) == "Class_5"] <- "Small-World"
# colnames(meta.shap.vals2$`Erdös-Rényi`)=meta_shap_column_names2
# colnames(meta.shap.vals2$`Stochastic-Block-Model`)=meta_shap_column_names2
# colnames(meta.shap.vals2$`Scale-Free`)=meta_shap_column_names2
# colnames(meta.shap.vals2$Spatial)=meta_shap_column_names2
# colnames(meta.shap.vals2$`Small-World`)=meta_shap_column_names2
# meta.shap.vals2
# 
# meta.shap.feat2=get_feature_values(meta_shap2)
# names(meta.shap.feat2)[names(meta.shap.feat2) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.feat2)[names(meta.shap.feat2) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.feat2)[names(meta.shap.feat2) == "Class_3"] <- "Scale-Free"
# names(meta.shap.feat2)[names(meta.shap.feat2) == "Class_4"] <- "Spatial"
# names(meta.shap.feat2)[names(meta.shap.feat2) == "Class_5"] <- "Small-World"
# colnames(meta.shap.feat2$`Erdös-Rényi`)=meta_shap_column_names2
# colnames(meta.shap.feat2$`Stochastic-Block-Model`)=meta_shap_column_names2
# colnames(meta.shap.feat2$`Scale-Free`)=meta_shap_column_names2
# colnames(meta.shap.feat2$Spatial)=meta_shap_column_names2
# colnames(meta.shap.feat2$`Small-World`)=meta_shap_column_names2
# meta.shap.feat2
# 
# 
# meta.shap.baseline2=get_baseline(meta_shap2)
# names(meta.shap.baseline2)[names(meta.shap.baseline2) == "Class_1"] <- "Erdös-Rényi"
# names(meta.shap.baseline2)[names(meta.shap.baseline2) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta.shap.baseline2)[names(meta.shap.baseline2) == "Class_3"] <- "Scale-Free"
# names(meta.shap.baseline2)[names(meta.shap.baseline2) == "Class_4"] <- "Spatial"
# names(meta.shap.baseline2)[names(meta.shap.baseline2) == "Class_5"] <- "Small-World"
# 
# 
# sp.meta.shap2=shapviz(meta.shap.vals2$Spatial,meta.shap.feat2$Spatial,
#                       meta.shap.baseline2$Spatial)
# 
# er.meta.shap2=shapviz(meta.shap.vals2$`Erdös-Rényi`,meta.shap.feat2$`Erdös-Rényi`,
#                       meta.shap.baseline2$`Erdös-Rényi`)
# 
# sbm.meta.shap2=shapviz(meta.shap.vals2$`Stochastic-Block-Model`,
#                        meta.shap.feat2$`Stochastic-Block-Model`,
#                        meta.shap.baseline2$`Stochastic-Block-Model`)
# sw.meta.shap2=shapviz(meta.shap.vals2$`Small-World`,meta.shap.feat2$`Small-World`,
#                       meta.shap.baseline2$`Small-World`)
# sf.meta.shap2=shapviz(meta.shap.vals2$`Scale-Free`,meta.shap.feat2$`Scale-Free`,
#                       meta.shap.baseline2$`Scale-Free`)
# 
# final_meta_shap2 <- mshapviz(c('Erdös-Rényi' = er.meta.shap2,
#                                'Stochastic-Block-Model'=sbm.meta.shap2,
#                                'Scale-Free'= sf.meta.shap2,
#                                'Spatial'=sp.meta.shap2,
#                                'Small-World'=sw.meta.shap2))
# 
# saveRDS(final_meta_shap2,"final_meta_shap2.rds")
# 
# final_meta_shap2=readRDS("final_meta_shap2.rds")
# 
# 
# sp.meta.shap.imp2=sv_importance(final_meta_shap2$Spatial,kind = "both",bee_width = 0.3)
# 
# sp.meta.shap.imp2.plot=sp.meta.shap.imp2+ggtitle(" ")+
#   xlab("SHAP Values") +
#   xlim(NA,2.5)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position = "none") & scale_color_continuous() &
#   theme(legend.position='bottom', legend.direction='horizontal')
# 
# sp.meta.shap.imp2.plot
# 
# ggsave("spatial.shap.summary.plot2.png", width = 7,height=4)
# #sp.meta.shap.imp1+ scale_x_continuous(limits = c(0.01, 0.05))
# ###----combine shapely summary plot for spatial----####
# spatial.combine.shapsummary.plot <- ggarrange(sp.meta.shap.imp.plot,
#                                               ggarrange(sp.meta.shap.imp2.plot,
#                                                         align = "v",widths = c(0.5,1)),
#                                               nrow = 2,
#                                               labels = c("A)","B)"),
#                                               hjust = -5.9,#-3.1,
#                                               vjust = 1.2,
#                                               font.label = list(size = 12, color = "black", face = "bold",
#                                                                 family = "serif"))
# spatial.combine.shapsummary.plot
# ggsave("spatial.combine.shapsummary.meta.png", width = 10, height = 10)
# 
# 
# 
# ############################################################
# # Shap Dependency plot (without species)
# ############################################################
# top.meta.feats2=c("Interaction_type (physical_contact)",
#                   "Interaction_type (spatial_proximity)","Captive (no)",
#                   "Data_collection (video)","Data_collection (focal_sampling)",
#                   "Data_duration (days)")
# ###----shapely dependency plot----
# metashap.dependency.plot2=sv_dependence(final_meta_shap2$Spatial, v =top.meta.feats2,color_var = NULL ) &
#   theme_gray(base_size = 9)& theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) 
# 
# metashap.dependency.plot2
# 
# ###----2D dependency plot
# metashap.2Ddependency.plot2=sv_dependence2D(final_meta_shap2$Spatial, 
#                                             x ="Interaction_type (physical_contact)", 
#                                             y = c("Data_duration (days)",
#                                                   "Interaction_type (spatial_proximity)","Captive (no)",
#                                                   "Data_collection (video)",
#                                                   "Data_collection (focal_sampling)"), alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) & scale_color_continuous()
# 
# metashap.2Ddependency.plot2#+plot_layout(widths = c(0.8, 0.8))
# 
# 
# ###----combine shapely summary plot for spatial----####
# spatial.combine.shapdepend.plot2 <- ggarrange(metashap.dependency.plot2,
#                                               ggarrange(metashap.2Ddependency.plot2,
#                                                         align = "v",widths = c(0.5,1)),
#                                               nrow = 2,
#                                               labels = c("A)","B)"),
#                                               hjust = -2.6,#-3.1,
#                                               vjust = 1.1,
#                                               font.label = list(size = 12, color = "black", face = "bold",
#                                                                 family = "serif"))
# spatial.combine.shapdepend.plot2
# ggsave("spatial.combine.shapdepend.plot2.png", width = 13, height = 14)
# 
# 
# ###############################################################################
# # shapely  interactions without species
# ###############################################################################
# 
# ##----Interaction
# meta_shap_interactn2<- shapviz(extract_fit_engine(xgb.final.metadata.fit2),
#                                X_pred = meta_data_bake2, 
#                                X = last_metadata_df2,
#                                interactions = TRUE)
# 
# saveRDS(meta_shap_interactn2,"meta_shap_interactn2.rds")
# meta_shap_interactn2=readRDS("meta_shap_interactn2.rds")
# 
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_3"] <- "Scale-Free"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_4"] <- "Spatial"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_5"] <- "Small-World"
# 
# 
# colnames(meta_shap_interactn2$"Erdös-Rényi"$X)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Erdös-Rényi"$S)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Erdös-Rényi"$S_inter)=meta_shap_column_names2
# dimnames(meta_shap_interactn2$"Erdös-Rényi"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Erdös-Rényi"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Stochastic-Block-Model"$X)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Stochastic-Block-Model"$S)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Stochastic-Block-Model"$S_inter)=meta_shap_column_names2
# dimnames(meta_shap_interactn2$"Stochastic-Block-Model"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Stochastic-Block-Model"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Scale-Free"$X)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Scale-Free"$S)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Scale-Free"$S_inter)=meta_shap_column_names2
# dimnames(meta_shap_interactn2$"Scale-Free"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Scale-Free"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Spatial"$X)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Spatial"$S)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Spatial"$S_inter)=meta_shap_column_names2
# dimnames(meta_shap_interactn2$"Spatial"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Spatial"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Small-World"$X)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Small-World"$S)=meta_shap_column_names2
# colnames(meta_shap_interactn2$"Small-World"$S_inter)=meta_shap_column_names2
# dimnames(meta_shap_interactn2$"Small-World"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Small-World"$S_inter)[[2]])
# 
# 
# saveRDS(meta_shap_interactn2,"final_meta_shap.interactn2.rds")
# 
# final_meta_shap.interactn2=readRDS("final_meta_shap.interactn2.rds")
# 
# ###----NB:SPATIAL NETWORK IS CLASS 4
# ##----Interaction for "time_resolution_secs_Very_fine"
# sv_dependence(final_meta_shap.interactn2$Spatial, "Time_resolution_secs (very_fine)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "social projections bipartite"
# sv_dependence(final_meta_shap.interactn2$Spatial,
#               "Interaction_type (social_projection_bipartite)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "data collection focal sampling"
# sv_dependence(final_meta_shap.interactn2$Spatial,
#               "Data_collection (focal_sampling)",
#               color_var = "auto", interactions = TRUE)
# 
# ##----Interaction for "time resolution intermediate"
# sv_dependence(final_meta_shap.interactn2$Spatial, 
#               "time_resolution_secs_Intermediate",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "class_Mammalia"
# sv_dependence(final_meta_shap.interactn2$Spatial, "Class (mammalia)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ##----Interaction for "interaction_type_Grooming"
# sv_dependence(final_meta_shap.interactn2$Spatial, "Interaction_type (grooming)",
#               color_var = "auto", interactions = TRUE)
# 
# ##----Interaction for "species_Camponotus_fellah"
# sv_dependence(final_meta_shap.interactn2$Spatial, "Time_resolution_secs (focal_follow)",
#               color_var = "auto", interactions = TRUE)
# 
# ##----Interaction for "interaction_type_Grooming"
# sv_dependence(final_meta_shap.interactn2$Spatial, "Data_duration (days)",
#               color_var = "auto", interactions = TRUE)
# 
# 
# ####----Interaction for multiple plots at once
# sample.meta.feats2=c("Interaction_type (physical_contact)",
#                      "Interaction_type (spatial_proximity)","Captive (no)",
#                      "Data_collection (video)","Data_collection (focal_sampling)",
#                      "Data_duration (days)")
# #"interaction_type_Grooming"
# 
# meta.interactn2.plot=sv_dependence(final_meta_shap.interactn2$Spatial,
#                                    v = "Interaction_type (spatial_proximity)", 
#                                    color_var = sample.meta.feats2, interactions = TRUE) & theme_bw()+
#   theme(plot.title = element_text(size = 34),
#         text = element_text(size = 34,family="serif"),
#         axis.text.x = element_text(size = 34),
#         axis.text.y = element_text(size = 34),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# meta.interactn2.plot+plot_layout(widths = c(1.2, 1.2))
# ggsave("shap.spatial.interaction2.png", width = 26, height=20)




# iml.spatial.features.dummy <- mlr::createDummyFeatures(iml.spatial.features)

#model_unified <- ranger.unify(xgb.final.metadata.model, iml.spatial)

# library(treeshap)
# aps_data <- mlr::createDummyFeatures(DALEX::apartments)
# 



#corrr::correlate(df.final, method = "pearson")

# 
# final_resample_data <- df.final#[sample(nrow(df.final), 1000), ]
# final_resample_data_prep <- bake(
#   df.final.prep, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = final_resample_data, 
#   composition = "matrix"
# )
# head(final_resample_dataprep)
# 
# shap <- shapviz(extract_fit_engine(final.fitted.model), X_pred = final_resample_data_prep, X = final_resample_data)
# saveRDS(shap,"shap.rds")
# shap=readRDS("shap.rds")

# install.packages("DALEX")
# install.packages("ggplot2")
# library(DALEX)
# library(ggplot2)
# df.final=readRDS("df.final.rds")
# colSums(is.na(df.final))
# ################################################################
# # Pre processing final data for model
# ################################################################
# #----final data recipe
# df.final.rec=readRDS("df.final.rec.rds")
# df.final.prep=readRDS("df.final.prep.rds")
# 
# set.seed(384)
# #----final sample and split
# df.final.split=df.final[sample(1:nrow(df.final)), ]%>%
#   initial_split(prop = 0.7)
# df.final.split=readRDS("df.final.split.rds")
# df.final.train=readRDS("df.final.train.rds")
# df.final.test=readRDS("df.final.test.rds")
# #----Print number of training and testing set
# cat("training set:", nrow(df.final.train), "\n",
#     "testing set :", nrow(df.final.test), "\n")
# #------cross validation----
# df.cv.splits=readRDS("df.cv.splits.rds")
# ##----Final fitted model----##
# final.fitted.model = readRDS("final.fitted.model.rds")
# extract_final.fitted.model=readRDS("extract_final.fitted.model.rds")
# #model=extract_fit_engine(final.fitted.model) #model
# model=extract_final.fitted.model
# 
# x.data <- df.final %>%
#   dplyr::select(-c(graph_name)) %>%
#   as.data.frame()
# 
# y.data <- df.final%>%
#   dplyr::select(graph_name)
# 
# #prediction function
# predict.wrapper <- function(model, newdata){
#   workflows:::predict.workflow(object = model, new_data = newdata)
# }
# ##The predictor function enables to interrogate or get explanation of the data
# predictorfunc <- Predictor$new(
#   model =model,
#   data =  x.data,
#   y = y.data,
#   predict.function = predict.wrapper
# )
# 
# predictor <- Predictor$new(predictorfunc$model,
#                                        data = x.data,
#                                        type = "prob")
# saveRDS(predictor,"predictor.rds")
# # predictor <- Predictor$new(model,
# #                   data = x.data,
# #                   y = y.data,
# #                   type = "prob",class = "Spatial")#Predictor$new(model, data = x.data, y = y.data)
# 
# mod_trans.ale <- FeatureEffect$new(predictor, feature = c("modularity"),
#                                    method = "pdp")
#                         # method = "pdp",)#method = "pdp+ice"
# y=mod_trans.ale$results%>%
#   filter(.class==".pred_ER")
# y=y%>%
#   select(.class,.ale,modularity,transitivity)
# 
# colnames(y)=c("class","value","modularity","transitivity")
# y
# 
# # Create the heatmap plot
# plot <- ggplot(y, aes(x = modularity, y = transitivity)) +
#   geom_tile(aes(fill =value), colour = "blue")+
#   labs(title = "Heatmap Interaction Plot",
#        x = "Modularity",
#        y = "Transitivity") +
#   theme_minimal()
# 
# 
# plot
# 
# saveRDS("mod_trans.ale.rds")
# 
# mod_deg.ale <- FeatureEffect$new(predictor, feature = c("modularity","deg_centr"),
#                                  # method = "pdp",
#                                  grid.size = 30)#method = "pdp+ice"
# saveRDS("mod_deg.ale.rds")
# 
# mod_nf.ale <- FeatureEffect$new(predictor, feature = c("modularity","normalized_fiedler_value"),
#                                    # method = "pdp",
#                                    grid.size = 30)#method = "pdp+ice"
# saveRDS("mod_nf.ale.rds")
# 
# mod_degassort.ale <- FeatureEffect$new(predictor, feature = c("modularity","deg_assort_coef"),
#                                 # method = "pdp",
#                                 grid.size = 30)#method = "pdp+ice"
# saveRDS("mod_degassort.ale.rds")
# 
# mod_sr.ale <- FeatureEffect$new(predictor, feature = c("modularity","spectral_radius"),
#                                        # method = "pdp",
#                                        grid.size = 30)#method = "pdp+ice"
# saveRDS("mod_sr.ale.rds")
# 
# mod_eigen.ale <- FeatureEffect$new(predictor, feature = c("modularity","eigen_centr"),
#                                 # method = "pdp",
#                                 grid.size = 30)#method = "pdp+ice"
# saveRDS("mod_eigen.ale.rds")
# 
# trans_eigen.ale <- FeatureEffect$new(predictor, feature = c("transitivity","eigen_centr"),
#                                    # method = "pdp",
#                                    grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("trans_eigen.ale.rds")
# 
# trans_sr.ale <- FeatureEffect$new(predictor, feature = c("transitivity","spectral_radius"),
#                                      # method = "pdp",
#                                      grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("trans_sr.ale.rds")
# 
# trans_nf.ale <- FeatureEffect$new(predictor, feature = c("transitivity","normalized_fiedler_value"),
#                                   # method = "pdp",
#                                   grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("trans_nf.ale.rds")
# 
# trans_degassort.ale <- FeatureEffect$new(predictor, feature = c("transitivity","deg_assort_coef"),
#                                   # method = "pdp",
#                                   grid.size = 30)#method = "pdp+ice"
# saveRDS("trans_degassort.ale.rds")
# 
# degassort_sr.ale <- FeatureEffect$new(predictor, feature = c("deg_assort_coef","spectral_radius"),
#                                   # method = "pdp",
#                                   grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("degassort_sr.ale.rds")
# 
# degassort_nf.ale <- FeatureEffect$new(predictor, feature = c("deg_assort_coef","normalized_fiedler_value"),
#                                       # method = "pdp",
#                                       grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("degassort_nf.ale.rds")
# 
# degassort_degcentr.ale <- FeatureEffect$new(predictor, feature = c("deg_assort_coef","deg_centr"),
#                                       # method = "pdp",
#                                       grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("degassort_degcentr.ale.rds")
# 
# degcentr_nf.ale <- FeatureEffect$new(predictor, feature = c("deg_centr","normalized_fiedler_value"),
#                                             # method = "pdp",
#                                             grid.size = 30)#method = "pdp+ice"
# saveRDS("degcentr_nf.ale.rds")
# 
# degcentr_sr.ale <- FeatureEffect$new(predictor, feature = c("deg_centr","spectral_radius"),
#                                      # method = "pdp",
#                                      grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("degcentr_sr.ale.rds")
# 
# nf_sr.ale <- FeatureEffect$new(predictor, feature = c("normalized_fiedler_value","spectral_radius"),
#                                      # method = "pdp",
#                                      grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("nf_sr.ale.rds")
# 
# degassort_sr.ale <- FeatureEffect$new(predictor, feature = c("deg_assort_coef","spectral_radius"),
#                                # method = "pdp",
#                                grid.size = 30)#method = "pdp+ice"
# 
# saveRDS("degassort_sr.ale.rds")
# 
# # degcentr_sr.ale <- FeatureEffect$new(predictor, feature = c("deg_centr","spectral_radius"),
# #                                       # method = "pdp",
# #                                       grid.size = 30)#method = "pdp+ice"
# 
# 
# #target variable
# #df.final.test[sample(1:nrow(df.final.test)), ]
# 
# #y.data <- df.final%>%
#   # mutate(job_search = as.factor(job_search)) %>%
# #  dplyr::select(graph_name)
# 
# ############################################################################################
# #Variable Relationships
# #############################################################################################
# 
# #----[1]----PARTIAL DEPENDENCY PLOT-----#
# # rf <- rpart(medv ~ ., data = Boston)
# # mod <- Predictor$new(rf, data = Boston)
# # 
# # # Compute the accumulated local effects for the first feature
# # eff <- FeatureEffect$new(mod, feature = "rm", grid.size = 30)
# # eff$plot()
# # 
# # # Again, but this time with a partial dependence plot and ice curves
# # eff <- FeatureEffect$new(mod,
# #                          feature = "rm", method = "pdp+ice",
# #                          grid.size = 30
# # )
# 
# #----[2]----ICE PLOT-----#
# 
# 
# 
# 
# 
# 
# 
# 
# #prediction function
# predict.wrapper <- function(model, newdata){
#   workflows:::predict.workflow(object = model, new_data = newdata)
# }
# 
# ##The predictor function enables to interrogate or get explanation of the data
# predictorfunc <- Predictor$new(
#   model =extract_final.fitted.model,
#   data = features,
#   y = target,
#   predict.fun = predict.wrapper
# )
# 
# 
# 
# # best.model.pred.class <- Predictor$new(predictorfunc$model,
# #                                        data = x.data,
# #                                        type = "prob")
# 
# 
# 
# mod_trans.preds <- FeatureEffect$new(best.model.pred.class,
#                                       feature = c("modularity","edges"),
#                                       grid.size = 30)
# 
# levels(mod_trans.preds$results$.class)= c('Erdös-Rényi', 'Stochastic-Block-Model', 
#                                            'Scale-Free', 'Spatial','Small-World')
# 
# 
# #mod_trans.preds$plot()
# 
# #plot(mod_trans.preds$results)
# 
# ggplot(mod_trans.preds$results, aes(x = modularity, y = edges, 
#                 color =.ale))+
#   facet_grid(~.class)+
#   geom_point(stat="identity")#+
#  # scale_fill_gradientn(colours = c("blue", "white", "red")) +
#  # theme_minimal()
# 
# 
# 
# #effect1 <- modularity.preds#FeatureEffect$new(explainer, feature = "feature1")
# #effect2 <- transitivity.preds#FeatureEffect$new(explainer, feature = "feature2")
# 
# #Replace "feature1" and "feature2" with the actual names of your predictor variables.
# #ale_interaction <- effect1$plot(feature = effect2, order = 2)
# #Here, we use the plot() function of the first feature's FeatureEffect object (effect1) and pass the second feature's FeatureEffect object (effect2) as an argument. The order parameter is set to 2 to calculate the 2nd order ALE interactions.
# 
# 
# 
# # Adjust the aesthetics (x, y, and fill) and the theme according to your preferences.
# # 
# # The resulting plot will show the 2nd order ALE interaction between the two features, where the color indicates the accumulated local effects. Darker colors represent higher effects, and lighter colors represent lower effects.
# # 
# # Make sure to replace the relevant variable names (data, response, feature1, feature2) and adapt the code to your specific dataset and modeling context.
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# Examples
# # We train a random forest on the Boston dataset:
# data("Boston", package = "MASS")
# library("rpart")
# rf <- rpart(medv ~ ., data = Boston)
# mod <- Predictor$new(rf, data = Boston)
# 
# # Compute the accumulated local effects for the first feature
# eff <- FeatureEffect$new(mod, feature = "rm", grid.size = 30)
# eff$plot()
# 
# # Again, but this time with a partial dependence plot and ice curves
# eff <- FeatureEffect$new(mod,
#                          feature = "rm", method = "pdp+ice",
#                          grid.size = 30
# )
# plot(eff)
# 
# # Since the result is a ggplot object, you can extend it:
# library("ggplot2")
# plot(eff) +
#   # Adds a title
#   ggtitle("Partial dependence") +
#   # Adds original predictions
#   geom_point(
#     data = Boston, aes(y = mod$predict(Boston)[[1]], x = rm),
#     color = "pink", size = 0.5
#   )
# 
# # If you want to do your own thing, just extract the data:
# eff.dat <- eff$results
# head(eff.dat)
# 
# # You can also use the object to "predict" the marginal values.
# eff$predict(Boston[1:3, ])
# # Instead of the entire data.frame, you can also use feature values:
# eff$predict(c(5, 6, 7))
# 
# # You can reuse the pdp object for other features:
# eff$set.feature("lstat")
# plot(eff)
# 
# # Only plotting the aggregated partial dependence:
# eff <- FeatureEffect$new(mod, feature = "crim", method = "pdp")
# eff$plot()
# 
# # Only plotting the individual conditional expectation:
# eff <- FeatureEffect$new(mod, feature = "crim", method = "ice")
# eff$plot()
# 
# # Accumulated local effects and partial dependence plots support up to two
# # features:
# eff <- FeatureEffect$new(mod, feature = c("crim", "lstat"))
# plot(eff)
# 
# # FeatureEffect plots also works with multiclass classification
# rf <- rpart(Species ~ ., data = iris)
# mod <- Predictor$new(rf, data = iris, type = "prob")
# 
# # For some models we have to specify additional arguments for the predict
# # function
# plot(FeatureEffect$new(mod, feature = "Petal.Width"))
# 
# # FeatureEffect plots support up to two features:
# eff <- FeatureEffect$new(mod, feature = c("Sepal.Length", "Petal.Length"))
# eff$plot()
# 
# # show where the actual data lies
# eff$plot(show.data = TRUE)
# 
# # For multiclass classification models, you can choose to only show one class:
# mod <- Predictor$new(rf, data = iris, type = "prob", class = 1)
# plot(FeatureEffect$new(mod, feature = "Sepal.Length"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # species.plot=ggplot(species.ale.df, aes(x = species, y = .value, color = .type)) +
# #   geom_bar(stat = "identity") +
# #   geom_bar(data = species.pdp.df, aes(x = species, y = .value), color = "blue") +
# #   labs(x = "Species", y = "ALE", color = "method",title = "Spatial") +
# #   scale_color_manual(values = c("ale" = "red", "pdp" = "blue"))+
# #   theme(strip.text = element_text(size = 12))+
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=12),
# #         axis.line = element_line(colour = "black"),
# #         panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         strip.background = element_rect(colour="white", fill="white"),
# #         panel.background = element_blank(),
# #         text = element_text(size = 12,family="serif"),
# #         axis.text.y = element_text(size = 12),
# #         axis.title.x = element_text(size = 12),
# #         axis.title.y = element_text(size = 12),
# #         legend.text = element_text(size = 12))
# # 
# # species.plot

























# ######################################################################################
# # Creating the model -one hot encoding
# ########################################################################################
# df_final_meta_data=readRDS("df_final_meta_data.rds")
# #----creating training, test and validation set
# set.seed(123)
# meta.df.split <- df_final_meta_data[sample(1:nrow(df_final_meta_data)),]%>%
#   initial_split(strata =  Target,prop=0.6)
# 
# saveRDS(meta.df.split,"meta.df.split.rds")
# 
# metadata_train<- training(meta.df.split)
# saveRDS(metadata_train,"metadata_train.rds")
# 
# metadata_test<- testing(meta.df.split)
# saveRDS(metadata_test,"metadata_test.rds")
# 
# 
# #check na's for the train and test data
# colSums(is.na(metadata_train))
# metadata_train
# metadata_test
# 
# ##----Creting the recipe object
# metadata.rec=recipe(Target ~ ., data = metadata_train)%>%
#   step_dummy(all_nominal_predictors(),one_hot = T)
# #step_dummy(Target, one_hot = T)
# 
# #--compute missing values for numeric predictors
# saveRDS(metadata.rec,"metadata.rec.rds")
# 
# metadata.prep=metadata.rec%>%
#   prep()
# 
# saveRDS(metadata.prep,"metadata.prep.rf")
# 
# metadata.juice=juice(metadata.prep)
# 
# saveRDS(metadata.juice,"metadata.juice")
# t=readRDS("metadata.juice")
# t
# colSums(is.na(metadata.juice))
# 
# x.meta=metadata.juice%>%
#   select(-Target)
# 
# y.meta=metadata.juice%>%
#   select(Target)
# 
# x.meta.feat=x.meta%>%
#   mutate_all(as.numeric)
# 
# last_metadata_df=cbind(y.meta,x.meta.feat)
# 
# saveRDS(last_metadata_df,"last_metadata_df.rds")
# 
# 
# str(last_metadata_df)
# 
# #----creating final training, test and validation set
# #last_metadata_df=FinalTrainedMetadata
# set.seed(123)
# last_meta.df.split <- last_metadata_df[sample(1:nrow(last_metadata_df)),]%>%
#   initial_split(strata =  Target,prop=0.6)
# 
# saveRDS(last_meta.df.split,"last_meta.df.split.rds")
# 
# last_metadata_train<- training(last_meta.df.split)
# saveRDS(last_metadata_train,"last_metadata_train.rds")
# 
# last_metadata_test<- testing(last_meta.df.split)
# saveRDS(last_metadata_test,"last_metadata_test.rds")
# 
# colnames(last_metadata_train)
# 
# last_metadata_rec=recipe(Target ~ ., data = last_metadata_train)
# saveRDS(last_metadata_rec,"last_metadata_rec.rds")
# 
# last_metadata_prep=last_metadata_rec%>%prep()
# saveRDS(last_metadata_prep,"last_metadata_prep.rds")
# 
# last_metadata_juice=last_metadata_prep%>%juice()
# saveRDS(last_metadata_juice,"last_metadata_juice.rds")
# 
# 
# #bootstraps(df.final.train, times = 100)
# last_metadata_folds <- last_metadata_train %>% bootstraps(times = 50)
# last_metadata_folds
# saveRDS(last_metadata_folds,"last_metadata_folds.rds")
# 
# 
# ###----model-----
# xgb.model.spec <-
#   boost_tree(mtry = tune(), tree = tune(),
#              learn_rate = tune(), tree_depth = tune()) %>%
#   set_engine("xgboost",importance = "permutation") %>%
#   set_mode("classification")
# 
# 
# #Creating workflow object
# xgb.wkflw <-
#   workflow() %>%
#   add_model(xgb.model.spec) %>%
#   add_recipe(last_metadata_rec)
# 
# library(finetune)
# doParallel::registerDoParallel()
# 
# set.seed(345)
# xgb.res <-
#   xgb.wkflw %>% 
#   tune_race_anova(last_metadata_folds,
#                   grid = 25,
#                   control = control_race(save_pred = TRUE)
#                   #control = control_grid(save_pred = TRUE)#,
#                   #metrics = metric_set(roc_auc)
#   )
# 
# saveRDS(xgb.res,"xgb.res.rds")
# 
# 
# ####----Model Evaluation----####
# xgb.metadata.res=xgb.res
# 
# xgb.metadata.res%>%
#   collect_metrics()
# 
# library(here)
# #save trained RF classifier
# saveRDS(xgb.metadata.res,"xgb_metadata_res.rds")
# #Show best metric
# xgb.metadata.res %>%
#   show_best(metric = "roc_auc")
# #Shows best estimated tuned parameters
# autoplot(xgb.metadata.res)
# 
# # xgb.metadata.res %>%
# #   collect_metrics() %>%
# #   filter(.metric == "roc_auc") %>%
# #   select(mean, tree_depth, mtry) %>%
# #   pivot_longer(tree_depth:mtry,
# #                values_to = "value",
# #                names_to = "parameter"
# #   ) %>%
# #   ggplot(aes(value, mean, color = parameter)) +
# #   geom_point(show.legend = FALSE) +
# #   facet_wrap(~parameter, scales = "free_x") +
# #   labs(x = NULL, y = "AUC")
# # 
# # xgb.metadata.res %>%
# #   collect_metrics() %>%
# #   dplyr::filter(.metric == "roc_auc") %>%
# #   dplyr::select(mean, tree_depth, mtry) %>%
# #   pivot_longer(tree_depth:mtry,
# #                values_to = "value",
# #                names_to = "parameter")%>%
# #   ggplot(aes(value, mean, color = parameter)) +
# #   geom_point(show.legend = FALSE) +
# #   facet_wrap(~parameter, scales = "free_x") +
# #   labs(x = NULL, y = "AUC")
# # #Viewing best results
# # xgb.metadata.res %>%
# #   collect_metrics() %>%
# #   dplyr::filter(.metric == "roc_auc") %>%
# #   mutate(tree_depth = factor(tree_depth)) %>%
# #   ggplot(aes(mtry, mean, color = tree_depth)) +
# #   geom_line(alpha = 0.5, linewidth = 1.5) +
# #   geom_point() +
# #   labs(y = "AUC")
# # 
# # ####----Select best tuned parameters for final model-----####
# xgb.best_metadata_auc <- select_best(xgb.metadata.res, "roc_auc")
# xgb.best_metadata_auc
# 
# xgb.final.model.metadata <- finalize_model(
#   xgb.model.spec,
#   xgb.best_metadata_auc)
# # 
# 
# #----The last workflow
# xgb.final.metadata.wkflw <- workflow() %>%
#   add_recipe(last_metadata_rec) %>%
#   add_model(xgb.final.model.metadata)
# 
# #----The last fitted model
# xgb.final.metadata.fit <-
#   xgb.final.metadata.wkflw %>%
#   last_fit(last_meta.df.split)
# 
# 
# xgb.final.metadata.fit%>%
#   collect_metrics()
# 
# saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# 
# ##############################################################
# # Variable Importance
# #############################################################
# set.seed(539)
# meta.folds=last_metadata_folds#bootstraps(last_metadata_train, times = 100)
# 
# meta.get_xgb_imp <- function(x) {
#   x %>% 
#     extract_fit_parsnip() %>% 
#     vip::vi()
# }
# 
# meta.ctrl_imp<- control_grid(extract = meta.get_xgb_imp)
# 
# xgb.best.model.specs <-
#   xgb.metadata.res%>%
#   select_best(metric = "accuracy")
# 
# 
# meta.varimp.model=xgb.final.model.metadata%>%
#   finalize_model(select_best(xgb.metadata.res))%>%
#   set_engine("xgboost",importance="permutation")
# 
# meta.varimp.res=workflow() %>%
#   add_recipe(last_metadata_rec) %>% 
#   add_model(meta.varimp.model)%>% 
#   fit_resamples(meta.folds, control = meta.ctrl_imp)
# 
# meta.varimp.res
# #save trained models
# #saveRDS(varimp.res,"varimp.res.rds")
# 
# #varimp.res=readRDS("varimp.res.rds")
# 
# 
# rename_var_imp.meta=
#   meta.varimp.res%>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts) %>%
#   group_by(Variable) 
# 
# 
# ##----renaming variables
# rename_var_imp.meta$Variable=factor(rename_var_imp.meta$Variable)
# 
# levels(rename_var_imp.meta$Variable)
# 
# #meta_df_new_column_names=last_metadata_df%>%
# #                               clean_names()#case = "upper_camel")
# 
# #meta_df_new=last_metadata_df
# new.colname.meta.data=c("Captive (no)","Captive (semi_ranging)",
#                         "Captive (yes)","Class (aves)", 
#                         "Class (insecta)" , "Class (mammalia)",
#                         "Class (reptilia)","Data_collection (focal_sampling)",
#                         "Data_collection (logger)", "Data_collection (mark_recapture)",
#                         "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#                         "Data_collection (unknown)", "Data_collection (video)",  
#                         "Data_duration (days)","Edge_weight (duration)",                        
#                         "Edge_weight (frequency)","Edge_weight (half_weight_index)",               
#                         "Edge_weight (simple_ratio_index)","Edge_weight (twice_weight_index)",              
#                         "Edge_weight (udoi)","Edge_weight (unweighted)",
#                         "Interaction_type (dominance)","Interaction_type (foraging)",
#                         "Interaction_type (grooming)", "Interaction_type (group_membership)",                 
#                         "Interaction_type (physical_contact)","Interaction_type (social_projection_bipartite)",           
#                         "Interaction_type (spatial_proximity)","Species (bolitotherus_cornutus)",
#                         "Species (bos_taurus)","Species (branta_leucopsis)",  
#                         "Species (camponotus_fellah)", "Species (crocuta_crocuta)",
#                         "Species (giraffa_camelopardalis)","Species (gopherus_agassizii)",
#                         "Species (haemorhous_mexicanus)","Species (hirundo_rustica)" ,
#                         "Species (macaca_fuscata)", "Species (macaca_mulatta)",
#                         "Species (macropus_giganteus)","Species (meles_meles)",
#                         "Species (microtus_agrestis)", "Species (mirounga_angustirostris)", 
#                         "Species (pan_troglodytes)", "Species (papio_cynocephalus)",                    
#                         "Species (philetairus_socius)", "Species (procyon_lotor)",                 
#                         "Species (tursiops_truncatus)","Species (zonotrichia_atricapilla)", 
#                         "Time_resolution_secs (coarse)","Time_resolution_secs (fine)",
#                         "Time_resolution_secs (focal_follow)","Time_resolution_secs (intermediate)",
#                         "Time_resolution_secs (very_fine)")
# 
# 
# 
# #levels(rename_var_imp$Variable)=clean_names(case = "upper_camel")
# 
# levels(rename_var_imp.meta$Variable)=new.colname.meta.data
# rename_var_imp.meta$Variable=as.character(rename_var_imp.meta$Variable) 
# 
# sim.var.imp.plot.meta=rename_var_imp.meta%>%
#   summarise(Mean = scale(mean(Importance),center = FALSE, scale = 0.1),
#             Variance = scale(sd(Importance),center = FALSE, scale = 0.1)) %>%
#   slice_max(Mean, n = 15) %>%
#   ggplot(aes(Mean, reorder(Variable, Mean))) +
#   #  xlim(-0.1,3)+
#   geom_crossbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
#   labs(x = " ", y = NULL)+
#   theme_classic()+
#   theme(text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# sim.var.imp.plot.meta
# ggsave("sim.var.imp.plot.meta.png",width = 7,height = 5)
# #############################################################################
# # xgb.final.metadata.fit %>%
# #   extract_fit_parsnip()%>%
# #   vip(geom = "col")+
# #   theme_bw()+
# #   theme(plot.title = element_text(size = 12),
# #         text = element_text(size = 12,family="serif"),
# #         axis.text.x = element_text(size = 12),
# #         axis.text.y = element_text(size = 12),
# #         axis.line = element_line(colour = "black"),
# #         panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         panel.border = element_blank())
# # 
# # ggsave("xgb.metadata.varimp.png",width = 10,height = 5)
# ###----Save----Trained----RF----last----model----###
# 
# 
# #Collect prediction
# xgb.final.metadata.fit %>%
#   collect_predictions()
# 
# xgb.final.metadata.fit %>%
#   collect_predictions() %>%
#   roc_curve(Target, c(`.pred_Stochastic-Block-Model`,
#                       `.pred_Erdös-Rényi`,`.pred_Scale-Free`,.pred_Spatial,
#                       `.pred_Small-World`)) %>%autoplot()
# 
# saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# #To make predictions on new data set, we call the saved xgb model eg
# load_fitted_xgb_metadata_model=readRDS("xgb.final.metadata.fit.rds")
# #extract workflow from fitted model
# xgb.final.metadata.model=extract_workflow(xgb.final.metadata.fit)
# saveRDS(xgb.final.metadata.model,"xgb.final.metadata.model.rds")
# #Prediction on test set
# xgb.pred_result_metadata=augment(xgb.final.metadata.model,last_metadata_test)
# 
# xgb.predValsmetadata=xgb.pred_result_metadata%>%
#   slice_head(n=10)
# xgb.predValsmetadata
# 
# #c----confusion matrix----
# #pred_result_metadata%>%
# confmatrix.meta1=table(xgb.predValsmetadata$Target,xgb.predValsmetadata$.pred_class)
# saveRDS(confmatrix.meta1,"confmatrix.meta1.rds")
# 
# 
# ########################################################################################
# # Shapely analysis for meta data with species
# #########################################################################################
# meta_data_bake <- bake(
#   last_metadata_prep, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = last_metadata_df, 
#   composition = "matrix"
# )
# #head(final_resample_data_prep)
# 
# meta_shap<- shapviz(extract_fit_engine(xgb.final.metadata.fit),
#                     X_pred = meta_data_bake, 
#                     X = last_metadata_df)
# 
# # sv_interaction(meta_shap, kind = "no")
# 
# saveRDS(meta_shap,"meta_shap.rds")
# meta_shap=readRDS("meta_shap.rds")
# 
# ##----renaming variables
# meta_shap_column_names=
#   c("Data_duration (days)","Captive (no)",
#     "Captive (semi_ranging)","Captive (yes)",
#     "Edge_weight (duration)","Edge_weight (frequency)",
#     "Edge_weight (half_weight_index)","Edge_weight (simple_ratio_index)",               
#     "Edge_weight (twice_weight_index)", "Edge_weight (udoi)",         
#     "Edge_weight (unweighted)","Interaction_type (dominance)",
#     "Interaction_type (foraging)", "Interaction_type (grooming)",                  
#     "Interaction_type (group_membership)","Interaction_type (physical_contact)",           
#     "Interaction_type (social_projection_bipartite)","Interaction_type (spatial_proximity)",
#     "Interaction_type (trophallaxis)","Data_collection (focal_sampling)",
#     "Data_collection (logger)", "Data_collection (mark_recapture)",
#     "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#     "Data_collection (unknown)", "Data_collection (video)", 
#     "Species (acanthiza_sp)", "Species_Ateles (hybridus)",
#     "Species (bison_bison)" ,"Species (bolitotherus_cornutus)",            
#     "Species (bos_taurus)","Species (branta_leucopsis)",  
#     "Species (camponotus_fellah)","Species (camponotus_pennsylvanicus)",
#     "Species (Canis_familiaris)","Species (crocuta_crocuta)",
#     "Species (desmodus_rotundus)","Species (elephas_maximus)",
#     "Species (equus_grevyi)" ,"Species (gallus_gallus)",
#     "Species (giraffa_camelopardalis)","Species (gopherus_agassizii)" ,
#     "Species (haemorhous_mexicanus)","Species (hirundo_rustica)" ,
#     "Species (macaca_fuscata)", "Species (macaca_mulatta)",
#     "Species (macropus_giganteus)","Species (meles_meles)",
#     "Species (microtus_agrestis)","Species (mirounga_angustirostris)",
#     "Species (myotis_sodalis)","Species (ovis_canadensis)",
#     "Species (pan_troglodytes)", "Species (papio_cynocephalus)",                    
#     "Species (philetairus_socius)", "Species (procyon_lotor)",                 
#     "Species (tiliqua_rugosa)", "Species (tursiops_truncatus)",
#     "Species (zonotrichia_atricapilla)","Class (aves)",
#     "Class (insecta)" , "Class (mammalia)",
#     "Class (reptilia)","Time_resolution_secs (coarse)",
#     "Time_resolution_secs (fine)","Time_resolution_secs (focal_follow)",
#     "Time_resolution_secs (intermediate)","Time_resolution_secs (very_fine)")
# 
# 
# names(meta_shap)[names(meta_shap)=="Class_1"]<-"Erdös-Rényi"
# names(meta_shap)[names(meta_shap) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap)[names(meta_shap) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap)[names(meta_shap) == "Class_3"] <- "Scale-Free"
# names(meta_shap)[names(meta_shap) == "Class_4"] <- "Spatial"
# names(meta_shap)[names(meta_shap) == "Class_5"] <- "Small-World"
# 
# colnames(meta_shap$`Erdös-Rényi`$X)=meta_shap_column_names
# colnames(meta_shap$`Erdös-Rényi`$S)=meta_shap_column_names
# #colnames(meta_shap$`Erdös-Rényi`$S_inter)=meta_shap_column_names
# colnames(meta_shap$`Stochastic-Block-Model`$X)=meta_shap_column_names
# colnames(meta_shap$`Stochastic-Block-Model`$S)=meta_shap_column_names
# #colnames(meta_shap$`Stochastic-Block-Model`$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$`Scale-Free`$X)=meta_shap_column_names
# colnames(meta_shap$`Scale-Free`$S)=meta_shap_column_names
# #colnames(meta_shap$`Scale-Free`$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$Spatial$X)=meta_shap_column_names
# colnames(meta_shap$Spatial$S)=meta_shap_column_names
# #colnames(meta_shap$Spatial$S_inter)=meta_shap_column_names
# 
# colnames(meta_shap$`Small-World`$X)=meta_shap_column_names
# colnames(meta_shap$`Small-World`$S)=meta_shap_column_names
# #colnames(meta_shap$`Small-World`$S_inter)=meta_shap_column_names
# 
# saveRDS(meta_shap,"final_meta_shap.rds")
# final_meta_shap=readRDS("final_meta_shap.rds")
# 
# ############################################################
# # Shapely variable importance and summary plots
# ############################################################
# ###########################################################
# sp.meta.shap.imp.bar=sv_importance(final_meta_shap$Spatial, kind = "bar",bee_width = 0.3)#,
# #show_numbers = TRUE, bee_width = 0.2)
# #saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
# sp.meta.shap.imp.bar.plot=sp.meta.shap.imp.bar+
#   ggtitle(" ")+
#   xlab(" ") +
#   # xlim(-0.3,0.5)+
#   theme_bw()+
#   theme(axis.title.y = element_blank(),  # Remove y-axis title
#         axis.text.y = element_blank(),  # Remove y-axis labels
#         axis.ticks.y = element_blank(),  # Remove y-axis ticks
#         axis.title.x = element_blank(),  # Remove y-axis title
#         axis.text.x = element_blank(),  # Remove y-axis labels
#         axis.ticks.x = element_blank(),  # Remove y-axis ticks
#         axis.line = element_blank(),
#         plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# sp.meta.shap.imp.bar.plot
# 
# sp.meta.shap.beeswarm.imp=sv_importance(final_meta_shap$Spatial, 
#                                         kind = "beeswarm",bee_width = 0.5)#,
# #show_numbers = TRUE, bee_width = 0.2)
# #saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
# sp.meta.shap.imp.beeswarm.plot=sp.meta.shap.beeswarm.imp+
#   ggtitle("Spatial")+
#   xlab("SHAP Value")+
#   # xlim(-0.3,0.7)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 19),
#         text = element_text(size = 19,family="serif"),
#         axis.text.x = element_text(size = 15),
#         axis.text.y = element_text(size = 19),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #        plot.margin=unit(c(1,-1,0.5,0), "cm"),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red")
# 
# sp.meta.shap.imp.beeswarm.plot
# figure1=sp.meta.shap.imp.beeswarm.plot+
#   theme(legend.position="none")
# 
# figure2=sp.meta.shap.imp.bar.plot
# sp.meta.combine.var.imp=plot_grid(figure1, figure2, align = "h", ncol = 2)
# # Get the legend from one of the plots
# legend <- get_legend(sp.meta.shap.imp.beeswarm.plot)
# 
# # Add the legend to the combined plot
# combined_plot_with_legend.sp <- plot_grid(sp.meta.combine.var.imp, 
#                                           legend, ncol = 1, rel_heights = c(1, 0.2))
# 
# combined_plot_with_legend.sp
# ggsave("sp_shap_varimp.png", width = 14, height = 7)
# 
# ############################################################
# # Shap Dependency plot
# ############################################################
# 
# ###----shapely dependency plot----
# 
# 
# top.meta.feats=c("Time_resolution_secs (very_fine)",
#                  "Interaction_type (grooming)","Class (mammalia)",
#                  "Data_collection (focal_sampling)","Species (camponotus_fellah)",
#                  "Data_duration (days)")
# ###----shapely dependency plot----
# metashap.dependency.plot1=sv_dependence(final_meta_shap$Spatial, 
#                                         v =top.meta.feats,color_var = NULL )&
#   theme_gray(base_size = 9)& theme_bw()+
#   theme(plot.title = element_text(size = 16),
#         text = element_text(size = 16,family="serif"),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())&
#   scale_color_gradient(low = "blue", high = "red")
# 
# metashap.dependency.plot1
# ggsave("metashap.dependency.plot1.png", width = 14, height = 7)
# 
# 
# ###----2D dependency plot
# metashap.2Ddependency.plot1=sv_dependence2D(final_meta_shap$Spatial, 
#                                             x ="Species (camponotus_fellah)", 
#                                             y = c("Species (papio_cynocephalus)",
#                                                   "Data_duration (days)",
#                                                   "Interaction_type (physical_contact)",
#                                                   "Captive (no)"
#                                             ), alpha = 0.5)& 
#   theme_bw()+
#   theme(plot.title = element_text(size = 14),
#         text = element_text(size = 14,family="serif"),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) & 
#   scale_color_gradient(low = "blue", high = "red")
# 
# metashap.2Ddependency.plot1
# ggsave("metashap.2Ddependency.plot1.png", width = 14, height = 7)
# 
# ###----combine shapely summary plot for spatial----####
# spatial.combine.shapdepend.plot1 <- ggarrange(metashap.dependency.plot1,
#                                               ggarrange(metashap.2Ddependency.plot1,
#                                                         align = "v",widths = c(0.5,1)),
#                                               nrow = 2,
#                                               labels = c("A)","B)"),
#                                               hjust = -2.6,#-3.1,
#                                               vjust = 1.1,
#                                               font.label = list(size = 12, color = "black", face = "bold",
#                                                                 family = "serif"))
# spatial.combine.shapdepend.plot1
# ggsave("spatial.combine.shapdepend.plot1.png", width = 13, height = 14)
# 
# ###############################################################################
# # shapely  interactions
# ###############################################################################
# meta_shap_interactn<- shapviz(extract_fit_engine(xgb.final.metadata.fit),
#                               X_pred = meta_data_bake, 
#                               X = last_metadata_df,
#                               interactions = TRUE)
# 
# 
# 
# # sv_interaction(meta_shap_interactn, kind = "no")
# saveRDS(meta_shap_interactn,"meta_shap_interactn.rds")
# meta_shap_interactn=readRDS("meta_shap_interactn.rds")
# 
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_3"] <- "Scale-Free"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_4"] <- "Spatial"
# names(meta_shap_interactn)[names(meta_shap_interactn) == "Class_5"] <- "Small-World"
# 
# 
# colnames(meta_shap_interactn$"Erdös-Rényi"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Erdös-Rényi"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Erdös-Rényi"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Erdös-Rényi"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Erdös-Rényi"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Stochastic-Block-Model"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Stochastic-Block-Model"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Stochastic-Block-Model"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Stochastic-Block-Model"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Stochastic-Block-Model"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Scale-Free"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Scale-Free"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Scale-Free"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Scale-Free"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Scale-Free"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Spatial"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Spatial"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Spatial"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Spatial"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Spatial"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn$"Small-World"$X)=meta_shap_column_names
# colnames(meta_shap_interactn$"Small-World"$S)=meta_shap_column_names
# colnames(meta_shap_interactn$"Small-World"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn$"Small-World"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn$"Small-World"$S_inter)[[2]])
# 
# 
# saveRDS(meta_shap_interactn,"final_meta_shap.interactn.rds")
# 
# final_meta_shap.interactn=readRDS("final_meta_shap.interactn.rds")
# 
# 
# 
# ##----Interactions----
# meta.shap.interactn=sv_dependence(final_meta_shap.interactn$Spatial, 
#                                   "Species (camponotus_fellah)",
#                                   color_var = "auto", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# saveRDS(meta.shap.interactn,"meta.shap.interactn.rds")
# 
# meta.shap.interactn=readRDS("meta.shap.interactn.rds")
# 
# meta.shap.interactn.df=meta.shap.interactn$data
# 
# colnames(meta.shap.interactn.df)=c("shap","Species","Captive")
# 
# meta.shap.interactn.plot=
#   ggplot(meta.shap.interactn.df, aes(x = Species, y = shap,
#                                      color=Captive)) +
#   geom_point(stat = "identity") +
#   ggtitle("Spatial")+
#   xlab("Species")+
#   ylab("SHAP interaction values for \n Species and Captive (no)")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(1, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# meta.shap.interactn.plot=meta.shap.interactn.plot+plot_layout(widths = 1.,heights = 1.)
# meta.shap.interactn.plot
# 
# 
# 
# meta.shap.interactn.sp.plot=
#   sv_interaction(final_meta_shap.interactn$Spatial,
#                  max_display = 5 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 24),
#         text = element_text(size = 24,family="serif"),
#         axis.text.x = element_text(size = 24),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# meta.shap.interactn.sp.plot
# 
# 
# ggsave("meta.fig.interactn.png", width = 28, height = 22)
# 
# ##----Interaction for "interaction_type_Grooming"
# # sv_dependence(final_meta_shap.interactn$Spatial, "Data_duration (days)",
# #               color_var = "auto", interactions = TRUE)
# 
# 
# ####----Interaction for multiple plots at once
# # sample.meta.feats=c("Species (camponotus_fellah)",
# #                     "Species (papio_cynocephalus)",
# #                     "Data_duration (days)",
# #                     "Interaction_type (physical_contact)",
# #                     "Captive (no)")
# # 
# # sv_dependence(final_meta_shap.interactn$Spatial,
# #               v = "Species (camponotus_fellah)", 
# #               color_var = sample.meta.feats, interactions = TRUE) & theme_bw()+
# #   theme(plot.title = element_text(size = 16),
# #         text = element_text(size = 16,family="serif"),
# #         axis.text.x = element_text(size = 16),
# #         axis.text.y = element_text(size = 16),
# #         axis.line = element_line(colour = "black"),
# #         panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank(),
# #         panel.background = element_blank(),
# #         panel.border = element_blank()) &
# #   scale_color_gradient(low = "blue", high = "red")
# # 
# # ggsave("shap.spatial.interaction1.png", width = 34, height=22)
# 
# #########################################################################################
# # Analysis without species
# #########################################################################################
# df_final_meta_data2=df_final_meta_data%>%
#   select(-species)
# 
# saveRDS(df_final_meta_data2,"df_final_meta_data2.rds")
# #----creating training, test and validation set
# set.seed(123)
# meta.df.split2 <- df_final_meta_data2[sample(1:nrow(df_final_meta_data2)),]%>%
#   initial_split(strata =  Target,prop=0.6)
# 
# #saveRDS(meta.df.split,"meta.df.split.rds")
# 
# metadata_train2<- training(meta.df.split2)
# #saveRDS(metadata_train,"metadata_train.rds")
# metadata_test2<- testing(meta.df.split2)
# #saveRDS(metadata_test,"metadata_test.rds")
# 
# 
# #check na's for the train and test data
# colSums(is.na(metadata_train2))
# metadata_train2
# metadata_test2
# 
# ##----Creting the recipe object
# metadata.rec2=recipe(Target ~ ., data = metadata_train2)%>%
#   step_dummy(all_nominal_predictors(),one_hot = T)
# #step_dummy(Target, one_hot = T)
# 
# #--compute missing values for numeric predictors
# #saveRDS(metadata.rec,"metadata.rec.rds")
# 
# metadata.prep2=metadata.rec2%>%
#   prep()
# 
# #saveRDS(metadata.prep,"metadata.prep.rf")
# 
# metadata.juice2=juice(metadata.prep2)
# 
# #saveRDS(metadata.juice,"metadata.juice")
# 
# colSums(is.na(metadata.juice2))
# 
# x.meta2=metadata.juice2%>%
#   select(-Target)
# 
# y.meta2=metadata.juice2%>%
#   select(Target)
# 
# x.meta.feat2=x.meta2%>%
#   mutate_all(as.numeric)
# 
# last_metadata_df2=cbind(y.meta2,x.meta.feat2)
# colnames(last_metadata_df2)
# 
# saveRDS(last_metadata_df2,"last_metadata_df2.rds")
# 
# set.seed(123)
# last_meta.df.split2 <- last_metadata_df2[sample(1:nrow(last_metadata_df2)),]%>%
#   initial_split(strata =  Target,prop=0.6)
# 
# saveRDS(last_meta.df.split2,"last_meta.df.split2.rds")
# #saveRDS(last_meta.df.split2,"last_meta.df.split2.rds")
# 
# last_metadata_train2<- training(last_meta.df.split2)
# saveRDS(last_metadata_train2,"last_metadata_train2.rds")
# 
# last_metadata_test2<- testing(last_meta.df.split2)
# saveRDS(last_metadata_test2,"last_metadata_test2.rds")
# 
# last_metadata_rec2=recipe(Target ~ ., data = last_metadata_train2)
# saveRDS(last_metadata_rec2,"last_metadata_rec2.rds")
# 
# last_metadata_prep2=last_metadata_rec2%>%prep()
# saveRDS(last_metadata_prep2,"last_metadata_prep2.rds")
# 
# last_metadata_juice2=last_metadata_prep2%>%juice()
# saveRDS(last_metadata_juice2,"last_metadata_juice2.rds")
# 
# 
# #bootstraps(df.final.train, times = 100)
# last_metadata_folds2 <- last_metadata_train2 %>% bootstraps(times = 50)
# 
# saveRDS(last_metadata_folds2,"last_metadata_folds2.rds")
# 
# #saveRDS(last_metadata_df,"last_metadata_df.rds")
# ###----model-----
# xgb.model.spec2 <-
#   boost_tree(mtry = tune(), tree = tune(),
#              learn_rate = tune(), tree_depth = tune()) %>%
#   set_engine("xgboost",importance = "permutation") %>%
#   set_mode("classification")
# 
# 
# #Creating workflow object
# 
# xgb.wkflw2 <-
#   workflow() %>%
#   add_model(xgb.model.spec2) %>%
#   add_recipe(last_metadata_rec2)
# 
# library(finetune)
# doParallel::registerDoParallel()
# 
# set.seed(345)
# xgb.res2 <-
#   xgb.wkflw2 %>% 
#   tune_race_anova(last_metadata_folds2,
#                   grid = 25,
#                   control = control_race(save_pred = TRUE)
#                   #control = control_grid(save_pred = TRUE)#,
#                   #metrics = metric_set(roc_auc)
#   )
# 
# saveRDS(xgb.res2,"xgb.res2.rds")
# 
# ####----Model Evaluation----####
# xgb.metadata.res2=xgb.res2
# 
# xgb.metadata.res2%>%
#   collect_metrics()
# 
# xgb.metadata.res2%>%
#   collect_predictions
# 
# xgb.metadata.res2 %>%
#   show_best(metric = "roc_auc")
# #Shows best estimated tuned parameters
# #autoplot(xgb.metadata.res2)
# 
# 
# # ####----Select best tuned parameters for final model-----####
# xgb.best_metadata_auc2 <- select_best(xgb.metadata.res2, "roc_auc")
# xgb.best_metadata_auc2
# 
# xgb.final.model.metadata2 <- finalize_model(
#   xgb.model.spec2,
#   xgb.best_metadata_auc2)
# 
# #----The last workflow
# xgb.final.metadata.wkflw2 <- workflow() %>%
#   add_recipe(last_metadata_rec2) %>%
#   add_model(xgb.final.model.metadata2)
# 
# #----The last fitted model
# xgb.final.metadata.fit2 <-
#   xgb.final.metadata.wkflw2 %>%
#   last_fit(last_meta.df.split2)
# 
# saveRDS(xgb.final.metadata.fit2,"xgb.final.metadata.fit2.rds")
# 
# 
# xgb.final.metadata.fit2%>%
#   collect_metrics()
# 
# xgb.final.metadata.fit2 %>%
#   extract_fit_parsnip()%>%
#   vip(geom = "col")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())
# 
# 
# 
# #Collect prediction
# xgb.final.metadata.fit2 %>%
#   collect_predictions()
# 
# xgb.final.metadata.fit2 %>%
#   collect_predictions() %>%
#   roc_curve(Target, c(`.pred_Stochastic-Block-Model`,
#                       `.pred_Erdös-Rényi`,`.pred_Scale-Free`,.pred_Spatial,
#                       `.pred_Small-World`)) %>%autoplot()
# 
# #saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# #To make predictions on new data set, we call the saved xgb model eg
# load_fitted_xgb_metadata_model2=readRDS("xgb.final.metadata.fit2.rds")
# #extract workflow from fitted model
# xgb.final.metadata.model2=extract_workflow(xgb.final.metadata.fit2)
# saveRDS(xgb.final.metadata.model2,"xgb.final.metadata.model2.rds")
# #Prediction on test set
# xgb.pred_result_metadata2=augment(xgb.final.metadata.model2,last_metadata_test2)
# 
# xgb.predValsmetadata2=xgb.pred_result_metadata2%>%
#   slice_head(n=10)
# xgb.predValsmetadata2
# 
# #c----confusion matrix----
# #pred_result_metadata%>%
# confmatrix.meta2=table(xgb.predValsmetadata2$Target,xgb.predValsmetadata2$.pred_class)
# saveRDS(confmatrix.meta2,"confmatrix.meta2.rds")
# 
# ### var importance for multiple simulations
# set.seed(95321)
# meta.folds2=bootstraps(last_metadata_train2, times = 100)
# 
# #folds
# 
# meta.get_xgb_imp2 <- function(x) {
#   x %>% 
#     extract_fit_parsnip() %>% 
#     vip::vi()
# }
# 
# meta.ctrl_imp2<- control_grid(extract = meta.get_xgb_imp2)
# 
# xgb.best.model.specs2 <-
#   xgb.metadata.res2 %>%
#   select_best(metric = "accuracy")
# 
# meta.varimp.model2=xgb.final.model.metadata2%>%
#   finalize_model(select_best(xgb.metadata.res2))%>%
#   set_engine("xgboost",importance="permutation")
# 
# meta.varimp.res2=workflow() %>%
#   add_recipe(last_metadata_rec2) %>% 
#   add_model(meta.varimp.model2)%>% 
#   fit_resamples(meta.folds2, control = meta.ctrl_imp2)
# 
# meta.varimp.res2
# #save trained models
# #saveRDS(varimp.res,"varimp.res.rds")
# #varimp.res=readRDS("varimp.res.rds")
# rename_var_imp.meta2=
#   meta.varimp.res2 %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts) %>%
#   group_by(Variable) 
# 
# 
# ##----renaming variables
# rename_var_imp.meta2$Variable=factor(rename_var_imp.meta2$Variable)
# 
# #meta_df_new=last_metadata_df
# new.colname.meta.data2=c("Captive (no)","Captive (semi_ranging)",
#                          "Captive (yes)","Class (aves)", 
#                          "Class (insecta)" , "Class (mammalia)",
#                          "Class (reptilia)","Data_collection (focal_sampling)",
#                          "Data_collection (logger)", "Data_collection (mark_recapture)",
#                          "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#                          "Data_collection (unknown)", "Data_collection (video)",  
#                          "Data_duration (days)","Edge_weight (duration)",                        
#                          "Edge_weight (frequency)","Edge_weight (half_weight_index)",               
#                          "Edge_weight (simple_ratio_index)","Edge_weight (twice_weight_index)",
#                          "Edge_weight (udoi)","Edge_weight (unweighted)",
#                          "Interaction_type (dominance)","Interaction_type (foraging)",
#                          "Interaction_type (grooming)", "Interaction_type (group_membership)",                 
#                          "Interaction_type (physical_contact)","Interaction_type (social_projection_bipartite)",           
#                          "Interaction_type (spatial_proximity)","Interaction_type(trophallaxis)",
#                          "Time_resolution_secs (coarse)","Time_resolution_secs (fine)",
#                          "Time_resolution_secs (focal_follow)", "Time_resolution_secs (intermediate)",
#                          "Time_resolution_secs (very_fine)")
# 
# 
# levels(rename_var_imp.meta2$Variable)=new.colname.meta.data2
# rename_var_imp.meta2$Variable=as.character(rename_var_imp.meta2$Variable) 
# 
# sim.var.imp.plot.meta2=rename_var_imp.meta2%>%
#   summarise(Mean = scale(mean(Importance),center = FALSE, scale = 0.1),
#             Variance = scale(sd(Importance),center = FALSE, scale = 0.1)) %>%
#   slice_max(Mean, n = 18) %>%
#   ggplot(aes(Mean, reorder(Variable, Mean))) +
#   geom_crossbar(aes(xmin = Mean - Variance, xmax = Mean + Variance)) +
#   labs(x = "Variable importance", y = NULL)+
#   theme_classic()+
#   theme(text = element_text(size = 15,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.title.x = element_text(size = 12),
#         axis.title.y = element_text(size = 12),
#         legend.position="none")
# 
# sim.var.imp.plot.meta2
# 
# ggsave("sim.var.imp.meta2.plot.png", width = 7, height = 5)
# 
# ######----combine metadata variable importance plot-----####
# meta.figure <- ggarrange(sim.var.imp.plot.meta,
#                          ggarrange(sim.var.imp.plot.meta2,
#                                    align = "v",widths = c(0.5,1)),
#                          nrow = 2,
#                          labels = c("A)","B)"),
#                          hjust = -5.9,#-3.1,
#                          vjust = 0.9,
#                          font.label = list(size = 12, color = "black", face = "bold",
#                                            family = "serif"))
# 
# meta.figure
# ggsave("meta.figure.png", width = 10, height = 8)
# 
# 
# 
# #Collect prediction
# xgb.final.metadata.fit2 %>%
#   collect_predictions()
# 
# 
# xgb.final.metadata.fit2 %>%
#   collect_predictions() %>%
#   roc_curve(Target, c(`.pred_Stochastic-Block-Model`,
#                       `.pred_Erdös-Rényi`,`.pred_Scale-Free`,.pred_Spatial,
#                       `.pred_Small-World`)) %>%autoplot()
# 
# #saveRDS(xgb.final.metadata.fit,"xgb.final.metadata.fit.rds")
# #To make predictions on new data set, we call the saved xgb model eg
# #load_fitted_xgb_metadata_model=readRDS("xgb.final.metadata.fit.rds")
# #extract workflow from fitted model
# xgb.final.metadata.model2=extract_workflow(xgb.final.metadata.fit2)
# #saveRDS(xgb.final.metadata.model,"xgb.final.metadata.model.rds")
# #Prediction on test set
# xgb.pred_result_metadata2=augment(xgb.final.metadata.model2,last_metadata_test2)
# 
# xgb.predValsmetadata2=xgb.pred_result_metadata2%>%
#   slice_head(n=10)
# xgb.predValsmetadata2
# 
# #c----confusion matrix----
# #pred_result_metadata%>%
# table(xgb.predValsmetadata2$Target,xgb.predValsmetadata2$.pred_class)
# 
# ########################################################################################
# # Shapely analysis for meta data without species 
# #########################################################################################
# meta_data_bake2 <- bake(
#   last_metadata_prep2, # prep(df.final.rec), 
#   has_role("predictor"),
#   new_data = last_metadata_df2, 
#   composition = "matrix"
# )
# #head(final_resample_data_prep)
# 
# ##----No interactions first
# meta_shap2<- shapviz(extract_fit_engine(xgb.final.metadata.fit2),
#                      X_pred = meta_data_bake2, 
#                      X = last_metadata_df2)
# 
# 
# saveRDS(meta_shap2,"meta_shap2.rds")
# meta_shap2=readRDS("meta_shap2.rds")
# 
# ##----renaming variables
# #meta_shap_column_names2=colnames(meta.shap.vals2$Class_1)
# meta_shap_column_names2=
#   c("Data_duration (days)","Captive (no)",
#     "Captive (semi_ranging)","Captive (yes)",
#     "Edge_weight (duration)", "Edge_weight (frequency)",
#     "Edge_weight (half_weight_index)","Edge_weight (simple_ratio_index)",
#     "Edge_weight (twice_weight_index)","Edge_weight (udoi)",
#     "Edge_weight (unweighted)","Interaction_type (dominance)",
#     "Interaction_type (foraging)","Interaction_type (grooming)",
#     "Interaction_type (group_membership)","Interaction_type (physical_contact)",
#     "Interaction_type (social_projection_bipartite)", "Interaction_type (spatial_proximity)",
#     "Interaction_type(trophallaxis)","Data_collection (focal_sampling)",
#     "Data_collection (logger)", "Data_collection (mark_recapture)",
#     "Data_collection (rfid)",  "Data_collection (survey_scan)",                      
#     "Data_collection (unknown)", "Data_collection (video)",  
#     "Class (aves)", "Class (insecta)" ,
#     "Class (mammalia)","Class (reptilia)",
#     "Time_resolution_secs (coarse)","Time_resolution_secs (fine)",
#     "Time_resolution_secs (focal_follow)", "Time_resolution_secs (intermediate)",
#     "Time_resolution_secs (very_fine)")
# 
# 
# 
# 
# names(meta_shap2)[names(meta_shap2) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap2)[names(meta_shap2) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap2)[names(meta_shap2) == "Class_3"] <- "Scale-Free"
# names(meta_shap2)[names(meta_shap2) == "Class_4"] <- "Spatial"
# names(meta_shap2)[names(meta_shap2) == "Class_5"] <- "Small-World"
# colnames(meta_shap2$`Erdös-Rényi`$X)=meta_shap_column_names2
# colnames(meta_shap2$`Erdös-Rényi`$S)=meta_shap_column_names2
# colnames(meta_shap2$`Stochastic-Block-Model`$X)=meta_shap_column_names2
# colnames(meta_shap2$`Stochastic-Block-Model`$S)=meta_shap_column_names2
# colnames(meta_shap2$`Scale-Free`$X)=meta_shap_column_names2
# colnames(meta_shap2$`Scale-Free`$S)=meta_shap_column_names2
# colnames(meta_shap2$Spatial$X)=meta_shap_column_names2
# colnames(meta_shap2$Spatial$S)=meta_shap_column_names2
# colnames(meta_shap2$`Small-World`$X)=meta_shap_column_names2
# colnames(meta_shap2$`Small-World`$S)=meta_shap_column_names2
# 
# saveRDS(meta_shap2,"final_meta_shap2.rds")
# final_meta_shap2=readRDS("final_meta_shap2.rds")
# 
# 
# ###########################################################
# sp.meta.shap.imp.bar2=sv_importance(final_meta_shap2$Spatial, kind = "bar",bee_width = 0.3)#,
# #show_numbers = TRUE, bee_width = 0.2)
# #saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
# sp.meta.shap.imp.bar2.plot=sp.meta.shap.imp.bar2+
#   ggtitle(" ")+
#   xlab(" ") +
#   # xlim(-0.3,0.5)+
#   theme_bw()+
#   theme(axis.title.y = element_blank(),  # Remove y-axis title
#         axis.text.y = element_blank(),  # Remove y-axis labels
#         axis.ticks.y = element_blank(),  # Remove y-axis ticks
#         axis.title.x = element_blank(),  # Remove y-axis title
#         axis.text.x = element_blank(),  # Remove y-axis labels
#         axis.ticks.x = element_blank(),  # Remove y-axis ticks
#         axis.line = element_blank(),
#         plot.margin=unit(c(-1,-1,0.5,-1.2), "cm"),
#         panel.border = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())
# 
# #sp.meta.shap.imp.bar2.plot
# 
# sp.meta.shap.beeswarm2.imp=sv_importance(final_meta_shap2$Spatial, 
#                                          kind = "beeswarm",bee_width = 0.5)#,
# 
# #saveRDS(sp.meta.shap.imp,"sp.meta.shap.imp.rds")
# sp.meta.shap.imp.beeswarm2.plot=sp.meta.shap.beeswarm2.imp+
#   ggtitle("Spatial")+
#   xlab("SHAP Value")+
#   # xlim(-0.3,0.7)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 19),
#         text = element_text(size = 19,family="serif"),
#         axis.text.x = element_text(size = 15),
#         axis.text.y = element_text(size = 19),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #        plot.margin=unit(c(1,-1,0.5,0), "cm"),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red")
# 
# sp.meta.shap.imp.beeswarm2.plot
# figure1=sp.meta.shap.imp.beeswarm2.plot+
#   theme(legend.position="none")
# 
# figure2=sp.meta.shap.imp.bar2.plot
# 
# sp.meta.combine.var.imp.2=plot_grid(figure1, figure2, align = "h", ncol = 2)
# 
# # Get the legend from one of the plots
# legend <- get_legend(sp.meta.shap.imp.beeswarm2.plot)
# 
# # Add the legend to the combined plot
# combined_plot_with_legend.sp2 <- plot_grid(sp.meta.combine.var.imp.2, 
#                                            legend, ncol = 1, rel_heights = c(1, 0.2))
# 
# combined_plot_with_legend.sp2
# 
# ggsave("sp_shap_varimp2.png", width = 14, height = 7)
# 
# 
# ###----combine shapely summary plot for spatial with and without species----####
# spatial.combine.shapsummary.plot <- ggarrange(combined_plot_with_legend.sp,
#                                               ggarrange(combined_plot_with_legend.sp2,
#                                                         align = "v",widths = c(0.5,1)),
#                                               nrow = 2,
#                                               labels = c("A)","B)"),
#                                               hjust = -5.9,#-3.1,
#                                               vjust = 1.2,
#                                               font.label = list(size = 19, color = "black", face = "bold",
#                                                                 family = "serif"))
# spatial.combine.shapsummary.plot
# ggsave("spatial.combine.shapsummary.meta.png", width = 14, height = 10)
# 
# 
# 
# ############################################################
# # Shap Dependency plot (without species)
# ############################################################
# # & geom_smooth(method = "gam", se = FALSE,stat = "smooth",
# #               position = "identity",color="red")
# top.meta.feats2=c("Data_duration (days)",
#                   "Data_collection (focal_sampling)","Interaction_type (physical_contact)",
#                   "Data_collection (video)","Interaction_type (spatial_proximity)",
#                   "Captive (no)")
# ###----shapely dependency plot----
# metashap.dependency.plot2=sv_dependence(final_meta_shap2$Spatial, v =top.meta.feats2,color_var = NULL ) &
#   theme_gray(base_size = 9)&
#   theme_bw()+
#   theme(plot.title = element_text(size = 16),
#         text = element_text(size = 16,family="serif"),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank())&
#   scale_color_gradient(low = "blue", high = "red")
# 
# metashap.dependency.plot2
# ggsave("metashap.dependency.plot2.png", width = 14, height = 7) 
# 
# 
# ###----2D dependency plot
# metashap.2Ddependency.plot2=sv_dependence2D(final_meta_shap2$Spatial, 
#                                             x ="Data_duration (days)", 
#                                             y = c("Interaction_type (physical_contact)",
#                                                   "Data_collection (video)",
#                                                   "Interaction_type (spatial_proximity)",
#                                                   "Data_collection (focal_sampling)"),
#                                             alpha = 0.5)& theme_bw()+
#   theme(plot.title = element_text(size = 16),
#         text = element_text(size = 16,family="serif"),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) & 
#   scale_color_gradient(low = "blue", high = "red")
# 
# metashap.2Ddependency.plot2
# ggsave("metashap.2Ddependency.plot1.png", width = 8, height = 7)
# 
# 
# 
# 
# ###----combine shapely summary plot for spatial----####
# spatial.combine.shapdepend.plot2 <- ggarrange(metashap.dependency.plot2,
#                                               ggarrange(metashap.2Ddependency.plot2,
#                                                         align = "v",widths = c(0.5,1)),
#                                               nrow = 2,
#                                               labels = c("A)","B)"),
#                                               hjust = -2.6,#-3.1,
#                                               vjust = 1.1,
#                                               font.label = list(size = 12, color = "black", face = "bold",
#                                                                 family = "serif"))
# spatial.combine.shapdepend.plot2
# ggsave("spatial.combine.shapdepend.plot2.png", width = 13, height = 14)
# 
# ###############################################################################
# # shapely  interactions without species
# ###############################################################################
# 
# ##----Interaction
# meta_shap_interactn2<- shapviz(extract_fit_engine(xgb.final.metadata.fit2),
#                                X_pred = meta_data_bake2, 
#                                X = last_metadata_df2,
#                                interactions = TRUE)
# 
# saveRDS(meta_shap_interactn2,"meta_shap_interactn2.rds")
# meta_shap_interactn2=readRDS("meta_shap_interactn2.rds")
# 
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_1"] <- "Erdös-Rényi"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_2"] <- "Stochastic-Block-Model"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_3"] <- "Scale-Free"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_4"] <- "Spatial"
# names(meta_shap_interactn2)[names(meta_shap_interactn2) == "Class_5"] <- "Small-World"
# 
# 
# colnames(meta_shap_interactn2$"Erdös-Rényi"$X)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Erdös-Rényi"$S)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Erdös-Rényi"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn2$"Erdös-Rényi"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Erdös-Rényi"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Stochastic-Block-Model"$X)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Stochastic-Block-Model"$S)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Stochastic-Block-Model"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn2$"Stochastic-Block-Model"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Stochastic-Block-Model"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Scale-Free"$X)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Scale-Free"$S)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Scale-Free"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn2$"Scale-Free"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Scale-Free"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Spatial"$X)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Spatial"$S)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Spatial"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn2$"Spatial"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Spatial"$S_inter)[[2]])
# 
# colnames(meta_shap_interactn2$"Small-World"$X)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Small-World"$S)=meta_shap_column_names
# colnames(meta_shap_interactn2$"Small-World"$S_inter)=meta_shap_column_names
# dimnames(meta_shap_interactn2$"Small-World"$S_inter)[[3]]=
#   c(dimnames(meta_shap_interactn2$"Small-World"$S_inter)[[2]])
# 
# 
# saveRDS(meta_shap_interactn2,"final_meta_shap.interactn2.rds")
# 
# final_meta_shap.interactn2=readRDS("final_meta_shap.interactn2.rds")
# 
# ##----Interactions----
# ##----Interactions----
# meta.shap.interactn2=sv_dependence(final_meta_shap.interactn2$Spatial, 
#                                    "Data_duration (days)",
#                                    color_var = "Interaction_type (physical_contact)", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# saveRDS(meta.shap.interactn2,"meta.shap.interactn2.rds")
# 
# meta.shap.interactn2=readRDS("meta.shap.interactn2.rds")
# 
# meta.shap.interactn2.df=meta.shap.interactn2$data
# 
# colnames(meta.shap.interactn2.df)=c("shap","Data_duration","Interaction_type")
# 
# meta.shap.interactn2.plot=
#   ggplot(meta.shap.interactn2.df, aes(x = Data_duration, y = shap,
#                                       color=Interaction_type)) +
#   geom_point(stat = "identity") +
#   ggtitle("Spatial")+
#   xlab("Species")+
#   ylab("SHAP interaction values for \n Data_duration (days) and Interaction_type (physical_contact)")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(1, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# meta.shap.interactn2.plot=meta.shap.interactn2.plot+plot_layout(widths = 1.,heights = 1.)
# meta.shap.interactn2.plot
# 
# 
# 
# meta.shap.interactn2.sp.plot=
#   sv_interaction(final_meta_shap.interactn2$Spatial,
#                  max_display = 5 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 24),
#         text = element_text(size = 24,family="serif"),
#         axis.text.x = element_text(size = 24),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# meta.shap.interactn2.sp.plot
# 
# # meta.fig.interactn.<- plot_grid(meta.shap.interactn2.plot,
# #                                 meta.shap.interactn2.sp.plot,nrow = )
# ggsave("meta.fig.interactn2.png", width = 30, height = 22)
# 
# ####----Interaction for multiple plots at once
# sample.meta.feats21=c("Time_resolution_secs (focal_follow)",
#                       "Data_collection (focal_sampling)",
#                       "Class (mammalia)",
#                       "Data_duration (days)")
# #"interaction_type_Grooming"
# 
# sv_dependence(final_meta_shap.interactn2$Spatial,
#               v = "Class (mammalia)", 
#               color_var = sample.meta.feats21, interactions = TRUE) & theme_bw()+
#   theme(plot.title = element_text(size = 26),
#         text = element_text(size = 26,family="serif"),
#         axis.text.x = element_text(size = 26),
#         axis.text.y = element_text(size = 26),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank()) #& scale_color_continuous()
# 
# ggsave("shap.spatial.interaction2.png", width = 30, height=24)





############################################################################
# SHAPELY INTERACTION
############################################################################
# nf.shap.interactn.er=sv_dependence(final_interactn_shap$`Erdös-Rényi`, 
#                                    "Normalized_Fiedler_value",
#                                    color_var = "auto", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# saveRDS(nf.shap.interactn.er,"nf.shap.interactn.er.rds")
# 
# nf.shap.interactn.er=readRDS("nf.shap.interactn.er.rds")
# 
# nf.shap.interactn.er.df=nf.shap.interactn.er$data
# 
# nf.shap.interactn.er.plot=
#   ggplot(nf.shap.interactn.er.df, aes(x = Normalized_Fiedler_value, y = shap,
#                                       color=Modularity)) +
#   geom_point(stat = "identity") +
#   ggtitle("Erdös-Rényi")+
#   xlab("Normalized_fiedler_value")+
#   ylab("SHAP interaction values for \n Normalized_Fiedler_value and Modularity")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# nf.shap.interactn.er.plot=nf.shap.interactn.er.plot+plot_layout(widths = 1.2,heights = 1.2)
# nf.shap.interactn.er.plot
# 
# 
# ##[1] we construct the interaction plot for the most important feature (Modularity)
# ## Stochastic-Block-Model
# mod.shap.interactn.sbm=
#   sv_dependence(final_interactn_shap$`Stochastic-Block-Model`,
#                 "Modularity",
#                 color_var = "auto", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# 
# 
# saveRDS(mod.shap.interactn.sbm,"mod.shap.interactn.sbm.rds")
# 
# mod.shap.interactn.sbm=readRDS("mod.shap.interactn.sbm.rds")
# 
# mod.shap.interactn.sbm.df=mod.shap.interactn.sbm$data
# 
# mod.shap.interactn.sbm.plot=
#   ggplot(mod.shap.interactn.sbm.df, aes(x = Modularity, y = shap,
#                                         color=Mean_path_length)) +
#   geom_point(stat = "identity") +
#   ggtitle("Stochastic-Block-Model")+
#   xlab("Modularity")+
#   ylab("SHAP interaction values for \n Modularity and Mean_path_length")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(0.5, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# mod.shap.interactn.sbm.plot=mod.shap.interactn.sbm.plot+plot_layout(widths = 1.2,heights = 1.2)
# mod.shap.interactn.sbm.plot
# 
# 
# figure.interactn.1 <- ggarrange(nf.shap.interactn.er.plot,
#                                 mod.shap.interactn.sbm.plot,
#                                 labels = c("A1)", "B1)"),
#                                 ncol = 2, nrow = 1,
#                                 hjust = -2.0,#-3.1,
#                                 vjust = 1.1,
#                                 align = "h",widths = c(0.5,0.5),
#                                 heights = c(0.5,0.5))
# 
# figure.interactn.1
# 
# 
# #sv_interaction(final_interactn_shap$`Erdös-Rényi`, kind = "no")
# nf.shap.interactn.er.plot2=
#   sv_interaction(final_interactn_shap$`Erdös-Rényi` ,
#                  max_display = 2, size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.4,
#                  bee_adjust = 0.3)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.key.width = unit(2, "cm"),
#         legend.position="none")+
#   #panel.border = element_blank(),
#   #        legend.position='bottom', legend.direction='horizontal',
#   #       legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# nf.shap.interactn.er.plot2
# 
# 
# 
# mod.shap.interactn.sbm.plot2=
#   sv_interaction(final_interactn_shap$`Stochastic-Block-Model`,
#                  max_display = 2, size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.2,
#                  bee_adjust = 0.2)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         #axis.text.y = element_text(size = 18),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.key.width = unit(2, "cm"),
#         #panel.border = element_blank(),
#         legend.position='bottom')+
#   #legend.direction='horizontal',
#   #legend.key.width = unit(1, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# mod.shap.interactn.sbm.plot2
# 
# 
# figure.interactn.2 <- ggarrange(nf.shap.interactn.er.plot2, 
#                                 mod.shap.interactn.sbm.plot2,
#                                 labels = c("A2)", "B2)"),
#                                 ncol = 2, nrow = 1,
#                                 hjust = -0.1,
#                                 vjust = 0.9,
#                                 align = "h",widths = c(0.5,0.5),
#                                 heights = c(0.5,0.5),
#                                 common.legend = TRUE, legend = "bottom")
# 
# figure.interactn.2+
#   theme(legend.key.width = unit(2, "cm"))
# 
# 
# interactionplo1=plot_grid(figure.interactn.1,figure.interactn.2,
#                           nrow=2)
# ggsave("interactionplot1.png", width = 14, height = 15)
# 
# 
# ###----Transitivity interaction plot----###
# trans.shap.interactn.sp=sv_dependence(final_interactn_shap$Spatial, 
#                                       "Transitivity",
#                                       color_var = "Mean_eccentricity", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# saveRDS(trans.shap.interactn.sp,"trans.shap.interactn.sp.rds")
# 
# trans.shap.interactn.sp=readRDS("trans.shap.interactn.sp.rds")
# 
# trans.shap.interactn.sp.df=trans.shap.interactn.sp$data
# 
# trans.shap.interactn.sp.plot=
#   ggplot(trans.shap.interactn.sp.df, aes(x = Transitivity, y = shap,
#                                          color=Mean_eccentricity)) +
#   geom_point(stat = "identity") +
#   ggtitle("Spatial")+
#   xlab("Transitivity")+
#   ylab("SHAP interaction values for \n Transitivity and Mean_eccentricity")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(1, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# trans.shap.interactn.sp.plot=trans.shap.interactn.sp.plot+plot_layout(widths = 1.2,heights = 1.2)
# trans.shap.interactn.sp.plot
# 
# 
# 
# deg_centr.shap.interactn.sf=sv_dependence(final_interactn_shap$`Scale-Free`, 
#                                           "Degree_centrality",
#                                           color_var = "auto", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# saveRDS(deg_centr.shap.interactn.sf,"deg_centr.shap.interactn.sf.rds")
# 
# deg_centr.shap.interactn.sf=readRDS("deg_centr.shap.interactn.sf.rds")
# 
# deg_centr.shap.interactn.sf.df=deg_centr.shap.interactn.sf$data
# 
# deg_centr.shap.interactn.sf.plot=
#   ggplot(deg_centr.shap.interactn.sf.df, aes(x = Degree_centrality, y = shap,
#                                              color=Eigen_centrality)) +
#   geom_point(stat = "identity") +
#   ggtitle("Scale-Free")+
#   xlab("Degree_centrality")+
#   ylab("SHAP intsfaction values for \n Degree_centrality and Eigen_centrality")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_text(size = 18),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(1, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# deg_centr.shap.interactn.sf.plot=deg_centr.shap.interactn.sf.plot+plot_layout(widths = 1.2,heights = 1.2)
# deg_centr.shap.interactn.sf.plot
# 
# figure.interactn.3 <- ggarrange(trans.shap.interactn.sp.plot,
#                                 deg_centr.shap.interactn.sf.plot,
#                                 labels = c("A3)", "B3)"),
#                                 ncol = 2, nrow = 1,
#                                 hjust = -2.0,#-3.1,
#                                 vjust = 1.1,
#                                 align = "h",widths = c(0.5,0.5),
#                                 heights = c(0.5,0.5))
# 
# figure.interactn.3
# 
# 
# 
# trans.shap.interactn.sp.plot2=
#   sv_interaction(final_interactn_shap$Spatial,
#                  max_display = 2, size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.4,
#                  bee_adjust = 0.3)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.key.width = unit(2, "cm"),
#         #panel.border = element_blank(),
#         legend.position='none')+
#   #legend.direction='horizontal',
#   #legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# trans.shap.interactn.sp.plot2
# 
# 
# 
# deg_centr.shap.interactn.sf.plot2=
#   sv_interaction(final_interactn_shap$`Scale-Free`,
#                  max_display = 2, size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.2,
#                  bee_adjust = 0.2)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 18),
#         text = element_text(size = 18,family="serif"),
#         axis.text.x = element_text(size = 18),
#         #axis.text.y = element_text(size = 18),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.key.width = unit(2, "cm"),
#         #panel.border = element_blank(),
#         legend.position='none')+
#   # legend.direction='horizontal',
#   # legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red")
# 
# 
# figure.interactn.4 <- ggarrange(trans.shap.interactn.sp.plot2, 
#                                 deg_centr.shap.interactn.sf.plot2,
#                                 labels = c("A4)", "B4)"),
#                                 ncol = 2, nrow = 1,
#                                 hjust = -0.1,
#                                 vjust = 0.9,
#                                 align = "h",widths = c(0.5,0.5),
#                                 heights = c(0.5,0.5),
#                                 common.legend = TRUE, legend = "bottom")
# 
# figure.interactn.4+
#   theme(legend.key.width = unit(2, "cm"))
# 
# figure.interactn.4 
# 
# interactionplot2=ggarrange(figure.interactn.3,figure.interactn.4,
#                            nrow=2)
# ggsave("interactionplot2.png", width = 14, height = 15)
# 
# 
# 
# ###----Spectral_radius interaction plot----###
# spec_radius.shap.interactn.sw=sv_dependence(final_interactn_shap$`Small-World`, 
#                                             "Spectral_radius",
#                                             color_var = "Degree_centrality", interactions = TRUE)# & ylim(-6000, 13000)
# 
# 
# saveRDS(spec_radius.shap.interactn.sw,"spec_radius.shap.interactn.sw.rds")
# 
# spec_radius.shap.interactn.sw=readRDS("spec_radius.shap.interactn.sw.rds")
# 
# spec_radius.shap.interactn.sw.df=spec_radius.shap.interactn.sw$data
# 
# spec_radius.shap.interactn.sw.plot=
#   ggplot(spec_radius.shap.interactn.sw.df, aes(x = Spectral_radius, y = shap,
#                                                color=Degree_centrality)) +
#   geom_point(stat = "identity") +
#   ggtitle("Small-World")+
#   xlab("Spectral_radius")+
#   ylab("SHAP interaction values \n for Spectral_radius \n and  Degree_centrality")+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(1, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# spec_radius.shap.interactn.sw.plot=spec_radius.shap.interactn.sw.plot+plot_layout(widths = 1.2,heights = 1.2)
# spec_radius.shap.interactn.sw.plot
# 
# 
# 
# spec_radius.shap.interactn.sw.plot2=
#   sv_interaction(final_interactn_shap$`Small-World`,
#                  max_display = 2, size = 2,
#                  alpha = 0.2,
#                  bee_width = 0.4,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 12),
#         text = element_text(size = 12,family="serif"),
#         axis.text.x = element_text(size = 12),
#         #axis.text.y = element_text(size = 18),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position='bottom', legend.direction='horizontal',
#         legend.key.width = unit(0.8, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# spec_radius.shap.interactn.sw.plot2
# 
# 
# figure.interactn.5 <- plot_grid(spec_radius.shap.interactn.sw.plot, 
#                                 spec_radius.shap.interactn.sw.plot2,
#                                 ncol = 2)
# 
# 
# figure.interactn.5 
# ggsave("interactionplot3.png", width = 8, height = 4)
# 
# ###########----multiple interaction plots----######
# 
# 
# 
# trans.top5.shap.interactn.plot=
#   sv_interaction(final_interactn_shap$Spatial,
#                  max_display = 7 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 24),
#         text = element_text(size = 24,family="serif"),
#         axis.text.x = element_text(size = 24),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(3, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# trans.top5.shap.interactn.plot
# ggsave("trans.top5.shap.interactn.plot.png", width = 30, height = 26)
# 
# 
# 
# mod.top5.shap.interactn.plot=
#   sv_interaction(final_interactn_shap$`Stochastic-Block-Model`,
#                  max_display = 7 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 24),
#         text = element_text(size = 24,family="serif"),
#         axis.text.x = element_text(size = 24),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(3, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# mod.top5.shap.interactn.plot
# ggsave("mod.top5.shap.interactn.plot.png", width = 30, height = 26)
# 
# 
# 
# nf.top5.shap.interactn.plot=
#   sv_interaction(final_interactn_shap$`Erdös-Rényi`,
#                  max_display = 7 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 24),
#         text = element_text(size = 24,family="serif"),
#         axis.text.x = element_text(size = 24),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(3, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# nf.top5.shap.interactn.plot
# ggsave("nf.top5.shap.interactn.plot.png", width = 30, height = 26)
# 
# 
# 
# deg_centr.top5.shap.interactn.plot=
#   sv_interaction(final_interactn_shap$`Scale-Free`,
#                  max_display = 7 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 24),
#         text = element_text(size = 24,family="serif"),
#         axis.text.x = element_text(size = 24),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(3, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# deg_centr.top5.shap.interactn.plot
# ggsave("deg_centr.top5.shap.interactn.plot.png", width = 30, height = 26)
# 
# 
# spec.top5.shap.interactn.plot=
#   sv_interaction(final_interactn_shap$`Small-World`,
#                  max_display = 7 , size = 2,
#                  alpha = 0.3,
#                  bee_width = 0.6,
#                  bee_adjust = 0.4)+
#   theme_bw()+
#   theme(plot.title = element_text(size = 28),
#         text = element_text(size = 28,family="serif"),
#         axis.text.x = element_text(size = 28),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         #panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         #panel.border = element_blank(),
#         legend.position = "bottom",
#         legend.direction='horizontal',
#         legend.key.width = unit(3, "cm"))+
#   scale_color_gradient(low = "blue", high = "red") 
# 
# spec.top5.shap.interactn.plot
# ggsave("spec.top5.shap.interactn.plot.png", width =20 , height = 14)



