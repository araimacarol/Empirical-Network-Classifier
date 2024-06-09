ui<- shinyUI(
  fluidPage(
    titlePanel("Empirical Network Classifier"),
    sidebarLayout(
      sidebarPanel(
        fileInput("folder", "Upload Folder",
                  multiple = TRUE, accept = c(".edges, .edgelist, .csv")),
        actionButton("classify", "Classify"),
        uiOutput("h_dropdown"),
        uiOutput("feature_importance_dropdown"),
        uiOutput("shap_ui")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Predictions",
                   tableOutput("predictions"),
                   conditionalPanel(
                     condition = "input.classify > 0",
                     fluidRow(
                       column(12,
                              HTML("This is a data frame of estimated features for the empirical graphs and their 
                                   predicted classes (network generative model).")
                       )
                     ),
                     downloadButton("downloadPredictionsCSV", "Download")
                   ),
                   # Show combine_res_df below the predictions table
                   tableOutput("combineResTable"),
                   fluidRow(
                     column(12,
                            HTML("This is a data frame of the count of the
                              predicted class and their corresponding  predicted probabilities.")
                     )
                   ),
          ),
          tabPanel("Network Information", htmlOutput("networkInfo")), # Use htmlOutput for rendering HTML

          tabPanel("H-Statistics",
                   textOutput("h_description"),
                   verbatimTextOutput("h_output"),
                   plotOutput("hstatPlot"),  # Display both text and plot outputs in the main panel
                   
                   conditionalPanel(
                     condition = "input.h_option == 'Feature importance' && input.feature_importance_plot_type == 'Permutation Importance'",
                     fluidRow(
                       column(12,
                              HTML("Permutation Importance plot for a set of features or feature groups [1].
                                   The permutation importance is calculated for all features except the response [1,2].
                                   It is the increase in the average loss when shuffling the corresponding feature values before calculating predictions. 
                                   The final results are averaged. In most of the cases, importance values should be derived from an independent test data set [1,2].\n<br><br>",

                                   "References <br><br>",
                                   "[1] Mayer, Michael, Steven C. Bourassa, Martin Hoesli, and Donato Scognamiglio.
                                   Machine Learning Applications to Land and Structure Valuation. Journal of Risk and Financial Management 15, no. 5 (2022): 193.<br><br>",
                                   "[2] Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful:
                                   Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance.<br><br>")
                       )
                     ),
                     downloadButton("download_permutation_importance_plot", "Export")
                   ),

                   conditionalPanel(
                     condition = "input.h_option == 'Feature importance' && input.feature_importance_plot_type == 'Partial Dependence Importance'",
                     fluidRow(
                       column(12,
                              HTML("Partial Dependence Experimental Variable Importance plot is based on
        partial dependence function and closely related to the work by Greenwell et al. [2].
        Greenwell et al. [2], proposed the standard deviation of the partial 
        dependence function as a measure of variable importance (for continuous predictors).
        <br>
        This plot describes not only main effect strength but also interaction effects,
        and is very closely related to Hⱼ² (overall interaction strength) [1]. 
        This quantity is an experimental measure, therefore care should be taken interpreting it [1]. \n<br><br>",

                                   "References <br><br>",
                                   "[1] Mayer, Michael, Steven C. Bourassa, Martin Hoesli, and Donato Scognamiglio. Machine Learning Applications to Land and Structure Valuation. Journal of Risk and Financial Management 15, no. 5 (2022): 193.<br><br>",
                                   "[2] Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy.")
                       )
                     ),
                     downloadButton("download_partial_dependence_importance_plot", "Export")
                   ),

                   conditionalPanel(
                     condition = "input.h_option == 'Overall interaction strength (Hⱼ²)'",
                     fluidRow(
                       column(12,
                              HTML("The initial step to understand interaction among features is to determine interaction exist
                              and to quantify the degree and strength of their interaction [2]. 
                              Friedman and Popescu's H-statistics is used to quantify the strength and degree of these interactions [1,2]. <br><br> 
                              
                            The overall interaction effect plot developed by Mayer et al. [2] based on the work by Friedman and Popescu [1] shows the overall strength (Hⱼ²) of features in predicting a models final output.
                            The plot shows the proportion of prediction variability explained by interactions on feature j [2].
                            We considered the unnormalized statistic for this measure [2].\n<br><br>",

                                   "References <br><br>",
                                   "[1] Friedman, Jerome H., and Bogdan E. Popescu. Predictive Learning via Rule Ensembles. The Annals of Applied Statistics 2, no. 3 (2008): 916-54.<br><br>",
                                   "[2] Mayer, Michael, Steven C. Bourassa, Martin Hoesli, 
                                   and Donato Scognamiglio. Machine Learning Applications to 
                                   Land and Structure Valuation. Journal of Risk and Financial Management 15, no. 5 (2022): 193.<br><br>"
                     
                                   )
                       )
                     ),
                     downloadButton("downloadOverallHPlot", "Export")
                   ),
                   conditionalPanel(
                     condition = "input.h_option == 'Pairwise interaction strength (Hⱼₖ²)'",
                     fluidRow(
                       column(12,
                              HTML("The initial step to understand interaction among features is to determine interaction exist
                              and to quantify the degree and strength of their interaction [2]. 
                              Friedman and Popescu's H-statistics is used to quantify the strength and degree of these interactions [1,2]. <br><br> 
                              
                              Pairwise Interaction plot developed by Mayer et al. [2] based on the work by Friedman and Popescu [1] shows the proportion of the joint effect variability
                              from the combined effect of two features (j,and k). 
                              Pairwise interactions are calculated only for features with strong overall interactions [2].
                              Here, we used unnormalized statistic to study pairwise interactions between pairs of features strongest
                              in absolute numbers [2].\n<br><br>",

                                   "References <br><br>",
                                   "[1] Friedman, Jerome H., and Bogdan E. Popescu. Predictive Learning via Rule Ensembles. The Annals of Applied Statistics 2, no. 3 (2008): 916-54.<br><br>",
                                   "[2] Mayer, Michael, Steven C. Bourassa, Martin Hoesli, 
                                   and Donato Scognamiglio. Machine Learning Applications to 
                                   Land and Structure Valuation. Journal of Risk and Financial Management 15, no. 5 (2022): 193.<br><br>"
                                   
                                   )
                       )
                     ),
                     downloadButton("downloadPairwiseHPlot", "Export")
                   ),
                   conditionalPanel(
                     condition = "input.h_option == 'Three-way interaction strength (Hⱼₖₗ²)'",
                     fluidRow(
                       column(12,
                              HTML("The initial step to understand interaction among features is to determine interaction exist
                                   and to quantify the degree and strength of their interaction [2]. 
                                   Friedman and Popescu's H-statistics is used to quantify the strength and degree of these interactions [1,2]. <br><br> 
                              
                              The Three-way interaction plot developed by Mayer et al. [2] based on the work by Friedman and Popescu [1]
                              shows the joint effect variability of three features from their three-way interactions.
                              Here, we used unnormalized statistic to study three way joint interaction effect between features j,k, and l [2].\n <br><br>",

                                   "References <br><br>",
                                   "[1] Friedman, Jerome H., and Bogdan E. Popescu. Predictive Learning via Rule Ensembles. The Annals of Applied Statistics 2, no. 3 (2008): 916-54.<br><br>",
                                   "[2] Mayer, Michael, Steven C. Bourassa, Martin Hoesli, 
                                   and Donato Scognamiglio. Machine Learning Applications to 
                                   Land and Structure Valuation. Journal of Risk and Financial Management 15, no. 5 (2022): 193.<br><br>"
                                   
                              )
                       )
                     ),
                     downloadButton("downloadThreeWayHPlot", "Export")
                   ),
                   conditionalPanel(
                     condition = "input.h_option == 'Feature importance'", # Add condition for new feature importance dropdown
                     uiOutput("feature_importance_plot_type") # Add placeholder for feature importance plot type dropdown
                   )
          ),
          tabPanel("SHAP Plots",
                   plotOutput("shapPlot"),
                   conditionalPanel(
                     condition = "input.shap_plot == 'Force'",
                     fluidRow(
                       column(12,
                              HTML("
                              Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
                              feature importance measure [1,2].",
                              "SHAP, whose mathematical foundation and interpretation of feature importance 
                              for model output closely align with the way humans perceive things [1,2].",
                              "SHAP is based on game theory principles, and assigns distinct importance weights to individual
                              features according to their contributions to model output and their interactions with other features [1,2,3]. <br><br>",
                              
                              "The SHAP force plot is simiar to the waterfall plot, however
                                   whiles the waterfall plot focuses on the sequential contribution of features,
                                   the force plot provides a detailed breakdown of all feature contributions for a single prediction [1,2]. <br><br>",

                                   "References <br><br>",
                                   "[1] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
information processing systems 30 (2017).<br><br>",
                                   "[2] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347. doi: (https : / / doi . org / 10 . 1109 / SIBGRAPI51738 . 2020 .
00053).<br><br>",
"[3] E. Štrumbelj and I. Kononenko. “Explaining prediction models and individual predictions with feature
contributions”. Knowledge and information systems 41 (2014), pp. 647–665."                              
                              )
                       )
                     ),
                     downloadButton("downloadForcePlot", "Export"),
                   ),
                   conditionalPanel(
                     condition = "input.shap_plot == 'Waterfall'",
                     fluidRow(
                       column(12,
                              HTML("Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
                                   feature importance measure [1,2].",
                              "SHAP, whose mathematical foundation and interpretation of feature importance 
                                   for model output closely align with the way humans perceive things [1,2].",
                              "SHAP is based on game theory principles, and assigns distinct importance weights to individual
                                   features according to their contributions to model output and their interactions with other features [1,2,4].<br><br>",
                              
                              "The SHAP Waterfall plot serves as a local feature importance visualization, displaying the contribution
of each feature to a model’s final output for a specific data instance [1,2,3].
This plot helps visualize how individual features influence a particular prediction, aiding in understanding the model’s decision-making process [1,2].<br><br>",

"That is the direction and magnitude of the contribution of each feature to the predictions.
It starts from a baseline value and shows how each feature either increases or decreases the 
prediction relative to the baseline [1,2,3]. \n <br><br>",

"The x-axis represents SHAP values’ magnitude, indicating the expected predicted values of the model’s
output. The y-axis, represented by horizontal bars, corresponds to the features and their contributions to
shifting the model’s output from a reference (base) value [1,2]. 

Bar length signifies the SHAP value’s magni-
tude for a specific feature, and bar color indicates whether the feature’s effect on the prediction pushes
it toward a higher (positive, red bars pointing right) or lower (negative, blue bars pointing left) model
output [1,2].\n <br><br>",

"References <br><br>",
"[1] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
information processing systems 30 (2017)<br><br>",
"[2] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347. doi: (https : / / doi . org / 10 . 1109 / SIBGRAPI51738 . 2020 .
00053).<br><br>",
"[3] X. Zhou et al. “An interpretable model for the susceptibility of rainfall-induced shallow landslides based
on SHAP and XGBoost”. Geocarto International 37.26 (2022), pp. 13419–13450.<br><br>",
"[4] E. Štrumbelj and I. Kononenko. “Explaining prediction models and individual predictions with feature
contributions”. Knowledge and information systems 41 (2014), pp. 647–665." 

                              )
                       )
                     ),
                     downloadButton("downloadWaterfallPlot", "Export"),
                  ),
                   conditionalPanel(
                     condition = "input.shap_plot == 'Importance'",
                     fluidRow(
                       column(12,
                              HTML("Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
                                   feature importance measure [1,2].",
                              "SHAP, whose mathematical foundation and interpretation of feature importance 
                                   for model output closely align with the way humans perceive things [1,2].",
                              "SHAP is based on game theory principles, and assigns distinct importance weights to individual
                                   features according to their contributions to model output and their interactions with other features [1,2,4].<br><br>",
                              
                              "The SHAP global feature importance can be presented as a feature summary plot, or a variable importance plot,
                              or a combination of both [1,2,3]. These plots illustrate the direction and magnitude of
each feature’s impact on a model’s final predictions [1,2,3]. \n <br><br>",

"They can be visualized as bars, similar to variable
importance bar charts, or as dots representing feature summaries [1,2,3].
Dots may be colored to indicate the
feature’s value (high or low), while bar length corresponds to each feature’s strength. These
plots identify the most influential model features for both individual and global predictions, offering insights
into the model’s behavior across the datase [1,2,3]. \n <br><br>",

"References <br><br>",
"[1] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
information processing systems 30 (2017)<br><br>",
"[2] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347. doi: (https : / / doi . org / 10 . 1109 / SIBGRAPI51738 . 2020 .
00053).<br><br>",
"[3] X. Zhou et al. “An interpretable model for the susceptibility of rainfall-induced shallow landslides based
on SHAP and XGBoost”. Geocarto International 37.26 (2022), pp. 13419–13450.<br><br>",
"[4] E. Štrumbelj and I. Kononenko. “Explaining prediction models and individual predictions with feature
contributions”. Knowledge and information systems 41 (2014), pp. 647–665." 

                              )
                       )
                     ),
                     downloadButton("downloadImportancePlot", "Export"),
                   ),
                   conditionalPanel(
                     condition = "input.shap_plot == 'Dependence'",
                     fluidRow(
                       column(12,
                              HTML("Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
                                   feature importance measure [1,2].",
                                   "SHAP, whose mathematical foundation and interpretation of feature importance 
                                   for model output closely align with the way humans perceive things [1,2].",
                                   "SHAP is based on game theory principles, and assigns distinct importance weights to individual
                                   features according to their contributions to model output and their interactions with other features [1,2,4].<br><br>",
                              
                              "SHAP dependency plot shows sthe relationship between a feature and its
                              impact on a models final output/prediction in a scatter plot [1,2,3]. \n <br><br>",
                                   
                                   "References <br><br>",
                                   "[1] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
information processing systems 30 (2017)<br><br>",
                                   "[2] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347. doi: (https : / / doi . org / 10 . 1109 / SIBGRAPI51738 . 2020 .
00053).<br><br>",
                                   "[3] X. Zhou et al. “An interpretable model for the susceptibility of rainfall-induced shallow landslides based
on SHAP and XGBoost”. Geocarto International 37.26 (2022), pp. 13419–13450.<br><br>",
                                   "[4] E. Štrumbelj and I. Kononenko. “Explaining prediction models and individual predictions with feature
contributions”. Knowledge and information systems 41 (2014), pp. 647–665." 
                                   
                              )
                       )
                     ),
                     downloadButton("downloadDependencePlot", "Export"),
                   ),
                   conditionalPanel(
                     condition = "input.shap_plot == '2D Dependence'",
                     fluidRow(
                       column(12,
                              HTML("Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
                                   feature importance measure [1,2].",
                                   "SHAP, whose mathematical foundation and interpretation of feature importance 
                                   for model output closely align with the way humans perceive things [1,2].",
                                   "SHAP is based on game theory principles, and assigns distinct importance weights to individual
                                   features according to their contributions to model output and their interactions with other features [1,2,4].<br><br>",
                              
                              "The SHAP 2D dependence plot visualizes the dependence between two features and the model's output. 
                              It displays how the prediction changes as one feature varies, while holding the values of other features constant.
This plot is useful for understanding the interaction between two specific features and how their joint values influence the model's prediction [1,2,3].\n <br><br>",
                                   
                                   "References <br><br>",
                                   "[1] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
information processing systems 30 (2017)<br><br>",
                                   "[2] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347. doi: (https : / / doi . org / 10 . 1109 / SIBGRAPI51738 . 2020 .
00053).<br><br>",
                                   "[3] X. Zhou et al. “An interpretable model for the susceptibility of rainfall-induced shallow landslides based
on SHAP and XGBoost”. Geocarto International 37.26 (2022), pp. 13419–13450.<br><br>",
                              "[4] E. Štrumbelj and I. Kononenko. “Explaining prediction models and individual predictions with feature
contributions”. Knowledge and information systems 41 (2014), pp. 647–665." 
                              )
                       )
                     ),
                     downloadButton("download2DDependencePlot", "Export"),
                   ),
                   conditionalPanel(
                     condition = "input.shap_plot == 'Interaction'",
                     fluidRow(
                       column(12,
                              HTML("Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
                                   feature importance measure [1,2].",
                                   "SHAP, whose mathematical foundation and interpretation of feature importance 
                                   for model output closely align with the way humans perceive things [1,2].",
                                   "SHAP is based on game theory principles, and assigns distinct importance weights to individual
                                   features according to their contributions to model output and their interactions with other features.<br><br>",
                              
                              "The SHAP interaction plot aides insights into the interaction effects between pairs of features on the model's predictions.
It visualizes the combined effect of two or more features on the model's output, capturing non-linear interactions [1,2,3].
This plot helps in understanding how the relationship between one feature and the target variable varies depending on the value of another feature [1,2,3]. \n <br><br>",
   
  "In summary, while both SHAP 2D dependence and interaction plots are used to understand the relationships between features and model predictions, 
  the 2D dependence plot focuses on the interaction between two specific features, while the interaction plot provides a broader view
  of how multiple features interact with each other to 
  influence predictions.\n <br><br>",                                                                 
                                   "References <br><br>",
                                   "[1] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
information processing systems 30 (2017)<br><br>",
                                   "[2] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347.<br><br>",
                                   "[3] X. Zhou et al. “An interpretable model for the susceptibility of rainfall-induced shallow landslides based
on SHAP and XGBoost”. Geocarto International 37.26 (2022), pp. 13419–13450.<br><br>",
  "[4] E. Štrumbelj and I. Kononenko. “Explaining prediction models and individual predictions with feature
contributions”. Knowledge and information systems 41 (2014), pp. 647–665." 
                                   
                              )
                       )
                     ),
                     downloadButton("downloadInteractionPlot", "Export"),
                   )
          )
        )
      )
    )
  )
)

