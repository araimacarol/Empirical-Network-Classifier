#source("NetworkSummaryFunction.R")  # Load Network.Summary function
#source("Plot_igraph.R")  # Plot graphs
###----Normalized Laplacina function--##
normalized_laplacian=function(Graphs){
  laplacian_matrix(Graphs,normalized = T)
}

###----Graph features
calcGraphFeatures <- function(Graphs=NULL) {
  
  features <- c(
    "order",                    # number of vertices
    "edges",                     # number of edges
    "connected",                # True / False
    "max_component",            # maximum component size (=order iff the graph is connected)
    "minDegree",                # minimum degree of any vertex
    "maxDegree",                # maximum degree of any vertex
    "mean_degree",                # average degree of any vertex
    "minCut",                   # minimum cut weight of the graph (might take a while to compute)
    "FiedlerValue",             # second-highest eigenvalue of the Laplacian matrix
    "Normalized_FiedlerValue",   # second-highest eigenvalue of the Normaized Laplacian matrix
    "closeness_centr",                # average inverse of distance between any pair of vertices
    "modularity",               # DEFINITION REQUIRED
    "diameter",                 # maximum distance between any two vertices (NAN if not connected)
    "betw_centr",              # max_{v} proportion of shortest paths going through vertex v
    "transitivity",             # aka Clustering Coefficient, is proportion of connected triples that form triangles: e.g., (a--b--c--a) when (a--b--c) is present.
    "threshold",                 # 1/max(eigen value of A)
    "spectral_radius"         # max (eigen value of A)
    
  )
  
  df <- as.data.frame(matrix(ncol=length(features),nrow=length(Graphs)))
  colnames(df)=features
  
  ###----Stuff that is simple to apply and needs no interim components:
  
  df$order = base::as.numeric(lapply(Graphs, gorder))
  df$edges = base::as.numeric(lapply(Graphs, gsize))
  df$connected = base::as.numeric(lapply(Graphs, is_connected))
  df$minCut = base::as.numeric(lapply(Graphs, min_cut))
  df$diameter = base::as.numeric(lapply(Graphs, diameter))
  df$transitivity = base::as.numeric(lapply(Graphs, transitivity))
  
  ###----stuff that needs interim things:
  degrees = lapply(Graphs,igraph::degree )
  df$minDegree = base::as.numeric(lapply(degrees, min))
  df$maxDegree = base::as.numeric(lapply(degrees, max))
  df$mean_degree = base::as.numeric(lapply(degrees, mean))
  
  ####----stuff that lapply doesn't like so has to be done in a loop:
  communities <-lapply(Graphs, cluster_walktrap) #lapply(Graphs, cluster_leading_eigen)
  Adj <- lapply(Graphs, as_adjacency_matrix)
  L <- lapply(Graphs, laplacian_matrix)
  Norm_Lap<-lapply(Graphs, normalized_laplacian)
  Fiedler.value=NULL
  norm.fiedler.value=NULL
  
  for (i in 1:length(Graphs)) {
    if (is.null(Graphs[[i]]$type)) { Graphs[[i]]$type = "untyped" }
    df$modularity[i] <- modularity(communities[[i]])
    df$spectral_radius[i] <- eigen(Adj[[i]], symmetric=TRUE, only.values=TRUE)$values[1]
    
    Fiedler.value[[i]]=eigen(L[[i]], symmetric=TRUE, only.values=TRUE)$values
    
    df$FiedlerValue[i] <- Fiedler.value[[i]][length(Fiedler.value[[i]])-1]
    
    norm.fiedler.value[[i]]=eigen(Norm_Lap[[i]], symmetric=TRUE, only.values=TRUE)$values
    
    df$Normalized_FiedlerValue[i] <- norm.fiedler.value[[i]][length(norm.fiedler.value[[i]])-1]
    
    df$eigen_centr[i] <- centr_eigen(Graphs[[i]])$centralization
    df$deg_centr[i] <- centr_degree(Graphs[[i]])$centralization
    
    df$betw_centr[i] <- centr_betw(Graphs[[i]])$centralization
    
    df$max_component[i] <- max(components(Graphs[[i]])$csize)
    df$mean_eccentr[i]<-mean(eccentricity(Graphs[[i]]))
    df$radius[i]<-radius(Graphs[[i]])
    df$mean_path_length[i]<-mean_distance(Graphs[[i]])
    #df$trace[i]<-sum(diag(Adj[[i]]))
    df$graph_energy[i]<-sum(abs(eigen(Adj[[i]], symmetric=TRUE, only.values=TRUE)$values))
    df$min_triangle[i]= min(count_triangles(Graphs[[i]]))
    df$mean_triangle[i]= mean(count_triangles(Graphs[[i]]))
    df$sd_triangle[i]= sd(count_triangles(Graphs[[i]]))
    df$max_triangle[i]= max(count_triangles(Graphs[[i]]))
    df$num_triangle[i]= sum(count_triangles(Graphs[[i]]))
    df$deg_assort_coef[i]=assortativity_degree(Graphs[[i]])
    
    df$threshold[i] <- 1/(df$spectral_radius[i])
    
    if (df$connected[i]==TRUE) {
      df$closeness_centr[i] = mean(closeness(Graphs[[i]]))
    } else { #----handle the case where G isn't connected
      df$closeness_centr[i] = -1
    }
  }
  return (df)
}



###----Finding largest component 
largest_comp <- function(df) {
  
  # Convert the dataframe to an igraph object
  G <- graph_from_data_frame(as.matrix(df), directed = FALSE)
  
  # Simplify the graph to remove multiple edges and loops
  df.graph <- igraph::simplify(G, remove.multiple = TRUE, remove.loops = TRUE)
  
  # Remove isolated vertices
  Isolated <- which(igraph::degree(df.graph) == 0)
  Net <- igraph::delete_vertices(df.graph, Isolated)
  
  # Find the components of the graph
  Graph_components <- igraph::components(Net, mode = "weak")
  
  # Identify the largest component
  biggest_cluster_id <- which.max(Graph_components$csize)
  vert_ids <- V(Net)[Graph_components$membership == biggest_cluster_id]
  
  # Create the subgraph corresponding to the largest component
  graph <- igraph::induced_subgraph(Net, vert_ids)
  
  return(graph)
}
# largest_comp=function(df){
#   G=graph_from_data_frame(as.matrix(df),directed=FALSE)
#   df.graph=igraph::simplify(G,remove.multiple = T,remove.loops = T)
#   Isolated = which(igraph::degree(df.graph)==0)
#   Net=igraph::delete_vertices(df.graph, Isolated)
#   Graph_components = igraph::components(Net, mode="weak")
#   biggest_cluster_id = which.max(Graph_components$csize)
#   vert_ids = V(df.graph)[Graph_components$membership== biggest_cluster_id]
#   graph=igraph::induced_subgraph(df.graph, vert_ids)
#   
#   return(graph)
# }

###----Create graph object
readtable <- function(edgelist = "aves-barn-swallow-non-physical.edges") {
  x <- read.table(edgelist, fill = TRUE, header = FALSE, stringsAsFactors = FALSE)
  return(x)
}


Network.Summary <- function(folder_path) {
  all_files <- list.files(path = folder_path, full.names = TRUE)
  
  edge_files <- lapply(all_files, function(file) {
    # Determine the separator based on the first line of the file or the presence of commas in the filename
    
    first_line <- readLines(file, n = 1)
    sep_char <- ifelse(grepl("\t", first_line),
                       "\t", ifelse(grepl(",", first_line), ",", " "))
    
    # Read the table using the determined separator
    read.table(file, sep = sep_char, fill = TRUE, header = FALSE)
  })
  #----Initialize a list to store network feature summaries
  GraphNames <- basename(all_files)
  
  G <- lapply(edge_files, largest_comp)
  features <- calcGraphFeatures(G)
  
  # Combine GraphNames and features into a data frame
  all_graphs <- data.frame(GraphNames = GraphNames, features)
  
  #----Return the list of feature summaries
  return(list(all_graphs, edge_files))
}

set.seed((346))

#####################################################################
#####################################################################
#####################################################################

server <- shinyServer(function(input, output, session) {
  trained_model <- readRDS("final.fitted.model.rds")# Include your fitted model here
    #Define data_new outside of reactive expressions
    data_new <- reactiveVal(NULL)
    combine_res_df<- reactiveVal(NULL)

    # Reactive expression to compute predictions, feature summary, network info, and SHAP interaction values
    predictions <- eventReactive(input$classify, {
      req(input$folder)

      ## ----Path to folder containing the .edges or .csv two-column edgelist
      folder_path <- dirname(input$folder$datapath)


      ## ----Extract names of the networks in the folder
      folder_name <- tools::file_path_sans_ext(input$folder$name)

      ## ----Data of graph features for the networks in the folder
      data <- Network.Summary(folder_path)[[1]]
      
      
      data <- data %>%
        rename(graph_name = GraphNames) %>%
        #rename(NumOfNodes = order) %>%
        select(
          graph_name, order, edges, mean_eccentr, mean_path_length,
          graph_energy, modularity, diameter, betw_centr, transitivity,
          spectral_radius, eigen_centr, deg_centr, mean_degree, minCut,
          FiedlerValue, Normalized_FiedlerValue, closeness_centr,
          deg_assort_coef
        ) %>%
        mutate_if(is.character, factor) %>%
        clean_names() %>%
        as_tibble()

      ## ----Remove duplicates from the graph_name column using group_by and slice
      data_new_val <- data %>%
        group_by(graph_name) %>%
        dplyr::slice(1) %>%
        ungroup()  # Ungroup to remove grouping

      ## ----Sort the remaining rows under graph_name
      data_new_val <- data_new_val %>%
        arrange(as.numeric(sub(".*?([0-9]+)\\..*", "\\1", as.character(graph_name))))

      ## ----Set names of the networks to the names of the networks in the folder
      data_new_val$graph_name <- c(folder_name)

      # Store data_new_val in data_new reactiveVal
      data_new(data_new_val)

      ## ----Store data frame of features for networks in the folder
      feature_summary <- data_new_val

      ## ----Classify empirical networks

      predictions <- predict(extract_workflow(trained_model),
                             new_data =  data_new_val)
      
      
      
      
      

      probs <- predict(extract_workflow(trained_model),
                             new_data =  data_new_val,type="prob")
      
     
      
      # Aggregate probabilities across all samples
      mean_probabilities <- probs%>%
        summarise(
          Mean_Erdös_Rényi = mean(.pred_ER),
          Mean_Stochastic_Block_Model = mean(.pred_sbm),
          Mean_Scale_Free = mean(.pred_SF),
          Mean_Spatial = mean(.pred_Spatial),
          Mean_Small_World = mean(.pred_SW)
        )
      colnames(mean_probabilities)=c("Erdös-Rényi",
                                     "Stochastic-Block-Model",
                                     "Scale-Free",
                                     "Spatial",
                                     "Small-World")
      
      row.names(mean_probabilities)="Mean_Predicted_Probabilities"
     
      # Determine the class with the highest overall mean probability
      #reliable_class <- colnames(mean_probabilities)[which.max(mean_probabilities)]

      data_new_val$graph_name = folder_name


      data_new_df <- data_new_val %>%
        mutate("Predicted_Class" = predictions$.pred_class) %>%
        select(graph_name, "Predicted_Class", everything())
      
      
      # #Apply normalizations
      #  normalized_data_new_df <- data_new_df %>%
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

      # Drop original columns if not needed

      # graph_name <- data_new_df$graph_name
      # Predicted_Class<-data_new_df$Predicted_Class
      # data_new_df[, colnames(normalized_data_new_df)] <- normalized_data_new_df
      # data_new_df$graph_name <- graph_name
      # data_new_df$Predicted_Class <- Predicted_Class


      new_names <- c('Graph_name',"Predicted_Class",'Nodes','Edges','Mean eccentricity','Mean path length',
                     'Graph energy','Modularity','Diameter','Betweenness centrality',
                     'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                     'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
                     'Closeness centrality','Degree assortativity')

      colnames(data_new_df) <- new_names


      result_df <- data_new_df

      result_df <- result_df %>%
        mutate(Predicted_Class = case_when(
          Predicted_Class == "ER" ~ "Erdös-Rényi",
          Predicted_Class == "SF" ~ "Scale-Free",
          Predicted_Class == "Spatial" ~ "Spatial",
          Predicted_Class == "SW" ~ "Small-World",
          Predicted_Class == "sbm" ~ "Stochastic-Block-Model",
          TRUE ~ as.character(Predicted_Class) # Keep other values unchanged
        ))
      
      # count predicted classes
      count_pred_class <- result_df %>%
        dplyr::count(Predicted_Class, sort = TRUE)
      
      
      # Ensure all classes are included and have counts of zero if missing
#######################################################################
      
      # Ensure all classes are included and have counts of zero if missing
      all_classes <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
      missing_classes <- setdiff(all_classes, count_pred_class$Predicted_Class)
      count_pred_class <- rbind(count_pred_class, data.frame(Predicted_Class = missing_classes, n = 0))
      
      # Sort count_pred_class to ensure consistency with mean_probabilities column order
      count_pred_class_new <- count_pred_class[order(factor(count_pred_class$Predicted_Class, levels = all_classes)), ]
      
      # Create data frame with mean predicted probabilities
      mean_prob_df <- t(data.frame(Mean_Predicted_Probabilities =
                                     as.numeric(unlist(mean_probabilities))))
    colnames(mean_prob_df) <- all_classes
    
    # Create data frame with graph counts
    graph_counts_df <- t(data.frame(Predicted_Class_Counts = as.numeric(count_pred_class_new$n)))
    colnames(graph_counts_df) <- all_classes
    
    # Combine data frames
    # combine_res_df <- as.data.frame(rbind(mean_prob_df, graph_counts_df))
    # 
    # colnames(combine_res_df) <- gsub("\\.", " ", colnames(combine_res_df))
    # 
    # combine_res_df(combine_res_df) # Assign to reactiveVal
    combine_res_df_val <- as.data.frame(rbind(graph_counts_df,mean_prob_df))
    
    # Set column names
    colnames(combine_res_df_val) <- gsub("\\.", " ", colnames(combine_res_df_val))
    
    # Assign to reactiveVal
    combine_res_df(combine_res_df_val)
  
######################################################################
      # Sort predicted class
      result_df <- result_df[order(result_df$Graph_name), ]
      
      # Sort the Predicted_Class column within each group of Graph_name
      result_df <- result_df %>%
        group_by(Graph_name) %>%
        arrange(Predicted_Class)
      
    
     
      ## ----Generate network information as text
      network_info <- paste(
        "1) <b>Spatial networks</b> ",
        "Spatial networks are embedded in physical space,",
        "where nodes represent locations in the real world.\n",
        "Examples include transportation networks, sensor networks, and the\n",
        "Internet's physical infrastructure.\n",
        "Network topological characteristics include:\n",
        "(i) Geographical constraints on network connections\n",
        "nodes connect with each other within a specified threshold).\n",
        "(ii) High clustering.\n",
        "(iii) Positive assortativity mixing depending on the network size and\n",
        "threshold connection parameter\n\n",

        "2)<b> Scale-free (SF) networks</b> ",
        "exhibit a power-law degree distribution, ",
        "meaning that a few nodes (hubs) have a significantly higher\n",
        "number of connections compared to the majority of nodes.\n",
        "They often represent real-world networks like the World Wide Web,\n",
        "social networks, and citation networks.\n",
        "Network topological characteristics include:\n",
        "(i) High degree heterogeneity\n",
        "(captured by the centrality measures in network analysis).\n",
        "(ii) Robustness to node perturbation (random node removal)\n",
        "but vulnerable to targeted attacks on hubs.\n",
        "(iii) Short average path length and\n",
        "(iv) High clustering coefficient.\n",
        "(v) Negative assortativity mixing as a result of high degree nodes\n",
        "mixing with both high and low degree nodes\n\n",

        "3)<b> Stochastic-block-model (SBM)</b>",
        "is a generative model for networks where ",
        "nodes are partitioned into groups, and connections are formed\n",
        "probabilistically based on the group memberships.\n",
        "It is often used to model community structures in networks.\n",
        "Network Topological Characteristics include:\n",
        "(i) Clear community structure (quantified by modularity in network analysis).\n",
        "(ii) Within-community edges are more prevalent than between.\n",
        "(iii) Degree distribution within each block can vary.\n",
        "(iv) Positive assortativity mixing due to community/block structure\n",
        "making nodes within the same community mix more\n\n",

        "4) <b> Small-world (SW) networks</b> ",
        "These networks exhibit a balance between regular (lattice) and random structures.\n",
        "They are characterized by the short average path lengths observed in\n",
        "random networks and high clustering coefficients seen in regular networks.\n",
        "Other network topological properties include negative assortativity\n",
        "mixing, and low spectral radius.\n",
        "They are often used to model social networks and the\n",
        "brain's neural connections\n\n",

        "5) <b>Erdös-Rényi (ER) networks</b> ",
        "random graphs are generated by
                      connecting nodes randomly",
        "with a fixed probability.
                      They are a fundamental model for studying\n",
        "random graphs and phase transitions in networks.\n",
        "Network topological characteristics:\n",
        "(i) A Poisson degree distribution.\n",
        "(i) Lower average clustering.\n",
        "(iii) Longer average path length compared to scale-free networks.\n",
        "(iv) Lack of community structure.\n",
        "(v) High normalized Fiedler and negative degree assortativity coefficient\n",
        sep = "\n\n"

      )

      # Return the network information as part of the result
      return(list(result_df, network_info))
    })
 
    # Check file size function
    check_file_size <- function(file_paths, max_size_mb) {
      file_sizes_mb <- sapply(file_paths, function(path) file.info(path)$size / 1024^2)
      any(file_sizes_mb > max_size_mb)
    }
    
    
    observeEvent(input$folder, {
      max_size_mb <- 5000  # Maximum file size in MB
      file_paths <- input$folder$datapath
      # Check the file size
      if (check_file_size(file_paths, max_size_mb)) {
        # Show an alert if any file size exceeds the limit
        showModal(
          modalDialog(
            title = "File Size Exceeded",
            "One or more selected files exceed the maximum allowed size of 500 MB. Please choose smaller files.",
            easyClose = TRUE
          )
        )
        # Clear the file input
        updateFileInput(session, "folder", label = "Select a folder", multiple = FALSE)
      }
    })
    

  # Define data_new outside of reactive expressions
    # Download predictions table as CSV
    output$downloadPredictionsCSV <- downloadHandler(
      filename = function() {
        "predictions.csv"
      },
      content = function(file) {
        write.csv(predictions()[[1]], file, row.names = FALSE)
      }
    )
    
    output$predictions <- renderTable({
      if (input$classify > 0) {
        predictions()[[1]] # Display result_df
      }
    })
    
    # Output the combine_res_df as a table
    # output$combineResTable <- renderTable({
    #   combine_res_df()
    # })
    # output$combineResTable <- renderTable({
    #   req(combine_res_df())  # Ensure combine_res_df is not NULL
    #   combine_res_df()
    # })
    output$combineResTable <- renderTable({
      df <- combine_res_df()
      # Ensure row names are included
      if (!is.null(rownames(df))) {
        df <- cbind(Row_Names = rownames(df), df)
      }
      df
    })
    
    
    
    # Render the network information in the text output area
    output$networkInfo <- renderUI({
      if (input$classify > 0) {
        network_info <- predictions()[[2]]
        network_info_html <- paste("<p>", gsub("\n", "</p><p>", network_info), "</p>", sep = "")
        HTML(network_info_html)
      }
    })
    
    
    
  
      # Reactive expression to compute SHAP interaction values
    shap_interactn <- reactive({
        req(input$classify) # Ensure that classification has been performed before computing SHAP interaction values

        # Access data_new reactiveVal
        data_new_val <- data_new()
        data_new_val_pred <- data_new_val %>%
          dplyr::select(-c(graph_name))

        if (!is.null(data_new_val)) {
          # Compute SHAP interactions
          print("Computing SHAP interactions...")
          shap_interactn <- shapviz(
            extract_fit_engine(trained_model),
            X_pred = data.matrix(data_new_val_pred),
            X = data_new_val,
            interactions = TRUE
          )

          # Check the structure of shap_interactn
          print("Structure of shap_interactn:")
          print(str(shap_interactn))


          # Rename variables
          new_column_names <- c('Nodes','Edges','Mean eccentricity','Mean path length',
                                'Graph energy','Modularity','Diameter','Betweenness centrality',
                                'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                                'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
                                'Closeness centrality','Degree assortativity') # Modify as needed

          # Rename class names
          names(shap_interactn)[names(shap_interactn) == "Class_1"] <- "Erdös-Rényi"
          names(shap_interactn)[names(shap_interactn) == "Class_2"] <- "Stochastic-Block-Model"
          names(shap_interactn)[names(shap_interactn) == "Class_3"] <- "Scale-Free"
          names(shap_interactn)[names(shap_interactn) == "Class_4"] <- "Spatial"
          names(shap_interactn)[names(shap_interactn) == "Class_5"] <- "Small-World"

          # Rename column names and dimensions for each class
          for (class_name in names(shap_interactn)) {
            colnames(shap_interactn[[class_name]]$X) <- new_column_names
            colnames(shap_interactn[[class_name]]$S) <- new_column_names
            colnames(shap_interactn[[class_name]]$S_inter) <- new_column_names

            # Ensure consistent dimension names
            dimnames(shap_interactn[[class_name]]$S_inter)[[3]] <- dimnames(shap_interactn[[class_name]]$S_inter)[[2]]
          }

          # Return the modified SHAP interactions
          return(shap_interactn)
        } else {
          # If data_new_val is NULL, return NULL
          print("data_new_val is NULL. Returning NULL.")
          return(NULL)
        }
      })


  # Add this to your server function to include the dropdown menu
   output$shapPlot <- renderPlot({
    req(input$classify) # Ensure that classification has been performed before rendering SHAP plots

     

    shap_data <- shap_interactn()
    
    data_new_val2 <- data_new()
    data_new_val_pred2 <- data_new_val2 %>%
      dplyr::select(-c(graph_name))

    colnames(data_new_val_pred2) <- c('Nodes','Edges','Mean eccentricity','Mean path length',
                                      'Graph energy','Modularity','Diameter','Betweenness centrality',
                                      'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                                      'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
                                      'Closeness centrality','Degree assortativity')


    features <- c()
    if (!is.null(input$feature_variables) && input$feature_variables != "") {
      features <- unlist(strsplit(input$feature_variables, ",\\s*"))
    } else {
      features <- colnames(data_new_val_pred2)
    }
   
    

    if (!is.null(shap_data)) {
      selected_class <- input$shap_class

      # Plot the selected SHAP plot type for the selected class
      if (selected_class == "Erdös-Rényi") {
        sv_plot <- shap_data$`Erdös-Rényi`
      } else if (selected_class == "Stochastic-Block-Model") {
        sv_plot <- shap_data$`Stochastic-Block-Model`
      } else if (selected_class == "Scale-Free") {
        sv_plot <- shap_data$`Scale-Free`
      } else if (selected_class == "Spatial") {
        sv_plot <- shap_data$Spatial
      } else if (selected_class == "Small-World") {
        sv_plot <- shap_data$`Small-World`
      }

      plot_type <- input$shap_plot

      # Plot the selected SHAP plot type for the selected class
      if (plot_type == "Waterfall") {
        # Plot waterfall plot
        sv_waterfall(sv_plot)
      } else if (plot_type == "Force") {
        # Plot force plot
        sv_force(sv_plot)
      } else if (plot_type == "Importance") {
        # Plot importance plot
        sv_importance(sv_plot,
                      kind = input$importance_kind,
                      show_numbers = FALSE, bee_width = 0.3,
                      max_display = 6L) &
          theme_classic() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      } else if (plot_type == "Dependence") {
        # Plot dependence plot
        sv_dependence(sv_plot, features, color_var = NULL) &
          geom_smooth(method = "auto", se = FALSE, stat = "smooth",
                      position = "identity", color
                      = "black") &
          theme_classic() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      }else if (plot_type == "2D Dependence") {
        # Plot 2D dependence plot
        if (!is.null(input$dependence_var) && input$dependence_var != "") {
          # Check if custom features are provided
          if (!is.null(input$feature_variables) && input$feature_variables != "") {
            features <- unlist(strsplit(input$feature_variables, ",\\s*"))
          } else {
            features <- colnames(data_new_val_pred2)
          }

          sv_dependence2D(sv_plot, input$dependence_var, features) &
            geom_smooth(method = "auto", se = FALSE, stat = "smooth",
                        position = "identity", color = "black") &
            theme_classic() &
            theme(
              text = element_text(size = 12),
              axis.text = element_text(size = 12),
              plot.title = element_text(size = 12))
        } else {
          print("Error: No dependence variable selected for 2D dependence plot.")
          # Handle the error appropriately
        }
      } else if (plot_type == "Interaction") {
        # Plot interaction plot
        sv_interaction(sv_plot) &
          theme_bw() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      }
    }
  })

  data_new <- reactiveVal(NULL)

  # Define data_new_val_pred2 as a reactive expression
  data_new_val_pred2 <- reactive({
    req(data_new())
    data_new_val2 <- data_new()
    data_new_val2_pred <- data_new_val2 %>%
      dplyr::select(-c(graph_name))
    colnames(data_new_val2_pred) <- c('Nodes','Edges','Mean eccentricity','Mean path length',
                                      'Graph energy','Modularity','Diameter','Betweenness centrality',
                                      'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                                      'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
                                      'Closeness centrality','Degree assortativity')
    return(data_new_val2_pred)
  })

  # Dynamically generate UI elements for SHAP plot options
  output$shap_ui <- renderUI({
    if (input$classify > 0) {
      fluidRow(
        column(6,
               selectInput("shap_class", "Select SHAP Class:",
                           choices = c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World"),
                           selected = "Erdös-Rényi")
        ),
        column(6,
               conditionalPanel(
                 condition = "input.shap_plot == 'Dependence' || input.shap_plot == '2D Dependence'",
                 selectizeInput("dependent_feature", "Dependent Features:", choices = colnames(data_new_val_pred2()), multiple = TRUE)
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "input.shap_plot == '2D Dependence'",
                 selectizeInput("feature_variables", "Select primary feature:", choices = colnames(data_new_val_pred2()), multiple = TRUE)
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "input.shap_plot == 'Importance'",
                 selectInput("importance_kind", "Select Variable Importance Kind:",
                             choices = c("beeswarm", "bar", "both"),
                             selected = "beeswarm")
               )
        ),
        column(12,
               selectInput("shap_plot", "SHAP Plot Type:",
                           choices = c("Waterfall", "Force", "Importance", "Dependence", "2D Dependence", "Interaction"),
                           selected = "Waterfall")
        )
      )
    }
  })
  # Server function dropdown menu
  output$shapPlot <- renderPlot({
    req(input$classify)

    shap_data <- shap_interactn()
    data_new_val2 <- data_new()
    data_new_val_pred2 <- data_new_val2 %>%
      dplyr::select(-c(graph_name))

    colnames(data_new_val_pred2) <- c('Nodes','Edges','Mean eccentricity','Mean path length',
                                      'Graph energy','Modularity','Diameter','Betweenness centrality',
                                      'Transitivity','Spectral radius','Eigen centrality','Degree centrality',
                                      'Mean degree','Min cut','Fiedler value','Normalized Fiedler',
                                      'Closeness centrality','Degree assortativity')

    features <- c()
    if (!is.null(input$feature_variables) && input$feature_variables != "") {
      features <- unlist(strsplit(input$feature_variables, ",\\s*"))
    } else {
      features <- colnames(data_new_val_pred2)
    }

    if (!is.null(shap_data)) {
      selected_class <- input$shap_class

      # Plot the selected SHAP plot type for the selected class
      if (selected_class == "Erdös-Rényi") {
        sv_plot <- shap_data$`Erdös-Rényi`
      } else if (selected_class == "Stochastic-Block-Model") {
        sv_plot <- shap_data$`Stochastic-Block-Model`
      } else if (selected_class == "Scale-Free") {
        sv_plot <- shap_data$`Scale-Free`
      } else if (selected_class == "Spatial") {
        sv_plot <- shap_data$Spatial
      } else if (selected_class == "Small-World") {
        sv_plot <- shap_data$`Small-World`
      }

      plot_type <- input$shap_plot

      # Plot the selected SHAP plot type for the selected class
      if (plot_type == "Waterfall") {
        # Plot waterfall plot
        sv_waterfall(sv_plot)
      } else if (plot_type == "Force") {
        # Plot force plot
        sv_force(sv_plot)
      } else if (plot_type == "Importance") {
        # Plot importance plot
        sv_importance(sv_plot,
                      kind = input$importance_kind,
                      show_numbers = FALSE, bee_width = 0.3,
                      max_display = 6L) &
          theme_classic() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      }else if (plot_type == "Dependence") {
        # Plot dependence plot

        # Check if dependent features are selected
        if (!is.null(input$dependent_feature) && input$dependent_feature != "") {
          features <- input$dependent_feature
        } else {
          features <- colnames(data_new_val_pred2)
        }

        sv_dependence(sv_plot, features, color_var = NULL) &
          # geom_smooth(method = "auto", se = FALSE, stat = "smooth",
          #             position = "identity", color = "black") &
          theme_classic() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      } else if (plot_type == "2D Dependence") {
        # Plot 2D dependence plot

        # Determine primary feature
        if (is.null(input$feature_variables) || input$feature_variables == "") {
          primary_feature <- colnames(data_new_val_pred2)[1]
        } else {
          primary_feature <- input$feature_variables
        }

        # Determine dependent features
        if (!is.null(input$dependent_feature) && input$dependent_feature != "") {
          dependent_features <- unlist(strsplit(input$dependent_feature, ",\\s*"))
        } else {
          if (length(primary_feature) == 1) {
            dependent_features <- colnames(data_new_val_pred2)
          } else {
            dependent_features <- primary_feature
            primary_feature <- colnames(data_new_val_pred2)[1]
          }
        }

        # If both primary feature and dependent features are empty, use default
        if ((is.null(input$feature_variables) || input$feature_variables == "") &&
            (is.null(input$dependent_feature) || input$dependent_feature == "")) {
          primary_feature <- colnames(data_new_val_pred2)[1]
          dependent_features <- colnames(data_new_val_pred2)
        }

        # Remove primary feature from dependent features if they are the same
        dependent_features <- dependent_features[dependent_features != primary_feature]

        # Plot 2D dependence plot
        sv_dependence2D(sv_plot, primary_feature, dependent_features) &
          theme_classic() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      }else if (plot_type == "Interaction") {
        # Plot interaction plot
        sv_interaction(sv_plot) &
          theme_bw() &
          theme(
            text = element_text(size = 12),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 12))
      }
    }
  })



  network_colors <- c("Erdös-Rényi" = "red", 
                      "Stochastic-Block-Model" = "blue", 
                      "Scale-Free" = "green", 
                      "Spatial" = "orange", 
                      "Small-World" = "purple")
  
  # Create a function to map network names to colors
  # network_color_mapper <- function(network_name) {
  #   return(network_colors[network_name])
  # }
  

  # Reactive expression for printing Hstat_df_new
  Hstat_df_new<- reactive({
    data_new_val2 <- data_new()
    if (!is.null(data_new_val2)) {
      Hstat_df_new <- data_new_val2 %>%
        dplyr::select(-c(graph_name))
      return(Hstat_df_new)
    } else {
      return(NULL)
    }
  })
 

  # Reactive expression for printing Hstat_df_new2
  Hstat_df_new2 <- reactive({
    data_new_val3 <- data_new()
    if (!is.null(data_new_val3)) {
      Hstat_df_new2 <- data_new_val3 %>%
        dplyr::select(c(graph_name))
      print("Printing Hstat_df_new2:")
      print(Hstat_df_new2)
      return(Hstat_df_new2)
    } else {
      return(NULL)
    }
  })
  
    # Reactive expression for computing Hstat
    Hstat <- reactive({
      set.seed(1486)
      x1 <- setdiff(colnames(Hstat_df_new()), "graph_name")
      model1 <- extract_workflow(trained_model)
      H <- hstats(model1, v = x1,
                  n_max = nrow(Hstat_df_new()), X = Hstat_df_new(),
                  type = "prob",
                  threeway_m = 5)

      return(H)
    })
  
 

  # Reactive expression for permutation importance
  permutation_importance <- reactive({
    model1 <- extract_workflow(trained_model)
    old_names <- c('num_of_nodes', 'edges', 'mean_eccentr', 'mean_path_length',
                   'graph_energy', 'modularity', 'diameter', 'betw_centr',
                   'transitivity', 'spectral_radius', 'eigen_centr', 'deg_centr',
                   'mean_degree', 'min_cut', 'fiedler_value', 'normalized_fiedler_value',
                   'closeness_centr', 'deg_assort_coef')

    new_names <- c('Nodes', 'Edges', 'Mean eccentricity', 'Mean path length',
                   'Graph energy', 'Modularity', 'Diameter', 'Betweenness centrality',
                   'Transitivity', 'Spectral radius', 'Eigen centrality', 'Degree centrality',
                   'Mean degree', 'Min cut', 'Fiedler value', 'Normalized Fiedler',
                   'Closeness centrality', 'Degree assortativity')


    set.seed(10)
    var_imp <- perm_importance(model1, X = data.matrix(Hstat_df_new()),
                               y = data.matrix(Hstat_df_new2()),
                               m_rep = 5)
    var_imp$M <- data.frame(var_imp$M)
    var_imp$M <- var_imp$M[rowSums(abs(var_imp$M[, c("ER", "sbm", "SF", "Spatial", "SW")]) > 0.0005) > 0, ]
    colnames(var_imp$M) <- c("Erdös-Rényi", "Stochastic-Block-Model", 
                             "Scale-Free", "Spatial", "Small-World")
    data_imp <- var_imp$M
    data_imp$variable <- rownames(data_imp)

    # Replacing variable names
    for (i in seq_along(old_names)) {
      data_imp$variable[data_imp$variable == old_names[i]] <- new_names[i]
    }
    data_imp$variable <- make.unique(data_imp$variable)
    # Reshape data for plotting
    data_imp_long <- pivot_longer(data_imp, cols = -variable, names_to = "network", values_to = "importance")
    

    print("Printing permutation importance:")
    print(data_imp)

    return(data_imp_long)
  })

  # Reactive expression for partial dependence importance
  partial_dependence_importance <- reactive({
    H=Hstat()
    old_names <- c('num_of_nodes', 'edges', 'mean_eccentr', 'mean_path_length',
                   'graph_energy', 'modularity', 'diameter', 'betw_centr',
                   'transitivity', 'spectral_radius', 'eigen_centr', 'deg_centr',
                   'mean_degree', 'min_cut', 'fiedler_value', 'normalized_fiedler_value',
                   'closeness_centr', 'deg_assort_coef')

    new_names <- c('Nodes', 'Edges', 'Mean eccentricity', 'Mean path length',
                   'Graph energy', 'Modularity', 'Diameter', 'Betweenness centrality',
                   'Transitivity', 'Spectral radius', 'Eigen centrality', 'Degree centrality',
                   'Mean degree', 'Min cut', 'Fiedler value', 'Normalized Fiedler',
                   'Closeness centrality', 'Degree assortativity')

    set.seed(100)
    colnames(H$pd_importance$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
    names(H$pd_importance$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
    rownames(H$pd_importance$num) <- new_names

    partial_imp <- hstats::pd_importance(H,normalize = FALSE,sort = TRUE,
                                 squared = FALSE,
                                 zero =TRUE)
    return(partial_imp)

  })

  
# Dropdown menu for selecting between different options for H
    output$h_dropdown <- renderUI({
      selectInput("h_option", "Select H-Statistics option:",
                  choices = c("Total interaction strength (H²)",
                              "Overall interaction strength (Hⱼ²)",
                              "Pairwise interaction strength (Hⱼₖ²)",
                              "Three-way interaction strength (Hⱼₖₗ²)",
                              "Feature importance"),  # Include "Feature importance" option
                  selected = "Overall interaction strength (Hⱼ²)")
    })
    
    # Render additional dropdown for feature importance plot selection
    output$feature_importance_dropdown <- renderUI({
      if (!is.null(input$h_option) && input$h_option == "Feature importance") {
        selectInput("feature_importance_plot_type", "Select Feature Importance Plot Type:",
                    choices = c("Permutation Importance", "Partial Dependence Importance"),
                    selected = "Partial Dependence Importance")
      }
    })
    
   
    
    # output$h_description <- renderText({
    #   if (input$h_option == "Total interaction strength (H²)") {
    #     "Total interaction strength (H²): proportion of predicted variability unexplained by main effects"
    #   } else {
    #     ""
    #   }
    # })

    output$h_output <- renderPrint({
      req(input$classify, input$h_option) # Ensure that classification has been performed and an option is selected

      # Compute Hstat based on selected option
       H <- Hstat()
      # Assign the new column names to your matrix
      colnames(H$h2$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")

#Display H based on selected option
      if (input$h_option == "Total interaction strength (H²)") {
        
        print(H)
        
        cat("
        
         The initial step to understand interaction among features is to determine interaction exist
         and to quantify the degree and strength of their interaction [2]. 
         Friedman and Popescu's H-statistics is used to quantify the strength and degree of these interactions [4].
        
         The Total Interaction Strength (H²) is a global statistic measuring the proportion of prediction variability unexplained by main effects, 
         and an experimental feature importance measure [1]. 
         Since H-statistics are partial dependence estimates, they are as good or bad as there is [2].
         A value of 0 means there are no interaction effects at all [1]. 
         A caveat is that the model is applied to unseen/impossible feature combinations.\n 
         
         In extreme cases, H-statistics intended to be in the range between 0 and 1 can become larger than 1.
         Accumulated local effects (ALE) can account for the partial dependence defects [1]. 
         Mayer et al. [1] noted they depend on the notion of closeness,
         which is highly important in higher dimension and for discrete features.\n
        
         Alternatively, Shapley Additive Explanations (SHAP) model agnostic interpretation method is a
         feature importance measure, whose mathematical foundation and interpretation of feature importance 
         for model output closely align with the way humans perceive things can also be used [2,3].\n
         
         References
         
         [1] Mayer, Michael, Steven C. Bourassa, Martin Hoesli, and Donato Scognamiglio. Machine Learning Applications to Land and Structure Valuation 
             Journal of Risk and Financial Management 15, no. 5 (2022): 193.
             
         [2] S. M. Lundberg and S.-I. Lee. “A unified approach to interpreting model predictions”. Advances in neural
        information processing systems 30 (2017).
         
         [3] W. E. Marcı́lio and D. M. Eler. “From explanations to feature selection: assessing SHAP values as fea-
ture selection mechanism”. In: 2020 33rd SIBGRAPI conference on Graphics, Patterns and Images (SIB-
GRAPI). IEEE. 2020, pp. 340–347.

         [4] Friedman, Jerome H., and Bogdan E. Popescu. Predictive Learning via Rule Ensembles.
         The Annals of Applied Statistics 2, no. 3 (2008): 916-54.\n" )

      }
    })

   
    # Render selected H option
    output$hstatPlot <- renderPlot({
      tryCatch({
        req(input$classify) # Ensure that classification has been performed before rendering H plots

        H <- Hstat()
        partial_dependence_data <- partial_dependence_importance()
        
        permutation_data <- permutation_importance()

        old_names <- c('num_of_nodes', 'edges', 'mean_eccentr', 'mean_path_length',
                       'graph_energy', 'modularity', 'diameter', 'betw_centr',
                       'transitivity', 'spectral_radius', 'eigen_centr', 'deg_centr',
                       'mean_degree', 'min_cut', 'fiedler_value', 'normalized_fiedler_value',
                       'closeness_centr', 'deg_assort_coef')

        new_names2 <- c('Nodes', 'Edges', 'Mean eccentricity', 'Mean path length',
                        'Graph energy', 'Modularity', 'Diameter', 'Betweenness centrality',
                        'Transitivity', 'Spectral radius', 'Eigen centrality', 'Degree centrality',
                        'Mean degree', 'Min cut', 'Fiedler value', 'Normalized Fiedler',
                        'Closeness centrality', 'Degree assortativity')

        colnames(H$X) <- new_names2
        H$X <- new_names2
       
      
        
        if (input$h_option == "Feature importance") {
          # Check if feature_importance_plot_type is not NULL
          if (!is.null(input$feature_importance_plot_type)) {
            if (input$feature_importance_plot_type == "Permutation Importance") {
              # Render permutation importance plot
              permutation.imp <- ggplot(permutation_data, aes(x = importance, y =  variable, fill = network)) +
                geom_bar(stat = "identity",position = "dodge") +
                labs(#title = "Variable Importance Plot",
                     x = "Permutation Importance",
                     y = "Features",
                     fill = "Networks") +
                theme_bw() +
                theme(
                  strip.background = element_rect(),
                  plot.title = element_text(size = 12, family = "serif"),
                  axis.text.x = element_text(size = 12, family = "serif"),
                  axis.text.y = element_text(size = 12, family = "serif"),
                  axis.title = element_text(size = 12, family = "serif"),
                  text = element_text(size = 12, family = "serif"),
                  legend.text = element_text(size = 12, family = "serif"),
                  legend.title = element_text(size = 12, family = "serif"),
                  legend.position = "bottom",
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background.y = element_blank(),
                  strip.background.x = element_blank()
                )
              permutation.imp <- permutation.imp +
                scale_fill_manual(values = network_colors)
              
              return(permutation.imp)
            } else if (input$feature_importance_plot_type == "Partial Dependence Importance") {
              # Render partial dependence importance plot
              partial_plot <- plot(partial_dependence_data, top_m = 5,zero = FALSE) +
                theme_bw() +
                labs(y = "Features")+
                theme(
                  strip.background = element_rect(),
                  plot.title = element_text(size = 12, family = "serif"),
                  axis.text.x = element_text(size = 12, family = "serif"),
                  axis.text.y = element_text(size = 12, family = "serif"),
                  axis.title = element_text(size = 12, family = "serif"),
                  text = element_text(size = 12, family = "serif"),
                  legend.text = element_text(size = 12, family = "serif"),
                  legend.title = element_text(size = 12, family = "serif"),
                  legend.position = "bottom",
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background.y = element_blank(),
                  strip.background.x = element_blank()
                )
              partial_plot <- partial_plot +
                scale_fill_manual(name= "Networks", values = network_colors)
              #+ scale_fill_discrete(name= "Networks")
              return(partial_plot)
            } 
          }
        } else {
          
          if (input$h_option == "Overall interaction strength (Hⱼ²)") {
          # Plot hplot
            rownames(H$h2_overall$num) <- new_names2
            colnames(H$h2_overall$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
            names(H$h2_overall$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
            
            
            hplot <- h2_overall(H, normalize = FALSE, squared = FALSE,
                              top_m = 5,zero = FALSE)

            hplot_plot1 <- plot(hplot) + theme_bw() +
            theme(
              strip.background = element_rect(),
              plot.title = element_text(size = 12, family = "serif"),
              axis.text.x = element_text(size = 12, family = "serif"),
              axis.text.y = element_text(size = 12, family = "serif"),
              axis.title = element_text(size = 12, family = "serif"),
              text = element_text(size = 12, family = "serif"),
              legend.text = element_text(size = 12, family = "serif"),
              legend.title = element_text(size = 12, family = "serif"),
              legend.position = "bottom",
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background.y = element_blank(),
              strip.background.x = element_blank()
            )
            hplot_plot1=hplot_plot1 + 
              scale_fill_manual(name= "Networks", values = network_colors)
              # scale_fill_discrete(name= "Networks",labels=c("Small-World",
              #                                               "Spatial",
              #                                   "Scale-Free",
              #                                   "Stochastic-Block-Model",
              #                                   "Erdös-Rényi"))
          return(hplot_plot1)

        } else if (input$h_option == "Pairwise interaction strength (Hⱼₖ²)") {
          replace_names <- function(old_names, new_names2, row_names) {
            for (i in seq_along(old_names)) {
              for (j in seq_along(old_names)) {
                if (i != j) {
                  old_pair <- paste0("\\b", old_names[i], ":", old_names[j], "\\b")
                  new_pair <- paste0(new_names2[i], ":", new_names2[j])
                  row_names <- str_replace_all(row_names, old_pair, new_pair)
                  
                  old_pair <- paste0("\\b", old_names[j], ":", old_names[i], "\\b")
                  new_pair <- paste0(new_names2[j], ":", new_names2[i])
                  row_names <- str_replace_all(row_names, old_pair, new_pair)
                }
              }
            }
            return(row_names)
          }
          new_row_names <- replace_names(old_names, new_names2, rownames(H$h2_pairwise$num))
        
          # Update the row names
            rownames(H$h2_pairwise$num) <- new_row_names
            colnames(H$h2_pairwise$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
            names(H$h2_pairwise$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
            
           
            hplot2 <- h2_pairwise(H, normalize = FALSE, squared = FALSE,
                                zero = FALSE)
            hplot_plot2 <- plot(hplot2) +
            theme_bw() +
            theme(
              strip.background = element_rect(),
              plot.title = element_text(size = 12, family = "serif"),
              axis.text.x = element_text(size = 12, family = "serif"),
              axis.text.y = element_text(size = 12, family = "serif"),
              axis.title = element_text(size = 12, family = "serif"),
              text = element_text(size = 12, family = "serif"),
              legend.text = element_text(size = 12, family = "serif"),
              legend.title = element_text(size = 12, family = "serif"),
              legend.position = "bottom",
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background.y = element_blank(),
              strip.background.x = element_blank()
            )
            hplot_plot2=hplot_plot2 + 
              scale_fill_manual(name= "Networks", values = network_colors)
             
          return(hplot_plot2)
        } else if (input$h_option == "Three-way interaction strength (Hⱼₖₗ²)") {
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
            new_row_names <- replace_names(old_names, new_names2, rownames(H$h2_threeway$num))

          # Update the row names
            rownames(H$h2_threeway$num) <- new_row_names
            colnames(H$h2_threeway$num) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
            names(H$h2_threeway$denom) <- c("Erdös-Rényi", "Stochastic-Block-Model", "Scale-Free", "Spatial", "Small-World")
            

            hplot3 <- h2_threeway(H, normalize = FALSE, squared = FALSE,
                                 zero = FALSE)

            hplot_plot3=plot(hplot3)+
              theme_bw() +
              theme(
                strip.background = element_rect(),
                plot.title = element_text(size = 12, family = "serif"),
                axis.text.x = element_text(size = 12, family = "serif"),
                axis.text.y = element_text(size = 12, family = "serif"),
                axis.title = element_text(size = 12, family = "serif"),
                text = element_text(size = 12, family = "serif"),
                legend.text = element_text(size = 12, family = "serif"),
                legend.title = element_text(size = 12, family = "serif"),
                legend.position = "bottom",
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.background.y = element_blank(),
                strip.background.x = element_blank()
              )
            hplot_plot3=hplot_plot3 +
              scale_fill_manual(name= "Networks", values = network_colors)
              # scale_fill_discrete(name= "Networks",
              #                     labels=c("Small-World","Spatial",
              #                           "Scale-Free",
              #                         "Stochastic-Block-Model",
              #                         "Erdös-Rényi"))
            return(hplot_plot3)
        }
        }
      }, error = function(e) {
        # Print error message
        message("Error in hstatPlot:", e)
        # Return NULL plot
        NULL
      })
    })
    
    
    # Download handlers for SHAP plots
    output$downloadWaterfallPlot <- downloadHandler(
      filename = function() {
        paste("waterfall_plot", input$format, sep = ".")
        #"waterfall_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 10, height = 7)
      }
    )
    
    output$downloadForcePlot <- downloadHandler(
      filename = function() {
        paste("force_plot", input$format, sep = ".")
        #"force_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 7, height = 7)
      }
    )
    
    output$downloadImportancePlot <- downloadHandler(
      filename = function() {
        paste("importance_plot", input$format, sep = ".")
        #"importance_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 10)
      }
    )
    
    
    output$downloadDependencePlot <- downloadHandler(
      filename = function() {
        paste("dependence_plot", input$format, sep = ".")
        #"dependence_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 8)
      }
    )
    
    
    output$download2DDependencePlot <- downloadHandler(
      filename = function() {
        paste("2d_dependence_plot", input$format, sep = ".")
        #"2d_dependence_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 8)
      }
    )
    
    
    output$downloadInteractionPlot <- downloadHandler(
      filename = function() {
        paste("interaction_plot", input$format, sep = ".")
        #"interaction_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 10)
      }
    )
    
    # Download handler for Overall interaction strength (Hⱼ²) plot
    output$downloadOverallHPlot <- downloadHandler(
      filename = function() {
        paste("overall_interaction_strength_plot", input$format, sep = ".")
        #"overall_interaction_strength_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 7)
      }
    )

    # Download handler for Pairwise interaction strength (Hⱼₖ²) plot
    output$downloadPairwiseHPlot <- downloadHandler(
      filename = function() {
        paste("pairwise_interaction_strength_plot", input$format, sep = ".")
        #"pairwise_interaction_strength_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 7)
      }
    )

    # Download handler for Three-way interaction strength (Hⱼₖₗ²) plot
    output$downloadThreeWayHPlot <- downloadHandler(
      filename = function() {
        paste("three_way_interaction_strength_plot", input$format, sep = ".")
        #"three_way_interaction_strength_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 7)
      }
    )
    
    output$download_permutation_importance_plot <- downloadHandler(
      filename = function() {
        paste("permutation_importance_plot", input$format, sep = ".")
        #"permutation_importance_plot.png"
        
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 7)
      }
    )
    
    # Download handler for partial dependence importance plot
    output$download_partial_dependence_importance_plot <- downloadHandler(
      filename = function() {
        paste("partial_dependence_importance_plot", input$format, sep = ".")
        #"partial_dependence_importance_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 12, height = 7)
      }
    )
})