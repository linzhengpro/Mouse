library(metacoder)
print(hmp_otus)
print(hmp_samples)
hmp_otus$lineage[1:4]
data<-hmp_otus
data2<-hmp_samples



#单个图
data = read.csv('otu.csv', header = TRUE)  
data2 = read.csv('group.csv', header = TRUE) 
obj <- parse_tax_data(data,
                      class_cols = "lineage", # the column that contains taxonomic information
                      class_sep = ";", # The character used to separate taxa in the classification
                      class_regex = "^(.+)__(.+)$", # Regex identifying where the data for each taxon is
                      class_key = c(tax_rank = "info", # A key describing each regex capture group
                                    tax_name = "taxon_name"))


obj$data$tax_data <- zero_low_counts(obj, data = "tax_data", min_count = 1)

no_reads <- rowSums(obj$data$tax_data[, data2$sample_id]) == 0
sum(no_reads)

obj <- filter_obs(obj, target = "tax_data", ! no_reads, drop_taxa = TRUE)

obj$data$tax_data <- calc_obs_props(obj, "tax_data")

obj$data$tax_abund <- calc_taxon_abund(obj, "tax_data",
                                       cols = data2$sample_id)


obj$data$tax_occ <- calc_n_samples(obj, "tax_abund", groups = data2$body_site, cols = data2$sample_id)


set.seed(1) # This makes the plot appear the same each time it is run 
heat_tree(obj, 
          node_label = taxon_names,
          node_size = n_obs,
          node_color = KF1, 
          node_size_axis_label = "OTU count",
          node_color_axis_label = "Samples with reads",
          layout = "davidson-harel", # The primary layout algorithm
          initial_layout = "reingold-tilford") # The layout algorithm that initializes node locations


set.seed(1)
heat_tree(obj, 
          node_label = obj$taxon_names(),
          node_size = obj$n_obs(),
          node_color = obj$data$tax_occ$KF1, 
          node_size_axis_label = "OTU count",
          node_color_axis_label = "Samples with reads",
          layout = "davidson-harel", # The primary layout algorithm
          initial_layout = "reingold-tilford") # The layout algorithm that initializes node locations



#多组互相比较
obj$data$diff_table <- compare_groups(obj, dataset = "tax_abund",
                                      cols = data2$sample_id, # What columns of sample data to use
                                      groups = data2$sex) # What category each sample is assigned to


set.seed(1)

heat_tree_matrix(obj,
                 data = "diff_table",
                 node_size = n_obs, # n_obs is a function that calculates, in this case, the number of OTUs per taxon
                 node_label = taxon_names,
                 node_color = log2_median_ratio, # A column from `obj$data$diff_table`
                 node_color_range = c("#FFAF50", "gray", "#519CBA"), # The built-in palette for diverging data
                 node_color_trans = "linear", # The default is scaled by circle area
                 node_color_interval = c(-3, 3), # The range of `log2_median_ratio` to display
                 edge_color_interval = c(-3, 3), # The range of `log2_median_ratio` to display
                 node_size_axis_label = "Number of OTUs",
                 node_color_axis_label = "Log2 ratio median proportions",
                 layout = "davidson-harel", # The primary layout algorithm
                 initial_layout = "reingold-tilford", # The layout algorithm that initializes node locations
                 output_file = "differential_heat_tree.pdf") # Saves the plot as a pdf file


