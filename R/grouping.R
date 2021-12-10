make_clust <- function(tab, categorical=FALSE) {
  if(categorical) {
    # need to transpose data and factorise
    x <- tab %>% as.matrix() %>% t() %>% as_tibble() %>% mutate_all(as_factor) %>% as.data.frame()
    rownames(x) <- colnames(tab)
    dist <- daisy(x, metric="gower")
  } else {
    corp <- cor(tab, use="pairwise.complete.obs") %>% 
      replace_na(0)
    dist <- as.dist(1 - abs(corp))
  }
  hclust(dist)
}


cut_clusters <- function(hc, prefix, cut.tree) {
  cutree(hc, h=cut.tree) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>% 
    set_names(c("variable", "cluster")) %>% 
    group_by(cluster) %>% 
    add_tally() %>%
    ungroup() %>% 
    mutate(cluster = sprintf("%s_%03d", prefix, as.integer(as_factor(cluster))))
}

group_clusters <- function(cut_clust, props) {
  cut_clust %>% 
    left_join(props, by="variable") %>% 
    group_by(cluster) %>% 
    arrange(missing, -unique) %>% 
    summarise(representative=first(variable), variables = paste(variable, collapse=", "), size=n(), missing=first(missing), unique=first(unique))
}



# Cluster numerical variables - compound descriptors and software predictors separately -
# into groups of similar variables, based on correlation clustering.
#
# Adds new columns to "variables":
#  - cluster - cluster ID the variables belongs to
#  - representative - logical, if true the variable represents the cluster
#  - grouped - selection of all grouped variables, that is grouped descriptors and everything else

group_variables <- function(tab, variables, cut_tree=0.25, min_unique=100) {
  vars_num <- variables %>% filter(class == "numeric" & !mis & !null & unique > min_unique)
  
  # descriptors
  desc_num <- vars_num %>% filter(predictor)
  desc_hc <- make_clust(tab[, desc_num$variable])
  desc_hc_cut <- cut_clusters(desc_hc, "ds", cut_tree)
  desc_hc_groups <- group_clusters(desc_hc_cut, desc_num) 
  
  vars <- variables %>% 
    left_join(select(desc_hc_cut, -n), by="variable") %>% 
    mutate(representative = variable %in% desc_hc_groups$representative) %>% 
    mutate(grouped = representative | !(variable %in% vars_num$variable))

  list(
    variables = vars,
    hc = desc_hc,
    hc_groups = desc_hc_groups
  )
}