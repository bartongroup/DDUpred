# Main function to process raw data

process_training_data <- function(raw, info, cols_id, cols_remove=NULL, cols_nolog=NULL, cols_pka=NULL,
                        missing_row_limit = 0.95, max_missing = 100, sigma_limit = 5,
                        cut_tree=0.25, min_group_unique=100,
                        relevel_moka_status=TRUE, verbose=FALSE) {
  
  if(verbose) cat("Processing training data\n")
  
  dg <- raw
  
  # remove specified variables
  if(!is.null(cols_remove)) dg <- dg %>% select(-all_of(cols_remove))
  
  # properties of all variables
  props <- variable_properties(dg %>% select(-all_of(cols_id)), cols_nolog = cols_nolog) %>% 
    left_join(select(info, variable, package, quantity), by=c("original_variable" = "variable")) %>% 
    mutate(response = !is.na(package))
  
  # find variables that lead to no more than missing_row_limit missing rows
  # and with no more than max_missing missing values
  # response variables are kept anyway
  min_good <- floor(nrow(raw) * missing_row_limit)
  mis_stats <- missing_stats(raw, cols_id)
  cols_mis <- mis_stats %>% 
    filter(n_row_good < min_good & nbad > max_missing) %>% 
    pull(variable)
  props <- props %>% mutate(mis = original_variable %in% cols_mis)
  
  # find useless "null" variables - all missing or only one unique value
  cols_null <- props %>% filter(null) %>% pull(original_variable)
  dg <- dg %>% select(-all_of(cols_null))
  
  cols_real <- props %>% filter(!null & real) %>% pull(original_variable)
  cols_int <- props %>% filter(!null & integer) %>% pull(original_variable)
  cols_numcat <- props %>% filter(!null & numcat) %>% pull(original_variable)
  cols_cat <- props %>% filter(!null & categorical) %>% pull(original_variable)

  # make tables for ids, numerical and categorical variables
  d_id <- dg[, cols_id]
  d_real <- dg[, cols_real]
  d_int <- dg[, cols_int]
  d_cat <- dg[, cols_cat] %>% 
    mutate_all(convert_yes_no) %>%
    bind_cols(select(dg, all_of(cols_numcat))) %>% 
    mutate_all(as_factor)
  
  # Convert some numerical variables into logarithms; change names
  d_numlog <- convert_log(d_real, props) %>% 
    remove_outliers(sigma_limit) %>% 
    bind_cols(d_int)
  
  var2log <- set_names(props$variable, props$original_variable)
  
  # detailed summaries of numerical and categorical variables
  num_props <- numeric_variable_summary(d_numlog) 
  cat_props <- categorical_variable_summary(d_cat)
  
  # separate response and predictor variables
  cols_exp <- props %>% filter(!null & package == "Experiment")
  cols_resp_num <- props %>% filter(!null & response & numeric) %>% pull(variable)
  cols_resp_cat <- props %>% filter(!null & response & categorical) %>% pull(variable)
  cols_pred_num <- props %>% filter(!null & !response & numeric) %>% pull(variable)
  cols_pred_cat <- props %>% filter(!null & !response & categorical) %>% pull(variable)
  d_resp <- bind_cols(d_cat[, cols_resp_cat], d_numlog[, cols_resp_num])
  d_pred <- bind_cols(d_cat[, cols_pred_cat], d_numlog[, cols_pred_num])
  
  # full table of data: id columns, response columns and predictor columns
  tab <- bind_cols(d_id, d_resp, d_pred)
  
  # Moka_status should be ordered
  if(relevel_moka_status) {
    tab$Moka_status <- fct_relevel(tab$Moka_status, "Acid", "Neutral", "Base")
  }
  
  # all variable info
  vars <- props %>% 
    left_join(bind_rows(cat_props, num_props) %>% select(variable, levels, top_count, zeroes), by="variable") %>% 
    mutate(
      category = if_else(is.na(package), "descriptor", "software")
    ) %>% 
    # experimental values are not "software"
    mutate(category = as_factor(if_else(category == "software" & package == "Experiment", as.character(NA), category))) %>%  
    mutate(descriptor = category == "descriptor", software = category == "software")
  
  gr <- group_variables(tab, vars, cut_tree, min_group_unique)
  final_vars <- gr$variables %>% filter(variable %in% colnames(tab))
  
  if(verbose) {
    cat("  Variables found:\n")
    cat(sprintf("    %3d not usable\n", length(cols_null)))
    cat(sprintf("    %3d real\n", length(cols_real)))
    cat(sprintf("    %3d of them converted to logarithm\n", sum(props$log_trans)))
    cat(sprintf("    %3d integer\n", length(cols_int)))
    cat(sprintf("    %3d numeric converted into categorical\n", length(cols_numcat)))
    cat(sprintf("    %3d categorical\n", length(cols_cat)))
    cat(sprintf("    %3d experimental\n", length(cols_exp)))
    cat("  Grouping of similar variables perfomed:\n")
    cat(sprintf("    %3d variables selected for downstream analysis\n", sum(final_vars$grouped)))
  }
  
  list(
    all_variables = vars,
    variables = final_vars,
    cols_id = cols_id,
    missing_stats = mis_stats,
    hc = gr$hc,
    hc_groups = gr$hc_groups,
    tab = tab
  )
}



process_test_data <- function(raw, train_vars, cols_id, cols_remove=NULL, cols_pka=NULL, relevel_moka_status=TRUE, verbose=FALSE) {
  if(verbose) cat("Processing test data\n")
  
  dg <- raw
  
  # remove specified variables
  if(!is.null(cols_remove)) dg <- dg %>% select(-all_of(cols_remove))
  
  # convert pKa strings into numbers
  if(!is.null(cols_pka)) {
    dg[, cols_pka] <- dg[, cols_pka] %>% map_dfc(~convert_pka(.x))
  }
  
  descriptor_vars <- train_vars %>% 
    filter((descriptor | software) & grouped)
  
  test_vars <- colnames(dg)
  
  # properties of all variables
  props <- descriptor_vars %>% 
    filter(original_variable %in% test_vars)
  
  good_vars <- props$original_variable
  missing_vars <- setdiff(descriptor_vars$original_variable, test_vars)
  
  cols_real <- props %>% filter(!null & real) %>% pull(original_variable)
  cols_int <- props %>% filter(!null & integer) %>% pull(original_variable)
  cols_numcat <- props %>% filter(!null & numcat) %>% pull(original_variable)
  cols_cat <- props %>% filter(!null & categorical) %>% pull(original_variable)
  
  # make tables for ids, numerical and categorical variables
  d_id <- dg[, cols_id]
  d_real <- dg[, cols_real]
  d_int <- dg[, cols_int]
  d_cat <- dg[, cols_cat] 
  if(ncol(d_cat) > 0) {
    d_cat <- d_cat %>% 
      mutate_all(convert_yes_no) %>%
      bind_cols(select(dg, all_of(cols_numcat))) %>% 
      mutate_all(as_factor)
  }
  
  # Convert some numerical variables into logarithms; change names
  d_numlog <- convert_log(d_real, props) %>% 
    bind_cols(d_int)
  
  var2log <- set_names(props$variable, props$original_variable)
  
  # detailed summaries of numerical and categorical variables
  num_props <- numeric_variable_summary(d_numlog) 
  cat_props <- categorical_variable_summary(d_cat)
  
  # full table of data: id columns, response columns and predictor columns
  tab <- bind_cols(d_id, d_numlog, d_cat)
  
  # Moka_status should be ordered
  if(relevel_moka_status) {
    tab$Moka_status <- fct_relevel(tab$Moka_status, "Acid", "Neutral", "Base")
  }
  
  # all variable info
  vars <- props %>% 
    left_join(bind_rows(cat_props, num_props) %>% select(variable, levels, top_count, zeroes), by="variable")

  if(verbose) {
    cat("  Variables found:\n")
    cat(sprintf("    %3d variables in test set\n", length(test_vars)))
    cat(sprintf("    %3d selected variables in the training set\n",nrow(descriptor_vars)))
    cat(sprintf("    %3d test variables match training set\n", nrow(vars)))
  }
    
  list(
    variables = vars,
    cols_id = cols_id,
    missing_variables = missing_vars,
    tab = tab
  )
}

