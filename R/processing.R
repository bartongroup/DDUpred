is_integer <- function(v) {
  if(!is.numeric(v)) return(FALSE)
  vn <- na.omit(v)
  sum(vn != as.integer(vn)) == 0
}

variable_properties <- function(d, cols_nolog=NULL) {
  props <- d %>%
    map_df(function(v) {
      tibble(
        class = class(v),
        good = sum(!is.na(v)),
        missing = sum(is.na(v)),
        unique = length(unique(na.omit(v))),
        integer = is_integer(v) & unique > 2,    # "integer" does not include 0/1 variables
      )
    }) %>%
    mutate(
      null = (good == 0 | unique == 1),  # useless "null" variables - all missing or only one unique value
      numeric = (class == "numeric" & unique > 2),          # proper numerical variables
      real = (class == "numeric" & unique > 2 & !integer),  # non-integer values with more than 2 levels
      numcat = (class == "numeric" & unique == 2),          # numerical variables with 2 levels are categorical
      categorical = (class == "character" & !null)          # proper categorical variables
    ) %>% 
    add_column(original_variable = names(d), .before="class") %>% 
    mutate(class = as_factor(class))
  
  cols_real <- props %>% filter(real) %>% pull(original_variable)
  log_tab <- detect_log_distribution(d[, cols_real], cols_nolog)
  
  props %>% 
    left_join(log_tab, by="original_variable") %>% 
    mutate(
      variable = if_else(is.na(variable), original_variable, variable),
      log_trans = replace_na(log_trans, FALSE)
    ) %>% 
    relocate(variable)
}


numeric_variable_summary <- function(tab) {
  tab %>% map_dfr(function(v) {
    vn <- na.omit(v)
    tibble(
      min = min(vn),
      median = median(vn),
      mean = mean(vn),
      max = max(vn),
      good = sum(!is.na(v)),
      missing = sum(is.na(v)),
      zeroes = sum(v == 0, na.rm=TRUE),
      unique = length(unique(v))
    )
  }) %>%
    add_column(variable = names(tab), .before="min")
}

categorical_variable_summary <- function(tab) {
  map_df(tab, function(v) {
    vn <- na.omit(v)
    tibble(
      good = sum(!is.na(v)),
      missing = sum(is.na(v)),
      unique = length(unique(vn)),
      levels = paste(levels(vn), collapse=","),
      top_count = max(fct_count(vn)$n)
    )
  }) %>% add_column(variable = names(tab), .before="good")
}

missing_stats <- function(d, id_cols, n_top = 30) {
  d <- d %>% select(-all_of(id_cols))
  mst <- d %>%
    map_df(function(v) tibble(ngood=sum(!is.na(v)), nbad=sum(is.na(v)))) %>%
    add_column(variable = names(d), .before="ngood") %>%
    arrange(ngood) %>% 
    head(n_top)
  nrowgood <- function(rownumber) {
    select(d, -c(mst$variable[1:rownumber])) %>% 
      na.omit %>% 
      nrow
  }
  nt <- NULL
  for(i in 1:nrow(mst)) {
    nt <- c(nt, nrowgood(i))
  }
  mst$n_row_good <- nt
  mst
}


convert_yes_no <- function(x) {
  map_chr(x, function(s) {
    if(is.na(s)) return(NA)
    if(str_detect(s, "^(Yes|No)")) {
      s <- str_remove(s, "\\s+\\(\\d+%\\)$")
    } else if(s == "NoSites") {
      s <- NA
    }
    s
  })
}

detect_log_distribution <- function(tab, cols_nolog=NULL, min_good=30, max_bad=10) {
  colnames(tab) %>%
    map_dfr(function(nm) {
      v <- tab %>% pull(nm)
      ret <- FALSE
      nbad <- sum(v <= 0, na.rm=TRUE)
      vgood <- v[!is.na(v) & v > 0]
      if(length(vgood) >= min_good & nbad <= max_bad & !(nm %in% cols_nolog)) {
        log.norm <- capture.output({
          fit.norm <- tryCatch(fitdist(vgood, "norm"), error = function(err) list(aic=1e16))
        })
        log.lnorm <- capture.output({
          fit.lnorm <- tryCatch(fitdist(vgood, "lnorm"), error = function(err) list(aic=1e16))
        })
        if(fit.norm$aic > fit.lnorm$aic) ret <- TRUE
      }
      tibble(original_variable = nm, log_trans=ret)
    }) %>%
    mutate(variable = if_else(log_trans, paste("log", original_variable), original_variable), .before="log_trans")
}

convert_log <- function(tab, props) {
  logvars <- props %>% filter(log_trans) %>% pull(original_variable)
  for(v in logvars) {
    lg <- log10(tab[, v])[[1]]
    lg[is.nan(lg) | is.infinite(lg)] <- NA
    tab[, v] <- lg
  }
  
  newnames <- tibble(original_variable = colnames(tab)) %>% 
    left_join(props, by="original_variable") %>% 
    pull(variable)
  
  colnames(tab) <- newnames
  tab
}

remove_outliers <- function(tab, sigma_limit = 5) {
  map_dfc(names(tab), function(nm) {
    v <- tab[[nm]]
    outl <- which(abs(scale(v)) > sigma_limit)
    if(length(outl) > 0) v[outl] <- NA
    tibble(v) %>% set_names(nm)
  }) 
}


# try: find the smallest pka per compound
convert_pka <- function(v) {
  v %>% 
    as.character() %>%
    str_split(";") %>% 
    map_dbl(function(x) {
      if(is.na(x[[1]])) return(NA)
      x %>% 
        as.numeric() %>% 
        min()
    })
}

variable_correlation <- function(tab) {
  ct <- cor(tab, use="pairwise.complete.obs") %>% 
    as.data.frame()
  names <- rownames(ct)
  n <- length(names)
  colnames(ct) <- 1:n
  
  ct %>% 
    as_tibble() %>% 
    add_column(id = 1:n) %>% 
    pivot_longer(-id) %>% 
    set_names(c("i1", "i2", "correlation")) %>% 
    mutate(i2 = as.integer(i2)) %>% 
    filter(i1 != i2) %>% 
    mutate(rep = i1 < i2) %>% 
    mutate(var1 = names[i1], var2 = names[i2]) %>% 
    select(var1, var2, correlation, rep)
}


# ANOVA of categorical software predicted variables vs experimental variables
anova_resp <- function(set, max_unique = 10, min_good = 100) {
  sel <- set$variables %>% 
    filter(response & !null & (package == "Experiment" | (class == "categorical" & unique < max_unique & unique > 1 & good> min_good))) %>% 
    mutate(experimental = package == "Experiment") %>% 
    select(variable, experimental)
  
  t_ex <- set$tab[, c("Name", sel %>% filter(experimental) %>% pull(variable))] %>% 
    pivot_longer(-Name, names_to="exp", values_to="y") %>% 
    drop_na()
  t_sf <- set$tab[, c("Name", sel %>% filter(!experimental) %>% pull(variable))] %>% 
    pivot_longer(-Name, names_to="soft", values_to="x") %>% 
    drop_na()
  
  inner_join(t_ex, t_sf, by="Name") %>% 
    group_by(exp, soft) %>% 
    mutate(nlev = length(unique(as.character(x)))) %>% 
    filter(nlev > 1) %>% 
    nest(data = c(Name, x, y)) %>% 
    mutate(
      fit = map(data, function(d) {lm(y ~ x, data=d) %>% aov()}),
      tidied = map(fit, broom::tidy)
    ) %>% 
    unnest(tidied) %>% 
    select(-c(data, fit)) %>% 
    filter(term == "x")
}

# Main function to process raw data

process_raw <- function(raw, info, cols_id, cols_remove=NULL, cols_nolog=NULL, cols_pka=NULL,
                              missing_row_limit = 0.95, max_missing = 100, sigma_limit = 5,
                              cut_tree=0.25, min_group_unique=100,
                              relevel_moka_status=FALSE) {
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

  list(
    all_variables = vars,
    variables = gr$variables %>% filter(variable %in% colnames(tab)),
    cols_id = cols_id,
    missing_stats = mis_stats,
    hc = gr$hc,
    hc_groups = gr$hc_groups,
    tab = tab
  )
}








process_test <- function(raw, train_vars, cols_id, cols_remove=NULL, cols_pka=NULL) {
  dg <- raw
  
  # remove specified variables
  if(!is.null(cols_remove)) dg <- dg %>% select(-all_of(cols_remove))
  
  # convert pKa strings into numbers
  if(!is.null(cols_pka)) {
    dg[, cols_pka] <- dg[, cols_pka] %>% map_dfc(~convert_pka(.x))
  }
  
  descriptor_vars <- train_vars %>% 
    filter((descriptor | software) & (is.na(cluster) | representative))
  
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
  
  list(
    variables = vars,
    cols_id = cols_id,
    missing_variables = missing_vars,
    tab = tab
  )
}


# when NAs are dropped, some of the variables might end up having
# only one level, we need to reject them before modelling
reduce_for_levels <- function(tab, min_unique) {
  tab <- drop_na(tab)
  vars <- names(tab)
  bad_vars <- vars %>% 
    map_dfr(function(vr) tibble(variable=vr, n=length(unique(tab[[vr]])))) %>% 
    filter(n < min_unique) %>% 
    pull(variable)
  tab %>% select(-all_of(bad_vars))
}

# For a given data set 'set', response variable 'resp_var' and category 'categ', create a subset of the main set, with variables with at least 'min_unique' unique values, at least 'min_good' good (non-missing) values, at most 'max_cat_levels' levels in categorical variables. The subset is done of grouped (cluster centroids) variables with no 'mis' flag. 
select_predictors_for_models <- function(set, resp_var, categ, min_unique, min_good, max_cat_levels, sel=NULL) {
  pred <- set$variables %>% 
    filter(category %in% categ & !null & !mis & grouped & good >= min_good & (class == "numeric" | unique <= max_cat_levels)) 
  if(!is.null(sel)) pred <- filter(pred, variable %in% sel)
  pred_vars <- pull(pred, variable)
  reduce_for_levels(set$tab[, c("Name", resp_var, pred_vars)], min_unique) %>% 
    rename(response = !!sym(resp_var)) %>% 
    filter(!is.na(response))
}

