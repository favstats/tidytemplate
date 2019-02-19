#'
#' This function saves a ggplot with the same name as the object.
#'
#'
#' @export

ggsave_it <- function(x, plot = ggplot2::last_plot(), ...){
  if(!dir.exists("images")){
    dir.create("images")
  }
  ggplot2::ggsave(filename = paste0("images/", deparse(substitute(x)), ".png"), plot = plot, ...)
}

#'
#' This function saves a ggplot with the same name as the object.
#'
#'
#' @export

save_it <- function(x){
  if(!dir.exists("data")){
    dir.create("data")
  }
  save(x, file = paste0("data/", lazyeval::expr_find(x), ".Rdata"))
}

#'
#' This function loads a dataset with the same name as the object.
#'
#'
#' @export

load_it <- function(x) {
  get(load(x))
}


#'
#' This function standardizes a variable from 0 to 1.
#'
#'
#' @export

range01 <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}


#'
#' This function creates a tidy template.
#'
#'
#' @export

open_template <- function(filename = "Untitled1") {

tidypath_fragment <- "/tidytemplate/rmarkdown/templates/template-name/skeleton/skeleton.Rmd"

tidypath <- paste0(.libPaths(), tidypath_fragment)[1]

wdpath <- paste0(getwd(), "/", filename, ".Rmd")

if (file.exists(tidypath)) {
  file.copy(from = tidypath, to = wdpath)
  file.edit(wdpath)
  }
}


#'
#'
#' Get Stars from P-Values
#'
#' @export

get_stars <- function (pval) {
  dplyr::case_when(
    is.na(pval) ~ "", 
    pval < 0.001 ~ "***", 
    pval < 0.01 ~ "**", 
    pval < 0.05 ~ "*", 
    pval < 0.10 ~ "^", 
    TRUE ~ "")
}



#'
#'
#' Get Labels from P-Values
#'
#' @export

get_plabs <- function (pval) {
  dplyr::case_when(is.na(pval) ~ "", 
                   pval < 0.001 ~ "p < 0.001",
                   pval < 0.01 ~ "p < 0.01", 
                   pval < 0.05 ~ "p < 0.05", 
                   pval < 0.1 ~ "p < 0.10", 
                   TRUE ~ "p > 0.10")
}

#'
#'
#' Get percentage helper function
#'
#' @export

get_percentage <- function(part, total, digits = NULL) {
  
  if(!is.numeric(part) | !is.numeric(total)){
    cat("Non-numeric variables.. converting\n")
    part <- as.numeric(part)
    total <- as.numeric(total)
  }
  val <- part / total * 100
  if(!is.null(digits)){
    val <- round(val, digits)
  }
  return(val) 
}

#'
#'
#' Get R Squared with percentage (broom)
#'
#' @export


get_r2_label <- function(mod, digits = 3, adj = F){
  if(!adj){#'
    message("R Squared\n")
    r2 <- broom::glance(mod) %>%
      mutate(r.squared = r.squared * 100) %>% 
      .$r.squared %>% 
      format(., digits = digits) %>% 
      paste0(., "%")
    return(r2)
  } else {
    message("Getting Adjusted R Squared\n")
    r2 <- broom::glance(mod) %>%
      mutate(adj.r.squared = adj.r.squared * 100) %>% 
      .$adj.r.squared %>% 
      format(., digits = digits) %>% 
      paste0(., "%")    
    return(r2)
  }
}


#'
#'
#' Identify an SEM Model
#'
#' @export

identify <- function(fit, var_number = NULL, parameters = NULL) {
  
  if (is.null(var_number) & is.null(parameters)) { 
    
    parameters <- parameterestimates(fit) %>% 
      select(pvalue) %>% 
      na.omit %>% 
      nrow()
    
    var_number <- lavaan::lav_partable_independence(fit) %>% 
      .$lhs %>% 
      length()
  }
  
  p <- var_number*(var_number + 1)/2
  i <- p - parameters
  cat(paste0("\n Total Number of Datapoints: ", p))
  if (i > 0) {
    cat("\n Modell ist überidentifiziert \n")
  } 
  if (i == 0) {
    cat("\n Modell ist identifiziert \n")
  } 
  if (i < 0) {
    message("\n Modell ist unteridentifiziert \n")
  } 
  return(list(p = p, i = i))
}


#' Create table with standardized estimates
#'
#' This function creates a stargazer table
#' with standardized estimates
#'
#' @param model put in the model (lm object) here.
#' @param ... stargazer options.
#'
#' @return returns a stargazer table.
#' @examples
#' # #fake data
#' var1<-rnorm(100, mean=10, sd=5)
#' var2<-rnorm(100, mean=5, sd=2)
#' var3<-rnorm(100, mean=2, sd=3)
#' var4<-rnorm(100, mean=5, sd=1)
#' df<-data.frame(var1, var2, var3, var4)
#' #model with unstandardized betas
#' model1<-lm(var1~var2+var3+var4, data=df)
#'
#' tbl_std(model1)
#' @export

tbl_std <- function(model, type = "text", ...) {
  #Standardized betas
  model1.beta <- suppressWarnings(suppressMessages(lm.beta(model)))
  #print
  #model1.beta$standardized.coefficients[1] <- NA
  #coef(summary(model1.beta))[, "Std. Error"]  <- NA
  
  suppressWarnings(suppressMessages(stargazer(model, model1.beta, type = type,
            coef = list(model$coefficients, 
                        model1.beta$standardized.coefficients),
            column.labels = c("b", "std.b"), table.placement = "ht!",
            header = F, ...)))
}






#' Get Percentages and Frequencies
#'
#' This is a function that can be used in a pipe-workfolow
#' in order to get grouped frequencies and percentages
#'
#'
#' @param x add in all grouping variables
#'
#' @return returns a dataframe with percentages and frequencies
#' @examples
#'
#' iris %>% 
#'   freqs(Species)
#' 
#' @export

freqs <- function(x, ...) {
  g <- group_vars(x)
  x <- group_by(x, ..., add = T)
  x <- summarize(x, n = n())
  x <- mutate(x, freq = n / sum(n))
  x <- ungroup(x)
  x <- complete(x, ..., fill = list(n = 0, freq = 0))
  grouped_df(x, g)
}


#' Get Tidy Estimates
#'
#'
#' 
#' @export
#' 
tidy_wide <- function(model) {
  model_wide <- tidy(model) %>% # tidy extrahiert die Parameter
    select(term, estimate) %>%    # wir brauchen nur die logit-koeffs
    spread(term, estimate) %>%    # konvertieren in wide format
    rename(intercept = `(Intercept)`) # umbenennen
  return(model_wide)            # gib model_wide aus
}


#' Get Tidy Estimates
#'
#'
#' 
#' @export
#' 
tidy_wide <- function(model) {
  model_wide <- tidy(model) %>% # tidy extrahiert die Parameter
    select(term, estimate) %>%    # wir brauchen nur die logit-koeffs
    spread(term, estimate) %>%    # konvertieren in wide format
    rename(intercept = `(Intercept)`) # umbenennen
  return(model_wide)            # gib model_wide aus
}

#' Turns logits to probability
#'
#'
#' 
#' @export
#' 
logit2prob <- function(logit) { # logit ist der input
  odds <- exp(logit)          # e hoch logit = odds
  prob <- odds / (1 + odds)   # odds / 1 + odds = Wahrscheinlichkeit
  return(prob)               # gibt Wahrscheinlichkeit zurück
}

#' Turns logits to probability
#'
#'
#' 
#' @export
#' 
logit2prob <- function(logit) { # logit ist der input
  odds <- exp(logit)          # e hoch logit = odds
  prob <- odds / (1 + odds)   # odds / 1 + odds = Wahrscheinlichkeit
  return(prob)               # gibt Wahrscheinlichkeit zurück
}


#' Plot Average Marginal Effect Coefficient Plot
#'
#'
#' 
#' @export
#' 
plot_ame <- function(mod1, nudge_y = 0.05, nudge_x = 0.1) {
  
  model_dat <- margins(mod1) %>% summary()
  
  model_dat %>%
    mutate(AME = AME * 100) %>% 
    mutate(lower = lower * 100) %>%
    mutate(upper = upper * 100) %>% 
    mutate(stars = tidytemplate::get_stars(p)) %>% 
    mutate(AME_label = round(AME, 2)) %>% 
    mutate(AME_label = paste0(AME_label, stars)) %>% 
    ggplot() +
    geom_point(aes(factor, AME)) +
    geom_errorbar(aes(x = factor, ymin = lower, ymax = upper), width = 0) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", alpha = 0.75) +
    geom_text(aes(factor, AME, label = AME_label), nudge_y = nudge_y, nudge_x = nudge_x) +
    theme_minimal() +
    coord_flip() +
    labs(x = "", y = "Average Marginal Effect (AME)", title = "Average Marginal Effects Plot")
}



#' Parse UTC Date
#'
#'
#' 
#' @export
#' 
parse_utc_date <- function(date) {
  str_extract(date,"\\d{4}-\\d{2}-\\d{2}T\\d{2}\\:\\d{2}\\:\\d{2}....Z") %>% as_date
}

#' T-Test Report
#'
#'
#' 
#' @export
#' 
t.report <- function(tt){
  tvalue <- tt$statistic %>% formatC(digits = 2, format = "f")
  pvalue <- tt$p.value %>% formatC(digits = 2, format = "f")
  # if (round(tt$parameter, 0) == tt$parameter) {
  df <- tt$parameter
  # } else {
  # df <- formatC(digits = 2, format = "f")
  # }
  if (tt$p.value < 0.0005) {
    pvalue <- " < 0.001" 
  } else { 
    if (tt$p.value < 0.005) {
      pvalue <- paste0(" = ",tt$p.value %>% formatC(digits = 3, format = "f"))
    } else {
      pvalue <- paste0(" = ",tt$p.value %>% formatC(digits = 2, format = "f"))
    }
  } 
  paste0("t-test: V",df," = ",tvalue, ", p", pvalue)
}

#' Cohen's Report
#'
#'
#' 
#' @export
#' 
cohen_d_report <- function(res_cohen) {
  cohen_d <- res_cohen$estimate %>% formatC(digits = 2, format = "f")
  cohen_lower <- res_cohen$conf.int[1] %>% formatC(digits = 2, format = "f")
  cohen_upper <- res_cohen$conf.int[2] %>% formatC(digits = 2, format = "f")
  cohen_size <- res_cohen$magnitude
  
  paste0("Cohen's d = ", cohen_d, " [", cohen_lower, "-", cohen_upper, "]; ", 
         "Effect Size: '", res_cohen$magnitude, "'")
}