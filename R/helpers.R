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
#' This function saves a dataset with the same name as the object.
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
#' tbl_std(model1, type = "text")
#' @export

tbl_std <- function(model, type = "text", ...) {
  #Standardized betas
  model1.beta <- suppressWarnings(suppressMessages(lm.beta(model)))
  #print
  #model1.beta$standardized.coefficients[1] <- NA
  #coef(summary(model1.beta))[, "Std. Error"]  <- NA
  
  suppressWarnings(suppressMessages(stargazer(model, model1.beta,
            coef = list(model$coefficients, type = type,
                        model1.beta$standardized.coefficients),
            column.labels = c("b", "std.b"), table.placement = "ht!",
            header = F, ...)))
}
