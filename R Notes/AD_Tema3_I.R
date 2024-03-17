rm(list=ls())
##################Tema 1#############################################

requiredPackages <- c("arsenal", "car","chemometrics","corrplot", "gapminder","dplyr","DescTools", "foreign", "e1071", "expss", "GGally", "ggplot2", "haven", 
                      "knitr","plotly", "psych","remotes", "summarytools","ggridges","table1", "tableone", "tidyverse", "SmartEDA")

sesion1 <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

sesion1(requiredPackages)
######################################################
##LOAD DATA
#Data Lending Club -https://www.kaggle.com/wordsforthewise/lending-club. Factores que determinan el Default en los créditos. Modelo de riesgo
Datalc<-read.csv("https://raw.githubusercontent.com/millerjanny/Custom_UNIR/main/Data_LendingClub.csv")
Datalc$Default=recode_factor(Datalc$Default, `1` = "Default", `0` = "Non-default")
# Exploración inicial
######################################################
# Sample mean

mean(Datalc$dti_n)
mean(Datalc$dti_n,trim=0.05)
winsor.mean(Datalc$dti_n, trim = 0.05)

mean(filter(Datalc, Datalc$Default == "Default")$dti_n)
mean(filter(Datalc, Datalc$Default == "Non-default")$dti_n)

# Dispersión

var(Datalc$annual_inc)
sd(Datalc$annual_inc)
var(Datalc$dti_n)
sd(Datalc$dti_n)
sd_trim(Datalc$dti_n,trim=0.05, const =FALSE)# const=TRUE,incluye factor de consistencia solo para distr. normal, disponible solo para trim=0.1 o 0.2
winsor.sd(Datalc$dti_n, trim = 0.05)

# Coefficient of variation

sd(Datalc$annual_inc)/mean(Datalc$annual_inc)
sd(Datalc$dti_n)/mean(Datalc$dti_n)

# Resumen de todas las variables
summary(Datalc$dti_n)
summary(Datalc)

# descriptivo de variables cuantitativas by default
by(select(Datalc, dti_n),factor(Datalc$Default),summary)
(by(select(Datalc, annual_inc, loan_amnt, int_rate, fico_n, dti_n),factor(Datalc$Default),summary))

# descriptivo de variables cuantitativas by default (mean, sd)

(cuanti_summary<-Datalc %>% tab_cols(total(label = "Total"), Default) %>% tab_cells(annual_inc, dti_n, loan_amnt, int_rate, fico_n) 
  %>%tab_stat_fun(Mean = w_mean, "Std. dev." = w_sd, "Valid N" = w_n, method = list) %>% 
    tab_pivot%>% tab_caption("Resumen de variables cuantitativas"))



