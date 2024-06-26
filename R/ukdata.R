#' Bivariate count data set on 5567 respondents.
#'
#' This data set is on road safety published by Department for Transport, United Kingdom. 
#' The data comprises the information about the conditions of personal injury road accidents 
#' in Great Britain and the consequential casualties on public roads. 
#' Full data set is freely availabible from http://data.gov.uk/dataset/road-accidents-safety-data.
#' First two columns are two outcomes (y1, y2). Columns 3 to 7 are four covariates.
#' @format A data frame with 14005 rows and 7 variables:
#' \describe{
#'  \item{NV}{Total number of vehicles involved in the accident}
#'  \item{NCAUS}{Number of casualties }
#'  \item{SexDriver}{Gender of the driver. Male = 1, Female = 0.}
#'  \item{Area}{Area. 0 = Urban; 1 = Rural}
#'  \item{SevFatal}{Accident severity, Fatal severity = 1 else 0}
#'  \item{SevSerius}{Accident severity, Serious severity = 1 else=0, slight severity is reference}
#'  \item{LightCon}{Light condition, Daylight = 1, Others = 0}
#' }
#' @source \url{http://hrsonline.isr.umich.edu/}
"ukdata"

