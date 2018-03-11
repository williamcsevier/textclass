#' @title Air Force Installation Contracting Agency product and service contracts
#'
#' @description
#' A dataset containing the information technology contracts supplied by AFICA from years 2011-2016. It includes
#' the text description data (description) and product service code categories (requirement category) and product service code (sub-categories).
#' The variables are as follows:
#'
#' @format dataframe with 67,365 rows and 4 variables
#' \describe{
#'   \item{document}{contract identification number}
#'   \item{psc_cat}{product service code category}
#'   \item{psc}{produce or service code subcategory}
#'   \item{description}{text description of contract requirement}
#'  }
#'
"AFICAdata"
