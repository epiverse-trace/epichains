#' COVID-19 Confirmed Cases Data for South Africa
#'
#' An aggregated subset of the COVID-19 Data Repository for South Africa
#' created, maintained and hosted by the Data Science for Social Impact (DSFSI)
#' research group, led by Dr. Vukosi Marivate at the University of Pretoria,
#' South Africa.
#'
#' The data are originally provided as a linelist of individual confirmed cases.
#' This subset consists of the first 16 days of the outbreak
#' (5 March 2020 to 20 March 2020), aggregated into a daily incidence time
#' series. See `data-raw/covid19_sa.R` for the data preparation code.
#'
#' @format ## `covid19_sa`
#' A data frame with 19 rows and 2 columns:
#' \describe{
#'   \item{date}{Date case was reported}
#'   \item{cases}{Number of cases}
#' }
#' @source <https://github.com/dsfsi/covid19za>
#' @references
#' Marivate, V. and Combrink, H. M. (2020). Use of available data to inform
#' the COVID-19 outbreak in South Africa: a case study.
#' arXiv preprint arXiv:2004.04813.
#' \doi{10.48550/arXiv.2004.04813}
"covid19_sa"
