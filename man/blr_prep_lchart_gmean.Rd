% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blr-plots-data.R
\name{blr_prep_lchart_gmean}
\alias{blr_prep_lchart_gmean}
\alias{blr_prep_lchart_data}
\title{Lift Chart data}
\usage{
blr_prep_lchart_gmean(gains_table)

blr_prep_lchart_data(gains_table, global_mean)
}
\arguments{
\item{gains_table}{An object of clas \code{blr_gains_table}.}

\item{global_mean}{Overall conversion rate.}
}
\description{
Data for generating lift chart.
}
\examples{
model <- glm(honcomp ~ female + read + science, data = hsb2,
             family = binomial(link = 'logit'))
gt <- blr_gains_table(model)
globalmean <- blr_prep_lchart_gmean(gt)
blr_prep_lchart_data(gt, globalmean)

}
