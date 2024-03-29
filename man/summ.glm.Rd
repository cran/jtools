% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summ.R
\name{summ.glm}
\alias{summ.glm}
\alias{j_summ.glm}
\title{Generalized linear regression summaries with options}
\usage{
\method{summ}{glm}(
  model,
  scale = FALSE,
  confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", 0.95),
  robust = getOption("summ-robust", FALSE),
  cluster = NULL,
  vifs = getOption("summ-vifs", FALSE),
  digits = getOption("jtools-digits", default = 2),
  exp = FALSE,
  pvals = getOption("summ-pvals", TRUE),
  n.sd = 1,
  center = FALSE,
  transform.response = FALSE,
  scale.only = FALSE,
  data = NULL,
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE),
  model.coefs = getOption("summ-model.coefs", TRUE),
  which.cols = NULL,
  vcov = NULL,
  ...
)
}
\arguments{
\item{model}{A \code{glm} object.}

\item{scale}{If \code{TRUE}, reports standardized regression
coefficients by scaling and mean-centering input data (the latter can be
changed via the \code{scale.only} argument). Default is \code{FALSE}.}

\item{confint}{Show confidence intervals instead of standard errors? Default
is \code{FALSE}.}

\item{ci.width}{A number between 0 and 1 that signifies the width of the
desired confidence interval. Default is \code{.95}, which corresponds
to a 95\% confidence interval. Ignored if \code{confint = FALSE}.}

\item{robust}{If not \code{FALSE}, reports heteroskedasticity-robust standard
errors instead of conventional SEs. These are also known as Huber-White
standard errors. There are several options provided by
\code{\link[sandwich:vcovHC]{sandwich::vcovHC()}}: \code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"},
\code{"HC4m"}, \code{"HC5"}.

Default is \code{FALSE}.

This requires the \code{sandwich} package to compute the
standard errors.}

\item{cluster}{For clustered standard errors, provide the column name of
the cluster variable in the input data frame (as a string). Alternately,
provide a vector of clusters. Note that you must set \code{robust} to either
"HC1", "HC2", or "HC3" in order to have clustered standard errors ("HC4"
and "HC5" are not supported.}

\item{vifs}{If \code{TRUE}, adds a column to output with variance inflation
factors (VIF). Default is \code{FALSE}.}

\item{digits}{An integer specifying the number of digits past the decimal to
report in the output. Default is 2. You can change the default number of
digits for all jtools functions with
\code{options("jtools-digits" = digits)} where digits is the desired
number.}

\item{exp}{If \code{TRUE}, reports exponentiated coefficients with
confidence intervals for exponential models like logit and Poisson models.
This quantity is known as an odds ratio for binary outcomes and incidence
rate ratio for count models.}

\item{pvals}{Show p values? If \code{FALSE}, these
are not printed. Default is \code{TRUE}.}

\item{n.sd}{If \code{scale = TRUE}, how many standard deviations should
predictors be divided by? Default is 1, though some suggest 2.}

\item{center}{If you want coefficients for mean-centered variables but don't
want to standardize, set this to \code{TRUE}. Note that setting this to
false does not affect whether \code{scale} mean-centers variables. Use
\code{scale.only} for that.}

\item{transform.response}{Should scaling/centering apply to response
variable? Default is \code{FALSE}.}

\item{scale.only}{If you want to scale but not center, set this to \code{TRUE}.
Note that for legacy reasons, setting \code{scale = TRUE} and \code{center = FALSE}
will not achieve the same effect. Default is \code{FALSE}.}

\item{data}{If you provide the data used to fit the model here, that data
frame is used to re-fit the model (if \code{scale} is \code{TRUE})
instead of the \code{\link[stats:model.frame]{stats::model.frame()}}
of the model. This is particularly useful if you have variable
transformations or polynomial terms specified in the formula.}

\item{model.info}{Toggles printing of basic information on sample size,
name of DV, and number of predictors.}

\item{model.fit}{Toggles printing of model fit statistics.}

\item{model.coefs}{Toggles printing of model coefficents.}

\item{which.cols}{Developmental feature. By providing columns by name,
you can add/remove/reorder requested columns in the output. Not fully
supported, for now.}

\item{vcov}{You may provide your own variance-covariance matrix for the
regression coefficients if you want to calculate standard errors in
some way not accommodated by the \code{robust} and \code{cluster} options.}

\item{...}{Among other things, arguments are passed to \code{\link[=scale_mod]{scale_mod()}} or
\code{\link[=center_mod]{center_mod()}} when \code{center} or \code{scale} is \code{TRUE}.}
}
\value{
If saved, users can access most of the items that are returned in
the output (and without rounding).

\item{coeftable}{The outputted table of variables and coefficients}
\item{model}{The model for which statistics are displayed. This would be
most useful in cases in which \code{scale = TRUE}.}

Much other information can be accessed as attributes.
}
\description{
\code{summ()} prints output for a regression model in a fashion similar to
\code{summary()}, but formatted differently with more options.
}
\details{
By default, this function will print the following items to the
console:
\itemize{
\item The sample size
\item The name of the outcome variable
\item The chi-squared test, (Pseudo-)R-squared value and AIC/BIC.
\item A table with regression coefficients, standard errors, z values, and
p values.
}

There are several options available for \code{robust}. The heavy
lifting is done by \code{\link[sandwich:vcovHC]{sandwich::vcovHC()}}, where those are better
described.
Put simply, you may choose from \code{"HC0"} to \code{"HC5"}. Based on the
recommendation of the developers of \pkg{sandwich}, the default is set to
\code{"HC3"}. Stata's default is \code{"HC1"}, so that choice may be better
if the goal is to replicate Stata's output. Any option that is understood by
\code{vcovHC()} will be accepted. Cluster-robust standard errors are
computed
if \code{cluster} is set to the name of the input data's cluster variable
or is a vector of clusters.

The \code{scale} and \code{center} options are performed via
refitting
the model with \code{\link[=scale_mod]{scale_mod()}} and \code{\link[=center_mod]{center_mod()}},
respectively. Each of those in turn uses \code{\link[=gscale]{gscale()}} for the
mean-centering and scaling.
}
\examples{
 ## Dobson (1990) Page 93: Randomized Controlled Trial :
 counts <- c(18,17,15,20,10,20,25,13,12)
 outcome <- gl(3,1,9)
 treatment <- gl(3,3)
 print(d.AD <- data.frame(treatment, outcome, counts))
 glm.D93 <- glm(counts ~ outcome + treatment, family = poisson)

 # Summarize with standardized coefficients
 summ(glm.D93, scale = TRUE)

}
\references{
King, G., & Roberts, M. E. (2015). How robust standard errors expose
methodological problems they do not fix, and what to do about it.
\emph{Political Analysis}, \emph{23}(2), 159–179.
\doi{10.1093/pan/mpu015}

Lumley, T., Diehr, P., Emerson, S., & Chen, L. (2002). The Importance of the
Normality Assumption in Large Public Health Data Sets. \emph{Annual Review
of
Public Health}, \emph{23}, 151–169.
\doi{10.1146/annurev.publhealth.23.100901.140546}
}
\seealso{
\code{\link[=scale_mod]{scale_mod()}} can simply perform the standardization if
preferred.

\code{\link[=gscale]{gscale()}} does the heavy lifting for mean-centering and scaling
behind the scenes.

Other summ: 
\code{\link{summ.lm}()},
\code{\link{summ.merMod}()},
\code{\link{summ.rq}()},
\code{\link{summ.svyglm}()}
}
\author{
Jacob Long \email{jacob.long@sc.edu}
}
\concept{summ}
