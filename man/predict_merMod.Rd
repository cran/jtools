% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lme4_utils.R
\name{predict_merMod}
\alias{predict_merMod}
\title{Alternative interface for \code{merMod} predictions}
\usage{
predict_merMod(
  object,
  newdata = NULL,
  se.fit = FALSE,
  use.re.var = FALSE,
  allow.new.levels = FALSE,
  type = c("link", "response", "terms"),
  na.action = na.pass,
  re.form = NULL,
  boot = FALSE,
  sims = 100,
  prog.arg = "none",
  ...
)
}
\arguments{
\item{object}{a fitted model object}

\item{newdata}{data frame for which to evaluate
    predictions.}

\item{se.fit}{Include standard errors with the predictions? Note that
these standard errors by default include only fixed effects variance.
See details for more info. Default is FALSE.}

\item{use.re.var}{If \code{se.fit} is TRUE, include random effects variance in
standard errors? Default is FALSE.}

\item{allow.new.levels}{logical if new levels (or NA values) in
    \code{newdata} are allowed. If FALSE (default), such new values in
    \code{newdata} will trigger an error; if TRUE, then the prediction
    will use the unconditional (population-level) values for data with
    previously unobserved levels (or NAs).}

\item{type}{character string - either \code{"link"}, the default, or
    \code{"response"} indicating the type of prediction object returned.}

\item{na.action}{\code{\link{function}} determining what should be done
    with missing values for fixed effects in \code{newdata}.
    The default is to predict \code{NA}: see \code{\link{na.pass}}.}

\item{re.form}{(formula, \code{NULL}, or \code{NA}) specify which random effects to condition on when predicting.  If \code{NULL},
    include all random effects; if \code{NA} or \code{~0},
    include no random effects.}

\item{boot}{Use bootstrapping (via \code{\link[lme4:bootMer]{lme4::bootMer()}}) to estimate
variance for \code{se.fit}? Default is FALSE}

\item{sims}{If \code{boot} is TRUE, how many simulations should be run? Default
is 100.}

\item{prog.arg}{If \code{boot} and \code{se.fit} are TRUE, a character string -
type of progress bar to display. Default is "none"; the function will look
for a relevant *ProgressBar function, so "txt" will work in general;
"tk" is available if the tcltk package is loaded; or "win" on Windows
systems. Progress bars are disabled (with a message) for parallel operation.}

\item{...}{When \code{boot} and \code{se.fit} are TRUE, any additional arguments are
passed to \code{lme4::bootMer()}.}
}
\description{
This function generates predictions for \code{merMod} models, but
with the ability to get standard errors as well.
}
\details{
The developers of \pkg{lme4} omit an \code{se.fit} argument for a
reason, which is that it's not perfectly clear how best to estimate
the variance for these models. This solution is a logical one, but perhaps
not perfect. Bayesian models are one way to do better.

The method used here is based on the one described here:
\url{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions}
}
