% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rGroovy.R
\name{Evaluate}
\alias{Evaluate}
\title{Function evaluates (executes) the groovy script and returns the result.}
\usage{
Evaluate(groovyShell = NULL, groovyScript)
}
\arguments{
\item{groovyShell}{The groovyShell with which to execute the specified groovy script. Note that the groovyShell can
be NULL, however if this is NULL then the Initialize function must have been called so that a global groovyShell
instance will be available in the environment otherwise an exception is raised.}

\item{groovyScript}{The groovy script being executed.}
}
\value{
The result of the script execution.
}
\description{
Function evaluates (executes) the groovy script and returns the result.
}
\examples{
 \dontrun{
 Initialize ()
 Evaluate (groovyScript="print 'Hello world!'")
 }

}
