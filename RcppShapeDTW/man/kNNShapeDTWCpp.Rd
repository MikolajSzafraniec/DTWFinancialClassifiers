\name{kNNShapeDTWCpp}
\alias{kNNShapeDTWCpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return results of shapeDTW kNN classification
}
\description{
General workflow for the shape DTW kNN algorithm implemented in the Rcpp.
It takes two matrices as the input which represents two multidimensional
time series - reference series and test series - and find nearest neighbour
for the reference series among the several subsequences of the test series.
}
\usage{
kNNShapeDTWCpp(NumericMatrix referenceSeries,
                    NumericMatrix testSeries,
                    int forecastHorizon,
                    int subsequenceWidth,
                    int subsequenceBreaks,
                    S4 shapeDescriptorParams,
                    std::string normalizationType = "Unitarization",
                    std::string distanceType = "Dependent",
                    Rcpp::Nullable<S4> ttParams = R_NilValue)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
