\name{unitization_with_zero_minimum}
\alias{unitization_with_zero_minimum}
\title{Rescale a set of data given values between 0 and 1}
\usage{
unitization_with_zero_minimum(x,na.rm=TRUE)

}
\arguments{
  \item{x}{
object to rescaled
}
\item{na.rm}{
a logical value indicating whether NA values should be stripped before the computation proceeds.
}
}

\value{
return values betwwen 0 and 1 regarding the distribution of x

}
\description{
unitization with zero minimum ((x-min)/range). Is exactly equal to data.Normalization {clusterSim} type="n4" but this fucntion accept NA values
}
\examples{
x<-c(rnorm(100),NA)
plot(unitization_with_zero_minimum(x))
}

