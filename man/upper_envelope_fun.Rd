\name{upper_envelope_fun}
\alias{upper_envelope_fun}
\title{Determine the envelope of data}
\usage{
upper_envelope_fun(x,n_seg=32)

}
\arguments{
  \item{x}{
object to determine the envelope
}
\item{n_seg}{
# of segments to calculated the envelope,
}
}

\value{
return the x_upper with the x vector selected in the range of the segment and the index of x

}
\description{
the number of segments will be recalculated by as.integer(length(x)/n_seg). Also the envelope will be calculated using the order of x to evaluate the local maximum within the segments.
}
\examples{
x<-rnorm(100)
plot(x)
x1<-upper_envelope_fun(x,50)
points(x1$index, x1$x_upper, col=3)

plot(x[order(x)])
x1<-upper_envelope_fun(x[order(x)],50)
points(x1$index, x1$x_upper, col=3)


}

