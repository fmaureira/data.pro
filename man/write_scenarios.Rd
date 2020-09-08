\name{write_file_scenario}
\alias{write_file_scenario}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to write Cropsyst Scenarios split in folders ~~
}
\description{
This function write the argument of Cropsyst Scenarios given a data frame that contain all the argument to be pass to a text files of Cropsyst format.
}
\usage{
write_file_scenario<-function(rot_filename=NULL, file_name_CS=NULL,weather_file=NULL,
                               CO2=NULL,soil=NULL,
                               simulation_date=NULL, write_to_file=FALSE,
                              additional_arguments=NULL,append=FALSE,overwrite=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{write_to_file}{
    logical; should return the arguments or write file in the path simualtion.}
  \item{overwrite}{
    logical; should overwrite scenarios that already were create. see details for more information to optimize time.
}
}
\details{
  ~~ the overwrite= FALSE should be used after clean up the scenarios folder. The option is several times efficiency that overwrite=TRUE.
}
\value{
If write_to_file = FALSe return the arguments, otherwise will return nothing and write a file in the simulation path.

}
\references{
%% ~~
}
\author{
  ~~Fidel Maureira~~
}


\seealso{
%% ~~objects to See Also as \code{\link{create_scenarios}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.



