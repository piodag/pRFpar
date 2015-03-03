\name{sigplot}
\alias{sigplot}

\title{
sigplot
}
\description{

Plots observed importance and null distribution for a pRF output list. 

}
\usage{
sigplot(pRF.list,threshold)
}

\arguments{
  \item{pRF.list}{ 
  output from a pRF run.
}
  \item{threshold}{
  
  False Discovery Rate cutoff for plotting. 
  Default is 0.05
  }

}
\value{

a ggplot2 plot

}

\author{
Ankur Chakravarthy
}

\seealso{

pRF

}
\examples{

#Load the iris dataset
data(iris)

#Set up the predictors object

predictors=iris[,c(1:4)]
colnames(predictors)<-colnames(iris[1:4])

#Execute the main pRF function
p.test<-pRF(response=factor(iris$Species),
predictors=iris[,c(1:4)],n.perms=20,mtry=3,
type="classification",alpha=0.05)

#Plot

sigplot(pRF.list=p.test,threshold=0.1)

}

