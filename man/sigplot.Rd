\name{sigplot}
\alias{sigplot}

\title{
sigplot
}
\description{

Plots observed importance and null distribution for a pRFpar output list. 

}
\usage{
sigplot(pRFpar.list,threshold)
}

\arguments{
  \item{pRFpar.list}{ 
  output from a pRFpar run.
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

pRFpar

}
\examples{

#Load the iris dataset
data(iris)

#Set up the predictors object

predictors=iris[,c(1:4)]
colnames(predictors)<-colnames(iris[1:4])

#Execute the main pRFpar function
p.test<-pRFpar(response=factor(iris$Species),
predictors=iris[,c(1:4)],n.perms=20,mtry=3,
type="classification",alpha=0.05)

#Plot

sigplot(pRFpar.list=p.test,threshold=0.1)

}

