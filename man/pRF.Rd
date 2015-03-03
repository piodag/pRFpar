\name{pRF}
\alias{pRF}

\title{
pRF 
}
\description{

The workhorse function - estimates statistical significance of feature importance by
permuting the response variable

}
\usage{
pRF(response, predictors, n.perms, alpha = 0.05, 
mtry = NULL, type = c("classification", "regression"),
ntree = 500,seed=12345, ...)
}

\arguments{
  \item{response}{a character vector or a factor for classification containing the group memberships for classification, a numeric vector for regression

}
  \item{predictors}{
A matrix consisting of features (measurements) corresponding to samples. The orientation per se does not matter - the function orients them correctly for 
Random Forest learning. 
}
  \item{n.perms}{
Number of permutations to estimate significance. If the number of all possible
permutations is less than this the latter will be used for estimation. 

}
  \item{alpha}{
The significance level threshold of p.values for estimating false discovery rate
using the two-step BH method for correlated test statistics, as implemented in the
multtest package's mt.rawp2adjp function.
}
  \item{mtry}{
see ?randomForest for details - defines how many features are randomly sampled for building trees
}
  \item{type}{

string, set to "classification" or "regression"

}
  \item{ntree}{

number of trees in the random forest, see documentation from the randomForest package for details. 

}
  \item{seed}{
  
  set seed to ensure reproducibility from run to run and to standardise runs on actual and permuted data
  
  }
  \item{\dots}{
Arguments to pass on to the randomForest function
}
}

\value{
A standardised list containing
\item{Res.table}{A data.frame containing significance, FDR, and the feature name. b= number of permutations yielding a higher importance than observed + 1, m= number of permutations + 1}
\item{obs}{named numeric vector, contains observed importances}
\item{perms}{data.frame, contains importance values from permutations}
\item{Model}{the randomForest model that was fit to the original data}
}

\references{

The main function is based on the idea presented in

Altmann A, Tolosi L, Sander O, Lengauer T. Permutation importance: a corrected
feature importance measure. Bioinformatics. 2010 May 15;26(10):1340-7. doi:
10.1093/bioinformatics/btq134. Epub 2010 Apr 12. PubMed PMID: 20385727.

The permutation p.values in the package are exact, calculated according to

Phipson B, Smyth GK. Permutation P-values should never be zero: calculating
exact P-values when permutations are randomly drawn. Stat Appl Genet Mol Biol.
2010;9:Article39. doi: 10.2202/1544-6115.1585. Epub 2010 Oct 31. PubMed PMID:
21044043.

False discovery rates account for correlations using the Two-Step BH procedure,
initially reported in 

Yoav Benjamini, Abba M. Krieger, and Daniel Yekutieli, 'Adaptive Linear Step-up Procedures That Control the False Discovery Rate', Biometrika, 93 (2006), 491-507.


}
\author{
Ankur Chakravarthy
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

#Put together a dataframe that consists of the 
#significance stats and observed importance metrics

df<-cbind(p.test$Res.table,p.test$obs)

}