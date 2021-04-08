#' Histogram to Visualize Confidence Intervalls
#'
#' The TeachHistConfInterv function is a variation of the TeachHistDens function of this package. It generates a histogram with two horizontal axis. One axis displays z-values the other one the
#' dimension of the displayed variable and it is optimized to visualize confidence intervalls.
#'
#'
#' The TeachHistConfInterv function supports confidence intervals based on the t-distribution (IsSdEstimated=TRUE) and the z-distribution (IsSdEstimated=FALSE).
#'
#' @param Mean (default=0) Value for sample mean.
#' @param StandardError (default=1) Standard error from sample.
#' @param DegreeFreedom (default=NULL) required for t-test (IsSdEstimated=TRUE)
#' @param Confidence (default=0.95) Confidence level.
#' @param TestType  (default="BothTails") User can also choose TestType="LeftTail" or TestType="RightTail"
#' @param IsSdEstimated (default=FALSE) standard deviation or standard error are known. Set to FALSE, if estimated.
#' @param NOfSimData (default=10000) Number of simulated data points. Higher NOfSimData results in better precision but possibly not all points are considered.
#' @param XAxisMax (default=5) X-axis ranges from negative to positive XAxisMax. If XAxisMax is not high enough, some of the simulated points might not be considered.
#' @param LabelSize (default=3) Size of the labels inside the diagram.
#' @param BinWidth (default= 0.5) BinWidth expressed in standard deviations.
#' @param PrintDensities  (default=FALSE) Set PrintDensities=TRUE to print densities in the histogram.
#' @param PrintRelFreq (default=TRUE) Set PrintRelFreq=FALSE to supress the printing of relative frequencies in the histogram.
#' @param PlotNormCurv (default=TRUE) If TRUE normal curve is plotted
#' @param RandVarName (default="Means") Variable name that is plotted in histogram at x-axis.
#' @param NOfSimData (default=10000) Number of simulated data points. Higher NOfSimData results in better precision but possibly not all points are considered.
#' @param SeedValue (default=NULL) Can be any integer and makes simulated points reproduceable.
#' @return Returns a histogram with confidence interval
#' @importFrom ggplot2 ggplot stat_function geom_segment geom_histogram aes scale_x_continuous scale_y_continuous xlab labs geom_vline geom_text
#' @importFrom graphics plot
#' @importFrom stats dnorm qnorm qt rnorm sd

#' @import dplyr
#' @examples
#' TeachHistConfInterv()
#' TeachHistConfInterv(Mean=2, StandardError=0.2,  IsSdEstimated = FALSE)
#' TeachHistConfInterv(Mean=2, StandardError=0.2, DegreeFreedom = 35,  IsSdEstimated = TRUE)
#' @export
TeachHistConfInterv<- function(
  Mean=0,
  StandardError=1,
  DegreeFreedom=NULL,
  Confidence=0.95,
  TestType="BothTails",
  IsSdEstimated=FALSE,
  NOfSimData=10000,
  XAxisMax=5,
  LabelSize=3,
  BinWidth=0.5, #in sd
  PrintDensities=FALSE,
  PrintRelFreq=TRUE,
  PlotNormCurv=TRUE,
  RandVarName="Means",
  SeedValue=NULL
)
{
TeachHistHypTest(
  NullHyp = Mean,
  SampleMean = Mean,
  StandardError=StandardError,
  Confidence=Confidence,
  TestType="BothTails",
  IsSdEstimated=IsSdEstimated,
  DegreeFreedom = DegreeFreedom,
  NOfSimData=NOfSimData,
  XAxisMax=XAxisMax,
  LabelSize=LabelSize,
  BinWidth=BinWidth, #in sd
  PrintDensities=PrintDensities,
  PrintRelFreq=PrintRelFreq,
  PlotNormCurv=PlotNormCurv,
  RandVarName=RandVarName,
  SeedValue=SeedValue
)
}
