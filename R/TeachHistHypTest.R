#' Histogram to Visualize Hypothesis Tests
#'
#' The TeachHistHypTest function is a variation of the TeachHistDens function of this package. It generates a histogram with two horizontal axis. One axis displays z-values the other one the
#' dimension of the displayed variable and it is optimized to visualize hypothesis testing.
#'
#'
#' The TeachHistHypTest function upports t-test (IsSdEstimated=TRUE) and z-test (IsSdEstimated=FALSE). It also supports
#' TestType="BothTails", TestType="LeftTail", and TestType="RightTail".
#' @param NullHyp required! Value for null hypotheses.
#' @param StandardError required!
#' @param SampleMean required! SampleMean is the mean from the research sample to support the alternative/research hypotheses.
#' @param DegreeFreedom required for t-test (IsSdEstimated=TRUE)
#' @param Confidence  (default=0.950 Confidence level.
#' @param TestType  (default="BothTails") User can also choose TestType="LeftTail" or TestType="RightTail"
#' @param IsSdEstimated  (default=FALSE) standard deviation or standard error are known. Set to FALSE, if estimated.
#' @param NOfSimData  (default=10000) Number of simulated data points. Higher NOfSimData results in better precision but possibly not all points are considered.
#' @param XAxisMax  (default=5) X-axis ranges from negative to positive XAxisMax. If XAxisMax is not high enough, some of the simulated points might not be considered.
#' @param AxisFontSize (default=10) Font size for axis labels.
#' @param LabelSize  (default=3) Size of the labels inside the diagram.
#' @param BinWidth  (default= 0.51) BinWidth expressed in standard deviations.
#' @param PrintDensities  (default=FALSE) Set PrintDensities=TRUE to print densities in the histogram.
#' @param PrintRelFreq  (default=TRUE) Set PrintRelFreq=FALSE to suppress the printing of relative frequencies in the histogram.
#' @param PlotNormCurv  (default=TRUE) If TRUE normal curve is plotted
#' @param RandVarName  (default="Means") Variable name that is plotted in histogram at x-axis.
#' @param SeedValue  (default=NULL) Can be any integer and makes simulated points reproducible.
#' @return Returns a histogram
#' @importFrom ggplot2 ggplot stat_function geom_segment geom_histogram aes scale_x_continuous scale_y_continuous xlab labs geom_vline geom_text
#' @importFrom graphics plot
#' @importFrom stats dnorm qnorm qt rnorm sd
#'
#' @import dplyr
#' @examples
#' TeachHistHypTest(70,2,77)
#' TeachHistHypTest(70,2,77, DegreeFreedom = 35,  IsSdEstimated = TRUE)
#' TeachHistHypTest(70,2,65, DegreeFreedom = 35,  IsSdEstimated = TRUE, TestType = "LeftTail")
#' @export
TeachHistHypTest<- function(
  NullHyp,
  StandardError,
  SampleMean,
  DegreeFreedom=NULL,
  Confidence=0.95,
  TestType="BothTails",
  IsSdEstimated=FALSE,
  NOfSimData=10000,
  XAxisMax=5,
  AxisFontSize=10,
  LabelSize=3,
  BinWidth=0.5, #in sd
  PrintDensities=FALSE,
  PrintRelFreq=TRUE,
  PlotNormCurv=TRUE,
  RandVarName="Means",
  SeedValue=NULL
  )
{
'..count..'<- '..density..' <- YPoint <- x <- NULL
geom_point=NULL
XAxisMax=ifelse((BinWidth==2|BinWidth==3),6,XAxisMax)

Alpha=1-Confidence

######### z-test (sd known) #############
if(IsSdEstimated==FALSE)
{
  if(TestType=="BothTails")
  {
    z=qnorm(Alpha/2)
    MarginOfError=abs(z*StandardError)
  }
  if(TestType=="LeftTail"||TestType=="RightTail")
  {
    z=qnorm(Alpha)
    MarginOfError=abs(z*StandardError)
  }
}

######### t-test (sd estimated) #############

if(IsSdEstimated==TRUE)
{
  if(!is.numeric(DegreeFreedom)){warning("t-tests require argument DegreeFreedom=")}
  if(TestType=="BothTails")
  {
    t=qt(Alpha/2,df=DegreeFreedom-1)
    MarginOfError=abs(t*StandardError)
  }
  if(TestType=="LeftTail"||TestType=="RightTail")
  {
    t=qt(Alpha,df=DegreeFreedom-1)
    MarginOfError=abs(t*StandardError)
  }
}

#### Graphing the Result
if(is.numeric(SeedValue)){set.seed(SeedValue)}

PlotData=dplyr::tibble(Means=rnorm(NOfSimData,NullHyp,StandardError))
PlotData=dplyr::filter(PlotData,PlotData[,1]>NullHyp-StandardError*XAxisMax&PlotData[,1]<NullHyp+StandardError*XAxisMax)
NOfSimData=nrow(PlotData)
if (abs(nrow(PlotData)-NOfSimData)/NOfSimData>0.05)
{

  warning("More than 5% of data for histogram were truncated. Consider to increase XAxisLimitRange parameter.")
}

BreakPointsForHist=seq(NullHyp-XAxisMax*StandardError,NullHyp+XAxisMax*StandardError,by=StandardError*BinWidth)
BreakPointsForHistZValues=seq(-XAxisMax,XAxisMax,BinWidth)



DigitsLabels=ifelse(max(abs(BreakPointsForHist))>=100,0,1)
if(min(abs(BreakPointsForHist))<=0.1){DigitsLabels=3}


MyLabels <- paste(BreakPointsForHistZValues,round(BreakPointsForHist,DigitsLabels), sep = "\n")

FirstColName=colnames(PlotData)[1]
ClPlot=ggplot2::ggplot(PlotData, ggplot2::aes(x=.data[[FirstColName]]))+
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..),breaks = BreakPointsForHist,fill="yellow",
                 color="orange", bins=XAxisMax/BinWidth*2)+
  ggplot2::scale_x_continuous(breaks = BreakPointsForHist, labels = MyLabels,
                     limits = c(min(NullHyp-XAxisMax*StandardError,SampleMean),max(NullHyp+XAxisMax*StandardError,SampleMean)),
                     expand = c(0,0)
                     )+
  ggplot2::scale_y_continuous(expand = c(0,0,0.05,0))+
  ggplot2::theme(axis.text = ggplot2::element_text(size = AxisFontSize))+
  ggplot2::theme(axis.title = ggplot2::element_text(size = AxisFontSize))

if(PrintRelFreq)
{
  ClPlot=ClPlot+ggplot2::geom_text(ggplot2::aes(y=..density../2,label=round(..count../sum(..count..),2)), stat = "bin",
                                   colour="black", size=LabelSize,breaks=BreakPointsForHist)
}

if(PrintDensities)
{
  ClPlot=ClPlot+ggplot2::geom_text(ggplot2::aes(y=..density..+0.01,label=round(..density..,2)), stat = "bin",
                                   colour="black", size=LabelSize,breaks=BreakPointsForHist)
}

if(IsSdEstimated)
{
ClPlot=ClPlot+ggplot2::xlab(paste("Upper Axis: t","Lower Axis: ", colnames(PlotData[1])))
}else{
ClPlot=ClPlot+ggplot2::xlab(paste("Upper Axis: z","Lower Axis: ", colnames(PlotData[1])))
}


ClPlot=ClPlot+ggplot2::labs(y="Density")

if(PlotNormCurv==TRUE)
{ClPlot=ClPlot+
  ggplot2::stat_function(fun = dnorm, args = list(mean = NullHyp, sd = StandardError))}

if(TestType=="BothTails")
{
  ClPlot=ClPlot+ggplot2::geom_segment(ggplot2::aes(x=NullHyp-MarginOfError ,
             y=0, xend=NullHyp+MarginOfError, yend=0), color="green",size=2)+
    ggplot2::geom_vline(xintercept = NullHyp- MarginOfError, color="green",size=2 )+
    ggplot2::geom_vline(xintercept = NullHyp+ MarginOfError, color="green",size=2 )
}
if(TestType=="RightTail")
{
  ClPlot=ClPlot+ggplot2::geom_segment(ggplot2::aes(x=NullHyp+MarginOfError ,
             y=0, xend=min(NullHyp-XAxisMax*StandardError,SampleMean), yend=0), color="green",size=2)+
             ggplot2::geom_vline(xintercept = NullHyp+ MarginOfError, color="green",size=2 )
}
if(TestType=="LeftTail")
{
  ClPlot=ClPlot+ggplot2::geom_segment(ggplot2::aes(x=NullHyp-MarginOfError ,
                                     y=0, xend=max(NullHyp+XAxisMax*StandardError,SampleMean), yend=0),
                             color="green",size=2)+
    ggplot2::geom_vline(xintercept = NullHyp- MarginOfError, color="green",size=2)
}


ClPlot=ClPlot+
    geom_vline(xintercept = SampleMean, color="blue",size=2)+
  geom_point(data=data.frame(NullHyp=NullHyp,YPoint=0),ggplot2::aes(NullHyp,YPoint),colour="black",size=5, shape = 17)

suppressWarnings(plot(ClPlot))
}
