#' Build a Histogram with Regular and z-Value Axis
#'
#' The TeachHistDens function is the main function of this package. It generates a density histogram with two horizontal axis. One axis displays z-values the other one the
#' dimension of the displayed variable. Relative Frequencies are shown in the midde of each bar.
#'
#' There are two ways to generate the histogram.:
#'    1) Simulated Data: Provide Mean and Standard Deviation and the histogram will be generated based on simulated data.
#'    2) Provide Data as a Data Frame (Tibble)
#'    Provide data (a Tibble with one column) and a histogram will be generated based on the provided data.
#' Histogram Design:
#' The Histogram will have two axis. One axis is for the simulated random variable the other is the related  Z-value (deviation from mean in standard deviations).
#' The user can also define up to two reference lines that will be plotted as vertical lines in the plot (see VLine1 and VLine2).
#'
#' @param Mean (default=0) Mean for simulated data (ignored if PlotData is set).
#' @param Sd (default=1) Standard Deviation for simulated data (ignored if PlotData is set).
#' @param PlotData If PlotData is set to a Tibble with one column, the histogram will be based on the provided data.
#' If PlotData is not set, the data for the histogram will be simulated based on the settings for Mean and Sd.
#' @param VLine1 (default= NA) A value for the simulated variable resulting in a vertical line at related x-axis position (can be se by user. E.g. lower boundary of conf intervall).
#' @param VLine2 (default=NA) A value for the simulated variable resulting in a vertical line at related x-axis position (can be se by user. E.g. upper boundary of conf intervall).
#' @param BinWidth (default=0.5) Bin width expressed in standard deviations.
#' @param XAxisMax (default=5) X-axis ranges from negative to positive XAxisMax. If XAxisMax is not high enough, some of the simulated points might not be considered.
#' @param LabelSize (default=3) Size of the labels inside the diagram.
#' @param PlotNormCurv (default=TRUE) If TRUE normal curve is plotted.
#' @param NOfSimData (default=1000) Number of simulated data points. Higher NOfSimData results in better precision but possibly not all points are considered.
#' @param SeedValue (default=NULL) Can be any integer and makes simulated points reproduceable.
#' @param PrintDensities  (default=FALSE) Set PrintDensities=TRUE to print densities in the histogram.
#' @param PrintRelFreq (default=TRUE) Set PrintRelFreq=FALSE to supress the printing of relative frequencies in the histogram.
#' @return Returns a histogram
#' @importFrom ggplot2 ggplot stat_function geom_histogram aes scale_x_continuous scale_y_continuous xlab geom_vline geom_text
#' @importFrom graphics plot
#' @importFrom stats dnorm qnorm qt rnorm sd
#'
#' @import dplyr
#' @examples
#' TeachHistDens()
#' TeachHistDens(70,2)
#' MyTibbleWithOneColumn=dplyr::tibble(x=rnorm(2000,20,2))
#' TeachHistDens(PlotData=MyTibbleWithOneColumn)
#' @export
TeachHistDens<-function(
Mean=0,
Sd=1,
PlotData= dplyr::tibble(x=c(0)),
VLine1=NULL,
VLine2=NULL,
BinWidth=0.5, #in sd
XAxisMax=5,
LabelSize=3,
PlotNormCurv=TRUE,
NOfSimData=1000,
SeedValue=NULL,
PrintDensities=FALSE,
PrintRelFreq=TRUE
)
{
'..count..'<- '..density..' <- YPoint <- x <- NULL

XAxisMax=ifelse((BinWidth==2|BinWidth==3),6,XAxisMax)

if (nrow(PlotData)!=1)
  {
    Mean=mean(PlotData[[1]])
    Sd=sd(PlotData[[1]])
    NOfSimData=nrow(PlotData)
    PlotData=dplyr::filter(PlotData,PlotData[,1]>Mean-Sd*XAxisMax&PlotData[,1]<Mean+Sd*XAxisMax)
    if (nrow(PlotData)<NOfSimData)
    {
      NOfSimData=nrow(PlotData)
      warning("Data for histogram were truncated. Consider to increase XAxisLimitRange parameter.")
    }
  }

if (nrow(PlotData)==1) #simulate the PlotData
{
  if(is.numeric(SeedValue)){set.seed(SeedValue)}

  PlotData=dplyr::tibble(x=rnorm(NOfSimData,Mean,Sd))
  PlotData=dplyr::filter(PlotData,x>Mean-Sd*XAxisMax&x<Mean+Sd*XAxisMax)
    if (nrow(PlotData)<NOfSimData)
    {
    NOfSimData=nrow(PlotData)
    }
}

  BreakPointsForHist=seq(Mean-XAxisMax*Sd,Mean+XAxisMax*Sd,by=Sd*BinWidth)
  BreakPointsForHistZValues=seq(-XAxisMax,XAxisMax,BinWidth)



  DigitsLabels=ifelse(max(abs(BreakPointsForHist))>=1000,0,1)
  if(min(abs(BreakPointsForHist))<=0.1){DigitsLabels=3}


  MyLabels <- paste(BreakPointsForHistZValues, round(BreakPointsForHist,DigitsLabels),sep = "\n")
FirstColName=colnames(PlotData)[1]
ClPlot=ggplot2::ggplot(PlotData, ggplot2::aes(x=.data[[FirstColName]]))+
  geom_histogram(ggplot2::aes(y = ..density..),breaks = BreakPointsForHist,fill="yellow",
                 color="orange", bins=XAxisMax/BinWidth*2)+
scale_x_continuous(breaks = BreakPointsForHist, labels = MyLabels,
                   limits = c(Mean-XAxisMax*Sd,Mean+XAxisMax*Sd),
                   expand = c(0,0))+
scale_y_continuous(expand = c(0,0,0.05,0))

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

ClPlot=ClPlot+ggplot2::xlab(paste("Upper Axis: z","Lower Axis: ", colnames(PlotData[1])))


if(PlotNormCurv==TRUE)
  {ClPlot=ClPlot+
    ggplot2::stat_function(fun = dnorm, args = list(mean = Mean, sd = Sd))}

  if(is.numeric(VLine1))
  {ClPlot=ClPlot+
    ggplot2::geom_vline(xintercept = VLine1, color="green",size=2 )}
if(is.numeric(VLine2))
{ClPlot=ClPlot+
  ggplot2::geom_vline(xintercept = VLine2, color="blue",size=2 )}

plot(ClPlot)
}



