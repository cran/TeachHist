#' Build a Histogram for Counts with Regular and z-Value Axis
#'
#'#' The TeachCounts function is a variation of the TeachHistDens function of this package. TeachHistCounts generates a
#' histogram based on counts with two horizontal axis. One axis displays z-values the other one the
#' dimension of the displayed variable.
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
#' @param BinWidth (default=1) Bin width expressed in standard deviations.
#' @param XAxisMax (default=5) X-axis ranges from negative to positive XAxisMax. If XAxisMax is not high enough, some of the simulated points might not be considered.
#' @param PrintZAxis (default=TRUE) TRUE adds a second axis with z values. FALSE suppresses z axis and plots only axis with original values.
#' @param LabelSize (default=3) Size of the labels inside the diagram.
#' @param AxisFontSize (default=10) Font size for axis labels.
#' @param NOfSimData (default=1000) Number of simulated data points. Higher NOfSimData results in better precision but possibly not all points are considered.
#' @param SeedValue (default=NULL) Can be any integer and makes simulated points reproducible.
#' @param PrintCounts (default=TRUE) Set PrintCounts=FALSE to suppress the printing of counts in the histogram.
#' @return Returns a histogram
#' @importFrom ggplot2 ggplot geom_histogram aes scale_x_continuous scale_y_continuous xlab labs geom_vline geom_text
#' @importFrom graphics plot
#' @importFrom stats dnorm qnorm qt rnorm sd
#'
#' @import dplyr
#' @examples
#' TeachHistCounts()
#' TeachHistCounts(70,2)
#' MyTibbleWithOneColumn=dplyr::tibble(x=rnorm(2000,20,2))
#' TeachHistCounts(PlotData=MyTibbleWithOneColumn)
#' @export
TeachHistCounts<-function(
Mean=0,
Sd=1,
PlotData= dplyr::tibble(x=c(0)),
VLine1=NULL,
VLine2=NULL,
BinWidth=1, #in sd
XAxisMax=5,
PrintZAxis=TRUE,
AxisFontSize=10,
LabelSize=3,
NOfSimData=1000,
SeedValue=NULL,
PrintCounts=TRUE
)
{

XAxisMax=ifelse((BinWidth==2|BinWidth==3),6,XAxisMax)
'..count..' = NULL
x=NULL
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
    warning("Data for histogram were truncated. Consider to increase XAxisLimitRange parameter.")
    }
}

BreakPointsForHist=seq(Mean-XAxisMax*Sd,Mean+XAxisMax*Sd,by=Sd*BinWidth)
BreakPointsForHistZValues=seq(-XAxisMax,XAxisMax,BinWidth)



DigitsLabels=ifelse(max(abs(BreakPointsForHist))>=1000,0,1)
if(min(abs(BreakPointsForHist))<=0.1){DigitsLabels=3}

MyLabels <- paste(BreakPointsForHistZValues, round(BreakPointsForHist,DigitsLabels),sep = "\n")

FirstColName=colnames(PlotData)[1]

ClPlot=ggplot2::ggplot(PlotData, ggplot2::aes(x=.data[[FirstColName]]))+
  geom_histogram(ggplot2::aes(y = ..count..),breaks = BreakPointsForHist,fill="cadetblue1",
                 color="orange", bins=XAxisMax/BinWidth*2)+
  scale_y_continuous(expand = c(0,0,0.05,0))+
  labs(y="Counts")+
  ggplot2::theme(axis.text = ggplot2::element_text(size = AxisFontSize))+
  ggplot2::theme(axis.title = ggplot2::element_text(size = AxisFontSize))

if (PrintZAxis){
  ClPlot=ClPlot+
    scale_x_continuous(breaks = BreakPointsForHist, labels = MyLabels,
                     limits = c(Mean-XAxisMax*Sd,Mean+XAxisMax*Sd),
                     expand = c(0,0))+
    xlab(paste("Upper Axis: z","Lower Axis: ", colnames(PlotData[1])))
} else {
  ClPlot=ClPlot+
    scale_x_continuous(breaks = round(BreakPointsForHist,DigitsLabels),
                       limits = c(Mean-XAxisMax*Sd,Mean+XAxisMax*Sd),
                       expand = c(0,0))+
    xlab(colnames(PlotData[1]))
}


if(PrintCounts)
  {
    ClPlot=ClPlot+ggplot2::geom_text(ggplot2::aes(y=..count..,label=round(..count..,2)), stat = "bin",
            colour="black", size=LabelSize,breaks=BreakPointsForHist)
}




if(is.numeric(VLine1))
  {ClPlot=ClPlot+
    ggplot2::geom_vline(xintercept = VLine1, color="green",size=2 )}
if(is.numeric(VLine2))
{ClPlot=ClPlot+
  ggplot2::geom_vline(xintercept = VLine2, color="blue",size=2 )}


suppressWarnings(plot(ClPlot))
}



