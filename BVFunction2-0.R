

optFUN.BV.AB.LOG <- function(x,y,logdoses,ECx.target=0.5,predictionsOnly=FALSE,weightedTF=!varFixed,debugTF=FALSE,scaleFactors=NULL){
     if(!is.null(scaleFactors))x <- x*scaleFactors
     if(debugTF)print(x)
     #notice that the model here has implemented constraints on the parameters by transformation
     #specifically, the Asym and scal parameters are exponentiated, so that the basic model parameters
     #are constrained to be strictly positive.  This means that the slope will be negative, no matter
     #what the data suggest, and these will also mean that the parameters themselves will be in a
     #closer neighborhood of magnitude.  If we wanted, we could implement box constraints by 
     #applying a logistic transformation of the parameters with hard-coded up/lower factors
     #such as transParm = lowerBD + plogis(parm)*(upperBD-lowerBD), so that parm can be any real number,
     #but the effective range of the transParm is (lowerBD,upperBD)
     predictions <- exp(x["Asym"])*pnorm(qnorm(1-ECx.target)+x["alpha"] + x["beta"]*logdoses)
     if(debugTF)print(predictions)
     adjN <- (N <- length(y))-3
     if(!predictionsOnly & weightedTF){
          #as in Pinheiro/Bates section 7.5.2
          yStar <- y/sqrt(predictions)
          fStar <- sqrt(predictions)
          #Section 7.5.2 suggests that the denominator of sigmaStar be adjusted for number of parameters estimated
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- yStar-fStar)^2)/adjN)
          #a normal likelihood version, not used:  -sum(dnorm(y-predictions,mean=0,sd=sigmaStar*sqrt(predictions),log=TRUE))
          result <- 0.5*(N*log(2*pi*sigmaStarSq)+sum(residsStar^2)/sigmaStarSq + sum(log(predictions)))#P & B 7.37
     }
     if(!predictionsOnly & !weightedTF){
          #if not weighted, then fit is just MLE, so return the log-likelihood
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- y-predictions)^2)/adjN)
          result <- -sum(dnorm(y,mean=predictions,sd=sigmaStar,log=TRUE))
     }
     if(predictionsOnly)result<-predictions
     result
}

optFUN.BV.LOG <- function(x,y,logdoses,ECx.target=0.5,predictionsOnly=FALSE,weightedTF=!varFixed,debugTF=FALSE,scaleFactors=NULL){
     if(!is.null(scaleFactors))x <- x*scaleFactors
     if(debugTF)print(x)
     #notice that the model here has implemented constraints on the parameters by transformation
     #specifically, the Asym and scal parameters are exponentiated, so that the basic model parameters
     #are constrained to be strictly positive.  This means that the slope will be negative, no matter
     #what the data suggest, and these will also mean that the parameters themselves will be in a
     #closer neighborhood of magnitude.  If we wanted, we could implement box constraints by 
     #applying a logistic transformation of the parameters with hard-coded up/lower factors
     #such as transParm = lowerBD + plogis(parm)*(upperBD-lowerBD), so that parm can be any real number,
     #but the effective range of the transParm is (lowerBD,upperBD)
     predictions <- exp(x["Asym"])*pnorm(qnorm(1-ECx.target)+(x["xmid"] - logdoses)/exp(x["scal"]))
     if(debugTF){
       print(predictions)
       print(c(sumLogPreds=sum(log(predictions))))
     }
     adjN <- (N <- length(y))-3
     if(!predictionsOnly & weightedTF){
          #as in Pinheiro/Bates section 7.5.2
          yStar <- y/sqrt(predictions)
          fStar <- sqrt(predictions)
          #Section 7.5.2 suggests that the denominator of sigmaStar be adjusted for number of parameters estimated
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- yStar-fStar)^2)/adjN)
          #a normal likelihood version, not used:  -sum(dnorm(y-predictions,mean=0,sd=sigmaStar*sqrt(predictions),log=TRUE))
          result <- 0.5*(N*log(2*pi*sigmaStarSq)+sum(residsStar^2)/sigmaStarSq + sum(log(predictions)))#P & B 7.37
          if(debugTF)print(c(N=N,adjN=adjN,sigmaStar=sigmaStar,sigmaStarSq=sigmaStarSq,sumResidStarSq=sum(residsStar^2),
                             sumLogPred=sum(log(predictions)),result=result))
     }
     if(!predictionsOnly & !weightedTF){
          #if not weighted, then fit is just MLE, so return the log-likelihood
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- y-predictions)^2)/adjN)
          result <- -sum(dnorm(y,mean=predictions,sd=sigmaStar,log=TRUE))
     }
     if(predictionsOnly)result<-predictions
     result
}

optFUN.BV.LOG.NULL <- function(x,y,logdoses,ECx.target=0.5,predictionsOnly=FALSE,weightedTF=!varFixed,debugTF=FALSE,scaleFactors=NULL){
     if(!is.null(scaleFactors))x <- x*scaleFactors
     if(debugTF)print(x)
     #notice that the model here has implemented constraints on the parameters by transformation
     #specifically, the Asym and scal parameters are exponentiated, so that the basic model parameters
     #are constrained to be strictly positive.  This means that the slope will be negative, no matter
     #what the data suggest, and these will also mean that the parameters themselves will be in a
     #closer neighborhood of magnitude.  If we wanted, we could implement box constraints by 
     #applying a logistic transformation of the parameters with hard-coded up/lower factors
     #such as transParm = lowerBD + plogis(parm)*(upperBD-lowerBD), so that parm can be any real number,
     #but the effective range of the transParm is (lowerBD,upperBD)
     predictions <- rep(exp(x),length(y))
     if(debugTF){
       print(predictions)
       print(c(sumLogPreds=sum(log(predictions))))
     }
     adjN <- (N <- length(y))-1
     if(!predictionsOnly & weightedTF){
          #as in Pinheiro/Bates section 7.5.2
          yStar <- y/sqrt(predictions)
          fStar <- sqrt(predictions)
          #Section 7.5.2 suggests that the denominator of sigmaStar be adjusted for number of parameters estimated
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- yStar-fStar)^2)/adjN)
          #a normal likelihood version, not used:  -sum(dnorm(y-predictions,mean=0,sd=sigmaStar*sqrt(predictions),log=TRUE))
          result <- 0.5*(N*log(2*pi*sigmaStarSq)+sum(residsStar^2)/sigmaStarSq + sum(log(predictions)))#P & B 7.37
          if(debugTF)print(c(N=N,adjN=adjN,sigmaStar=sigmaStar,sigmaStarSq=sigmaStarSq,sumResidStarSq=sum(residsStar^2),
                             sumLogPred=sum(log(predictions)),result=result))
     }
     if(!predictionsOnly & !weightedTF){
          #if not weighted, then fit is just MLE, so return the log-likelihood
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- y-predictions)^2)/adjN)
          result <- -sum(dnorm(y,mean=predictions,sd=sigmaStar,log=TRUE))
     }
     if(predictionsOnly)result<-predictions
     result
}

optFUN.BV.LOG.PL <- function(x,y,logdoses,xmid,ECx.target=0.5,weightedTF=!varFixed,scaleFactors=NULL,debugTF=FALSE){
     if(!is.null(scaleFactors))x <- x*scaleFactors[names(x)]
     predictions <- exp(x["Asym"])*pnorm(qnorm(1-ECx.target)+(xmid - logdoses)/exp(x["scal"]))
     if(debugTF)print(predictions)
     adjN <- (N <- length(y))-3
     if(weightedTF){
          #as in Pinheiro/Bates
          yStar <- y/sqrt(predictions)
          fStar <- sqrt(predictions)
          #Section 7.5.2 suggests that the denominator of sigmaStar be adjusted for number of parameters estimated
          #in profile, leave denominator the same, so the likelihoods match up at the MLE
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- yStar-fStar)^2)/adjN)
          #a normal likelihood version, not used:  -sum(dnorm(y-predictions,mean=0,sd=sigmaStar*sqrt(predictions),log=TRUE))
          result <- 0.5*(N*log(2*pi*sigmaStarSq)+sum(residsStar^2)/sigmaStarSq + sum(log(predictions)))#P & B 7.37
     }
     if(!weightedTF){
          #if not weighted, then fit is just MLE, so return the log-likelihood
          sigmaStar <- sqrt(sigmaStarSq <- sum((residsStar <- y-predictions)^2)/adjN)
          result <- -sum(dnorm(y,mean=predictions,sd=sigmaStar,log=TRUE))
     }
     ifelse(is.finite(result),yes=result,no=1e20)
}

boundCI <- function(testVals,indata.BCI,bestPars,ECX=0.5,quietTF=!verbose){
     boundFound <- 0
     for(i in seq(along=testVals)){
          #if(lowerTestVals[i]==bestPars["xmid"])lastBest <- sign(bestPars[c("Asym","scal")])
          if(i==1)lastBest <- bestPars[c("Asym","scal")]
          #unirootFUN.LOG(xmidVal,indata,doWeighted=!varFixed,scales=NULL,debugTF=FALSE,solution=FALSE,ECX=0.5)
          testGrid <- as.matrix(expand.grid(Asym=unname(lastBest[1]+seq(-.5,.5,length=21)),scal=unname(lastBest[2]+seq(-.5,.5,length=21))))
          testGridLL <- apply(testGrid,1,FUN = function(PG){
               optFUN.BV.LOG.PL(PG,y=indata.BCI$y,logdoses=log10(indata.BCI$dose),xmid=testVals[i],ECx.target=ECX,weightedTF = !varFixed)
          })
          #print(head(testGrid))
          #print(testGridLL)
          bestStart <- testGrid[which.min(testGridLL),]
          #print(str(bestStart))
          optimxPLL1 <- optimx(par=bestStart,optFUN.BV.LOG.PL,
                               y=indata.BCI$y,logdoses=log10(indata.BCI$dose),
                               xmid=testVals[i],ECx.target=ECX,
                               control=list(dowarn=FALSE,follow.on=TRUE),weighted=!varFixed,method=optimxMethods)
          optimxPLL1 <- optimxPLL1[order(optimxPLL1$value),]
          #print(optimxPLL1)
          #print(bestParsLL)
          fval <- optimxPLL1$value[1]
          #lastBest <- structure(unlist(tail(optimxPLL1$par,1)),names=c("Asym","scal"))
          lastBest <- unlist(optimxPLL1[1,c("Asym","scal")])
          if(!quietTF){
               print(c(test=testVals[i],fval=fval,best=bestParsLL,diff=fval-bestParsLL,crit=fBasedCritVal,pars=lastBest,unirootBasis=fval-bestParsLL-fBasedCritVal))
               if((fval-bestParsLL)>fBasedCritVal)print(paste(to3(10^testVals[i]),"is outside CI"),quote=FALSE)
               if((fval-bestParsLL)<fBasedCritVal)print(paste(to3(10^testVals[i]),"is inside CI"),quote=FALSE)
          }
          if((fval-bestParsLL)>fBasedCritVal){
               boundFound <- 1
               break
          }
     }
     c(sort(testVals[c(i-1,i)]),boundFound=boundFound,lastBest,unirootBasis=fval-bestParsLL-fBasedCritVal)
}


#unirootFUN always starts at best solution.  BoundCI starts at last best solution (probably better guess)
unirootFUN.LOG <- function(xmidVal,indata,parsGuess,doWeighted=!varFixed,scales=NULL,debugTF=FALSE,solution=FALSE,ECX=0.5){
     testGrid <- as.matrix(expand.grid(Asym=unname(parsGuess[1]+seq(-.5,.5,length=21)),scal=unname(parsGuess[2]+seq(-.5,.5,length=21))))
     testGridLL <- apply(testGrid,1,FUN = function(PG){
          optFUN.BV.LOG.PL(PG,y=indata$y,logdoses=log10(indata$dose),xmid=xmidVal,ECx.target=ECX,weightedTF = doWeighted)
     })
     #print(head(testGrid))
     #print(testGridLL)
     bestStart <- testGrid[which.min(testGridLL),]
     if(is.null(scales))optimxPLL1 <- optimx(par=bestStart,optFUN.BV.LOG.PL,
                                             y=indata$y,logdoses=log10(indata$dose),xmid=xmidVal,ECx.target=ECX,
                                             control=list(dowarn=FALSE,follow.on=TRUE),weightedTF=doWeighted,method=optimxMethods)
     if(!is.null(scales))optimxPLL1 <- optimx(par=1+0*bestPars[c("Asym","scal")],optFUN.BV.LOG.PL,
                                              y=indata$y,logdoses=log10(indata$dose),xmid=xmidVal,ECx.target=ECX,
                                              control=list(dowarn=FALSE,follow.on=TRUE),weightedTF=doWeighted,scaleFactors=scales,method=optimxMethods)
     optimxPLL1 <- optimxPLL1[order(optimxPLL1$value),]
     if(debugTF){print("Start");print(bestStart);print("sorted optimx");print(head(optimxPLL1))}
     if(FALSE){
          conv <-  unlist(optimxPLL1[,"convcode"])
          finiteFvals <- which(finiteTF <-(is.finite(fvals) & !is.null(fvals) & !is.na(fvals) & conv==0))
          #print(cbind(fvals,finiteTF,conv))
          optimxPLL1 <- optimxPLL1[finiteFvals,]
     }
     #print(optimxPLL1)
     #    bestPLLpars <- c(xmid=xmidVal,structure(optimxPLL1[,"par"][[iBest]],names=c("Asym","scal")))
     bestPLLpars <- c(xmid=xmidVal,coef(optimxPLL1)[1,])
     assign(x=".lastBest",bestPLLpars,pos=1)
     #  fvals[iBest]-bestParsLL-qchisq(0.95,1)
     # using F distribution here to empirically adjust for sample size (CI will be wider)
     result <- optimxPLL1[1,"value"]-bestParsLL-fBasedCritVal
     if(debugTF)print(c("unirootBasis"=result))
     if(solution)return(bestPLLpars)
     if(!solution)return(result)
}

#simple service function to getting results for figures to 3 significant digits.
#using scientific notation in the innermost part makes getting to 3 digits simple:  format(x,digits=3,scientific=TRUE)
#then the outer part converts it to standard decimal format for printing:  format(as.numeric(...),scientific=FALSE)
to3 <- function(x,nDigits=3){
     newx <- numeric()
     if(!is.numeric(x) | length(x)<1)return(newx)
     for(i in 1:length(x)){
          newx <- c(newx, format(as.numeric(format(x[i],digits=nDigits,scientific=TRUE)),scientific=FALSE))
     }
     return(newx)
}

#the latest plotting function.  Forces that at least one full log10 cycle separate the plotting of non-zero doses
#from the controls.  All numeric results in the details figure are reported to 3 sig digits.
plotBV.LOG <- function(
  inputData,
  bestPars,
  basePlot = FALSE,
  debugTF = FALSE,
  goodFit = TRUE,
  ylimInput = NULL,
  xlimInput = NULL,
  ECxTarget = NULL,
  titleStr=NULL,
  cleanTF=FALSE,
  littleLogs=FALSE,
  ylabel="Response",
  xlabel="Concentration"){
  #find dose spacing on log scale
  doseDelta <- 2*max(diff(log10(sort(unique(inputData$dose))[-1])))
  #print(doseDelta)
  #set dose range for plotting
  if(is.null(xlimInput)){
    xlimInput <- c(
      #        (sort(unique(inputData$dose))[2])-doseDelta,
      (min(unique(inputData$dose[inputData$dose>0])))-doseDelta,
      (max(unique(inputData$dose)))+doseDelta
    )
  }
  if(debugTF)print(c(doseDelta=doseDelta,xlimInput=xlimInput))
  inputData <- inputData[with(inputData,order(dose,y)),]
  jitterFactor <- min(diff(log10(na.omit(unique(inputData[inputData$dose>0,]$dose)))))/10
  if(debugTF)print(c(JFratio = signif(jitterFactor/diff(par("usr")[1:2]),3)))
  if(jitterFactor>0.002*diff(par("usr")[1:2]))jitterFactor <- 0.002*diff(par("usr")[1:2])
  if(debugTF)print("check0 plotBV.LOG")
  with(inputData[inputData$dose>0,],
       {
         plot(y=y,x=dose,log='x',xlim=xlimInput,xlab=xlabel,ylab=ylabel,ylim=ylimInput,axes=FALSE,
              cex=1.3,cex.lab=1.3,type="n")
         if(debugTF)print(c(jitterFactor=jitterFactor,parDiff=diff(par("usr")[1:2])))
         #for each unique dose, randomly jitter
         sapply(unique(dose),FUN = function(xdose){
           yPts <- y[dose==xdose]
           xPts <- dose[dose==xdose]
           if(length(yPts)>1)points(y=yPts,x=sample(xPts*(10^(jitterFactor*seq(-1,1,length=length(yPts))))),cex=1.3)
           if(length(yPts)==1)points(y=yPts,x=xPts,cex=1.3)
         })
         
       })
  orders<-seq(round(par("usr")[1]),round(par("usr")[2]))
  #orders<-seq(floor(par("usr")[1]),ceiling(par("usr")[2]))
  #axes etc
  axis(side=2,lwd=2)
  if(debugTF)print("check0B plotBV.LOG")
  if(goodFit)rug(side=1,lwd=1,
      x=10^(rep(orders[-1],each=8)+rep(log10(2:9),diff(range(orders[-1]))+1)),
      tck=-.01)
  if(littleLogs)axis(side=1,lwd=0,
                     at=10^(rep(orders[-1],each=8)+rep(log10(2:9),diff(range(orders[-1]))+1)),
                     labels=rep((2:9),diff(range(orders[-1]))+1),tck=-.01,cex.axis=0.5,mgp=c(3,0.3,0))
  axis(side=1,at=10^orders,labels=c("C",sapply(10^orders[-1],format)),lwd=2)
  #box()
  #zero dose points
  with(inputData[inputData$dose==0,],{
    yPts <- y
    xPts <- rep(10^(ceiling(par("usr")[1])),length(yPts))
    points(y=yPts,x=sample(xPts*(10^(jitterFactor*rep(c(-1,0,1),length=length(yPts))))),cex=1.3)
  })
  log.xVals <- seq(ceiling(par("usr")[1])+1,par("usr")[2],length=1000)
  log.xValsC <- c(ceiling(par("usr")[1]),ceiling(par("usr")[1])+.8)
  #model predictions
  #print("look here")
  #print(bestPars)
  #print(inputData$y)
  #print(log.xVals)
  #print(ECxTarget)
  #print(optFUN.BV.LOG(bestPars,y=inputData$y,logdoses=log.xVals,ECx.target=ECxTarget,predictionsOnly=TRUE))
  if(basePlot){
    aggData <- aggregate(y~dose,data=inputData,FUN = median,na.rm=TRUE)
    aggData$dose[aggData$dose==0] <- 10^(ceiling(par("usr")[1]))
    abline(h=              aggData$y[aggData$dose==min(aggData$dose)],lty=3,col="magenta",lwd=2)
    abline(h=(1-ECxTarget)*aggData$y[aggData$dose==min(aggData$dose)],lty=2,col="magenta",lwd=2)
    mtext(side=4,at=              aggData$y[aggData$dose==min(aggData$dose)] + 0.05*diff(par("usr")[3:4]),
          text = "Approx Bkgd/Control \nResponse Level",las=1,adj=1,cex=1.5,col="blue")
    mtext(side=4,at=(1-ECxTarget)*aggData$y[aggData$dose==min(aggData$dose)] - 0.03*diff(par("usr")[3:4]),
          text = paste("Approx EC",format(round(100*ECxTarget,1))," Level",sep=""),las=1,adj=1,cex=1.5,col="blue")
    with(aggData,lines(x=dose,y=y,type="b",pch=0,cex=0))
    axis.break(axis=1,breakpos=10^(ceiling(par("usr")[1])+.8),bgcol="white",breakcol="black",style="zigzag",brw=0.02)
    return()
  }
  if(debugTF)print(c(ECxTarget=ECxTarget))
  lines(x=10^log.xVals,y=optFUN.BV.LOG(bestPars,y=inputData$y,logdoses=log.xVals,ECx.target=ECxTarget,predictionsOnly=TRUE),col="blue",lwd=2)
  lines(x=10^log.xValsC,y=optFUN.BV.LOG(bestPars,y=inputData$y,logdoses=rep(log(0),2),ECx.target=ECxTarget,predictionsOnly=TRUE),col="blue",lwd=2)
  axis.break(axis=1,breakpos=10^(ceiling(par("usr")[1])+.8),bgcol="white",breakcol="black",style="zigzag",brw=0.02)
  if(debugTF)print("check0C plotBV.LOG")
  if(goodFit){
    #confidence limits
    lines(x=10^c(lowerCI["xmid"],upperCI["xmid"]),y=rep((1-ECxTarget)*exp(bestPars["Asym"]),2),lwd=2)
    lines(x=10^c(lowerCI["xmid"],upperCI["xmid"]),y=rep(par("usr")[3],2),lwd=5,col="blue")
    rug(side=1,x=10^bestPars["xmid"])
  }
  #print(c(lowerCI.xmid,upperCI.xmid))
  if(debugTF)print("check0D plotBV.LOG")
  if(debugTF)print(c(goodFit=goodFit))
  if (!cleanTF & goodFit){
    newY <- optFUN.BV.LOG(lowerCI,y=inputData$y,logdoses=log.xVals,ECx.target=ECxTarget,predictionsOnly=TRUE)
    lines(x=10^log.xVals,y=newY,col="blue",lty=2)
    newY <- optFUN.BV.LOG(lowerCI,y=inputData$y,logdoses=rep(log(0),2),ECx.target=ECxTarget,predictionsOnly=TRUE)
    lines(x=10^log.xValsC,y=newY,col="blue",lty=2)
    points(x=10^lowerCI["xmid"],y=optFUN.BV.LOG(lowerCI,y=inputData$y,logdoses=lowerCI["xmid"],ECx.target=ECxTarget,predictionsOnly=TRUE),col="blue",pch=16)
    if(debugTF)print("check1 plotBV.LOG")
    
    newY <- optFUN.BV.LOG(upperCI,y=inputData$y,logdoses=log.xVals,ECx.target=ECxTarget,predictionsOnly=TRUE)
    lines(x=10^log.xVals,y=newY,col="cyan3",lty=2)
    newY <- optFUN.BV.LOG(upperCI,y=inputData$y,logdoses=rep(log(0),2),ECx.target=ECxTarget,predictionsOnly=TRUE)
    lines(x=10^log.xValsC,y=newY,col="cyan3",lty=2)
    points(x=10^upperCI["xmid"],y=optFUN.BV.LOG(upperCI,y=inputData$y,logdoses=upperCI["xmid"],ECx.target=ECxTarget,predictionsOnly=TRUE),col="cyan3",pch=16)
    if(debugTF)print("check2 plotBV.LOG")
    
    #format values for display in figures
    formattedECvals <- structure(c(format(to3(10^c(lowerCI["xmid"],upperCI["xmid"],bestPars["xmid"])),scientific=FALSE),format(ECxTarget)),names=c("LCL.95","UCL.95","ECx","EC.Level"))
    if(debugTF){
      cat("\n")
      print(formattedECvals,quote=FALSE)
    }
    
    mtext(
      side=3,line=-1.2,cex=1,adj=0,at=10^(par("usr")[2]-.30*diff(par("usr")[1:2])),bquote(paste("EC",scriptscriptstyle("")[.(100*ECxTarget)])))
    mtext(
      side=3,line=-1.2,cex=1,at=10^(par("usr")[2]-.10*diff(par("usr")[1:2])),formattedECvals["ECx"])
    mtext(
      side=3,line=-2.2,cex=1,adj=0,at=10^(par("usr")[2]-.30*diff(par("usr")[1:2])),"95% CI: ")
    mtext(
      side=3,line=-2.2,cex=1,at=10^(par("usr")[2]-.10*diff(par("usr")[1:2])),
      paste("(",formattedECvals["LCL.95"],",",formattedECvals["UCL.95"],") ",sep=''))
    mtext(side=1,line=-2.5,outer=TRUE,text="Control values leftmost",cex=.5,adj=0.02)
    if( varFixed)mtext(side=1,line=-3.0,outer=TRUE,text="Variance is constant",cex=.5,adj=0.02)
    if(!varFixed)mtext(side=1,line=-3.0,outer=TRUE,text="Variance is proportional to mean",cex=.5,adj=0.02)
    #mtext(side=1,line=-2,outer=TRUE,text=inputFile,cex=.5,adj=0.02)
    mtext(side=1,line=-2,outer=TRUE,text=date(),cex=.5,adj=0.98)
    if(debugTF)print("check3 plotBV.LOG")
    mtext(side=3,adj=0.0,line=0,
          text=bquote(mu[y]==bgroup("(",
                                    atop(
                                      paste(A,phantom(Phi*group("[",z[1-.(ECxTarget)] + group("(",log[10](EC[.(100*ECxTarget)]) - log[10](Conc),")")/sigma,"]")),phantom(XX),Conc==0),
                                      paste(A*Phi*group("[",z[1-.(ECxTarget)] + group("(",log[10](EC[.(100*ECxTarget)]) - log[10](Conc),")")/sigma,"]"),phantom(XX),Conc>0)),
                                    "")))
    if(debugTF)print("check4 plotBV.LOG")
    mtext(side=3,line=0.3,adj=0.98,text=bquote(hat(sigma)==.(to3(exp(bestPars["scal"])))))
    mtext(side=3,line=1.4,adj=0.98,text=bquote(hat(EC[.(100*ECxTarget)])==.(to3(10^bestPars["xmid"]))))
    mtext(side=3,line=2.8,adj=0.98,text=bquote(hat(A)==.(to3(exp(bestPars["Asym"])))))
    plotData <- as.matrix(as.data.frame(lapply(inputData[,c("dose","y")],format)))
    if(debugTF)print("check5 plotBV.LOG")
    #plotData <- as.matrix(lapply(plotData,as.character))
    #par(new=TRUE)
    #plot(1,1,type='n',axes=FALSE,xlab='',ylab='')
    #addtable2plot(x=2*(10^ceiling(par("usr")[1])),y=par("usr")[3],table=plotData,cex=.5)
  }
  if(debugTF)print("Finish plotBV.LOG")
  invisible()
}

fitBV.PLL <- function(
  BVdata,
  ECXvalue,
  fileName=NULL,
  verbose=FALSE,
  do3Dstart=FALSE,
  FORCE=FALSE,
  zeroSub=NULL,
  varFixed=FALSE,
  xlabel = "Concentration",
  ylabel = "Response"){
  
  varFixed <<- varFixed
  verbose <<- verbose
  #these are required packages
  #library checks
  #libList <- c("plotrix", "minpack.lm", "optimx","rgl")
  libList <- c("plotrix","optimx")
  libChecks <- !sapply(libList,FUN=require,character.only=TRUE)
  if(!any(libChecks)&verbose)cat("\nLibrary load complete\n")
  if(any(libChecks))stop(paste("This program requires the following packages to be installed:  ",paste(libList[libChecks],collapse=" ")))
  
  fBasedCritVal <<- qf(0.95,1,nrow(BVdata)-1)
  
  optimxMethods <<- c("BFGS","Nelder-Mead","CG","nlminb","spg","ucminf","L-BFGS-B")#,"newuoa")#,"bobyqa")
  
  #BV.AB means mod of BV model to be a standard alpha + beta*dose, so that Fieller's Thm can be applied.  x will be a named
  #vector of parameters with "Asym", "alpha", "beta"
  
  
  ###check the data; zero's are not valid data; all groups of size 1; no evidence of trend
  BVdata <<- BVdata[order(BVdata$dose),]
  uDoses <- sort(unique(BVdata$dose))
  nzDoses <- uDoses[uDoses>0]
  
  if(verbose)print(BVdata)
  
  msgText <- ""
  groupVars <- with(BVdata,tapply(X=y,INDEX=dose,FUN=var))
  if(verbose)print(c(groupVars==groupVars))
  groupSizes <- with(BVdata,table(dose))
  if(verbose)print(data.frame(N=groupSizes,variance=groupVars))
  stopError<-FALSE
  if(max(groupSizes)==1 & var(BVdata$y)>0)msgText<- ""
  if(max(groupSizes)>1){
    if(max(groupVars[groupSizes>1])==0){
      msgText<- ifelse(nchar(msgText)>0,yes=paste(msgText,"No within-group variances > 0",sep=" // "),no="")
    }
  }
  if(var(BVdata$y)==0){
    msgText<- ifelse(nchar(msgText)>0,yes=paste(msgText,"All responses Identical",sep=" // "),no="")
  }
  #the previous problems are worthy of a stop at this point
  stopError <- FALSE
  if(nchar(msgText)>0){
    stopError <- TRUE
    print(msgText)
  }
  #fix zeros
  if(!is.null(zeroSub)){
    if(any(BVdata$y<=0))msgText<-ifelse(nchar(msgText)>0,yes=paste(msgText,paste("Zero values reset to",zeroSub),sep=" // "),no=paste("Zero values reset to",zeroSub))
    BVdata$y[BVdata$y<=0]<-zeroSub
  }
  
  if(max(groupSizes)==1)msgText<- ifelse(nchar(msgText)>0,yes=paste(msgText,"All dose groups of size 1",sep=" // "),no="All dose groups of size 1")
  if(any(BVdata$y<=0))msgText<-ifelse(nchar(msgText)>0,yes=paste(msgText,"ZEROS in data are reset to a small value",sep=" // "),no="ZEROS in data may be reset to a small value")
  if(stopError){
    print("In stop error wrap up")
    upperCI <- lowerCI <- plotLine <- c(Asym=log(mean(BVdata$y)),xmid=log10(max(BVdata$dose))+100,scal=.00001)
    try(plotBV.LOG(BVdata,plotLine,ECxTarget=.01,cleanTF=TRUE,debugTF=verbose,
                   ylimInput=c(0,max(BVdata$y)),
                   xlimInput=10^range(c(floor(c(log10(min(BVdata$dose[BVdata$dose>0]))))-1,ceiling(c(log10(max(BVdata$dose))))))
    ))
    if(is.character(fileName))mtext(side=1,line=-2,outer=TRUE,text=fileName,cex=.5,adj=0.02)
    mtext(side=1,line=-3,outer=TRUE,text=msgText,cex=.5,adj=0.98)
    return(c(EC.level=ECXvalue,ECx=NA,LCL.95=NA,UCL.95=NA,C.level=NA,Scale=NA,trustLower=NA,trustUpper=NA,ECx.outside=NA))
  }
  
  
  
  #while this has been programmed in the past to protect against log(0), it is actually not necessary
  #R/S know what inifinity is, and does the right thing.
  BVmodel <- y~exp(Asym)*pnorm((xmid - log10(dose))/exp(scal))
  #force dose order into the data
  
  
  #trendP <- jterp.R(y=BVdata$y,dose=BVdata$dose)
  BVdata$doseFactor <- as.factor(BVdata$dose)
  if(FALSE){# no longer needed
    if(all(groupSizes>1)){
      aovObject <- lm(y~doseFactor,data=BVdata)
      print(summary(aovObject))
      library(multcomp)
      williamsP <- summary(glht(aovObject,linfct=mcp(doseFactor=contrMat(rep(1,length(unique(BVdata$dose))),"Williams")),alternative="less"))$test$pvalues
      trendP <- summary(glht(aovObject,linfct=mcp(doseFactor=contr.poly(length(unique(BVdata$dose)))[,1]),alternative="less"))$test$pvalues
      print(c(WilliamsP=williamsP,linear=trendP))
      if(all(williamsP>0.05) & trendP>0.05){
        cat("\n",paste(rep("No evidence of decreasing trend in data",3),collapse="\n"),"\n","\n",sep="")
        cat("\nThe Williams test pvalues are p=",format(williamsP)," (One-sided for decreasing response)",sep="")
        cat("\nThe Bruce-Versteeg model is specifically for responses that decrease from control levels as test concentrations increase\n\n")
        #still throw a plot out, because the analysis probably won't work anyway; 
        (plotLine <- c(Asym=log(mean(BVdata$y)),xmid=log10(max(BVdata$dose))+2,scal=log(.1)))
        #do the plot only if we have not already done it
        if(nchar(msgText)>0)mtext(side=1,line=-2.5,outer=TRUE,text="No evidence of trend in data",cex=.5,adj=0.98)
        if(nchar(msgText)==0){
          try(plotBV.LOG(BVdata,plotLine,ECxTarget=ECXvalue,debugTF=verbose,
                         ylimInput=c(0,max(BVdata$y)),
                         xlimInput=10^range(c(floor(c(log10(min(BVdata$dose[BVdata$dose>0]))))-1,ceiling(c(log10(max(BVdata$dose))))))
          ))
          if(is.character(fileName))mtext(side=1,line=-2,outer=TRUE,text=fileName,cex=.5,adj=0.02)
          mtext(side=1,line=-3,outer=TRUE,text=msgText,cex=.5,adj=0.98)
          mtext(side=1,line=-2.5,outer=TRUE,text="No evidence of trend in data",cex=.5,adj=0.98)
        }
        if(!FORCE)stop(paste(c("\n",rep("No evidence of trend in data",3)),collapse="\n"))               
      }
    }
  }
  
  if(verbose & nchar(msgText)>0)print(msgText,quote=FALSE)
  #starting values.  In initial fits, the desired ECx level is irrelevant.  The model fit to the data is
  #not a function of ECx, so EC50 is targeted.  Empirical experience suggests that this is also a more
  #stable estimation target (for unknown reasons)
  startVals<-c(
    #Asym is based on largest group mean
    Asym=log(max(with(BVdata,tapply(X=y,INDEX=dose,FUN=mean)))),
    #xmid is based on finding doses that bracket the EC50.  If there is none, then the EC50 will be guessed to be
    #either the largest dose, or the smallest, depending on which side the data lies.
    xmid=log10(with(BVdata,{
      gMeans <- tapply(X=y,INDEX=dose,FUN=mean)
      above <- max(which(gMeans>max(gMeans)/2))
      below <- min(which(gMeans<max(gMeans)/2))
      if(below==Inf)below<-length(gMeans)
      if(above==-Inf)above<-ifelse(any(uDoses==0),yes=2,no=1)
      #print(c(above,below))
      mean(as.numeric(names(gMeans)[c(above,below)]))})),
    #scal is based simply on dose spacing
    scal=log(with(BVdata,{uDoses <- sort(unique(dose));uDoses <- uDoses[uDoses>0];max(diff(log10(uDoses)))}))
  )
  #
  #DEFUNCT:  try the very first starting value guess first.  If it converges, move on to MLE, and skip the grid search
  #this should be OK in the vast majority of cases.  Experience suggests that nlsLM is more robust to starting
  #values than are the optimx routines (see the file 'R nonlinear NIST.R', and associated references) regarding
  #permformance of nonlinear estimation methods.
  if(FALSE){
    firstNLS <- try(nlsLM(BVmodel,data=BVdata,start=startVals))
    #print(summary(firstNLS))
    if(firstNLS$convInfo$isConv){
      startVals <- coef(firstNLS)
      fullFit <- optimx(par=startVals,optFUN.BV.LOG,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=0.5,
                        control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
      fvals <- unlist(fullFit[,"fvalues"])
      conv <-  unlist(fullFit[,"convcode"])
      finiteFvals <- which(finiteTF <-(is.finite(fvals) & !is.null(fvals) & !is.na(fvals) & conv==0))
      fullFit <- fullFit[finiteFvals,]
      fullFit
    }
  }
  
  #do a grid search for starting values.  This should cast a wide enough net in order to get a reasonable start
  #if not, there is probably something in the data (undefined steep slope, zeros, etc) that causes trouble
  testGrid <- expand.grid(
    Asym=startVals["Asym"] + seq(log(.5),log(2),length=20),
    xmid=seq(min(log10(nzDoses)),max(log10(nzDoses))+max(diff(log10(nzDoses))),length=20),
    scal=startVals["scal"] + seq(log(.2),log(5),length=20))
  #use weighted RSS calculation, not MLE via optimization here (it is far quicker)
  #at this point, the estimated parameter is the EC50, by default
  SSvals <- t(with(BVdata,{
    apply(
      testGrid,
      1,
      FUN=function(pars){
        pred4nls <- optFUN.BV.LOG(x=pars,y=BVdata$y,logdoses=log10(BVdata$dose),predictionsOnly=TRUE)
        wRSS <- sum(((pred4nls-BVdata$y)^2)*(1/pred4nls))
        RSS <- sum(((pred4nls-BVdata$y)^2))
        return(c(wRSS=wRSS,RSS=RSS,LL=optFUN.BV.LOG(x=pars,y=BVdata$y,logdoses=log10(BVdata$dose)),pars))
      })}))
  if(!varFixed)SSvals <- as.data.frame(SSvals[order(SSvals[,1]),])
  if(varFixed)SSvals <- as.data.frame(SSvals[order(SSvals[,2]),])
  
  
  #
  if(verbose){
    print(head(testGrid))
    cat("\nBest weighted RSS values, then ranges\n")
    print(head(SSvals))#these should be different (same and large, or other oddness would suggest trouble)
    startRanges <- apply(SSvals,2,range)
    startRanges[,4] <- exp(startRanges[,4])
    startRanges[,5] <- 10^(startRanges[,5])
    startRanges[,6] <- exp(startRanges[,6])
    print(startRanges)
  }
  #identify the best guess
  if(!varFixed)startVals <- unlist(SSvals[which.min(SSvals[,"wRSS"]),c("Asym","xmid","scal")])
  if( varFixed)startVals <- unlist(SSvals[which.min(SSvals[,"RSS"]), c("Asym","xmid","scal")])
  
  if(verbose){
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check1 fitBV.PLL")
    cat("\nStarting parameter values (EC50)\n")
    #  print(sapply(c(exp(startVals["Asym"]),10^(startVals["xmid"]-qnorm(1-ECXvalue)*exp(startVals["scal"])),exp(startVals["scal"])),to3,nDigits=4),quote=FALSE)
    print(sapply(c(exp(startVals["Asym"]),10^(startVals["xmid"]),exp(startVals["scal"])),to3,nDigits=4),quote=FALSE)
    cat("\nStarting parameter values (EC50, log-scale)\n")
    #  print(sapply(c(exp(startVals["Asym"]),10^(startVals["xmid"]-qnorm(1-ECXvalue)*exp(startVals["scal"])),exp(startVals["scal"])),to3,nDigits=4),quote=FALSE)
    print(sapply(c((startVals["Asym"]),(startVals["xmid"]),(startVals["scal"])),to3,nDigits=4),quote=FALSE)
    cat("\n")
    #among the 20^3=8000 starting guesses, look at the top 2000 in each, and plot
    LLcut <- with(SSvals,max(LL[which(rank(LL)<2000)]))
    if(!varFixed)RSScut <- with(SSvals,max(wRSS[which(rank(wRSS)<2000)]))
    if( varFixed)RSScut <- with(SSvals,max(RSS[which(rank(RSS)<2000)]))
    print(c(LL=LLcut,wRSS=RSScut))
    if(!varFixed)bestGuesses <- SSvals[with(SSvals,LL<LLcut | wRSS<RSScut),]
    if( varFixed)bestGuesses <- SSvals[with(SSvals,LL<LLcut | RSS<RSScut),]
    
    if(!varFixed){
      print("wRSS plot")
      wRSS4plot <- subset(bestGuesses,is.finite(wRSS) & is.finite(LL))
      with(wRSS4plot,plot(x=wRSS,y=LL,log='x',ylim=range(LL)+c(-1,0)*diff(range(LL)),xlim=range(wRSS)*c(.1,1),
                          main="LL vs wRSS among top start grid guesses"))
      with(wRSS4plot,{
        minI <- which.min(bestGuesses[,"wRSS"]);points(wRSS[minI],LL[minI],col="red",pch=16);
        minI <- which.min(bestGuesses[,"LL"]);points(wRSS[minI],LL[minI],col="red",pch=16);
      })
    }
    if(varFixed){
      with(bestGuesses,plot(x=RSS,y=LL,log='x',ylim=range(LL)+c(-1,0)*diff(range(LL)),xlim=range(RSS)*c(.1,1),
                            main="LL vs RSS among top start grid guesses"))
      with(bestGuesses,{
        minI <- which.min(bestGuesses[,"RSS"]);points(RSS[minI],LL[minI],col="red",pch=16);
        minI <- which.min(bestGuesses[,"LL"]); points(RSS[minI],LL[minI],col="red",pch=16);
      })
    }
    title(sub = "Plot is for evaluating effectiveness of starting value guesses on two optimization criteria")
  }
  
  
  
  if(do3Dstart & FALSE){
    assign(".gridResults",SSvals,pos=1)
    open3d()
    ranges <- apply(testGrid,2,range)
    print(ranges)
    points3d(x=testGrid[,"Asym"],y=testGrid[,"xmid"],z=testGrid[,"scal"],col="cyan",alpha=0.3)
    aspect3d(1,1,1)
    axes3d()
    title3d(xlab="ln(Asym)",ylab="log10(EC50)",zlab="ln(Scale)")
    quads3d(cbind(ranges[rep(1,4),1],ranges[c(1,1,2,2),2],ranges[c(1,2,2,1),3]),col="cyan",alpha=.1)
    quads3d(cbind(ranges[rep(2,4),1],ranges[c(1,1,2,2),2],ranges[c(1,2,2,1),3]),col="cyan",alpha=.1)
    quads3d(cbind(ranges[rep(1,4),2],ranges[c(1,1,2,2),1],ranges[c(1,2,2,1),3])[,c(2,1,3)],col="cyan",alpha=.1)
    quads3d(cbind(ranges[rep(2,4),2],ranges[c(1,1,2,2),1],ranges[c(1,2,2,1),3])[,c(2,1,3)],col="cyan",alpha=.1)
    quads3d(cbind(ranges[rep(1,4),3],ranges[c(1,1,2,2),1],ranges[c(1,2,2,1),2])[,c(2,3,1)],col="cyan",alpha=.1)
    quads3d(cbind(ranges[rep(2,4),3],ranges[c(1,1,2,2),1],ranges[c(1,2,2,1),2])[,c(2,3,1)],col="cyan",alpha=.1)
    spheres3d(SSvals[which.min(SSvals[,"wRSS"]),c("Asym","xmid","scal")],col="red",radius=.1)
  }
  
  #MLE estimation of full model, after grid search for starting values.  ECx val is still EC50
  fullFit <- optimx(par=startVals,optFUN.BV.LOG,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=0.5,
                    control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
  fullFit <- fullFit[order(fullFit$value),]
  if(verbose){
    print(fullFit)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check2 fitBV.PLL")
  }
  
  if(FALSE){
    startValsAB <- unlist(fullFit[1,1:3])
    startValsAB <- c(startValsAB,alpha=unname(startValsAB["xmid"]/exp(startValsAB["scal"])),beta=unname(-1/exp(startValsAB["scal"])))
    startValsAB <- startValsAB[c("Asym","alpha","beta")]
    print(startValsAB)
    fit4Fieller <- optimx(par=startValsAB,optFUN.BV.AB.LOG,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=0.5,
                          control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods,hessian=TRUE)
    fit4Fieller <- fit4Fieller[order(fit4Fieller$value),]
    print(fit4Fieller)
    print((attr(fit4Fieller,"details")[,"nhatend"])[[1]])
    print(solve((attr(fit4Fieller,"details")[,"nhatend"])[[1]]))
    #print(solve(-1*(attr(fit4Fieller,"details")[,"nhatend"])[[1]]))
    print(10^FiellerAB(ABparms = unlist(fit4Fieller[1,2:3]),ABcov = solve((attr(fit4Fieller,"details")[,"nhatend"])[[1]])[-1,-1],DF = nrow(BVdata)-4))
    
    stop()
  }
  
  
  
  
  
  
  
  
  nullRange <- range(BVdata$y[BVdata$y>0])
  if(diff(nullRange)==0)nullRange<-range(BVdata$y)
  nullFit <- optimize(optFUN.BV.LOG.NULL,interval=log(nullRange),y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=0.5)
  #fvals <- unlist(fullFit[,"fvalues"])
  #conv <-  unlist(fullFit[,"conv"])
  #finiteFvals <- which(finiteTF <-(is.finite(fvals) & !is.null(fvals) & !is.na(fvals) & conv==0))
  #fullFit <- fullFit[finiteFvals,]
  if(verbose){print("fullFit then nullFit");print(fullFit);print(nullFit)}
  bestParsEC50 <- unlist(fullFit[1,c("Asym","xmid","scal")])
  bestParsECx <- bestParsEC50
  if(ECXvalue!=0.5){
    bestParsECx["xmid"] <- bestParsEC50["xmid"]-qnorm(1-ECXvalue)*exp(bestParsEC50["scal"])
  }
  bestParsEC50 <<- bestParsEC50
  bestParsECx <<- bestParsECx
  
  if(verbose)print(bestParsEC50)
  bestParsLL <<- fullFit$value[1]
  if(verbose){
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check3 fitBV.PLL")
  }
  
  #look at sparation of LL values when the endpoints of the interval are hypothetically the min and max non-zero doses
  #ideally, there will be enough separation that the interval can be inside of the range of doses
  #THIS MAY NOT BE A GREAT WAY TO GO.  IF THE DOSE RESPONSE IS STEEP, SAY WITH LOTS OF GROUPS THAT ARE SAME AS CONTROL BEFORE THE
  #DROP, THEN TRYING TO FIX THE ECX THERE (WITH ARG xmid=log10(min(nzDoses)) ) AND FITTING WILL BE SO FAR AWAY THAT GET ERRORS TO OPT FUNCTION
  if(FALSE){
    fitAtLow <- optimx(par=bestPars[c("Asym","scal")],optFUN.BV.LOG.PL,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=ECXvalue,xmid=log10(min(nzDoses)),
                       control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
    fitAtLowLL <- min(fitAtLow$value)
    fitAtLow <- fitAtLow[order(fitAtLow$value),]
    if(verbose){print("fitAtLow");print(fitAtLow)}
    
    fitAtHigh <- optimx(par=bestPars[c("Asym","scal")],optFUN.BV.LOG.PL,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=ECXvalue,xmid=log10(max(nzDoses)),
                        control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
    fitAtHigh <- fitAtHigh[order(fitAtHigh$value),]
    fitAtHighLL <- min(fitAtHigh$value)
    if(verbose){print("fitAtHigh");print(fitAtHigh)}
    if(verbose)print(c(nullLL=nullLL,fullLL=bestParsLL,lowLL=fitAtLowLL,highLL=fitAtHighLL,critVal=fBasedCritVal))
  }
  
  nullLL <- nullFit$objective
  #if No trend, do a plot and return
  #  if((fitAtLowLL-bestParsLL)<fBasedCritVal & (fitAtHighLL-bestParsLL)<fBasedCritVal){
  ### set a flag that helps control things when the fit does not work for CIs
  goodFlag <<- TRUE
  if(verbose){
    print("fullFit then nullFit");print(fullFit);print(nullFit)
    print(c(nullLL=nullLL,bestParsLL=bestParsLL,fBasedCritVal=fBasedCritVal))
  }
  if(1*abs(nullLL-bestParsLL)<fBasedCritVal){
    goodFlag <<- FALSE
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check4 fitBV.PLL")
    print("Model did not meet minimum standards to complete fit")
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    
    if(verbose)print(c(nullLL=nullLL,fullLL=bestParsLL,critVal=fBasedCritVal))
    lowerCI <<- bestParsEC50
    upperCI <<- bestParsEC50
    try(plotBV.LOG(inputData = BVdata,bestPars = bestParsECx,ECxTarget=ECXvalue,debugTF=verbose,
                   goodFit = FALSE,
                   ylimInput=c(0,max(BVdata$y)),
                   xlimInput=10^range(c(floor(c(log10(min(BVdata$dose[BVdata$dose>0]))))-1,ceiling(c(log10(max(BVdata$dose))))))
    ))
    title(main = "BV model dose not meet minimum requirements\nResults will be incomplete")
    if(is.character(fileName))mtext(side=1,line=-2,outer=TRUE,text=fileName,cex=.5,adj=0.02)
    mtext(side=1,line=-3,outer=TRUE,text=msgText,cex=.5,adj=0.98)
    mtext(side=1,line=-2.5,outer=TRUE,text="CI fully outside range of doses, or cannot be calculated",cex=.5,adj=0.98)
    return(list(EC.level=ECXvalue,ECx=10^unname(bestParsECx["xmid"]),LCL.95=NA,UCL.95=NA,
                C.level=exp(unname(bestParsECx["Asym"])),Scale=exp(unname(bestParsECx["scal"])),
                trustLower=0,trustUpper=0,ECx.outside=as.numeric((10^bestParsECx["xmid"])>max(nzDoses))))
    
  }
  if(do3Dstart & FALSE){
    #spheres3d(bestPars,col="blue",radius=.1)
  }
  if(verbose){
    pred4nls <- optFUN.BV.LOG(x=bestParsEC50,y=BVdata$y,logdoses=log10(BVdata$dose),predictionsOnly=TRUE)
    if(!varFixed){
      wRSS <- sum(((pred4nls-BVdata$y)^2)*(1/pred4nls))
      cat("\nweighted RSS value at MLE\n")
      print(wRSS)
      points(x=wRSS,y=bestParsLL,pch=16,col="blue")
    }
    if(varFixed){
      RSS <- sum(((pred4nls-BVdata$y)^2))
      cat("\nun-weighted RSS value at MLE\n")
      print(RSS)
      points(x=RSS,y=bestParsLL,pch=16,col="blue")
    }
  }
  
  #reparameterize if the ECX target is not 50% (most cases).  Experience suggests that estimating
  #EC50 is more stable, so we do that, and then reparameterize at the end.
  
  
  if(verbose){
    cat("\nFinal parameter values (EC50)\n")
    print(sapply(c(exp(bestParsEC50["Asym"]),10^(bestParsEC50["xmid"]),exp(bestParsEC50["scal"])),to3,nDigits=4),quote=FALSE)
    cat("\nFinal parameter values (ECx)\n")
    print(sapply(c(exp(bestParsECx["Asym"]),10^(bestParsECx["xmid"]),exp(bestParsECx["scal"])),to3,nDigits=4),quote=FALSE)
    cat("\n")
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check5 fitBV.PLL")
  }
  
  
  #with(BVdata,optFUN.BV.LOG(bestPars,predictionsOnly=TRUE,y=y,logdoses=log10(dose),ECx.target=ECXvalue))
  
  
  
  #This is where most of the compute time is spent (bounding, then uniroot)
  #BOUND the CI -- need to do a uniroot() to zero in, so do a crude interval first, by working out from ECx estimate in both directions
  if(bestParsECx["xmid"]>log10(min(nzDoses)))lowerCItestVals <- seq(bestParsECx["xmid"],min(log10(nzDoses))-2*max(diff(log10(nzDoses))),length=20)
  if(bestParsECx["xmid"]<=log10(min(nzDoses)))lowerCItestVals <- seq(bestParsECx["xmid"],bestParsECx["xmid"]-2*max(diff(log10(nzDoses))),length=20)
  if(verbose){
    cat("\nlowerCItestVals\n",sep="")
    print(to3(10^lowerCItestVals),quote=FALSE)
  }
  lowerCIbounds <- try(boundCI(testVals = lowerCItestVals,indata.BCI = BVdata,
                               bestPars = bestParsECx,ECX=ECXvalue))
  
  if(verbose){
    print(lowerCIbounds)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check6 fitBV.PLL")
  }
  
  #if error, try smaller grid
  if(regexpr("error",class(lowerCIbounds))>0){
    if(bestParsECx["xmid"]>log10(min(nzDoses)))lowerCItestVals <- seq(bestParsECx["xmid"],min(log10(nzDoses))-2*max(diff(log10(nzDoses))),length=200)
    if(bestParsECx["xmid"]<=log10(min(nzDoses)))lowerCItestVals <- seq(bestParsECx["xmid"],bestParsECx["xmid"]-2*max(diff(log10(nzDoses))),length=200)
    lowerCIbounds <- try(boundCI(testVals = lowerCItestVals,indata.BCI = BVdata,
                                 bestPars = bestParsECx,ECX=ECXvalue))
  }
  if(verbose){
    cat("\nLower CI Bounds\n",sep="")
    print(lowerCIbounds)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check7 fitBV.PLL")
  }
  
  
  if(bestParsECx["xmid"]<log10(max(nzDoses)))upperCItestVals <- seq(bestParsECx["xmid"],max(log10(nzDoses))+2*max(diff(log10(nzDoses))),length=20)
  if(bestParsECx["xmid"]>=log10(max(nzDoses)))upperCItestVals <- seq(bestParsECx["xmid"],bestParsECx["xmid"]+2*max(diff(log10(nzDoses))),length=20)
  if(verbose){
    cat("\nupperCItestVals\n",sep="")
    print(to3(10^upperCItestVals),quote=FALSE)
  }
  upperCIbounds <- try(boundCI(testVals = upperCItestVals,indata.BCI = BVdata,bestPars = bestParsECx,ECX=ECXvalue))
  #if error, try smaller grid
  if(regexpr("error",class(upperCIbounds))>0){
    upperCItestVals <- seq(bestParsECx["xmid"],max(log10(nzDoses))+2*max(diff(log10(nzDoses))),length=200)
    upperCIbounds <- try(boundCI(testVals = upperCItestVals,indata.BCI = BVdata,bestPars = bestParsECx,ECX=ECXvalue))
  }
  if(verbose){
    cat("\nupper CI Bounds\n",sep="")
    print(upperCIbounds)
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check8 fitBV.PLL")
  }
  
  
  #and then solve for the endpoints of the CI using uniroot -- this is the clean way
  if(regexpr("error",class(lowerCIbounds))<0 & lowerCIbounds[3]==1){
    lowerCI.xmid <- try(uniroot(f=unirootFUN.LOG,interval=lowerCIbounds[1:2],indata=BVdata,ECX=ECXvalue,parsGuess=lowerCIbounds[c("Asym","scal")])$root)
    lowerCI <- .lastBest
  }
  if(verbose){
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check9 fitBV.PLL")
  }
  
  #if lowerCI bounds did not crash out, just still lower, try the uniroot approach and let it extend interval
  if(regexpr("error",class(lowerCIbounds))<0 & lowerCIbounds[3]==0){
    if(verbose){
      print("trying to extend lower bound")
      print(lowerCIbounds)
    }
    #lowerCIbounds[1:2]+c(-1,0) extends the interval downwards, and, using extendInt = "downX" tells uniroot to keep looking even beyond
    #that to the low side (because we know the LL curve has a negative slope where to root is)
    #until it gives up (usually that will mean an error that the LL cant be calculated, it is always an error).  If this throws an error,
    #give up.
    lowerCI.xmid <- try(uniroot(f=unirootFUN.LOG,interval=lowerCIbounds[1:2]+c(-1,0),indata=BVdata,ECX=ECXvalue,parsGuess=lowerCIbounds[c("Asym","scal")],extendInt = "downX"))
    if(is(lowerCI.xmid,"error")){
      #give up, assign NAs
      lowerCI.xmid <- NA
      lowerCI <- c(NA,NA)
    }
    if(!is(lowerCI.xmid,"error")){
      #success, assign results
      lowerCI <- .lastBest
      if(verbose)print(lowerCI.xmid)
      lowerCI.xmid <- lowerCI.xmid$root
      if(verbose){
        print(lowerCI.xmid)
        print("end trying to lower extend")
      }
    }
  }
  if(verbose){
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check10 fitBV.PLL")
  }
  
  
  if(regexpr("error",class(upperCIbounds))<0 & upperCIbounds[3]==0){
    if(verbose){
      print("trying to extend upper bound")
      print(upperCIbounds) 
      print(upperCIbounds)
    }
    #upperCIbounds[1:2]+c(0,1) extends the interval upwards, and, using extendInt = "upX" tells uniroot to keep looking even beyond
    #until it gives up (usually that will mean an error that the LL cant be calculated)
    upperCI.xmid <- try(uniroot(f=unirootFUN.LOG,interval=upperCIbounds[1:2]+c(0,1),indata=BVdata,ECX=ECXvalue,parsGuess=upperCIbounds[c("Asym","scal")],extendInt = "upX"))
    if(is(upperCI.xmid,"error")){
      upperCI.xmid <- NA
      upperCI <- c(NA,NA)
    }
    if(!is(lowerCI.xmid,"error")){
      upperCI <- .lastBest
      if(verbose)print(upperCI.xmid)
      upperCI.xmid <- upperCI.xmid$root
      if(verbose){
        print(upperCI.xmid)
        print("end trying to extend upper")
      }
    }
  }
  if(verbose){
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check11 fitBV.PLL")
  }
  
  if(FALSE){
    #  if(!(is(lowerCIbounds,"error") & lowerCIbounds[3]==1)){
    #dont bother looking for CI limits if the bounding didn't work.  Set to pars at lowest dose checked
    print("check of lowerCIbounds")
    print(lowerCIbounds)
    if(verbose)print("Last Ditch on Lower CI")
    fitAtLow <- optimx(par=bestPars[c("Asym","scal")],optFUN.BV.LOG.PL,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=ECXvalue,xmid=log10(min(nzDoses)),
                       control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
    fitAtLow <- fitAtLow[order(fitAtLow$value),]
    print("fitAtLow")
    print(fitAtLow)
    startVals <- structure(unname(coef(fitAtLow)[1,]),names=c("Asym","scal"))
    print(startVals)
    fitAtLowLow <- optimx(par=startVals,optFUN.BV.LOG.PL,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=ECXvalue,xmid=tail(lowerCItestVals,1),
                          control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
    fitAtLow <- fitAtLow[order(fitAtLow$value),]
    lowerCI <- structure(unname(c(tail(coef(fitAtLowLow[1,],1)),tail(lowerCItestVals,1))),names=c("Asym","scal","xmid"))
    lowerCI.xmid <- lowerCI["xmid"]
    print(lowerCI)
  }
  
  if(regexpr("error",class(upperCIbounds))<0 & upperCIbounds[3]==1){
    if(verbose){
      print("check of upperCIbounds")
      print(upperCIbounds)
      print(unirootFUN.LOG(xmidVal = upperCIbounds[1],indata=BVdata,ECX=ECXvalue,parsGuess=upperCIbounds[c("Asym","scal")],debugTF = TRUE))
      print(unirootFUN.LOG(xmidVal = upperCIbounds[2],indata=BVdata,ECX=ECXvalue,parsGuess=upperCIbounds[c("Asym","scal")],debugTF = TRUE))
    }
    upperCI.xmid <- try(uniroot(f=unirootFUN.LOG,interval=upperCIbounds[1:2],indata=BVdata,ECX=ECXvalue,parsGuess=upperCIbounds[c("Asym","scal")])$root)
    upperCI <- .lastBest
  }
  if(verbose){
    print(paste(rep("#",80),collapse=""),quote=FALSE)
    print("check12 fitBV.PLL")
  }
  
  if(FALSE){
    #if(!(regexpr("error",class(upperCIbounds))<0 & upperCIbounds[3]==1)){
    #upperCI.xmid <- bestPars["xmid"]
    #upperCI <- bestPars
    if(verbose)print("Last Ditch on Upper CI")
    startVals <- structure(unname(unlist(tail(fitAtHigh$par,1))),names=c("Asym","scal"))
    print(startVals)
    fitAtHighHigh <- optimx(par=startVals[c("Asym","scal")],optFUN.BV.LOG.PL,y=BVdata$y,logdoses=log10(BVdata$dose),ECx.target=ECXvalue,xmid=tail(upperCItestVals,1),
                            control=list(dowarn=FALSE,follow.on=TRUE,save.failures=FALSE),method=optimxMethods)
    upperCI <- structure(unname(c(unlist(tail(fitAtHighHigh$par,1)),tail(upperCItestVals,1))),names=c("Asym","scal","xmid"))
    upperCI.xmid <- upperCI["xmid"]
    
  }
  lowerCI <<- lowerCI
  upperCI <<- upperCI
  #bestPars <<- bestPars
  
  #these xlim values guarantee a log cycle separation of control data from tested concentrations
  #print("start clean plot")
  #if(FALSE){
  
  plotBV.LOG(BVdata,bestPars = bestParsECx,ECxTarget=ECXvalue,debugTF=verbose,
             ylimInput=c(0,max(BVdata$y,exp(lowerCI["Asym"]),exp(upperCI["Asym"]))),
             xlim=10^range(c(floor(c(lowerCI["xmid"],log10(min(BVdata$dose[BVdata$dose>0]))))-1,ceiling(c(upperCI["xmid"],log10(max(BVdata$dose)))))),
             clean=TRUE,littleLogs=FALSE,ylabel=ylabel,xlabel=xlabel)
  if(is.character(fileName))mtext(side=1,line=-2,outer=TRUE,text=fileName,cex=.5,adj=0.02)
  #}
  
  #print("start annotated plot")
  if(verbose)print(c(EC.level=ECXvalue,ECx=10^unname(bestParsECx["xmid"]),LCL.95=10^unname(lowerCI["xmid"]),UCL.95=10^unname(upperCI["xmid"]),C.level=exp(unname(bestParsECx["Asym"])),Scale=exp(unname(bestParsECx["scal"])),
          trustLower=unname(lowerCIbounds[3]),trustUpper=unname(upperCIbounds[3]),ECx.outside=as.numeric((10^bestParsECx["xmid"])>max(nzDoses))))
  
  
  plotBV.LOG(BVdata,bestPars = bestParsECx,ECxTarget=ECXvalue,debugTF=verbose,
             ylimInput=c(0,max(BVdata$y,exp(lowerCI["Asym"]),exp(upperCI["Asym"]))),
             xlim=10^range(c(floor(c(lowerCI["xmid"],log10(min(BVdata$dose[BVdata$dose>0]))))-1,ceiling(c(upperCI["xmid"],log10(max(BVdata$dose)))))
             ),ylabel=ylabel,xlabel=xlabel
  )
  if(is.character(fileName))mtext(side=1,line=-2,outer=TRUE,text=fileName,cex=.5,adj=0.02)
  mtext(side=1,line=-2.5,outer=TRUE,text=msgText,cex=.5,adj=0.98)
  #
  return(list(EC.level=ECXvalue,ECx=10^unname(bestParsECx["xmid"]),
              LCL.95=10^unname(lowerCI["xmid"]),UCL.95=10^unname(upperCI["xmid"]),
              C.level=exp(unname(bestParsECx["Asym"])),Scale=exp(unname(bestParsECx["scal"])),
              trustLower=unname(lowerCIbounds[3]),trustUpper=unname(upperCIbounds[3]),
              ECx.outside=as.numeric((10^bestParsECx["xmid"])>max(nzDoses)),lowerCI=lowerCI))
}



#fit functions are coming from the beasley directory
if(FALSE){
  foo <- read.table(file="~/Documents/Versteeg//daphniaOffspring.txt",header=FALSE,sep="\t",colClasses="numeric")
  foo2 <- data.frame(
    y=as.vector(t(foo[,-1])),
    dose=rep(foo[,1],each=10))
  foo2 <- na.omit(foo2[order(foo2$dose),])
  foo2
  fitBV.PLL(BVdata=foo2, ECXvalue=.1,verbose=TRUE)
  
  #fitBV.PLL(data.frame(dose=rep(c(0,1,2,5,10,100),each=10),y=rnorm(n=60,mean=seq(100,50,length=60))), ECXvalue=.1,verbose=TRUE)
  
  fitBV.PLL(read.delim(file=file.choose()),ECXvalue=.9,verbose=FALSE,varFixed = TRUE)
  fitBV.PLL(read.delim(file=file.choose()),ECXvalue=.1,verbose=FALSE,varFixed = TRUE)
  #fitBV.PLL(read.delim("/Users/carrGJ/Documents/ESOtools/data/BVtable1.txt"),ECXvalue=.2,verbose=FALSE,varFixed=FALSE)
  
}
