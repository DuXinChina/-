D.test=function(com1,n,comtot,replicate)
{
  D=function(tan)
  {
    seq=c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)
    seq=log(seq)
    #per=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    per=c(0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)
    d=per
    for(i in 1:10){d[i]=sum(pnorm(per[i],tan,density(tan)$bw))/sum(pnorm(1,tan,density(tan)$bw))}
    per=d
    for (i in 1:9)
    {d[i+1]=per[i+1]-per[i]}
    per=d
    per=log(per)
    data=cbind(per,seq)
    data=data[which(abs(data[,1])<9999999 & abs(data[,2])<9999999 ),]
    lm=lm(data[,1]~data[,2]+0)
    D=3-lm$coefficients
    as.numeric(D)
  }
  Dvaule=D(com1)
  onetest=function()
  {
    sample=sample(comtot,n,replace = TRUE)
    D=D(sample)
    D
  }
  Re=replicate(replicate,onetest())
  row=1:200
  range=seq(min(Re)-3*density(Re)$bw,max(Re)+3*density(Re)$bw,length.out=200)
  confidence=matrix(1,nrow=200,ncol=1)
  for (i in 1:200){ confidence[i]=sum(pnorm(range[i],Re,density(Re)$bw))/sum(pnorm(max(range),Re,density(Re)$bw))}
  o.025=matrix(0.025,nrow=200,ncol=1)
  confidence=matrix(confidence,ncol=1,nrow=200)
  confidence0.025=abs(confidence-o.025)
  mino.025=min(confidence0.025)
  confidence0.025=cbind(row,confidence0.025)
  minrownum0.025=confidence0.025[confidence0.025[,2]==mino.025]
  low=range[minrownum0.025[1]]
  range=seq(min(Re)-2*density(Re)$bw,max(Re)+2*density(Re)$bw,length.out=200)
  for (i in 1:200){ confidence[i]=sum(pnorm(range[i],Re,density(Re)$bw))/sum(pnorm(max(range),Re,density(Re)$bw))}
  o.975=matrix(0.975,nrow=200,ncol=1)
  confidence=matrix(confidence,ncol=1,nrow=200)
  confidence0.975=abs(confidence-o.975)
  mino.975=min(confidence0.975)
  confidence0.975=cbind(row,confidence0.975)
  minrownum0.975=confidence0.975[confidence0.975[,2]==mino.975]
  up=range[minrownum0.975[1]]
  con=sum(pnorm(Dvaule,Re,density(Re)$bw))/sum(pnorm(max(range),Re,density(Re)$bw))
  conn=(sum(pnorm(max(range),Re,density(Re)$bw))-sum((pnorm(Dvaule,Re,density(Re)$bw))))/sum(pnorm(max(range),Re,density(Re)$bw))
  con=min(con,conn)*2
  library(ggplot2)
  p=ggplot(data=NULL,aes(x=Re))+geom_density(colour="black",fill="grey")+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"))
  p1=p+geom_vline(aes(xintercept=Dvaule), colour="#9d2933",size=1)+geom_vline(aes(xintercept=low), colour="#23238E", linetype="dashed",size=1)+geom_vline(aes(xintercept=up), colour="#23238E", linetype="dashed",size=1)
  p1=p1+labs(x = "加权进界邻体模式荫蔽度\n Fractal dimensions of weighted inside \n boundary neighbour shading degree", y = " Probability Density \n 概率密度")+xlim(min(low,Dvaule)-1.5*sd(Re),max(up,Dvaule)+1.5*sd(Re))
  p1=p1+annotate("text",x=min(low,Dvaule)-0.9*sd(Re),y=0.9*max(density(Re)$y),label = paste0("italic(p) ","==",round(con,3)), parse = TRUE)
  p1
}