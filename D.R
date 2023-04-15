
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