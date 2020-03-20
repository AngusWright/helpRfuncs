 
dev2png<-function(file='Rplot_%02d.png',res=220) { 
  dev.print(png,file=file,res=res,width=dev.size('in')[1]*res,height=dev.size('in')[2]*res)
}
