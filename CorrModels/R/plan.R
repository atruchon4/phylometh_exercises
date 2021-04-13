plan <- drake_plan(
  treeprimates = read.tree(text="((((Homo:0.21,Pongo:0.21):0.28,Macaca:0.49):0.13,Ateles:0.62):0.38,Galago:1.00);") ,
  X <- c(4.09434, 3.61092, 2.37024, 2.02815, -1.46968),
  Y <- c(4.74493, 3.33220, 3.36730, 2.89037, 2.30259),
  names(X) <- names(Y) <- c("Homo", "Pongo", "Macaca", "Ateles", "Galago"),
  picX = pic(X, treeprimates),
  picY = pic(Y, treeprimates),
  require("corHMM"),
  data(primates),
  print(primates),
  require(phytools),
  primates$trait[which(grepl("Hylobates",primates$trait[,1])),2]<- 1,
  trait1 = primates$trait[,2],
  names(trait1) <- primates$trait[,1],
  primates$tree <- ape::multi2di(primates$tree),
  plotSimmap(make.simmap(primates$tree, trait1), pts=FALSE, fsize=0.8),
  ratemater = rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=1, nstates=2, model="ER"),
  print(ratemater),
  pper = corHMM(primates$tree,primates$trait[,c(1,2)],rate.cat=1,rate.mat=ratemater,node.states="marginal"),
  print(pper),
  ratematard = rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=1, nstates=2, model="ARD"),
  print(ratematard),
  ppard = corHMM(primates$tree,primates$trait[,c(1,2)],rate.cat=1,rate.mat=ratematard,node.states="marginal"),
  print(ppard), # The AIC for the ARD model is higher, meaning the ER model is the better model.
  ratemater4state = rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=1, nstates=4, model="ER"),
  print(ratemater4state),
  fourstate.trait = rep(NA,Ntip(primates$tree)),
  print(fourstatetrait),
  for(i in sequence(Ntip(primates$tree))) {
    if(primates$trait[i,2]==0 && primates$trait[i,3]==0) {
      fourstate.trait[i]<-0
    }
    if(primates$trait[i,2]==0 && primates$trait[i,3]==1) {
      fourstate.trait[i]<-1
    }
    if(primates$trait[i,2]==1 && primates$trait[i,3]==0) {
      fourstate.trait[i]<-2
    }
    if(primates$trait[i,2]==1 && primates$trait[i,3]==1) {
      fourstate.trait[i]<-3
    }
  },
  fourstate.data = data.frame(Genus_sp=primates$trait[,1], T1=fourstate.trait),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, model="ER", node.states="marginal")),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat=ratemater4state, node.states="marginal", model="ARD")),
  rate.mat.ard.4state = rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=1, nstates=4, model="ARD"),
  print(rate.mat.ard.4state),
  rate.mat.gtr.4state = rate.mat.ard.4state,
  rate.mat.gtr.4state = rate.par.eq(rate.mat.gtr.4state, c(1,4)),
  rate.mat.gtr.4state = rate.par.eq(rate.mat.gtr.4state, c(2,6)),
  rate.mat.gtr.4state = rate.par.eq(rate.mat.gtr.4state, c(3,8)),
  rate.mat.gtr.4state = rate.par.eq(rate.mat.gtr.4state, c(4,6)),
  rate.mat.gtr.4state = rate.par.eq(rate.mat.gtr.4state, c(5,7)),
  rate.mat.gtr.4state = rate.par.eq(rate.mat.gtr.4state, c(6,7)),
  print(rate.mat.gtr.4state),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat= rate.mat.gtr.4state, node.states="marginal", model="ARD")),
  print(rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=2, nstates=2, model="ARD")),
  ratematpag94 = rate.par.drop(rate.mat.ard.4state, drop.par=c(3,5,8,10))
  )

hwplan <- drake_plan(
  data(primates),
  primates$trait[which(grepl("Hylobates",primates$trait[,1])),2]<-1,
  trait1<-primates$trait[,2],
  names(trait1)<-primates$trait[,1],
  fourstate.data<-data.frame(Genus_sp=primates$trait[,1], T1=fourstate.trait),
  empty.mat<-rate.mat.ard.4state<-rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=1, nstates=4, model="ARD"),
  pag94.mat<-rate.par.drop(empty.mat, drop.par=c(3,5,8,10)),
  noloseone.mat <- rate.par.drop(pag94.mat, drop.par = c(2, 4)),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat= noloseone.mat, node.states="marginal", model="ARD")),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat= pag94.mat, node.states="marginal", model="ARD")),
  # The model that drops any abiity of to lose state one has a lower AIC than the typical Pagel 94 model.
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat= noloseone.mat, node.states="marginal", model="ARD", root.p = "yang")),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat= noloseone.mat, node.states="marginal", model="ARD", root.p = "maddfitz")),
  # Applying the procedure Maddison and FitzJohn used for their root reveals the lowest AIC yet established for this set of data.
  new.mat <- rate.par.drop(pag94.mat, drop.par = c(5)),
  print(rayDISC(primates$tree, fourstate.data, ntraits=1, rate.mat= new.mat, node.states="marginal", model="ARD"))
  
  # This model does retrieve rates for the traits to go from (0,0) to (0,1) to (1,1). However the AIC it retrieves is about as high as previous models.
)
