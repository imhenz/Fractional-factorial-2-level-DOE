#importing the dataset
Dataset_defoamer <-
  read.table("C:/Users/brost/OneDrive/Desktop/project/udefoamer.csv",
             header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
editDataset(Dataset_defoamer)
editDataset(Dataset_defoamer)
defoamerfrf2Design <- FrF2(nruns= 8 ,nfactors= 3 , blocks= 1 ,
                           alias.block.2fis = FALSE , ncenter= 0 , MaxC2 = FALSE , resolution = NULL ,
                           replications= 1 ,repeat.only= FALSE ,randomize= TRUE ,seed= 20219 ,
                           factor.names=list( Diameter=c(0.2,0.4),GasThrougtput=c(1.15,5.5),
                                              TensideConc=c(0.05,0.1) ) )
## creator element of design.info will be different, when using the command line command!
print( defoamerfrf2Design )
summary( defoamerfrf2Design , brief = TRUE)

### adding response variable to the design
defoamerfrf2Designaddresponse <- add.response(defoamerfrf2Design,
                                              "C:/Users/brost/OneDrive/Desktop/project/zvalues2.csv", replace=FALSE)

#### main effect and interaction plot
MEPlot(defoamerfrf2Designaddresponse, abbrev=4, select=c(1,2), response="zValue")
IAPlot(defoamerfrf2Designaddresponse, abbrev=4, show.alias=FALSE, select=c(1,2))

###  fitting the model --linear model
LinearModel <- lm(zValue ~ Diameter +GasThrougtput, data=defoamerfrf2Designaddresponse)
summary(LinearModel)
plot(allEffects(LinearModel))

###To generate contour plot of the response values as a function of input of the independent variables
par(mfrow=c( 2 , 1 ), oma=c(3,0,2,0))
contour( linearModel, as.list(c( ~Diameter*Gasthroughput )), image= TRUE , atpos= 9 )
mtext( "Response surface plots for zValue" , outer=TRUE)
mtext( "Slice at: Diameter=0.3, Gasthroughput=3.31" , outer=TRUE, side=1)
mtext( "Response surface plots for zValue" , outer=TRUE)
mtext( "Slice at: Diameter=0.3, Gasthroughput=3.31" , outer=TRUE, side=1)