
install.packages('ggplot2')
install.packages('patchwork')
library(ggplot2)
library(patchwork)

#Importing Car details, gas prices, and inflation
MakeModelMPG <- read.csv("D:/RLearning/HybridvsRegular/MakeModelMPG.csv", stringsAsFactors = FALSE)
names(MakeModelMPG)[1] <- "Manufacturer"

MakeModelMPG2012 <- read.csv("D:/RLearning/HybridvsRegular/MakeModel2012.csv", stringsAsFactors = FALSE)
names(MakeModelMPG2012)[1] <- "Model"

CAgas <- read.csv("D:/RLearning/HybridvsRegular/CAretailgasprice.csv", stringsAsFactors = FALSE)
names(CAgas)[1] <- "Year"
CAgas <- CAgas[12:19,]

PCE <- read.csv("D:/RLearning/HybridvsRegular/AnnualPCE.csv", stringsAsFactors = FALSE)
PCE <- PCE[84:91,]
names(PCE)[2] <- "PCEindex"

## Importing Vehicle Miles Traveled and Licensed Drivers 
## Creating Yearly vMT per licensed driver
licenses <- read.csv("D:/RLearning/HybridvsRegular/licensed2012_2019.csv")
vmt <- read.csv("D:/RLearning/HybridvsRegular/vehiclemiles2012_2019.csv")
vmt$VMT <- vmt$VMT_millions * 1000000
vmt$vmt_licensed <- vmt$VMT/licenses$licensed

## Deflating gas prices to 2012
CAgas$realPrice <- CAgas$Price/PCE$PCEindex * 100

##Relative MSRP of Hybrid vs Regular
RelativePrice <- data.frame(PricePremium = c((MakeModelMPG2012[2,3]- MakeModelMPG2012[1,3])/MakeModelMPG2012[1,3],(MakeModelMPG2012[4,3]- MakeModelMPG2012[3,3])/MakeModelMPG2012[3,3],(MakeModelMPG2012[6,3]- MakeModelMPG2012[5,3])/MakeModelMPG2012[5,3],(MakeModelMPG2012[8,3]- MakeModelMPG2012[7,3])/MakeModelMPG2012[7,3], (MakeModelMPG2012[10,3]- MakeModelMPG2012[9,3])/MakeModelMPG2012[9,3]), MakeModel = c("Toyota Camry", "Ford Fusion", "Honda Civic", "Hyundai Sonata", "Kia Optima"))

##################Toyota Camry################################################
#Gas total expense
CAgas$camry_gasexpense <- CAgas$realPrice/MakeModelMPG2012[1,6] * vmt$vmt_licensed
CAgas$sum_camry <- cumsum(CAgas$camry_gasexpense)
CAgas$camry_totalexpense <- CAgas$sum_camry + MakeModelMPG2012[1,3]
#Hybrid total expense
CAgas$hcamry_gasexpense <- CAgas$realPrice/MakeModelMPG2012[2,6] * vmt$vmt_licensed
CAgas$hcamry_gasexpense <- CAgas$realPrice/MakeModelMPG2012[2,6] * vmt$vmt_licensed
CAgas$sum_hcamry <- cumsum(CAgas$hcamry_gasexpense)
CAgas$hcamry_totalexpense <- CAgas$sum_hcamry + MakeModelMPG2012[2,3]

######################Ford Fusion############################################
#Gas
CAgas$fusion_gasexpense <- CAgas$realPrice/MakeModelMPG2012[3,6] * vmt$vmt_licensed
CAgas$sum_fusion <- cumsum(CAgas$fusion_gasexpense)
CAgas$fusion_totalexpense <- CAgas$sum_fusion + MakeModelMPG2012[3,3]
#Hybrid
CAgas$hfusion_gasexpense <- CAgas$realPrice/MakeModelMPG2012[4,6] * vmt$vmt_licensed
CAgas$sum_hfusion <- cumsum(CAgas$hfusion_gasexpense)
CAgas$hfusion_totalexpense <- CAgas$sum_hfusion + MakeModelMPG2012[4,3]

#####################Honda Civic ###########################################
#Gas
CAgas$civic_gasexpense <- CAgas$realPrice/MakeModelMPG2012[5,6] * vmt$vmt_licensed
CAgas$sum_civic <- cumsum(CAgas$civic_gasexpense)
CAgas$civic_totalexpense <- CAgas$sum_civic + MakeModelMPG2012[5,3]
#Hybrid
CAgas$hcivic_gasexpense <- CAgas$realPrice/MakeModelMPG2012[6,6] * vmt$vmt_licensed
CAgas$sum_hcivic <- cumsum(CAgas$hcivic_gasexpense)
CAgas$hcivic_totalexpense <- CAgas$sum_hcivic + MakeModelMPG2012[6,3]

####################Hyundai Sonata ########################################
#Gas
CAgas$hyundai_gasexpense <- CAgas$realPrice/MakeModelMPG2012[7,6] * vmt$vmt_licensed
CAgas$sum_hyundai <- cumsum(CAgas$hyundai_gasexpense)
CAgas$hyundai_totalexpense <- CAgas$sum_hyundai + MakeModelMPG2012[7,3]
#Hybrid
CAgas$hhyundai_gasexpense <- CAgas$realPrice/MakeModelMPG2012[8,6] * vmt$vmt_licensed
CAgas$sum_hhyundai <- cumsum(CAgas$hhyundai_gasexpense)
CAgas$hhyundai_totalexpense <- CAgas$sum_hhyundai + MakeModelMPG2012[8,3]

#####################Kia Optima ############################################
#Gas
CAgas$kia_gasexpense <- CAgas$realPrice/MakeModelMPG2012[9,6] * vmt$vmt_licensed
CAgas$sum_kia <- cumsum(CAgas$kia_gasexpense)
CAgas$kia_totalexpense <- CAgas$sum_kia + MakeModelMPG2012[9,3]
#Hybrid
CAgas$hkia_gasexpense <- CAgas$realPrice/MakeModelMPG2012[10,6] * vmt$vmt_licensed
CAgas$sum_hkia <- cumsum(CAgas$hkia_gasexpense)
CAgas$hkia_totalexpense <- CAgas$sum_hkia + MakeModelMPG2012[10,3]

###############################################################################

##Relative MSRP Plot
RelativeMSRP <- ggplot(RelativePrice, aes(y = PricePremium, x = MakeModel)) + geom_bar(stat = "identity", aes(fill = MakeModel), color = "black") + labs(x = "Make and Model", y = "Relative Difference in MSRP", title = "Relative Difference in MSRP for Hybrid vs Gas", subtitle = "2012 Model Year", fill = "Make and Model") + geom_text(aes(label=PricePremium), vjust = 1.5, size = 6)


###Total Expense Plots with Y-Axis MSRP
toyota <- ggplot(data = CAgas, aes(x=Year)) + geom_line(aes(y=camry_totalexpense, color = "darkred")) + geom_line(aes(y=hcamry_totalexpense, color = "steelblue")) + geom_point(aes(y=camry_totalexpense)) + geom_point(aes(y=hcamry_totalexpense)) + labs(title = "Total Expense of a 2012 Toyota Camry vs Hybrid Variant", y = "Cost (2012 USD)", subtitle = "Gas Prices Based on California Annual Averages") + scale_color_discrete(name = "Engine Type", labels = c("Gas","Hybrid")) + geom_segment(aes(x=2011,xend=2012,y=22600,yend=24563.64), color = "darkred") + geom_segment(aes(x=2011,xend=2012,y=25990,yend=27426.81), color = "steelblue") + scale_x_continuous(limits=c(2011,2019.5),expand= c(0,0), breaks = c(2012,2014,2016,2018)) + scale_y_continuous(breaks = c(22600, 24000, 25990, 28000, 32000, 36000))

ford <- ggplot(data = CAgas, aes(x=Year)) + geom_line(aes(y=fusion_totalexpense, color = "darkred")) + geom_line(aes(y=hfusion_totalexpense, color = "steelblue")) + geom_point(aes(y=fusion_totalexpense)) + geom_point(aes(y=hfusion_totalexpense)) + labs(title = "Ford Fusion SEL", y = "Cost (2012 USD)") + scale_color_discrete(name = "Engine Type", labels = c("Gas","Hybrid")) + theme(legend.position = 'none') + geom_segment(aes(x=2011,xend=2012,y=25425,yend=27528.90), color = "darkred") + geom_segment(aes(x=2011,xend=2012,y=28775,yend=30305.11), color = "steelblue") + scale_x_continuous(limits=c(2011,2019.5),expand= c(0,0), breaks = c(2012,2014,2016,2018)) + scale_y_continuous(breaks = c(25425,28775,32500,35000,37500))

honda <- ggplot(data = CAgas, aes(x=Year)) + geom_line(aes(y=civic_totalexpense, color = "darkred")) + geom_line(aes(y=hcivic_totalexpense, color = "steelblue")) + geom_point(aes(y=civic_totalexpense)) + geom_point(aes(y=hcivic_totalexpense)) + labs(title = "Honda Civic EX", x = " ", y = " ") + theme(legend.position = 'none') + geom_segment(aes(x=2011,xend=2012,y=20665,yend=22413.48), color = "darkred") + geom_segment(aes(x=2011,xend=2012,y=24200,yend=25538.85), color = "steelblue") + scale_x_continuous(limits=c(2011,2019.5),expand= c(0,0), breaks = c(2012,2014,2016,2018)) + scale_y_continuous(breaks = c(20665,22500,24200,27500,30000,32500))

hyundai <- ggplot(data = CAgas, aes(x=Year)) + geom_line(aes(y=hyundai_totalexpense, color = "darkred")) + geom_line(aes(y=hhyundai_totalexpense, color = "steelblue")) + geom_point(aes(y=hyundai_totalexpense)) + geom_point(aes(y=hhyundai_totalexpense)) + labs(title = "Hyundai Sonata GLS", y = " ", x = ' ') + theme(legend.position = 'none') + geom_segment(aes(x=2011,xend=2012,y=20795,yend=22791.92), color = "darkred") + geom_segment(aes(x=2011,xend=2012,y=25850,yend=27463.95), color = "steelblue") + scale_x_continuous(limits=c(2011,2019.5),expand= c(0,0), breaks = c(2012,2014,2016,2018)) + scale_y_continuous(breaks = c(20795,25850,30000,34000))

kia <- ggplot(data = CAgas, aes(x=Year)) + geom_line(aes(y=kia_totalexpense, color = "darkred")) + geom_line(aes(y=hkia_totalexpense, color = "steelblue")) + geom_point(aes(y=kia_totalexpense)) + geom_point(aes(y=hkia_totalexpense)) + labs(title = "Kia Optima LX", y = " ", x = " ") + scale_color_discrete(name = "Engine Type", labels = c("Gas","Hybrid")) + geom_segment(aes(x=2011,xend=2012,y=21000,yend=22996.92), color = "darkred") + geom_segment(aes(x=2011,xend=2012,y=25800,yend=27413.95), color = "steelblue") + scale_x_continuous(limits=c(2011,2019.5),expand= c(0,0), breaks = c(2012,2014,2016,2018)) + scale_y_continuous(breaks = c(21000,25800,30000,34000))


(ford | honda | hyundai | kia) / RelativeMSRP

