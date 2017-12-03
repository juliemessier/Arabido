# Read F2 file 
F0.Dist<-read.csv("C:/Users/Julie/Desktop/Postdoc/Projet Arabido/Arabido/Geno&Pheno_Distances_by_combination.csv",dec='.',header=T)

dim(F0.Dist)
#[1] 203401     11

names(F0.Dist)
# [1] "idCombination" "Genotype1"     "Genotype2"     "GeneticDist"   "GeoDist"       "DistPhenoTot"  "DistPhenoPC1" 
# [8] "DistPhenoPC2"  "DistClimTot"   "DistClimPC1"   "DistClimPC2"


#Read F1 file
F1<-read.csv("C:/Users/Julie/Desktop/Postdoc/Projet Arabido/Arabido/F1.csv",dec='.',header=T)

dim(F1)
# [1] 367 25

names(F1)
# [1] "code"                         "idGenotype"                   "geno2"                       
# [4] "idMother"                     "idFather"                     "parent_1001G"                
# [7] "parent_231acc"                "Weight_1seed"                 "SeedWeight"                  
# [10] "estNbSeedsTot"                "could_include"                "NbSeeds.target"              
# [13] "weight_target"                "included_AraBreed"            "weight_seeds_put_in_AraBreed"
# [16] "DMmax"                        "T_repro"                      "nSiliques"                   
# [19] "GRtot"                        "AllomExp"                     "DM50"                        
# [22] "Age50"                        "GR50"                         "RGR50"                       
# [25] "FvFm_mean" 

# give same column name
# colnames(F1)[1:5]
# colnames(F2pops)[1:5]
# colnames(F1)[2]<-colnames(F2pops)[1]
# head(colnames(F1))

# Create subtable of F0 Distances for the F2 pops we have 
F2pops<-F0.Dist[(F0.Dist$idCombination%in%F1$idGenotype),]

dim(F2pops)
# 367 11
head(F2pops)[1:3]
  #      idCombination Genotype1 Genotype2
  # 545       108:7258       108      7258
  # 957       139:7288       139      7288
  # 1314      159:6975       159      6975
  # 1734      265:7036       265      7036
  # 2668      428:9512       428      9512
  # 2888      430:6040       430      6040

names(F2pops)
  # [1] "idCombination" "Genotype1"     "Genotype2"     "GeneticDist"   "GeoDist"       "DistPhenoTot" 
  # [7] "DistPhenoPC1"  "DistPhenoPC2"  "DistClimTot"   "DistClimPC1"   "DistClimPC2" 

            # Combine both datasets (F2pops & F1) into one with all the info. 
            
F2pops$idCombination%in%F1$idGenotype

F2pops.alldata<-merge(x=F2pops,y=F1, by.x='idCombination', by.y='idGenotype')
dim(F2pops.alldata) #367 35

# Plot GenoDist vs PhenoDist
plot(F2pops.alldata$GeneticDist,F2pops.alldata$DistPhenoTot,type='n',main='PhenoDist vs GenoDist')
text(F2pops.alldata$GeneticDist,F2pops.alldata$DistPhenoTot,labels=F2pops.alldata$code,cex=0.6)

# Plot log(GenoDist) vs log(PhenoDist)
plot(log(F2pops.alldata$GeneticDist),log(F2pops.alldata$DistPhenoTot),type='n',main='log(PhenoDist) vs log(GenoDist)')
text(log(F2pops.alldata$GeneticDist),log(F2pops.alldata$DistPhenoTot),labels=F2pops.alldata$code,cex=0.6)

# Identify which pops have 0, 1 ou 2 GP in our Accession sowing list ####

SowingList<-read.csv("C:/Users/Julie/Desktop/Postdoc/Projet Arabido/Arabido/F0_Sowinglist.csv",dec='.',header=T)

F2pops.alldata$GM.sown<-(F2pops.alldata$Genotype1%in%SowingList$accessionid)
F2pops.alldata$GF.sown<-(F2pops.alldata$Genotype2%in%SowingList$accessionid)

F2pops.alldata$GP.sown<-NA
for (r in 1:nrow(F2pops.alldata)){
  F2pops.alldata[r,'GP.sown']<-  sum(F2pops.alldata[r,c(38:39)]=='TRUE')
}
  
F2.GrandChildren<-F2pops.alldata[!(F2pops.alldata$GP.sown==0),]
dim(F2.GrandChildren) #177 40

# Plot GenoDist vs PhenoDist of GrandChildren only
plot(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,type='n',main='PhenoDist vs GenoDist')
text(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,labels=F2.GrandChildren$code,cex=0.6)

# Identify which pops to select in each group of low-, mid-, high- genetic and phenotypic distance

# which rows to select?
dim(F2pops.alldata) #367 35
# 6 lowest P distance = rows 1:6
367/2 = 183.5
# 6 mid P distance = rows 180-186
# 6 highest P distance = rows 361-367

# give relative ranking on phenotypic and genetic distance to add the two values
F2pops.alldata$rel.GeneticDist<-F2pops.alldata$GeneticDist/max(F2pops.alldata$GeneticDist)
F2pops.alldata$rel.PhenoDist<-F2pops.alldata$DistPhenoTot/max(F2pops.alldata$DistPhenoTot)

# Plot RELATIVE GenoDist vs PhenoDist
plot(F2pops.alldata$rel.GeneticDist,F2pops.alldata$rel.PhenoDist,type='n',main='PhenoDist vs GenoDist')
text(F2pops.alldata$rel.GeneticDist,F2pops.alldata$rel.PhenoDist,labels=F2pops.alldata$code,cex=0.6)


# Low Phenotypic distance group
lowP<-F2pops.alldata[order(F2pops.alldata$DistPhenoTot,decreasing = FALSE),][1:6,]
dim(lowP) # 6 35

# Mid Phenotypic distance group
midP<-F2pops.alldata[order(F2pops.alldata$DistPhenoTot,decreasing = FALSE),][181:186,]
dim(midP) # 6 35

# High Phenotypic distance group
highP<-F2pops.alldata[order(F2pops.alldata$DistPhenoTot,decreasing = FALSE),][362:367,]
dim(highP) # 6 35


