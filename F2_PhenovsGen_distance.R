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



# Create subtable of F0 Distances for the F2 pops we have 
F2pops<-F0.Dist[which(F1$idGenotype%in%F0.Dist$idCombination),]

dim(F2pops)
# 367 11

head(F1$idGenotype)
head(F2.Dist$idCombination)

# Plot GenoDist vs PhenoDist
plot(F2.Dist$GeneticDist,F2.Dist$DistPhenoTot,type='n',main='PhenoDist vs GenoDist')
text(F2.Dist$GeneticDist,F2.Dist$DistPhenoTot,labels=F2.Dist$idCombination,cex=0.7)