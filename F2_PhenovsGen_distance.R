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

# Create subtable of F0 distances for the F2pops with 2 Grand-Parents in 

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

# Option A - Identify F2s based on already-defined F0 sowing list
#=================================================================

    # Identify which pops have 0, 1 ou 2 GP in our Accession sowing list ####
    
    SowingList<-read.csv("C:/Users/Julie/Desktop/Postdoc/Projet Arabido/Arabido/F0_Sowinglist.csv",dec='.',header=T)
    
    F2pops.alldata$GM.sown<-(F2pops.alldata$Genotype1%in%SowingList$accessionid)
    F2pops.alldata$GF.sown<-(F2pops.alldata$Genotype2%in%SowingList$accessionid)
    
    F2pops.alldata$GP.sown<-NA
    for (r in 1:nrow(F2pops.alldata)){
      F2pops.alldata[r,'GP.sown']<-  sum(F2pops.alldata[r,c(38:39)]=='TRUE')
    }
    
    # Only select F2 pops who are in already defined F0 sowing list 
      
    F2.GrandChildren<-F2pops.alldata[!(F2pops.alldata$GP.sown==0),]
    dim(F2.GrandChildren) #177 40
    
    # From these, only select pops with both grand parents in F0 sowing list
    F2.w2GPsown<-F2pops.alldata[(F2pops.alldata$GP.sown==2),]
    dim(F2.w2GPsown)
    # 31
    
    # Plot GenoDist vs PhenoDist of GrandChildren only
    plot(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,type='n',main='PhenoDist vs GenoDist\n GrandChildren of sown Accessions ')
    text(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,labels=F2.GrandChildren$code,cex=0.7)
    
    # Show list of F2s with 2 GrandParents in sown list
    text(F2.w2GPsown$GeneticDist,F2.w2GPsown$DistPhenoTot,labels=F2.w2GPsown$code,cex=0.7,col='red',font=2)
    
    F2sow<-F2.w2GPsown[!(F2.w2GPsown$code=='rd039'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd326'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd278'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd351'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd034'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd099'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd081'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd086'),]
    dim(F2sow)
    F2sow<-F2sow[!(F2sow$code=='rd461'),]
    dim(F2sow) # 22 40
    F2sow<-F2sow[!(F2sow$code=='rd169'),]
    dim(F2sow) # 21 40
    F2sow<-F2sow[!(F2sow$code=='rd075'),]
    dim(F2sow) # 20 40
    
    # Plot F2s to sow on normal scale
    plot(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,type='n',main='PhenoDist vs GenoDist\n GrandChildren of sown Accessions ')
    text(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,labels=F2.GrandChildren$code,cex=0.7)
    text(F2sow$GeneticDist,F2sow$DistPhenoTot,labels=F2sow$code,cex=0.7,col='red',font=2)
    
    # Plot F2s to sow on log scale
    plot(log(F2.GrandChildren$GeneticDist),log(F2.GrandChildren$DistPhenoTot),type='n',main='PhenoDist vs GenoDist\n GrandChildren of sown Accessions ')
    text(log(F2.GrandChildren$GeneticDist),log(F2.GrandChildren$DistPhenoTot),labels=F2.GrandChildren$code,cex=0.7)
    text(log(F2sow$GeneticDist),log(F2sow$DistPhenoTot),labels=F2sow$code,cex=0.7,col='red',font=2)
    
    droplevels(F2sow$code)
      # [1] rd393 rd191 rd100 rd432 rd321 rd032 rd275 rd026 rd400 rd269 rd369 rd126 rd439 rd509 rd500 rd525 rd120 rd460 
      # [19] rd316 rd425

# Option B - Identify F2s based on already-defined F0 sowing list
#=================================================================
  
# Identify which pops have 0, 1 ou 2 GP in our Accession sowing list ####
##################################################################
### Code not necessary anymore, since we are not doing groups ####


# Identify which pops to select in each group of low-, mid-, high- genetic and phenotypic distance

#(1) LowP lowG
lowP.lowG<-F2.GrandChildren[F2.GrandChildren$DistPhenoTot<300&F2.GrandChildren$GeneticDist<0.022,]
dim(lowP.lowG) # 4
lowP.lowG$code # [1] rd190 rd120 rd533 rd512

text(lowP.lowG$GeneticDist,lowP.lowG$DistPhenoTot,labels=lowP.lowG$code,cex=0.6, col='darkgreen',font=2)


# (2) lowP midG
median(F2.GrandChildren$GeneticDist)#0.03
lowP.midG<-F2.GrandChildren[F2.GrandChildren$DistPhenoTot<100 &
                              F2.GrandChildren$GeneticDist<0.032 &
                              F2.GrandChildren$GeneticDist>0.028,]
dim(lowP.midG) # 4 40
lowP.midG$code # rd079 rd106 rd088 rd442

text(lowP.midG$GeneticDist,lowP.midG$DistPhenoTot,labels=lowP.midG$code,cex=0.6, col='red',font=2)

# (3) LowP HighG
lowP.highG<-F2.GrandChildren[order(F2.GrandChildren$GeneticDist,decreasing = TRUE),][1:4,]
dim(lowP.highG) # 4 40
lowP.highG$code # rd321 rd283 rd245 rd256

text(lowP.highG$GeneticDist,lowP.highG$DistPhenoTot,labels=lowP.highG$code,cex=0.6, col='blue2',font=2)

# (4) midP lowG
midP.lowG<-F2.GrandChildren[F2.GrandChildren$DistPhenoTot>900 &
                              F2.GrandChildren$DistPhenoTot<1250 & 
                              F2.GrandChildren$GeneticDist<0.025 ,]
dim(midP.lowG) # 4 40
midP.lowG$code #  rd153 rd496 rd499 rd126

text(midP.lowG$GeneticDist,midP.lowG$DistPhenoTot,labels=midP.lowG$code,cex=0.6, col='forestgreen',font=2)

# (5)midP midG
midP.midG<-F2.GrandChildren[F2.GrandChildren$DistPhenoTot>900 &
                              F2.GrandChildren$DistPhenoTot<1100 & 
                              F2.GrandChildren$GeneticDist>0.028 &
                              F2.GrandChildren$GeneticDist<0.032,]
dim(midP.midG) # 4 40
midP.midG$code # rd284 rd164 rd409 rd225

text(midP.midG$GeneticDist,midP.midG$DistPhenoTot,labels=midP.midG$code,cex=0.6, col='orange',font=2)

# (6) midP highG
midP.highG<-F2.GrandChildren[F2.GrandChildren$DistPhenoTot>900 &
                              F2.GrandChildren$DistPhenoTot<1800 & 
                              F2.GrandChildren$GeneticDist>0.036,]
dim(midP.highG) # 4 40
midP.highG$code # rd536 rd275 rd400 rd541

text(midP.highG$GeneticDist,midP.highG$DistPhenoTot,labels=midP.highG$code,cex=0.6, col='dodgerblue',font=2)

# (7) highP midG
highP.midG<-F2.GrandChildren[F2.GrandChildren$DistPhenoTot>2500,]

dim(highP.midG) # 4 40
highP.midG$code # rd393 rd537 rd348 rd535

text(highP.midG$GeneticDist,highP.midG$DistPhenoTot,labels=highP.midG$code,cex=0.6, col='deeppink1',font=2)

# Plot on a normal scale 
plot(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,type='n',main='PhenoDist vs GenoDist\n GrandChildren of sown Accessions ')
text(F2.GrandChildren$GeneticDist,F2.GrandChildren$DistPhenoTot,labels=F2.GrandChildren$code,cex=0.6)
text(lowP.lowG$GeneticDist,lowP.lowG$DistPhenoTot,labels=lowP.lowG$code,cex=0.6, col='darkgreen',font=2)
text(lowP.midG$GeneticDist,lowP.midG$DistPhenoTot,labels=lowP.midG$code,cex=0.6, col='red',font=2)
text(lowP.highG$GeneticDist,lowP.highG$DistPhenoTot,labels=lowP.highG$code,cex=0.6, col='blue2',font=2)
text(midP.lowG$GeneticDist,midP.lowG$DistPhenoTot,labels=midP.lowG$code,cex=0.6, col='forestgreen',font=2)
text(midP.highG$GeneticDist,midP.highG$DistPhenoTot,labels=midP.highG$code,cex=0.6, col='dodgerblue',font=2)
text(midP.midG$GeneticDist,midP.midG$DistPhenoTot,labels=midP.midG$code,cex=0.6, col='orange',font=2)
text(highP.midG$GeneticDist,highP.midG$DistPhenoTot,labels=highP.midG$code,cex=0.6, col='deeppink1',font=2)


# Plot on a log scale

plot(log(F2.GrandChildren$GeneticDist),log(F2.GrandChildren$DistPhenoTot),type='n',main='PhenoDist vs GenoDist\n GrandChildren of sown Accessions ')
text(log(F2.GrandChildren$GeneticDist),log(F2.GrandChildren$DistPhenoTot),labels=F2.GrandChildren$code,cex=0.6)
text(log(lowP.lowG$GeneticDist),log(lowP.lowG$DistPhenoTot),labels=lowP.lowG$code,cex=0.6, col='darkgreen',font=2)
text(log(lowP.midG$GeneticDist),log(lowP.midG$DistPhenoTot),labels=lowP.midG$code,cex=0.6, col='red',font=2)
text(log(lowP.highG$GeneticDist),log(lowP.highG$DistPhenoTot),labels=lowP.highG$code,cex=0.6, col='blue2',font=2)
text(log(midP.lowG$GeneticDist),log(midP.lowG$DistPhenoTot),labels=midP.lowG$code,cex=0.6, col='forestgreen',font=2)
text(log(midP.midG$GeneticDist),log(midP.midG$DistPhenoTot),labels=midP.midG$code,cex=0.6, col='orange',font=2)
text(log(midP.highG$GeneticDist),log(midP.highG$DistPhenoTot),labels=midP.highG$code,cex=0.6, col='dodgerblue',font=2)
text(log(highP.midG$GeneticDist),log(highP.midG$DistPhenoTot),labels=highP.midG$code,cex=0.6, col='deeppink1',font=2)
