exp.design<-data.frame(matrix(nrow=560,ncol=1))
dim(exp.design)

exp.design[,1]<-seq(from=1,to=560)
colnames(exp.design)<-"Pot.ID"

# Add Generation name
exp.design$Generation<-c(rep("F0",times=200),rep("F2",times=360))
head(exp.design)

# Add F0 and F2 genotype names
F0.list<-c(88,5151,6009,6040,6074,6076,6108,6145,6151,6177,6180,6184,6195,6216,6243,6898,
           6903,6911,6915,6922,6929,6938,6958,6961,6963,6970,6979,6987,7000,7002,7008,7028,7063,7077,7127,7143,7165,
           7186,7192,7296,7316,7320,7347,7394,8222,8230,8231,8240,8247,8351,8354,8357,8376,9057,9058,9371,9394,9399,
           9416,9470,9481,9507,9518,9520,9524,9528,9535,9537,9544,9548,9549,9553,9560,9567,9576,9577,9581,9586,9587,
           9589,9594,9600,9606,9625,9637,9640,9649,9669,9697,9726,9737,9741,9743,9749,9758,9784,9939,9943,9947,10006)

F2.list<-c('rd026','rd032','rd100','rd120','rd126','rd191','rd269','rd275','rd316','rd321','rd369','rd393','rd400',
           'rd425','rd432','rd439','rd460','rd500','rd509','rd525')

exp.design$Genotype<-c(rep(F0.list,each=2),rep(F2.list,each=18))

# Add Genotype replicate
exp.design$Genotype.replicate<-c(rep(1:2,times=100),rep(1:18,times=20))
head(exp.design)

# Add Unique Plant Name
exp.design$Plant.Unique.Name<-paste0(exp.design$Genotype,'-',exp.design$Genotype.replicate)
tail(exp.design)

# Add table numbers

# For F0, 2 plants per genotype, 1 plant on each of 2 different tables
F0.table.order<-c()
for (i in 1:4){
  for (j in (i+1):5){
    F0.table.order<-append(F0.table.order,values=c(i,j))
    }
}
F0.table.order
length(F0.table.order) # table assignment for 20 pots (10 genotypes)
200/20 #10 - Repeat 10 times to assign tables to 200 plants (and 100 genotypes)

table (F0.table.order)
  # 1 2 3 4 5 
  # 4 4 4 4 4

rep(F0.table.order,times=5)

# For F2, 18 plants per genotype, 4 plants on each of 3 different tables, 3 plants on remaining two tables

F2.table.order<-c()
t<-rep(seq(1:5),times=2)

for (i in 1:5){
    F2.table.order<-append(F2.table.order,
                           values=c(rep(t[i],times=4),
                                    rep(t[i+1],times=4),
                                    rep(t[i+2],times=4),
                                    rep(t[i+3],times=3),
                                    rep(t[i+4],times=3)))
}
F2.table.order

table(F2.table.order)
  # 1  2  3  4  5 
  # 18 18 18 18 18

length(F2.table.order) # table assignment for 90 pots (5 genotypes)
360/90 #4  - Repeat 4 times to assign tables to 360 plants (and 20 genotypes)

# Table assignment
exp.design$Table<-c(rep(F0.table.order,times=10),rep(F2.table.order,times=4))
head(exp.design)
tail(exp.design)

table(exp.design$Table)
  # 1   2   3   4   5 
  # 112 112 112 112 112 
sum(table(exp.design$Table)) # 560


# Re-order DataFrame by Table in order to assign random rows and columns
exp.design<- exp.design[order(exp.design$Table,decreasing=FALSE),]

# Assign random row numbers within each table (set of 112 rows, 5 times)
exp.design$Row<-rep(sample(rep(LETTERS[1:8],times=14),replace=F),times=5)#each table has 14 columns of rows A-G, repeat for 5 tables

# make sure each Table has 14 of each row
table(exp.design$Row,by=exp.design$Table)
    # by
    # 1  2  3  4  5
    # A 14 14 14 14 14
    # B 14 14 14 14 14
    # C 14 14 14 14 14
    # D 14 14 14 14 14
    # E 14 14 14 14 14
    # F 14 14 14 14 14
    # G 14 14 14 14 14
    # H 14 14 14 14 14

head(exp.design)
tail(exp.design)

# Re-order DataFrame by Rows within tables in order to assign unique column numbers within each row within each table. 
exp.design<-exp.design[order(exp.design$Table,exp.design$Row,decreasing=F),]
exp.design[1:14,]

# Assign random column numbers within each table (set of 112 columns, 5 times)
exp.design$Column<-rep(rep(sample(seq(1:14),replace=F),times=8),times=5)# each table has 8 rows of columns 1-14, repeat for 5 tables

# make sure each Table has 8 of each column
table(exp.design$Column,by=exp.design$Table)
  # by
  # 1 2 3 4 5
  # 1  8 8 8 8 8
  # 2  8 8 8 8 8
  # 3  8 8 8 8 8
  # 4  8 8 8 8 8
  # 5  8 8 8 8 8
  # 6  8 8 8 8 8
  # 7  8 8 8 8 8
  # 8  8 8 8 8 8
  # 9  8 8 8 8 8
  # 10 8 8 8 8 8
  # 11 8 8 8 8 8
  # 12 8 8 8 8 8
  # 13 8 8 8 8 8
  # 14 8 8 8 8 8

# make sure each row has 5 of each column
table(exp.design$Column,by=exp.design$Row)
  # by
  # A B C D E F G H
  # 1  5 5 5 5 5 5 5 5
  # 2  5 5 5 5 5 5 5 5
  # 3  5 5 5 5 5 5 5 5
  # 4  5 5 5 5 5 5 5 5
  # 5  5 5 5 5 5 5 5 5
  # 6  5 5 5 5 5 5 5 5
  # 7  5 5 5 5 5 5 5 5
  # 8  5 5 5 5 5 5 5 5
  # 9  5 5 5 5 5 5 5 5
  # 10 5 5 5 5 5 5 5 5
  # 11 5 5 5 5 5 5 5 5
  # 12 5 5 5 5 5 5 5 5
  # 13 5 5 5 5 5 5 5 5
  # 14 5 5 5 5 5 5 5 5

head(exp.design,14)
tail(exp.design,14)


#Make sure I don't have duplicates of locations
exp.design$Location<-paste0('t',exp.design$Table,'-row',exp.design$Row,'-col',exp.design$Column)
head(exp.design,14)
length(unique(exp.design$Location)) # 560 ! :-)

# Re-order in original order
exp.design<-exp.design[order(exp.design$Pot.ID),]
head(exp.design,14)
