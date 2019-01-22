# Cargando tabla Match y seleccionando 10755 partidos
library(RSQLite)
library(dplyr)

con <- dbConnect(SQLite(), dbname=paste0(getwd(),"/soccer/database.sqlite")) #borrar soccer
Match <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
M<-as.data.frame(Match[,1:85]) # no consideraremos datos de Odds bets
M_OK<-M[complete.cases(M),]
M_OK2<-subset(M_OK, grepl("21484|10223|7775|1729",M_OK$country_id))

## Exploración inicial
#Equipo Local
x<-as.data.frame(M_OK2[,12:22])
y<-as.data.frame(M_OK2[,34:44])
xx<-as.data.frame(matrix(x, nrow=nrow(x)*ncol(x), ncol=1))
yy<-as.data.frame(matrix(y, nrow=nrow(y)*ncol(y), ncol=1))

par(new=F)
par(mar=c(4, 4, 1, 1))
for (i in 1:ncol(x))
{
        plot(x[,i], y[,i], xlim=c(1,9), ylim=c(1,11), xlab="X", ylab="Y")
        par(new=T)
}

#Equipo Visita
x2<-as.data.frame(M_OK2[,23:33])
y2<-as.data.frame(M_OK2[,45:55])
xx2<-as.data.frame(matrix(x2, nrow=nrow(x2)*ncol(x2), ncol=1))
yy2<-as.data.frame(matrix(y2, nrow=nrow(y2)*ncol(y2), ncol=1))

for (i in 1:ncol(x2))
{
        plot(x2[,i], y2[,i], xlim=c(1,9), ylim=c(1,11), xlab="", ylab="")
        par(new=T)
}

# Graficando líneas divisorias de ejemplo
par(new=TRUE)
abline(a=4, b=0)
par(new=TRUE)
abline(a=9.5, b=0)
par(new=TRUE)
abline(a=0, b=0, v=3.5)
par(new=TRUE)
abline(a=0, b=0, v=6.5)

par(new=TRUE)
abline(a=6.5, b=0, col="red", lw=2)

## Codificando Formaciones
# Antes se debe recodificar el medio campo con volantes defensivos y ofensivos
# Vamos a considerar un y =5 e y=6 como volantes defensivos

y3 <- y
y4 <- y2

PosL2 <- matrix(nrow=nrow(y3), ncol=ncol(y3))
for (i in 1:nrow(y3))
{
        PosL2[i, which(y3[i,]==1)] <- "A_GK"
        PosL2[i, which(as.numeric(y3[i,])>1.0 & as.numeric(y3[i,])<3.5)] <- "B_DEF"
        PosL2[i, which(as.numeric(y3[i,])>3.5 & as.numeric(y3[i,])<6.5)] <- "C_MID_DEF"
        PosL2[i, which(as.numeric(y3[i,])>6.5 & as.numeric(y3[i,])<9.5)] <- "D_MID_FWD"
        PosL2[i, which(as.numeric(y3[i,])>9.5 & as.numeric(y3[i,])<12)] <- "E_FWD"
}

PosV2 <- matrix(nrow=nrow(y4), ncol=ncol(y4))
for (i in 1:nrow(y4))
{
        PosV2[i, which(y4[i,]==1)] <- "A_GK"
        PosV2[i, which(as.numeric(y4[i,])>1.0 & as.numeric(y4[i,])<3.5)] <- "B_DEF"
        PosV2[i, which(as.numeric(y4[i,])>3.5 & as.numeric(y4[i,])<6.5)] <- "C_MID_DEF"
        PosV2[i, which(as.numeric(y4[i,])>6.5 & as.numeric(y4[i,])<9.5)] <- "D_MID_FWD"
        PosV2[i, which(as.numeric(y4[i,])>9.5 & as.numeric(y4[i,])<12)]  <- "E_FWD"
}

# Ahora formaciones
FORL <- matrix(nrow=nrow(y3), ncol=5)
for (i in 1:nrow(y3))
{
        FORL[i, 1] <- length(which(PosL2[i,] == "A_GK"))
        FORL[i, 2] <- length(which(PosL2[i,] == "B_DEF"))
        FORL[i, 3] <- length(which(PosL2[i,] == "C_MID_DEF"))
        FORL[i, 4] <- length(which(PosL2[i,] == "D_MID_FWD"))
        FORL[i, 5] <- length(which(PosL2[i,] == "E_FWD"))
}

FORV <- matrix(nrow=nrow(y4), ncol=5)
for (i in 1:nrow(y4))
{
        FORV[i, 1] <- length(which(PosV2[i,] == "A_GK"))
        FORV[i, 2] <- length(which(PosV2[i,] == "B_DEF"))
        FORV[i, 3] <- length(which(PosV2[i,] == "C_MID_DEF"))
        FORV[i, 4] <- length(which(PosV2[i,] == "D_MID_FWD"))
        FORV[i, 5] <- length(which(PosV2[i,] == "E_FWD"))
}

FORL[,1] <- paste(FORL[,2], FORL[,3], FORL[,4], FORL[,5], sep="-")
FORL[,1] <- gsub("-0", "", FORL[,1], fixed = TRUE)
FORV[,1] <- paste(FORV[,2], FORV[,3], FORV[,4], FORV[,5], sep="-")
FORV[,1] <- gsub("-0", "", FORV[,1], fixed = TRUE)

#resultado
difgoals <- as.data.frame(M_OK2$home_team_goal - M_OK2$away_team_goal)
difgoals <- difgoals %>% mutate(resultado = ifelse(difgoals > 0, "L",
                                                      ifelse(difgoals == 0 ,"E",
                                                      ifelse(difgoals < 0, "V", NA))))

data <- cbind(M_OK2[,c('league_id','season')], difgoals[,2], FORL[,1], FORV[,1])
colnames(data)[3:5] <- c('resultado', 'formLocal', 'formVisita')
write.table(data, "tabla1_.csv", row.names=F, sep=",", col.names = T)

