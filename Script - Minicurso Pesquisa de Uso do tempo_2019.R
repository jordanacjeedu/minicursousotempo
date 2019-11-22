##############Apresenta��o
#Encontro Nacional sobre Popula��o, Trabalho, G�nero e Pol�ticas P�blicas - 2019
#Minicurso de pesquisas de Uso do Tempo na Am�rica Latina
#Ministrante: Jordana Cristina de Jesus (PPGDem/UFRN)
#Professora Adjunta (DDCA/UFRN)
#Doutora em Demografia (CEDEPLAR/UFMG)
# Apoio t�cnico das bolsistas de IC: Leticia Maia (CNPQ), Luciana Bruno (UFRN), Stephane Duarte (UFRN)


############### Pacotes necess�rios
# 1� - para leitura da base de dados que seja de algum formato diferente do R
# Instalar o pacote
install.packages("foreign")
# Carregar o pacote para utiliza��o 
library("foreign")

#Outros:
install.packages("dplyr")
library("dplyr")


############### ENDERE�O DAS BASES 

# PER� - Encuesta Nacional de Uso del Tiempo 2010
# Documenta��o sobre a pesquisa:
#https://webinei.inei.gob.pe/anda_inei/index.php/catalog/236/get_microdata
# Local para baixar as bases de dados 
#http://iinei.inei.gob.pe/microdatos/Consulta_por_Encuesta.asp



################ DESCRI��O DAS BASES

#Nro A�o Per�odo C�digo Encuesta Encuesta C�digo M�dulo 
#1	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	125	Caracter�sticas de la vivienda y del hogar			
#2	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	126	Caracter�sticas de los miembros del hogar			
#3	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	127	Ayudas recibidas por personas de otro hogar			
#4	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	128	Tareas de apoyo al hogar			
#5	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	129	Tareas realizadas para el hogar			
#6	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	130	Empleo e ingreso			


################ PROCEDIMENTO PARA DOWNLOAD DAS BASES

# PRIMEIRO PASSO: BAIXAR AS BASES DE 1 A 6 
# Baixar os arquivos em SPSS - j� est�o com algumas agrega��es feitas 
# Os arquivos em DBF exigem uma programa��o mais complexa 

## SEGUNDO PASSO: 
## DEFINIR A PASTA ONDE OS ARQUIVOS SER�O "DESCOMPRIMIDOS"

setwd ("C:\\Users\\Jordana\\Documents\\BASES DE DADOS\\PERU")


################ MANIPULA��O DAS BASES

# Nessa pr�tica, vamos utilizar os CAP�TULOS: 200, 400 E 500  

#2	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	126	Caracter�sticas de los miembros del hogar		
CAPITULO_200<- read.spss("286-Modulo126\\02_CAPITULO_200.sav",as.is = FALSE, to.data.frame = TRUE)
#4	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	128	Tareas de apoyo al hogar	
CAPITULO_400<- read.spss("286-Modulo128\\04_CAPITULO_400.sav",as.is = FALSE, to.data.frame = TRUE)
#5	2010	56	286	Encuesta Nacional de Uso del Tiempo - ENUT	129	Tareas realizadas para el hogar			
CAPITULO_500<- read.spss("286-Modulo129\\05_CAPITULO_500.sav",as.is = FALSE, to.data.frame = TRUE)


# Algumas informa��es sobre a base de dados:
# Dimens�o (LINHAS X COLUNAS)
dim(CAPITULO_200)
dim(CAPITULO_400)
dim(CAPITULO_500)

# Nomes das vari�veis
colnames(CAPITULO_200)
colnames(CAPITULO_400)
colnames(CAPITULO_500)

# Listar os primeiros casos de uma base
head(CAPITULO_200)
# Ver os tipos de vari�veis da base
str(CAPITULO_500)


 ################ C�lculo do Tempo de Trabalho Dom�stico n�o Remunerado (FORA DO PIB)


# Os tempos das atividades s�o coletados de forma individualizada
# � necess�rio somar os tempos de atividades feitas durante a semana e durante o final de semana
# Tamb�m � necess�rio somar todas as atividades realizadas para identificar o tempo total dedicado ao trabalho dom�stico 
# Faz-se necess�rio, nesse momento, substituir os valores NA por 0, para fazer a soma dos tempos
CAPITULO_500[is.na(CAPITULO_500)]=0
# Como essa ser� a base de trabalho daqui para frente, vamos "fixar" essa base
attach(CAPITULO_500) #dessa forma podemos utilizar as vari�veis sem ter que fazer refer�ncia todo o tempo � base em quest�o

########C0 Actividades culinarias
# HORAS DE Actividades culinarias
CAPITULO_500$horasculinaria=P502C_1_H+P503C_1_H+P502C_2_H+P503C_2_H+P502C_4_H+P503C_4_H+P502C_5_H+P503C_5_H+P502C_6_H+P503C_6_H+P502C_7_H+P503C_7_H
# MINUTOS DE Actividades culinarias
CAPITULO_500$minutosculinaria=P502C_1_M+P503C_1_M+P502C_2_M+P503C_2_M+P502C_4_M+P503C_4_M+P502C_5_M+P503C_5_M+P502C_6_M+P503C_6_M+P502C_7_M+P503C_7_M# TOTAL DE HORAS DE Actividades culinarias POR DIA 
# TOTAL DE HORAS DE Actividades culinarias POR DIA 
CAPITULO_500$tempoculinaria=(CAPITULO_500$horasculinaria+CAPITULO_500$minutosculinaria/60)/7




########D0 Aseo de la vivienda 
# HORAS DE limpeza
CAPITULO_500$horaslimpeza=P503D_1_H+P503D_2_H+P503D_3_H+P503D_4_H+P503D_5_H+P503D_6_H+P503D_7_H+P503D_8_H+P503D_9_H
# MINUTOS DE limpeza
CAPITULO_500$minutoslimpeza=P503D_1_M+P503D_2_M+P503D_3_M+P503D_4_M+P503D_5_M+P503D_6_M+P503D_7_M+P503D_8_M+P503D_9_M
# TOTAL DE HORAS DE limpeza POR DIA 
CAPITULO_500$tempolimpeza=(CAPITULO_500$horaslimpeza+CAPITULO_500$minutoslimpeza/60)/7



########E0 Cuidado y confecci�n de ropa 
# HORAS Cuidado y confecci�n de ropa 
CAPITULO_500$horasroupa= P502E_1_H+P503E_1_H+P502E_2_H+P503E_2_H+P502E_3_H+P503E_3_H+P502E_4_H+P503E_4_H+P502E_5_H+P503E_5_H
# MINUTOS Cuidado y confecci�n de ropa 
CAPITULO_500$minutosroupa= P502E_1_M+P503E_1_M+P502E_2_M+P503E_2_M+P502E_3_M+P503E_3_M+P502E_4_M+P503E_4_M+P502E_5_M+P503E_5_M
# TOTAL DE HORAS Cuidado y confecci�n de ropa  POR DIA 
CAPITULO_500$temporoupa=(CAPITULO_500$horasroupa+CAPITULO_500$minutosroupa/60)/7


########F0 Reparaci�n, construcci�n y mantenimiento en la vivienda
# HORAS Reparaci�n, construcci�n y mantenimiento en la vivienda
CAPITULO_500$horasmanutencao= P502F_1_H+P503F_1_H+P502F_2_H+P503F_2_H++P502F_4_H+P503F_4_H+P502F_5_H+P503F_5_H+P502F_6_H+P503F_6_H
# MINUTOS Reparaci�n, construcci�n y mantenimiento en la vivienda
CAPITULO_500$minutosmanutencao= P502F_1_M+P503F_1_M+P502F_2_M+P503F_2_M+P502F_4_M+P503F_4_M+P502F_5_M+P503F_5_M+P502F_6_M+P503F_6_M
# TOTAL DE HORAS Reparaci�n, construcci�n y mantenimiento en la vivienda POR DIA 
CAPITULO_500$tempomanutencao=(CAPITULO_500$horasmanutencao+CAPITULO_500$minutosmanutencao/60)/7


######## G0 Cuidado de bebes, ni�as, ni�os y adolescentes
# HORAS Cuidado de bebes, ni�as, ni�os y adolescentes
CAPITULO_500$horascuidadocrianca= P502G_1_H+P503G_1_H+P502G_2_H+P503G_2_H+P502G_3_H+P503G_3_H+P502G_4_H+P503G_4_H+P502G_6_H+P503G_6_H+P502G_7_H+P503G_7_H+P502G_8_H+P503G_8_H+P502G_9_H+P503G_9_H
# MINUTOS Cuidado de bebes, ni�as, ni�os y adolescentes
CAPITULO_500$minutoscuidadocrianca= P502G_1_M+P503G_1_M+P502G_2_M+P503G_2_M+P502G_3_M+P503G_3_M+P502G_4_M+P503G_4_M+P502G_6_M+P503G_6_M+P502G_7_M+P503G_7_M+P502G_8_M+P503G_8_M+P502G_9_M+P503G_9_M
# TOTAL DE HORAS Cuidado de bebes, ni�as, ni�os y adolescentes POR DIA 
CAPITULO_500$tempocuidadocrianca=(CAPITULO_500$horascuidadocrianca+CAPITULO_500$minutoscuidadocrianca/60)/7


######## H0 Cuidado de miembros del hogar que presentaron alg�n s�ntoma, malestar o enfermedad
# HORAS Cuidado de miembros del hogar que presentaron alg�n s�ntoma, malestar o enfermedad
CAPITULO_500$horasoutrocuidado= P502H_1_H+P503H_1_H+P502H_2_H+P503H_2_H+P502H_3_H+P503H_3_H
# MINUTOS Cuidado de miembros del hogar que presentaron alg�n s�ntoma, malestar o enfermedad
CAPITULO_500$minutosoutrocuidado= P502H_1_M+P503H_1_M +P502H_2_M +P503H_2_M +P502H_3_M +P503H_3_M
# TOTAL DE HORAS Cuidado de miembros del hogar que presentaron alg�n s�ntoma, malestar o enfermedad POR DIA 
CAPITULO_500$tempooutrocuidado=(CAPITULO_500$horasoutrocuidado+CAPITULO_500$minutosoutrocuidado/60)/7


######## I0 Compras para el hogar
# HORAS Compras para el hogar
CAPITULO_500$horascompra= P502I_1_H+P503I_1_H+P502I_2_H+P503I_2_H+P502I_3_H+P503I_3_H+P502I_4_H+P503I_4_H+P502I_5_H+P503I_5_H+P502I_6_H+P503I_6_H+P502I_7_H+P503I_7_H+P502I_8_H+P503I_8_H+P502I_9_H+P503I_9_H
# MINUTOS Compras para el hogar
CAPITULO_500$minutoscompra= P502I_1_M+P503I_1_M+P502I_2_M+P503I_2_M+P502I_3_M+P503I_3_M+P502I_4_M+P503I_4_M+P502I_5_M+P503I_5_M+P502I_6_M+P503I_6_M+P502I_7_M+P503I_7_M+P502I_8_M+P503I_8_M+P502I_9_M+P503I_9_M
# TOTAL DE HORAS Compras para el hogar POR DIA 
CAPITULO_500$tempocompra=(CAPITULO_500$horascompra+CAPITULO_500$minutoscompra/60)/7



######## J0 Gerencia y organizaci�n del hogar
# HORAS Gerencia y organizaci�n del hogar
CAPITULO_500$horasorganizacao= P502J_1_H+P503J_1_H+P502J_2_H+P503J_2_H+P502J_3_H+P503J_3_H+P502J_4_H+P503J_4_H+P502J_5_H+P503J_5_H+P502J_6_H+P503J_6_H+P502J_7_H+P503J_7_H+P502J_8_H+P503J_8_H+P502J_9_H+P503J_9_H+P502J_11_H+P503J_11_H+P502J_12_H+P503J_12_H+P502J_13_H+P503J_13_H
# MINUTOS  Gerencia y organizaci�n del hogar
CAPITULO_500$minutosorganizacao= P502J_1_M+P503J_1_M+P502J_2_M+P503J_2_M+P502J_3_M+P503J_3_M+P502J_4_M+P503J_4_M+P502J_5_M+P503J_5_M+P502J_6_M+P503J_6_M+P502J_7_M+P503J_7_M+P502J_8_M+P503J_8_M+P502J_9_M+P503J_9_M+P502J_11_M+P503J_11_M+P502J_12_M+P503J_12_M+P502J_13_M+P503J_13_M
# TOTAL DE HORAS Gerencia y organizaci�n del hogar POR DIA 
CAPITULO_500$tempoorganizacao=(CAPITULO_500$horasorganizacao+CAPITULO_500$minutosorganizacao/60)/7



######## N0 Tareas de apoyo a otro hogar
# HORAS Tareas de apoyo a otro hogar
CAPITULO_500$horasapoio=P502N_1_H+P503N_1_H+P502N_2_H+P503N_2_H+P502N_3_H+P503N_3_H+P502N_4_H+P503N_4_H+P502N_5_H+P503N_5_H+P502N_6_H+P503N_6_H+P502N_7_H+P503N_7_H+P502N_8_H+P503N_8_H+P502N_9_H+P503N_9_H+P502N_10_H+P503N_10_H+P502N_11_H+P503N_11_H
# MINUTOS Tareas de apoyo a otro hogar
CAPITULO_500$minutosapoio=P502N_1_M+P503N_1_M+P502N_2_M+P503N_2_M+P502N_3_M+P503N_3_M+P502N_4_M+P503N_4_M+P502N_5_M+P503N_5_M+P502N_6_M+P503N_6_M+P502N_7_M+P503N_7_M+P502N_8_M+P503N_8_M+P502N_9_M+P503N_9_M+P502N_10_M+P503N_10_M+P502N_11_M+P503N_11_M
# TOTAL DE HORAS Tareas de apoyo a otro hogar POR DIA
CAPITULO_500$tempoapoio=(CAPITULO_500$horasapoio+CAPITULO_500$minutosapoio/60)/7




######## O0 Trabajo voluntario para organizaciones o instituciones
# HORAS Trabajo voluntario
CAPITULO_500$horasvoluntario=P502O_1_H+P503O_1_H+P502O_2_H+P503O_2_H+P502O_3_H+P503O_3_H+P502O_4_H+P503O_4_H+P502O_5_H+P503O_5_H
# MINUTOS Trabajo voluntario
CAPITULO_500$minutosvoluntario=P502O_1_M+P503O_1_M+P502O_2_M+P503O_2_M+P502O_3_M+P503O_3_M+P502O_4_M+P503O_4_M+P502O_5_M+P503O_5_M
# TOTAL DE HORAS Trabajo voluntario POR DIA
CAPITULO_500$tempovoluntario=(CAPITULO_500$horasvoluntario+CAPITULO_500$minutosvoluntario/60)/7


########  P0 Cuidado de miembros del hogar con dificultades f�sicas, mentales o enfermedades permanentes o edad avanzada totalmente dependientes
# HORAS Cuidado de miembros
CAPITULO_500$horascuidado2=P502P_1_H+P503P_1_H+P502P_2_H+P503P_2_H+P502P_3_H+P503P_3_H+P502P_4_H+P503P_4_H+P502P_5_H+P503P_5_H+P502P_7_H+P503P_7_H+P502P_8_H+P503P_8_H
# MINUTOS Cuidado de miembros
CAPITULO_500$minutoscuidado2=P502P_1_M+P503P_1_M+P502P_2_M+P503P_2_M+P502P_3_M+P503P_3_M+P502P_4_M+P503P_4_M+P502P_5_M+P503P_5_M+P502P_7_M+P503P_7_M+P502P_8_M+P503P_8_M
# TOTAL DE HORAS Cuidado de miembros POR DIA
CAPITULO_500$tempocuidado2=(CAPITULO_500$horascuidado2+CAPITULO_500$minutoscuidado2/60)/7



############# TEMPO TOTAL DE TRABALHO DOM�STICO N�O REMUNERADO FORA DAS CONTAS SAT�LITES 

CAPITULO_500$TDNR=CAPITULO_500$tempoculinaria+CAPITULO_500$tempoapoio+CAPITULO_500$tempocompra+CAPITULO_500$tempocuidado2+CAPITULO_500$tempocuidadocrianca+CAPITULO_500$tempolimpeza+CAPITULO_500$tempomanutencao+CAPITULO_500$tempoorganizacao+CAPITULO_500$tempooutrocuidado+CAPITULO_500$temporoupa+CAPITULO_500$tempovoluntario
summary(CAPITULO_500$TDNR)


############## COMBINAR AS BASES (INDIV�DUO + TDNR)
# Para combinar as bases de dados, � necess�rio o identificador �nico de cada entrevistado
# Esse identificador �nico vem da sequ�ncia i.conglomerado ii.n�mero de sele��o do domic�lio iii. n�mero do domic�lio e iv. n�mero da pessoa
# Os dois primeiros c�digos se repetem nas bases de dados
# Entretanto, em cada base de dados, o n�mero da pessoa � apresentado em uma vari�vel diferente 
# Por isso vamos criar um identificador com o mesmo nome em todas as tr�s bases 
names(CAPITULO_200)[names(CAPITULO_200) == "P200_ID"] <- "ID"
CAPITULO_200$ID<-as.numeric(CAPITULO_200$ID)
names(CAPITULO_400)[names(CAPITULO_400) == "P400A"] <- "ID"
CAPITULO_400$ID<-as.numeric(CAPITULO_400$ID)
names(CAPITULO_500)[names(CAPITULO_500) == "P500_A"] <- "ID"
CAPITULO_500$ID<-as.numeric(CAPITULO_500$ID)
# 1 Partindo da base que tem as caracter�sticas dos indiv�duos, incoporar as demais bases 

usodotempo <- left_join(CAPITULO_200, CAPITULO_500, by=c('CONGLOMERADO','NSELV', 'HOGAR','ID'))
usodotempo <- left_join(usodotempo, CAPITULO_400, by=c('CONGLOMERADO','NSELV', 'HOGAR','ID'))


# Formatar tela para alocar os gr�ficos 
# 1 linha e 1 coluna
par(mfrow=c(1,1))

###### GR�FICO - Tempo m�dio di�rio de trabalho dom�stico n�o remunerado n�o considerado no PIB, PERU 2010

avg <- function(x) {
  dat <- aggregate(usodotempo$TDNR, by = list(usodotempo[, x]), FUN=mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Trabalho dom�stico n�o remunerado", col="orangered4",ylim=c(0, 6))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")

# 2 linhas e 4 colunas
par(mfrow=c(2,4))

###### GR�FICO - Actividades culinarias (m�dia di�ria segundo sexo)
avg <- function(x) {
  dat <- aggregate(usodotempo$tempoculinaria, by = list(usodotempo[, x]), FUN=mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Cozinhar", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")


###### # GR�FICO  - Aseo de la vivienda
avg <- function(x) {
  dat <- aggregate(usodotempo$tempolimpeza, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Limpar", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")



####### GR�FICO - Cuidado y confecci�n de ropa
avg <- function(x) {
  dat <- aggregate(usodotempo$temporoupa, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Roupas", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")

# GR�FICO 4 - Reparaci�n, construcci�n y mantenimiento en la vivienda
avg <- function(x) {
  dat <- aggregate(usodotempo$tempomanutencao, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Manuten��o", col="darkmagenta",ylim=c(0,2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")


#######  GR�FICO - Cuidado de bebes, ni�as, ni�os y adolescentes
avg <- function(x) {
  dat <- aggregate(usodotempo$tempocuidadocrianca, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas", main="Cuidado de crian�a", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")



#######  GR�FICO - Cuidado de miembros del hogar que presentaron alg�n s�ntoma, malestar o enfermedad
avg <- function(x) {
  dat <- aggregate(usodotempo$tempooutrocuidado, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Cuidado de moradores", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")

####### GR�FICO - Compras para el hogar
avg <- function(x) {
  dat <- aggregate(usodotempo$tempocompra, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Compras", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")


#######  GR�FICO - Gerencia y organizaci�n del hogar
avg <- function(x) {
  dat <- aggregate(usodotempo$tempoorganizacao, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Organiza��o", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")


####### GR�FICO - Tareas de apoyo a otro hogar
avg <- function(x) {
  dat <- aggregate(usodotempo$tempoapoio, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas", main="Apoio", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")


#######  GR�FICO - Trabajo voluntario para organizaciones o instituciones
avg <- function(x) {
  dat <- aggregate(usodotempo$tempovoluntario, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Voluntariado", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")

#######  GR�FICO - Cuidado de miembros del hogar con dificultades f�sicas, mentales o enfermedades permanentes o edad avanzada totalmente dependientes
avg <- function(x) {
  dat <- aggregate(usodotempo$tempocuidado2, by = list(usodotempo[, x]), FUN = mean, na.rm=TRUE)
  barplot(dat$x, xlab = x, ylab = "M�dia de horas di�rias", main="Outros cuidados", col="darkmagenta",ylim=c(0, 2))
  axis(side=1, at=1:nrow(dat), labels=dat$Group.1)
}

avg(x = "P204")



####MUITO OBRIGADA!
