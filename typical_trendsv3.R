# Eventuali dubbi:
# - il grafico sarà a barre? o sarà un grafico discreto?
# - massimo numero memorizzabile con Rstudio? 
# - media per pezzi... ? mediana a pezzi ? Varianza a pezzi ? 
# - intervalli di confidenza, per ipotesi si ha una distribuzione gaussiana?
# - statistica settimanale: settimana intesa numericamente oppure settimana intesa come settimana vera? 
# - una volta completato il codice R devo trasferirlo sulla piattaforma della VM?

# FUNCTIONS

erroreIntervalloDiConfidenza <- funciton(dati){
  devStandard = sd(dati)
  errore = qnorm(0.975)*devStandard/sqrt(length(dati))
  
  return(errore) # chiaramente poi l'intervallo sarà media campionaria +- errore
  
}

getDayGivenDate <- function(d, m, y){
  t = c(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
  if( m < 3 ){
    y = y - 1
  }
  day = ((y + as.integer(y/4) - as.integer(y/100) + as.integer(y/400) + t[m] + d)%%7)
  #print(typeof(day))
  #nota: vista la convenzione di R che i vettori iniziano da 1, allora faccio in modo che domenica è il giorno 1, lunedì è 2, ..., sabato è 7
  return(day + 1)
  
}

getMinute <- function(fullDate){
  return(as.numeric(substring(fullDate, first = 15, last = 16)))
}

getHour <- function(fullDate){
  return(as.numeric(substring(fullDate, first = 12, last = 13)))
}

getDay <- function(fullDate){
  return(as.numeric(substring(fullDate, first = 9, last = 10)))
}

getMonth <- function(fullDate){
  retrun(as.numeric(substring(fullDate, first = 6, last = 7)))
}

getYear <- function(fullDate){
  return(as.numeric(substring(fullDate, first = 1, last = 4)))
}
####################### END FUNCTIONS




################################## START MAIN BLOCK OF CODE: A SORT OF "MAIN FUNCTION"

library("jsonlite")

valueName = "airTemperature"

#query = "https://servicemap.disit.org/WebAppGrafo/api/v1/?queryId=d34f64047670d1abb71e157646f51685&format=json"
query = "https://servicemap.disit.org/WebAppGrafo/api/v1/?queryId=9ae88e6b9a39ccc5d8ea75c8a06ec4f1&format=json"
sensorCategoryJson <- fromJSON(query)
suri = sensorCategoryJson$Service$features$properties$serviceUri


suri = sensorCategoryJson$Service$features$properties$serviceUri
#print(suri)
fromTime = "2020-01-18T19:00:00"
#fromTime = "2020-01-11T10:00:00"
toTime = "2020-03-19T18:00:00"

#scommentando le seguenti sotto, se si fa con airtemperature si ha praticamente la temperatura di luglio 2019
#fromTime = "2019-07-01T10:00:00"
#toTime = "2019-08-01T10:00:00"

apiQuery = paste("https://servicemap.disit.org/WebAppGrafo/api/v1/?serviceUri=",suri,"&valueName=", valueName  ,"&fromTime=",fromTime, "&toTime=", toTime, sep = "")
lastSize = -1
j = 0

#purtroppo R non ha il do while.. closest thing è il break alla fine
tono di fare la divisione per la media, l'accesso si fa col dollaro, quindi: listaMese$valori, oppure listaMese$pesi

# inizializzo le liste mese, giorno, settimana
listaMese = list()
listaGiorno = list()
listaSettimana = list()
listaSettimanaUno = list()
listaSettimanaDue = list()
listaSettimanaTre = list()
listaSettimanaQuattro = list()

for(i in 1:31){
  listaMese[[i]] = vector()
  if(i <= 24){
    listaGiorno[[i]] = vector()
  }
  if(i <= 7){
    listaSettimana[[i]] = vector()
    listaSettimanaUno[[i]] = vector()
    listaSettimanaDue[[i]] = vector()
    listaSettimanaTre[[i]] = vector()
    listaSettimanaQuattro[[i]] = vector()
    
  }
}

#firstIteration = TRUE
dataRecente = toTime

while(TRUE){
  print(paste("Ultima query fatta da: ", fromTime, " a: ", dataRecente))
  iterativeQuery = paste("https://servicemap.disit.org/WebAppGrafo/api/v1/?serviceUri=",suri,"&valueName=", valueName  ,"&fromTime=",fromTime ,"&toTime=", dataRecente, sep = "")
  #print(iterativeQuery)
  cat(paste(iterativeQuery, "  "))
  data = fromJSON(iterativeQuery)
  #print(data$realtime)
  iterativeDates = data$realtime$results$bindings[2]
  informazioni = data$realtime$results$bindings[1]
  #print(paste("informazioni: ", as.numeric(informazioni[[1]]$value[1])))
  
  #i sottostanti vettori ssaranno quelli utilizzati
  iDatesList = iterativeDates[[1]]$value
  iInformationList = informazioni[[1]]$value
  
  firstIteration = TRUE
  
  #il seguente blocco serve a gestire le n-1 iterazioni da farsi
  if(firstIteration){
    y = 1
    firstIteration = FALSE
  } else {
    y = 2
  }
  print(paste("-------------->Inizio a lavorare da: ", iDatesList[y]))
  
  #print(paste("IPSILON E': ", y))
  for(i in y:length(iDatesList)){
    minuto = as.numeric(substring(iDatesList[i], first = 15, last = 16))
    ora = as.numeric(substring(iDatesList[i], first = 12, last = 13))
    #print(typeof(ora))
    if(ora == 0){
      ora = 24
    }
    giorno = as.numeric(substring(iDatesList[i], first = 9, last = 10))
    mese = as.numeric(substring(iDatesList[i], first = 6, last = 7))
    anno = as.numeric(substring(iDatesList[i], first = 1, last = 4))
    
    #print(paste("Giorno: ", giorno))
    listaMese[[giorno]] = append(listaMese[[giorno]], as.numeric(iInformationList[i]))
    listaGiorno[[ora]] = append(listaGiorno[[ora]], as.numeric(iInformationList[i]))
    
    dayOfWeek = getDayGivenDate(giorno, mese, anno) #ricorda che domenica è 1, sabato è 7
    
    listaSettimana[[dayOfWeek]] = append(listaSettimana[[dayOfWeek]], as.numeric(iInformationList[i]))
    # Da fare settimana 1, settimana 2, etc
  }

  
  print("---------------------------------------------------------")
  #print(iInformationList)
  dataRecente = substring(iDatesList[length(iDatesList)], first = 1, last = 19) #questa mi serve per l'iterazione successiva, toglie il fuso orario
  #print(paste("Data recente è: ", dataRecente))
  
  
  #print(paste("anno: ", anno,"mese:", mese, " giorno:", giorno, " ora:", ora, " minuto:", minuto))
  
  #con la prossima istruzione, rimuovo l'ultimo elemento
  #iDatesList[length(iDatesList)] = NULL --> questo nun funziona
  #[FATTO] DA FARE E DA TENERE IN MENTE CHE I CICLI PER LE SOMME E I CALCOLI DOVRANNO ESSERE FATTI N-1 VOLTE (l'indice da togliere è l'ultimo, anche se si potrebbe fare il primo)
  
  
  print(paste("last size is: ", lastSize, " iterativeList size is: ", length(iDatesList)))
  
  if(lastSize != -1 & length(iDatesList) < lastSize){
    break
  } 
  
  lastSize = length(iDatesList)
  }


print(listaGiorno[[1]])


############################################ END MAIN BLOCK OF CODE 


