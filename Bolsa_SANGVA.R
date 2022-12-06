#################
#Creo la función#
#################
obtenerListadoBolsaSANGVA<-function(turno,categoria,departamento) {
  #################################################################
  #Compruebo si están los paquetes necesarios, y sino, los instalo#
  ################################################################# 
  list.of.packages <- c("httr", "XML","xml2","rvest")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  #####################
  #CARGO LAS LIBRERIAS#
  #####################
  library(httr)
  library(XML)
  library(xml2) 
  library(rvest)
  
  #######################
  #PREPARO LAS VARIABLES#
  #######################
  urlBuscarEdicion<-"https://www2.san.gva.es/bolsa/bolsadetrabajoiiss.jsp?language=val&nw=true"
  url<-"http://www2.san.gva.es/bolsa/lstCandidatosListaOperativa.jsp?codedicion=#codedicion&turnoCod=#turnoCod&categoriaCod=#categoriaCod&departamentoCod=#departamentoCod&turnoDesc=Ordinario&categoriaDesc=#categoriaDesc&departamentoDesc=#departamentoDesc&posicionFinal=15000&posicionInicial=1&nw=true"
  urlsituacion<-"http://www2.san.gva.es/bolsa/lstSituacionCandidatos.jsp?turnoCod=#turnoCod&categoriaCod=#categoriaCod&departamentoCod=#departamentoCod&turnoDesc=Ordinario&categoriaDesc=#categoriaDesc&departamentoDesc=#departamentoDesc&posicionFinal=15000&posicionInicial=1&nw=true"
  ################
  #BUSCAR EDICION#
  ################
  
  html_edicion <-html_text2(read_html(urlBuscarEdicion))
  start_<-gregexpr("19",html_edicion)[[1]][1]
  edicion_len10<-substr(html_edicion,start_,start_+10)
  end_<-gregexpr(" ",edicion_len10)[[1]][1]
  edicion<-paste(substr(edicion_len10,1,end_-1),".0",sep = "")
  
  turnos <- data.frame(
    "turnoCod" = c("O","P"),
    "turnoDesc" = c("ORDINARIO","PROMOCIÓN INTERNA")
  )
  
  categorias <- data.frame(
    "categoriaCod" = c("0001",
                       "0002",
                       "0004",
                       "0006",
                       "0007",
                       "0008",
                       "0009",
                       "0010",
                       "0011",
                       "0015",
                       "0016",
                       "0017",
                       "0018",
                       "0019",
                       "0020",
                       "0037",
                       "0057",
                       "0087",
                       "0244",
                       "0246",
                       "0257",
                       "0258",
                       "0259",
                       "0260",
                       "0275",
                       "0276",
                       "0277",
                       "0279",
                       "0280",
                       "0285",
                       "0302",
                       "0303",
                       "0304",
                       "0305",
                       "0306",
                       "0307",
                       "0308",
                       "0310",
                       "0311",
                       "0312",
                       "0313",
                       "0314",
                       "0315",
                       "0317",
                       "0350",
                       "0362",
                       "0363",
                       "0364",
                       "0365",
                       "0366",
                       "0367",
                       "0368",
                       "0369",
                       "0370",
                       "0371",
                       "0375",
                       "0382",
                       "0400",
                       "0402",
                       "0404",
                       "0415",
                       "0441",
                       "0442",
                       "0443",
                       "0444",
                       "0446",
                       "0449",
                       "0450",
                       "0451",
                       "0452",
                       "0455",
                       "0457",
                       "0459",
                       "0460",
                       "0463",
                       "0473",
                       "0476",
                       "0477",
                       "0483",
                       "0484",
                       "0486",
                       "0516",
                       "0557",
                       "1001",
                       "1014",
                       "1015",
                       "1019",
                       "1020",
                       "1022",
                       "1024",
                       "1025",
                       "1026",
                       "1027",
                       "1028",
                       "1050",
                       "1051",
                       "1052",
                       "1053",
                       "1054",
                       "1055",
                       "1056",
                       "1057",
                       "1058",
                       "1059",
                       "1060",
                       "1061",
                       "1062",
                       "2000",
                       "2001",
                       "3001",
                       "3002",
                       "3003",
                       "3004",
                       "3005",
                       "3006",
                       "3007",
                       "3008",
                       "3100",
                       "3101",
                       "3102",
                       "3103",
                       "3200",
                       "3201",
                       "3202",
                       "3203",
                       "3204"),
    "categoriaDesc" = c("ENGINYER D'APLICACIONS I SISTEMES",
                        "ANALISTA PROGRAMADOR I DE SISTEMES",
                        "METGE CONDUCTES ADDICTIVES",
                        "METGE UNITAT DE CURTA ESTADA",
                        "METGE UNITAT DE PREVENCIÓ CÀNCER DE MAMA",
                        "METGE UNITAT HOSPITALITZACIÒ A DOMICILI",
                        "TÈCNIC D'INFORMATICA",
                        "METGE DOCUMENTACIÓ CLÍNICA I ADMISSIÓ",
                        "AUXILIAR ADMINISTRATIU",
                        "TÈCNIC DE MANTENIMENT",
                        "T.E. MEDICINA NUCLEAR",
                        "T.E. RADIOTERÀPIA",
                        "METGE PLANIFICACIÓ FAMILIAR",
                        "FARMACÈUTIC D'ÀREA",
                        "METGE EQUIP MÒBIL",
                        "F. ESP. BIOQUÍMICA CLÍNICA",
                        "TREBALLADOR SOCIAL",
                        "ADMINISTRATIU",
                        "METGE DE FAMILIA E.A.P.",
                        "INFERMERA/ER ESPECIALISTA OBSTÉTRICO-GINECOLÒGICA",
                        "F. ESP. PSIQUIATRIA",
                        "T.E. LABORATORI",
                        "F. ESP. INMUNOLOGIA",
                        "CONDUCTOR D SENSE LIMITACIÓ RADI ACCIÓ",
                        "TÈCNIC/A EN CURES AUXILIARS D'INFERMERIA",
                        "FISIOTERAPEUTA",
                        "ZELADOR",
                        "ODONTÒLEGS DE PRIMÀRIA",
                        "T.E. RADIODIAGNÒSTIC",
                        "INFERMERA/ER",
                        "F. ESP. MEDICINA INTERNA",
                        "F. ESP. REUMATOLOGIA",
                        "F. ESP. NEUROLOGIA",
                        "F. ESP. ENDOCRINOLOGIA I NUTRICIÓ",
                        "F. ESP. APARELL DIGESTIU / MEDICINA DIGESTIVA",
                        "F. ESP. NEUMOLOGIA",
                        "F. ESP. CARDIOLOGIA",
                        "F. ESP. DERMATOLOGIA M-Q Y VENEREOLOGIA",
                        "F. ESP. UROLOGIA",
                        "F. ESP. OFTALMOLOGIA",
                        "F. ESP. OTORRINOLARINGOLOGIA",
                        "F. ESP. TRAUMATOLOGIA I CIRURGIA ORTOPÈDICA",
                        "F. ESP. OBSTETRICIA / GINECOLOGIA",
                        "F. ESP. ANÀLISIS CLINIQUES",
                        "F. ESP. FARMÀCIA CLINICA",
                        "F. ESP. FARMÀCIA HOSPITALARIA",
                        "F. ESP. MEDICINA PREVENTIVA I SALUT PUBLICA",
                        "F. ESP. ANATOMIA PATOLÒGICA",
                        "F. ESP. RADIODIAGNÒSTIC",
                        "F. ESP. MICROBIOLOGIA I PARASITOLOGIA",
                        "F. ESP. REHABILITACIÓ",
                        "F. ESP. MEDICINA INTENSIVA",
                        "F. ESP. PEDIATRIA I ÀREES ESPECIFIQUES",
                        "F. ESP. CIRURGIA GENERAL I DE L'APARELL DIGESTIU",
                        "F. ESP. ANESTESIOLOGIA / REANIMACIÓ",
                        "F. ESP. RADIOFÍSICA HOSPITÀLARIA",
                        "TERAPEUTA OCUPACIONAL",
                        "F. ESP. RAFIOFARMÀCIA",
                        "LOCUTOR C.I.C.U.",
                        "GOVERNANTA",
                        "CONDUCTOR",
                        "F. ESP. NEUROFISIOLOGIA CLÍNICA",
                        "VETERINARI SALUT PÚBLICA",
                        "F. ESP. AL.LERGOLOGIA",
                        "F. ESP. NEFROLOGIA",
                        "F. ESP. ONCOLOGIA MEDICA",
                        "F. ESP. CIRURGIA PEDIÀTRICA",
                        "F. ESP. CIRURGIA CARDIOVASCULAR",
                        "F. ESP. CIRURGIA MAXIL.LOFACIAL",
                        "F. ESP. NEUROCIRURGIA",
                        "T.E. ANATOMIA PATOLÒGICA",
                        "F. ESP. ONCOLOGIA RADIOTERAPICA",
                        "F. ESP. ANGIOLOGIA I CIRURGIA VASCULAR",
                        "F. ESP. GERIATRIA",
                        "F. ESP. HEMATOLOGIA I HEMOTERAPIA",
                        "F. ESP. CIRURGIA PLÀSTICA I REPARADORA",
                        "F. ESP. MEDICINA NUCLEAR",
                        "PEDIATRIA AP",
                        "TÈCNIC DE FUNCIÓ ADMINISTRATIVA",
                        "GESTIÓ FUNCIÓ ADMINISTRATIVA",
                        "F. ESP. CIRURGIA TORÀCICA",
                        "ENGINYER TÈCNIC",
                        "METGE URGÈNCIA HOSPITALARIA",
                        "HIGIENISTA DENTAL",
                        "GESTIÓ SEGURETAT ALIMENTARIA I LABORATORI",
                        "SEGURETAT ALIMENTÀRIA",
                        "LABORATORI ANALISIS MICROBIOLÒGIQUES",
                        "LABORATORI D'ANALISI QUÍMIC",
                        "ESPECIALISTA SALUT PÚBLICA",
                        "FARMACÈUTIC ADMÓ SANITÀRIA I SALUT PÚBLICA",
                        "FARMACÈUTIC D'ADMINISTRACIÓ SANITÀRIA",
                        "METGE D'ADMÓ SANITÀRIA I SALUT PÚBLICA",
                        "ANÀLISIS EPIDEMIOLÒGIQUES I ESTADÍSTIQUES SA",
                        "INFERMERA/ER GESTIÓ SANITÀRIA I SALUT PÚBLICA",
                        "INFERMERA/ER ESPECIALISTA INFERMERIA FAMILIAR I COMUNITARIA",
                        "INFERMERA/ER ESPECIALISTA INFERMERIA DEL TREBALL",
                        "DIETISTA-NUTRICIONISTA",
                        "ÒPTIC-OPTOMETRISTA",
                        "PODÒLEG/OGA",
                        "TÈCNIC ESPECIALISTA EN AUDOLOGIA-PROTÈSICA",
                        "TÈCNIC EMERGENCIES SANITARIAS",
                        "AUXILIAR DE FARMÀCIA",
                        "SUPERIOR D'ADMINISTRACIÓ GENERAL SANITARIA",
                        "SANITAT AMBIENTAL (A1-S03-04)",
                        "METGE ADMINISTRACIÓ SANITÀRIA",
                        "GESTIÓ ADMINISTRACIÓ SANITÀRIA DE LA GENERALITAT",
                        "SUPERIOR GESTIÓ SANITAT AMBIENTAL (A2-S03-03)",
                        "INFERMERA/ER SAMU",
                        "METGE SAMU",
                        "INFERMERA/ER ESPECIALISTA EN INFERMERIA DE SALUT MENTAL",
                        "FACULTATIU ESPECILISTA EN MEDICINA DEL TREBALL",
                        "LOGOPEDA",
                        "TÈCNIC INTERMEDI EN PREVENCIÓ DE RISCOS LABORALS",
                        "TÈCNIC SUPERIOR EN HIGIENE DEL TREBALL",
                        "TÈCNIC SUPERIOR EN SEGURETAT EN EL TREBALL",
                        "TÈCNIC SUPERIOR EN PSICOSOCIOLOGIA I ERGONOMIA EN EL TREBALL",
                        "TÈCNIC ESPECIALISTA EN DOCUMENTACIÓ SANITARIA",
                        "INFERMERA/ER INSPECTOR",
                        "FAC. ESP. PSICOLOGÍA CLÍNICA",
                        "FARMACÈUTIC INSPECTOR",
                        "INSPECTOR METGE",
                        "INFERMER/A ESPECIALISTA PEDIATRIA",
                        "INFERMER/A/ER ESPECILISTA EN INFERMERIA GERIÀTRICA",
                        "PERIODISTA",
                        "OPERADOR/A CAMBRA HIPERBÀRICA",
                        "ENGINYER/A SUPERIOR")
  )
  
  departamentos <- data.frame(
    "departamentoCod" = c("ALI","ALY","ARN","CAS","CLI","DEN","ELD","ELX","GAN","GEN","LFE","MBA","ORI","PES","PLA","REQ","RIB","SA","SAG","SJO","SPA","SPC","SPV","SPY","SV","TRV","VIN","XAT"),
    "departamentoDesc" = c("ALICANTE","ALCOY","ARNAU","CASTELLON","CLINICO","DENIA","ELDA","ELX","GANDIA","GENERAL","LA FE","MARINA","ORIHUELA","PESET","LA PLANA","REQUENA","RIBERA","SES ALICANTE","SAGUNTO","SAN JOAN","S. P. ALICANTE","S. P.  CASTELLÓN","S. P. VALENCIA","S. P. ALCOY","SES VALENCIA","TORREVIEJA","VINAROZ","XATIVA")
  )
  
  
  
  turnos = turnos[turnos$turnoDesc==turno,]
  categorias = categorias[categorias$categoriaDesc==categoria,]
  
  if(departamento != "all"){
    departamentos = departamentos[departamentos$departamentoDesc==departamento,]
  }
  
  
  
  Personas <-data.frame("","","","","","","","")
  colnames(Personas)<-c("Puesto","Nombre","Puntos","departamentoDesc","categoriaDesc","Situacion","Categoria","Departamento")
  
  for (i in 1:length(categorias[,1]))
  {
    for (j in 1:length(departamentos[,1]))
    {
  
      #Puntuacion
      urlbucle <- url
      urlbucle <- gsub("#departamentoCod",gsub(" ","%",departamentos$departamentoCod[j]),urlbucle)
      urlbucle <- gsub("#departamentoDesc",gsub(" ","%",departamentos$departamentoDesc[j]),urlbucle)
      urlbucle <- gsub("#categoriaCod",gsub(" ","%",categorias$categoriaCod[i]),urlbucle)
      urlbucle <- gsub("#categoriaDesc",gsub(" ","%",categorias$categoriaDesc[i]),urlbucle)
      urlbucle <- gsub("#turnoCod",gsub(" ","%",turnos[1]),urlbucle)
      urlbucle <- gsub("#codedicion",gsub(" ","%",edicion),urlbucle)
      html_puntuacion <-read_html(urlbucle)
      nodos_table_puntuacion<-html_puntuacion %>% html_nodes('table')
      
      
      #Situacion
      urlbucleSituacion <- urlsituacion
      urlbucleSituacion <- gsub("#departamentoCod",gsub(" ","%",departamentos$departamentoCod[j]),urlbucleSituacion)
      urlbucleSituacion <- gsub("#departamentoDesc",gsub(" ","%",departamentos$departamentoDesc[j]),urlbucleSituacion)
      urlbucleSituacion <- gsub("#categoriaCod",gsub(" ","%",categorias$categoriaCod[i]),urlbucleSituacion)
      urlbucleSituacion <- gsub("#categoriaDesc",gsub(" ","%",categorias$categoriaDesc[i]),urlbucleSituacion)
      urlbucleSituacion <- gsub("#turnoCod",gsub(" ","%",turnos[1]),urlbucleSituacion)
      html_situacion <-read_html(urlbucleSituacion)
      nodos_table_situacion<-html_situacion %>% html_nodes('table')
      
      if (length(nodos_table_puntuacion) > 0)
      {
          #Puntos
          a<-html_table(nodos_table_puntuacion)
          a<-data.frame(a,departamentos$departamentoDesc[j], categorias$categoriaDesc[i])
          a<-a[2:(length(a[,1])-1),]
          colnames(a)<-c("Puesto","Nombre","Puntos","departamentoDesc","categoriaDesc")
          
          #Situacion
          b<-html_table(nodos_table_situacion)
          b<-data.frame(b)
          b<-b[2:length(b[,1]),]
          colnames(b)<-c("Puesto","Nombre","Situacion","Categoria","Departamento")
          
          #Magia
          c<-merge(x=a,y=b, by ="Nombre", all.x = TRUE)
          d <-c[,c(2,1,3,4,5,7,8,9)]
          colnames(d)<-c("Puesto","Nombre","Puntos","departamentoDesc","categoriaDesc","Situacion","Categoria","Departamento")
          Personas<-rbind(Personas,d)
      }
    }
  }
  con<-file('bolsa.csv')
  write.csv(Personas,file=con)
}

#Ejemplo de uso
obtenerListadoBolsaSANGVA("ORDINARIO","ANALISTA PROGRAMADOR Y DE SISTEMAS","PESET")
