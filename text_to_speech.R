# TEXT TO SPEECH BOTONES MARQUESINAS PLASENCIA

library(httr)
library(dplyr)
library(jsonlite)
library(lubridate)



# ------------------------------------------------------------------------------
# PETICIÓN TOKENs THB
# ------------------------------------------------------------------------------

cuerpo <- '{"username":"kepa@techfriendly.es","password":"kepatech"}'
post <- httr::POST(url = "http://plataforma:9090/api/auth/login",
                   add_headers("Content-Type"="application/json","Accept"="application/json"),
                   body = cuerpo,
                   verify= FALSE,
                   encode = "json",verbose()
)

resultado_peticion_token <- httr::content(post)
auth_thb <- paste("Bearer",resultado_peticion_token$token)


# ------------------------------------------------------------------------------
# GET DISPOSITIVOS TIPO: marquesina_audio
# ------------------------------------------------------------------------------

url_thb <- "http://plataforma:9090/api/tenant/devices?pageSize=1000&page=0"
peticion <- GET(url_thb, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

df <- jsonlite::fromJSON(rawToChar(peticion$content))
df <- as.data.frame(df)
df_audio <- df[df$data.type == "marquesina_audio",]
ids_df_audio <- df_audio$data.id$id

atributo_id_audio <- c()
keys <- URLencode(c("id"))
for(i in 1:nrow(df_audio)){
  
  url <- paste("http://plataforma:9090/api/plugins/telemetry/DEVICE/",ids_df_audio[i],"/values/attributes/SERVER_SCOPE?keys=", keys,sep = "")
  peticion <- GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  df <- as.data.frame(df)
  atributo_id_audio <- c(atributo_id_audio, df$value)
}

# ------------------------------------------------------------------------------
# GET ACTIVOS TIPO: parada. Objetivo: Relacionar ids
# ------------------------------------------------------------------------------

url_thb <- "http://plataforma:9090/api/tenant/assets?pageSize=1000&page=0"
peticion <- GET(url_thb, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

df <- jsonlite::fromJSON(rawToChar(peticion$content))
df <- as.data.frame(df)
df_parada <- df[df$data.type == "parada",]
ids_df_parada <- df_parada$data.id$id

atributo_id_parada <- c()
keys <- URLencode(c("id"))
for(i in 1:nrow(df_parada)){
  
  url <- paste("http://plataforma:9090/api/plugins/telemetry/ASSET/",ids_df_parada[i],"/values/attributes/SERVER_SCOPE?keys=", keys,sep = "")
  peticion <- GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
  # Tratamiento datos. De raw a dataframe
  df <- jsonlite::fromJSON(rawToChar(peticion$content))
  df <- as.data.frame(df)
  atributo_id_parada <- c(atributo_id_parada, df$value)
}

# ------------------------------------------------------------------------------
# RECOGIDA DE TIEMPOS DE LLEGADA EN ATRIBUTOS DE ACTIVO PARADA
# ------------------------------------------------------------------------------
keys <- URLencode(c("tiempo_llegada_linea_1,tiempo_llegada_linea_2,tiempo_llegada_linea_3,parada_destino_linea1,parada_destino_linea2,parada_destino_linea3"))

while(1){
  
  for(i in 1:nrow(df_audio)){
    
    # 1) RECOGIDA DE ATRIBUTOS ACTIVO TIPO PARADA
    posicion_id <- match(atributo_id_audio[i],atributo_id_parada)
    id_activo <- ids_df_parada[posicion_id]
    
    
    url <- paste("http://plataforma:9090/api/plugins/telemetry/ASSET/",id_activo,"/values/attributes/SERVER_SCOPE?keys=", keys,sep = "")
    peticion <- GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))
    # Tratamiento datos. De raw a dataframe
    df <- jsonlite::fromJSON(rawToChar(peticion$content))
    df <- as.data.frame(df)
    
    
    posicion_tiempos <- grep("tiempo",df$key)
    posicion_destinos <- grep("destino",df$key)
    
    
    # 2) GENERACIÓN TEXTO
    
    texto <- ""
    for(j in 1:length(posicion_destinos)){
      numero_linea <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$key[posicion_destinos[j]]))
      parada_destino <- df$value[posicion_destinos[j]]
      tiempo <- df$value[posicion_tiempos[j]]
      
      texto <- paste(texto, "Línea ",numero_linea," ",parada_destino," ", tiempo, ". ", sep = "")
    }
    
    
    
    # 3) ESCRITURA EN ATRIBUTO DE DISPOSITIVO marquesina_audio
    
    url <- paste("http://plataforma:9090/api/plugins/telemetry/DEVICE/", ids_df_audio[i], "/SHARED_SCOPE",sep = "")
    json_envio_plataforma <- paste('{"text2speech":"', texto,'"',
                                   '}',sep = "")
    post <- httr::POST(url = url,
                       add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                       body = json_envio_plataforma,
                       verify= FALSE,
                       encode = "json",verbose()
    )
    
  }
  
  Sys.sleep(60)
  
}

