#---------------------------------------------------------#
#      MEET-UP R-LADIES PARIS 24/01/2018 INTRO RVEST      #
#---------------------------------------------------------#
# encoding UTF-8

# LE FIGARO
library(rvest)

url_le_figaro <- "http://www.lefigaro.fr/"
le_figaro <- read_html(url_le_figaro)

## recuperation des sujets du moments

en_ce_moment <- trim_string( 
                unlist( strsplit( 
                  gsub( "\n", "", html_nodes(le_figaro, "div.fig-en-ce-moment.fig-en-ce-moment--actu") %>% 
                          html_text() ),
                  "                                            "))) [-1]

## recuperation des liens vers les sujets
en_ce_moment_links <- html_nodes(le_figaro, "div.fig-en-ce-moment.fig-en-ce-moment--actu a") %>% 
                      html_attr("href")

## creation d'une session virtuelle
sF <- html_session(url_le_figaro)

## recuperation du contenu
en_ce_moment_contents <- apply( as.array(en_ce_moment_links), 1, FUN=jump_to, x=sF)

le_figaro_df <- data.frame( "Journal"="", "Date"=Sys.Date(), "Type_article"="", "Theme"="",
                            "Titre"="", "Contenu"="", stringsAsFactors = FALSE )

## Ce code est a optimiser
for( i in 1:length(en_ce_moment_contents)){
  k = 1 + 5*(i-1)
  if(en_ce_moment[[i]] != "Aujourd'hui sur Figaro Live"){
    top5_links <- as.array( html_nodes(en_ce_moment_contents[[i]], "h2.fig-profile__headline a") %>% 
                              html_attr("href") ) [1:5]
    top5_contents <- apply( as.array( top5_links ), 1, FUN=jump_to, x=sF )
    for( j in 0:( length( top5_links ) -1) ){
      le_figaro_df[k+j, "Journal"] <- "Le Figaro"
      le_figaro_df[k+j, "Date"] <- Sys.Date() 
      le_figaro_df[k+j, "Type_article"] <- "En_ce_moment"
      le_figaro_df[k+j, "Theme"] <- en_ce_moment[i]
      le_figaro_df[k+j, "Titre"] <- html_nodes(top5_contents[[1]],"h1.fig-main-title") %>% html_text() 
      
      p <-  trim_string( html_nodes( top5_contents[[j+1]], "div p" ) %>% html_text() )  
      limite1 <- grep("Cet article est réservé aux abonnés", p)[1]
      limite2 <- grep("Abonnez-vous", p)[1]
      if(!is.na(limite1)){
        le_figaro_df[k+j, "Contenu"] <- concat( p[ 1:(limite1 -1) ] )
      }
      else if(!is.na(limite2)){
        le_figaro_df[k+j, "Contenu"] <- concat( p[ 1:(limite2 -1) ] )
      }
      else le_figaro_df[k+j, "Contenu"] <- concat( p )
    }
  }
  else{
    top5_links <- as.array( html_nodes(en_ce_moment_contents[[i]], "div.fig-content__body a") %>% 
                              html_attr("href") ) [1:5]
    top5_contents <- apply( as.array( top5_links ), 1, FUN=jump_to, x=sF )
    top5_titres <- as.array( html_nodes(en_ce_moment_contents[[i]], "div.fig-content__body p") %>% 
                               html_text() ) 
    top5_titres <- top5_titres[which(substr(x = top5_titres, start = 1, stop = 1) == "●")]
    for( j in 0:( length( top5_links ) -1) ){
      le_figaro_df[k+j, "Journal"] <- "Le Figaro"
      le_figaro_df[k+j, "Date"] <- Sys.Date() 
      le_figaro_df[k+j, "Type_article"] <- "Figaro Live"
      le_figaro_df[k+j, "Theme"] <- "Live sujets du moments"
      le_figaro_df[k+j, "Titre"] <- top5_titres[[j+1]]
      le_figaro_df[k+j, "Contenu"] <- top5_links[[j+1]]
    }
  }
}