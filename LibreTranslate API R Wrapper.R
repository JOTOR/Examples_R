translate_text <- function(source="en", target="es", text="Hello World"){
  library(httr)
  body <- list(q=text, source=source, target=target)
  r <- POST("https://libretranslate.com/translate", encode="json", body=body)
  return(content(r)[[1]])
  }


get_language <- function(text="Hello World"){
  library(httr)
  body <- list(q=text)
  r <- POST("https://libretranslate.com/detect", encode="json", body=body)
  #return the language and the confidence
  return(c(content(r)[[1]][[2]], content(r)[[1]][[1]]))
  }