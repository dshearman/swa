require(RCurl)
require(rjson)

# Build URL
URL = paste("https://graph.facebook.com/me?fields=id,friends&access_token=", token, sep="")
# Encode URL
URL = URLencode(URL)
# Download data
json.data <- getURL(URL)
# parse data
data <- fromJSON(json.data)
# print first friends name
print(data$friends$data[[1]]$name)


download.json.data <- function(URL) {
  ## Encode URL
  URL = URLencode(URL)
  ## Download data
  json.data <- getURL(URL)
  ## parse data
  data <- fromJSON(json.data)

  return(data)
}


get.fb.friends <- function(id, token) {
  ## build URL
  URL = paste("https://graph.facebook.com/", id,
    "/friends?access_token=", token, sep="")
  data = download.json.data(URL)
  return(data)
}


id = "560679740"
friends = get.fb.friends(id, token)


get.fb.mutualfriends <- function(id1, id2, token) {
  ## build URL
  URL = paste("https://graph.facebook.com/", id1,
    "/mutualfriends/", id2, "?access_token=", token, sep="")
  data = download.json.data(URL)
  return(data)
}


mut.data = get.fb.mutualfriends(id, friends$data[[1]]$id, token)

