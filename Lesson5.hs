getRequestUrl host key resource id = host ++
                                     "/" ++
                                     resource ++
                                     "/" ++
                                     id ++
                                     "?token=" ++
                                     key

getHostRequestBuilder host = (\apiKey resource id ->
                               getRequestUrl host apiKey resource id)

exampleUrlBuilder = getHostRequestBuilder "https://example.com"

genApiRequestBuilder hostBuilder apiKey = (/resource id -> hostBuilder apiKey resource id)
