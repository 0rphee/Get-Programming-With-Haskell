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

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "12321hask23ll"

-- 5.2.1 Partial application: making closures simple

add4 a b c d = a + b + c + d
