getRequestUrl host key resource id = host ++
                                     "/" ++
                                     resource ++
                                     "/" ++
                                     id ++
                                     "?token=" ++
                                     key

getHostRequestBuilder host = (\apiKey resource id ->
                               getRequestUrl host apiKey resource id)

getHostRequestBuilder2 host = (\apiKey resource id -> getRequestUrl host apiKey resource id)

exampleUrlBuilder = getHostRequestBuilder "https://example.com"

-- same function but built with partial application
exampleUrlBuilder2 = getRequestUrl "https://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "12321hask23ll"
-- same function but built with partial application
exampleUrlWithKeyBuilder = exampleUrlBuilder2 "myApiKey"
-- Quick check 5.3
finalExampleUrlBuilder = getRequestUrl "https://example.com" "1337hAsk3ll" "book"


-- 5.2.1 Partial application: making closures simple
add4 a b c d = a + b + c + d