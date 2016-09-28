module Data.MediaType.Common where

import Data.MediaType (MediaType(..))

applicationFormURLEncoded :: MediaType
applicationFormURLEncoded = MediaType "application/x-www-form-urlencoded"

applicationJSON :: MediaType
applicationJSON = MediaType "application/json"

applicationJavascript :: MediaType
applicationJavascript = MediaType "application/javascript"

applicationOctetStream :: MediaType
applicationOctetStream = MediaType "application/octet-stream"

applicationXML :: MediaType
applicationXML = MediaType "application/xml"

imageGIF :: MediaType
imageGIF = MediaType "image/gif"

imageJPEG :: MediaType
imageJPEG = MediaType "image/jpeg"

imagePNG :: MediaType
imagePNG = MediaType "image/png"

multipartFormData :: MediaType
multipartFormData = MediaType "multipart/form-data"

textCSV :: MediaType
textCSV = MediaType "text/csv"

textHTML :: MediaType
textHTML = MediaType "text/html"

textPlain :: MediaType
textPlain = MediaType "text/plain"

textXML :: MediaType
textXML = MediaType "text/xml"
