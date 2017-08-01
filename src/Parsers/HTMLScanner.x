{
module Parsers.HTMLScanner (lexer) where

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

$startTag = [\<]
$endTag = [\>]
tokens :- 
  \< $alpha [$alpha] * \> {\s -> makeTag s}
  \< \/ $alpha [$alpha] * \> {\s -> makeTag s}
  $alpha [$alpha] * {\s -> CharArray s}
  $endTag       ; -- ignore the end tag
-- How to do start tag end tag 
{
 -- The token type 
data Element  =  
          HTML String
         | Head String
         | Body String
         | Invalid String
         | CharArray String
         | EndHTML String
         | EndHead String
         | EndBody String
         | StartPara String
         | EndPara String
         deriving (Eq, Show)

makeTag aString = 

  case aString of 
    "<HTML>" -> HTML aString
    "<HEAD>" -> Head aString
    "<BODY>" -> Body aString 
    "</HTML>" -> EndHTML aString
    "</BODY>" -> EndBody aString
    "</HEAD>" -> EndHead aString 
    "<p>" -> StartPara aString
    "</p>" -> EndPara aString
    _      -> Invalid aString
lexer aString = return (alexScanTokens aString)

testLexer :: Monad m => m [Element]
testLexer = Parsers.HTMLScanner.lexer "<HTML> html <HEAD> head </HEAD> </HTML>"
} 