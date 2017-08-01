{
module Parsers.Test (lexer) where
}
%wrapper "basic" 

$digit = 0-9
$alpha = [a-zA-Z]
tokens :-
  $white+           ;
  "--".*            ;
  let               {\s -> Let}
  


{
 -- The token type 
data Token =  Let  | In deriving(Eq, Show) 


lexer aString = do 
  return (alexScanTokens aString)
}