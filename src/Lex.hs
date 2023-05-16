module Lex (lexs) where
    import Types ( ErrorInfo, Token(..) )
    lexs :: String -> Either ErrorInfo [Token]
    lexs str = lexs' str (0,0) [] 
    lexs' :: String -> (Int, Int) -> [Token] -> Either ErrorInfo [Token]
    lexs' [] _ acc = Right $ reverse acc
    lexs' ('>':str) (l,c) acc = lexs' str (l, c+1) (TokPRight (l,c) : acc)
    lexs' ('<':str) (l,c) acc = lexs' str (l, c+1) (TokPLeft (l,c) : acc)
    lexs' ('+':str) (l,c) acc = lexs' str (l, c+1) (TokInc (l,c) : acc)
    lexs' ('-':str) (l,c) acc = lexs' str (l, c+1) (TokDec (l,c) : acc)
    lexs' ('.':str) (l,c) acc = lexs' str (l, c+1) (TokPoint (l,c) : acc)
    lexs' (',':str) (l,c) acc = lexs' str (l, c+1) (TokComma (l,c) : acc)
    lexs' ('[':str) (l,c) acc = lexs' str (l, c+1) (TokLBrack (l,c) : acc)
    lexs' (']':str) (l,c) acc = lexs' str (l, c+1) (TokRBrack (l,c) : acc)
    lexs' ('\t':str) (l,c) acc = lexs' str (l, c+1) acc
    lexs' (' ':str) (l,c) acc = lexs' str (l, c+1) acc
    lexs' ('\n':str) (l,c) acc = lexs' str (l+1, c) acc
    lexs' ('\r':str) (l,c) acc = lexs' str (l+1, c) acc
    lexs' (t:_) (l,c) _ = Left ("Unrecognized token " ++ [t], l, c)
