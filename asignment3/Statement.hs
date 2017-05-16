module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    If Expr.T Statement Statement|
    While Expr.T Statement|
    Read String |
    Write Expr.T|
    Begin Statements|
    Comment
    deriving Show

type Statements = [Statement]    
    
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss 
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip a = Skip

if_stmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s

read_stmt = accept "read" -# word #- require ";" >-> buildRead
buildRead a = Read a

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

begin = accept "begin" -# iter parse #- require "end">-> buildBegin
buildBegin s = Begin s

comment = accept "--" -# newLineChar >-> buildComment
buildComment a = Comment

newLineChar = token $iter isNewLineChar
isNewLineChar = char ? (/='\n')

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment stringvar expr:stmts) dict input =
    exec stmts (Dictionary.insert (stringvar, Expr.value expr dict) dict) input 
exec (Skip:stmts) dict input = exec stmts dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond doStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (doStmts:While cond doStmts:stmts) dict input
    else exec stmts dict input
exec (Read string:stmts) dict (x:xs) = exec stmts (Dictionary.insert (string,x) dict) xs
exec (Write expr:stmts) dict input =  Expr.value expr dict : exec stmts dict input
exec (Begin begStmts:stmts) dict input = exec begStmts dict input ++ exec stmts dict input
exec (Comment:stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! if_stmt ! while ! read_stmt ! write ! begin ! comment
  toString = error "Statement.toString not implemented"
 