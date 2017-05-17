module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
type Statements = [Statement] 
data Statement =
    Assignment String Expr.T |
    Skip |
    If Expr.T Statement Statement|
    While Expr.T Statement|
    Read String |
    Write Expr.T|
    Begin Statements
    deriving Show

   
    
assignment = token $ word #- accept ":=" # Expr.parse #- require ";" >-> buildAss 
buildAss (v, e) = Assignment v e

skip = token $ accept "skip" #- require ";" >-> buildSkip
buildSkip a = Skip

if_stmt = token $ accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

while =token $ accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e,s) = While e s

read_stmt = token $ accept "read" -# word #- require ";" >-> buildRead
buildRead a = Read a

write = token $ accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

begin = token $ accept "begin" -# iter parse #- require "end">-> buildBegin
buildBegin s = Begin s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
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
exec (Begin begStmts:stmts) dict input =exec (begStmts ++ stmts) dict input

shw :: Int -> Statement -> String
shw prec (Assignment string expr) = string++" := "++(Expr.shw 0 expr)++";\n"
shw prec (Skip) = "skip;\n"
shw prec (If expr stmt elseStmt) = "if " ++ Expr.shw 0 expr ++ "\n then \n " ++ shw 0 stmt ++ "\n else\n" ++ shw 0 stmt 
shw prec (While expr stmt) = "while " ++ Expr.shw 0 expr ++ "\n do \n" ++ shw 0 stmt 
shw prec (Read string)="read" ++ string ++ "\n"
shw prec (Write expr)= "write " Expr.shw 0 expr ++ "\n"
shw prec (Begin (stmt:stmts)) = "begin \n" ++ "test"
instance Parse Statement where
  parse = assignment ! skip ! if_stmt ! while ! read_stmt ! write ! begin 
  toString = shw 0
