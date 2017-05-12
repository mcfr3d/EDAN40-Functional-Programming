module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    If Expr.T Statement Statement
   -- While Expr.T Begin |
   -- Read String |
   -- Write Expr.T|
   -- Begin Statements|
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss 
buildAss (v, e) = Assignment v e

skip = require "skip" #- require ";" >-> buildSkip
buildSkip a = Skip

if_stmt = require "if" -# Expr.parse #- require "then" -# stmt # stmt #- require ";" -# require "else" # stmt >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

stmt = assignment ! skip ! if_stmt 

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input



instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
