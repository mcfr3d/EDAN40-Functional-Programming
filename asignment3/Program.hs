module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program Statements deriving Show
type Statements = [Statement.T]
instance Parse T where
  parse = statements
  toString = error "Program.toString not implemented"

statements = iter Statement.parse >-> buildStatements
buildStatements ls = Program ls

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) intList = Statement.exec stmts Dictionary.empty intList
