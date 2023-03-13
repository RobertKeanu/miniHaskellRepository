
module REPLCommand where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec (anyChar)
import Control.Applicative (many , (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String

replDef : LanguageDef st
replDef = emptyDef {
  reservedNames = [":load", ":quit"],
  reservedOptions = [":l",":q"]
}
repl = TokenParser st
repl = makeTokenParser replDef

replQuit :: Parser REPLCommand
replQuit = pure Quit <$> (reservedOp repl ":q") <|> (reserved repl ":quit")

replLoad :: Parser REPLCommand
replLoad = do 
  ((reservedOp repl) ":l" ) <|> ((reservedOp repl) ":load")
  s <- some anyChar
  return $ Load s

replEval :: Parser REPLCommand
replEval = Eval <$> many anyChar

replCommand :: Parser REPLCommand
replCommand = undefined

