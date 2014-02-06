module Scanner
       where

import System.IO (IOMode(..), hClose, readFile, openFile)
import System.Environment (getArgs)
import Control.Monad (forM_)
import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.Map as Map

data TokenType = TkIdentifier
               | TkString
               | TkNumber
               | TkInt
               | TkVoid
               | TkIf
               | TkElse
               | TkWhile
               | TkReturn
               | TkWrite
               | TkWriteLn
               | TkRead
               | TkSemicolon
               | TkComma
               | TkLSquare
               | TkRSquare
               | TkLCurly
               | TkRCurly
               | TkLParen
               | TkRParen
               | TkRAngle
               | TkLAngle
               | TkLEQ
               | TkGEQ
               | TkDoubleEqual
               | TkNotEqual
               | TkSingleEqual
               | TkPlus
               | TkMinus
               | TkStar
               | TkSlash
               | TkPercent
               | TkAmpersand
               | TkEOF
               deriving (Eq, Show)

type LineNumber = Integer

data Token = Token { tokenType :: TokenType
                   , value :: String
                   , line :: LineNumber
                   } deriving (Eq, Show)

reservedWords :: Map.Map String TokenType
reservedWords = Map.fromList [ ("int", TkInt)
                             , ("void", TkVoid)
                             , ("if", TkIf)
                             , ("else", TkElse)
                             , ("while", TkWhile)
                             , ("return", TkReturn)
                             , ("write", TkWrite)
                             , ("writeln", TkWriteLn)
                             , ("read", TkRead)
                             ]

skipComments :: String -> LineNumber -> (String, LineNumber)
skipComments [] line = ([], line)
skipComments s line = readUntil "*/" s line
  where
    readUntil pat [] line = ([], line)
    readUntil pat ('\n':s) line = readUntil pat s (line + 1)
    readUntil pat s line = let beginning = take (length pat) s
                      in
                       if pat == beginning
                       then (drop (length pat) s, line)
                       else readUntil pat (tail s) line

findString :: String -> Maybe (String, String)
findString [] = Nothing
findString s = go s ""
  where go [] _ = Nothing
        go ('"':rest) acc = Just (reverse acc, rest)
        go ('\n':_) _ = Nothing -- unterminated string
        go (c:rest) acc = go rest (c:acc)

findNumber :: String -> (String, String)
findNumber [] = ("", "")
findNumber s = go s ""
  where go [] acc = ("", reverse acc)
        go (c:rest) acc = if isDigit c
                          then go rest (c:acc)
                          else (reverse acc, c:rest)

findIdentifier :: String -> (String, String)
findIdentifier [] = ("", "")
findIdentifier s = go s ""
  where go [] acc = ("", reverse acc)
        go (c:rest) acc = if isValidIdentifierChar c
                          then go rest (c:acc)
                          else (reverse acc, c:rest)
        isValidIdentifierChar c = c `elem` '_':['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

tokenize :: String -> Either String [Token]
tokenize s = findTokens s 1 []
  where
    findTokens [] line tokens = Right $ reverse ((Token TkEOF "" line):tokens)
    findTokens ('\n':s) line tokens = findTokens s (line + 1) tokens
    findTokens ('/':'*':s) line tokens = let (rest, newLine) = skipComments s line
                                         in findTokens rest newLine tokens
    findTokens ('(':s) line tokens = findTokens s line ((Token TkLParen "(" line):tokens)
    findTokens (')':s) line tokens = findTokens s line ((Token TkRParen ")" line):tokens)
    findTokens ('[':s) line tokens = findTokens s line ((Token TkLSquare "[" line):tokens)
    findTokens (']':s) line tokens = findTokens s line ((Token TkRParen "]" line):tokens)
    findTokens ('{':s) line tokens = findTokens s line ((Token TkLCurly "{" line):tokens)
    findTokens ('}':s) line tokens = findTokens s line ((Token TkRCurly "}" line):tokens)
    findTokens ('=':c:s) line tokens = case c of
      '=' -> findTokens s line ((Token TkDoubleEqual "==" line):tokens)
      _ -> findTokens (c:s) line ((Token TkSingleEqual "=" line):tokens)
    findTokens ('<':c:s) line tokens = case c of
      '=' -> findTokens s line ((Token TkLEQ "<=" line):tokens)
      _ -> findTokens (c:s) line ((Token TkLAngle "<" line):tokens)
    findTokens ('>':c:s) line tokens = case c of
      '=' -> findTokens s line ((Token TkGEQ ">=" line):tokens)
      _ -> findTokens (c:s) line ((Token TkRAngle ">" line):tokens)
    findTokens (';':s) line tokens = findTokens s line ((Token TkSemicolon ";" line):tokens)
    findTokens (',':s) line tokens = findTokens s line ((Token TkComma "," line):tokens)
    findTokens ('+':s) line tokens = findTokens s line ((Token TkPlus "+" line):tokens)
    findTokens ('-':s) line tokens = findTokens s line ((Token TkMinus "-" line):tokens)
    findTokens ('*':s) line tokens = findTokens s line ((Token TkStar "*" line):tokens)
    findTokens ('/':s) line tokens = findTokens s line ((Token TkSlash "/" line):tokens)
    findTokens ('%':s) line tokens = findTokens s line ((Token TkPercent "%" line):tokens)
    findTokens ('&':s) line tokens = findTokens s line ((Token TkAmpersand "&" line):tokens)
    findTokens ('!':'=':s) line tokens = findTokens s line ((Token TkNotEqual "!=" line):tokens)
    findTokens ('"':s) line tokens = case findString s of
      Nothing -> Left $ "unterminated string on line " ++ (show line)
      Just (token, rest) -> findTokens rest line ((Token TkString token line):tokens)
    findTokens (c:s) line tokens
      | isSpace c = findTokens s line tokens
      | isAlpha c = findTokens irest line ((Token tokenType identifier line):tokens)
      | isDigit c = findTokens nrest line ((Token TkNumber (c:num) line):tokens)
                    where (num, nrest) = findNumber s
                          (id, irest) = findIdentifier s
                          identifier = c:id
                          tokenType = case Map.lookup identifier reservedWords of
                            Nothing -> TkIdentifier
                            Just t -> t

main = do
  args <- getArgs
  let testFile = case args of
        (fname:_) -> fname
        _ -> "test.bpl"
  contents <- readFile testFile
  case tokenize contents of
    Left err -> putStrLn $ "PROBLEMTOWN: " ++ err
    Right tokens -> forM_ tokens (\t -> putStrLn (show t))
