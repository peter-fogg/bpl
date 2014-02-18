module BPL.Scanner
       ( TokenType(..)
       , LineNumber
       , Token(..)
       , tokenize
       )  where

import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.Map as Map

data TokenType = TkIdentifier
               | TkStringLiteral
               | TkNumber
               | TkString
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
                   } deriving (Eq)

instance Show Token where
  show (Token t v l) = show t ++ " \"" ++ v ++ "\"" ++ " : (line " ++ show l ++ ")"

reservedWords :: Map.Map String TokenType
reservedWords = Map.fromList [ ("string", TkString)
                             , ("int", TkInt)
                             , ("void", TkVoid)
                             , ("if", TkIf)
                             , ("else", TkElse)
                             , ("while", TkWhile)
                             , ("return", TkReturn)
                             , ("write", TkWrite)
                             , ("writeln", TkWriteLn)
                             , ("read", TkRead)
                             ]

singleCharTokens :: Map.Map Char (LineNumber -> Token)
singleCharTokens = Map.fromList [ ('(', Token TkLParen "(")
                                , (')', Token TkRParen ")")
                                , ('[', Token TkLSquare "[")
                                , (']', Token TkRSquare "]")
                                , ('{', Token TkLCurly "{")
                                , ('}', Token TkRCurly "}")
                                , (';', Token TkSemicolon ";")
                                , (',', Token TkComma ",")
                                , ('+', Token TkPlus "+")
                                , ('-', Token TkMinus "-")
                                , ('*', Token TkStar "*")
                                , ('/', Token TkSlash "/")
                                , ('%', Token TkPercent "%")
                                , ('&', Token TkAmpersand "&")
                                ]

skipComments :: String -> LineNumber -> (String, LineNumber)
skipComments [] line = ([], line)
skipComments s line = go 1 s line
  where
    go _ [] line = ([], line)
    go 0 s line = (s, line)
    go n ('/':'*':s) line = go (n + 1) s line
    go n ('*':'/':s) line = go (n - 1) s line
    go n ('\n':s) line = go n s (line + 1)
    go n s line = go n (tail s) line

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
    findTokens ('=':c:s) line tokens = case c of
      '=' -> findTokens s line ((Token TkDoubleEqual "==" line):tokens)
      _ -> findTokens (c:s) line ((Token TkSingleEqual "=" line):tokens)
    findTokens ('<':c:s) line tokens = case c of
      '=' -> findTokens s line ((Token TkLEQ "<=" line):tokens)
      _ -> findTokens (c:s) line ((Token TkLAngle "<" line):tokens)
    findTokens ('>':c:s) line tokens = case c of
      '=' -> findTokens s line ((Token TkGEQ ">=" line):tokens)
      _ -> findTokens (c:s) line ((Token TkRAngle ">" line):tokens)
    findTokens ('!':'=':s) line tokens = findTokens s line ((Token TkNotEqual "!=" line):tokens)
    findTokens ('"':s) line tokens = case findString s of
      Nothing -> Left $ "unterminated string on line " ++ (show line)
      Just (token, rest) -> findTokens rest line ((Token TkStringLiteral token line):tokens)
    findTokens (c:s) line tokens
      | c `elem` Map.keys singleCharTokens = case Map.lookup c singleCharTokens of
        Nothing -> Left "shouldn't be here"
        Just token -> findTokens s line ((token line):tokens)
      | isSpace c = findTokens s line tokens
      | isAlpha c = findTokens irest line ((Token tokenType identifier line):tokens)
      | isDigit c = findTokens nrest line ((Token TkNumber (c:num) line):tokens)
      | otherwise = Left $ "highly confused by character " ++ show c ++ " on line " ++ show line
                    where (num, nrest) = findNumber s
                          (id, irest) = findIdentifier s
                          identifier = c:id
                          tokenType = case Map.lookup identifier reservedWords of
                            Nothing -> TkIdentifier
                            Just t -> t
