{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Parser.Definition
  ( Lexer,
    Positioned (..),
    TokenParser,
    TokenStream (..),
    initialState
  )
where

import Data.Data
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Parser.Tokens as PT
import Text.Megaparsec
import Data.Maybe (listToMaybe)

type Lexer = Parsec Void Text

type TokenParser = Parsec Void TokenStream

data TokenStream = TokenStream
  { tokenStreamInput :: T.Text,
    tokenStream :: [Positioned PT.Token]
  }
  deriving (Show)

data Positioned a = Positioned
  { startPos :: SourcePos,
    endPos :: SourcePos,
    startOffset :: Int,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

instance Stream TokenStream where
  type Token TokenStream = Positioned PT.Token
  type Tokens TokenStream = [Positioned PT.Token]

  tokenToChunk :: Proxy TokenStream -> Positioned PT.Token -> [Positioned PT.Token]
  tokenToChunk Proxy x = [x]

  tokensToChunk :: Proxy TokenStream -> [Positioned PT.Token] -> [Positioned PT.Token]
  tokensToChunk Proxy xs = xs

  chunkToTokens :: Proxy TokenStream -> [Positioned PT.Token] -> [Positioned PT.Token]
  chunkToTokens Proxy = id

  chunkLength :: Proxy TokenStream -> [Positioned PT.Token] -> Int
  chunkLength Proxy = length

  chunkEmpty :: Proxy TokenStream -> [Positioned PT.Token] -> Bool
  chunkEmpty Proxy = null

  take1_ :: TokenStream -> Maybe (Positioned PT.Token, TokenStream)
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream _ [t]) = Just (t, TokenStream T.empty [])
  take1_ (TokenStream input (t : t' : ts)) = Just (t, TokenStream (T.drop (startOffset t' - startOffset t) input) (t' : ts))

  takeN_ :: Int -> TokenStream -> Maybe ([Positioned PT.Token], TokenStream)
  takeN_ n (TokenStream input ts)
    | n <= 0 = Just ([], TokenStream input ts)
    | null ts = Nothing
    | otherwise =
        case splitAt n ts of
          ([], _) -> Just ([], TokenStream input ts)
          (_, []) -> Just (ts, TokenStream T.empty [])
          (t : rest, t' : rest') -> Just (t : rest, TokenStream (T.drop (startOffset t' - startOffset t) input) (t' : rest'))

  takeWhile_ :: (Positioned PT.Token -> Bool) -> TokenStream -> ([Positioned PT.Token], TokenStream)
  takeWhile_ f (TokenStream input ts) =
    case span f ts of
      ([], _) -> ([], TokenStream input ts)
      (_, []) -> (ts, TokenStream T.empty [])
      (t : rest, t' : rest') -> (t : rest, TokenStream (T.drop (startOffset t' - startOffset t) input) (t' : rest'))


instance VisualStream TokenStream where
  showTokens :: Proxy TokenStream -> NE.NonEmpty (Token TokenStream) -> String
  showTokens Proxy =
    unwords
      . NE.toList
      . fmap (show . tokenVal)

  tokensLength :: Proxy TokenStream -> NE.NonEmpty (Token TokenStream) -> Int
  tokensLength Proxy xs =
    let lastToken = NE.last xs in
      startOffset lastToken - startOffset (NE.head xs) + tokenLength lastToken

instance TraversableStream TokenStream where
  reachOffset :: Int -> PosState TokenStream -> (Maybe String, PosState TokenStream)
  reachOffset o PosState {..} =
    ( Just (expandTab (unPos pstateTabWidth) $ prefix ++ restOfLine),
      PosState
        { pstateInput =
            TokenStream
              { tokenStreamInput = postStr,
                tokenStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (tokenStream pstateInput)
      (whitespacePrefix, input) = if pstateOffset == 0
                then T.splitAt (maybe 0 startOffset (listToMaybe pre)) $ tokenStreamInput pstateInput
                else (T.empty, tokenStreamInput pstateInput)
      (preStr, postStr) = T.splitAt charsConsumed input
      preLine = T.unpack . T.reverse . T.takeWhile (/= '\n') . T.reverse $ T.append whitespacePrefix preStr
      charsConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> 
            case post of
              [] -> tokensLength pxy nePre
              (t : _) -> startOffset t - startOffset (NE.head nePre)
      restOfLine = T.unpack $ T.takeWhile (/= '\n') postStr

-- Expands tabs to the tabWidth
-- This function takes the line offset into account, i.e. if the tabwidth is 4,
-- the string "aa\tb" is be expanded to "aa  b", because the tab goes to the next multipkle of tabWidth.
-- This functions only works properly for input without newlines
expandTab :: Int -> String -> String
expandTab tabWidth = go 0 0
  where
    go ::
      -- | The current offset in the line (modulo tabWidth)
      Int ->
      -- | The amount of spaces to add
      Int ->
      -- | The input string
      String ->
      -- | The output string with expanded tabs
      String
    go _ 0 [] = []
    go o 0 ('\t' : xs) = go 0 (tabWidth - o) xs
    go o 0 (x : xs) = x : go ((o + 1) `mod` tabWidth) 0 xs
    go _ n xs = ' ' : go 0 (n - 1) xs

pxy :: Proxy TokenStream
pxy = Proxy

initialState :: FilePath -> s -> State s e
initialState filePath input =
  State
    { stateInput = input,
      stateOffset = 0,
      statePosState =
        PosState
          { pstateInput = input,
            pstateOffset = 0,
            pstateSourcePos = initialPos filePath,
            -- The defaultTabWidth of megaparsec is 8, we set it to 4.
            -- Otherwise, errors will be reported with the wrong offsets,
            -- when there are tabs in the input program.
            pstateTabWidth = mkPos 4,
            pstateLinePrefix = ""
          },
      stateParseErrors = []
    }
