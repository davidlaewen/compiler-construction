{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Parser.Definition
  ( Lexer,
    Positioned (..),
    TokenParser,
    TokenStream (..),
  )
where

import Data.Data
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Parser.Tokens as PT
import Text.Megaparsec

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
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

instance Stream TokenStream where
  type Token TokenStream = Positioned PT.Token
  type Tokens TokenStream = [Positioned PT.Token]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t : ts)) =
    Just
      ( t,
        TokenStream (T.drop (tokensLength pxy (t :| [])) str) ts
      )
  takeN_ n (TokenStream str s)
    | n <= 0 = Just ([], TokenStream str s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream str s')
              Just nex -> Just (x, TokenStream (T.drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TokenStream str s) =
    let (x, s') = span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TokenStream str s')
          Just nex -> (x, TokenStream (T.drop (tokensLength pxy nex) str) s')

instance VisualStream TokenStream where
  showTokens Proxy =
    unwords
      . NE.toList
      . fmap (show . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine),
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
      (preStr, postStr) = T.splitAt tokensConsumed (tokenStreamInput pstateInput)
      preLine = T.unpack . T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = T.unpack $ T.takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy
