{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Controller.Badge
    ( Badge(..)
    , badgeRender
    , badgeEtag

    , SVG
    ) where

import           Prelude.Local

import           Codec.Base64             as B64
import qualified Crypto.Hash.SHA256       as SHA256
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BS.L
import qualified Data.Map.Strict          as Map
import           Data.Swagger
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Servant.API.ContentTypes

import           Controller.Types


data Badge = BadgeUnknown
           | BadgeOK
           deriving (Eq,Ord,Enum,Show,Bounded)

instance NFData Badge where
  rnf !_ = ()

{-# NOINLINE badges #-}
badges :: Map Badge (ETag, BS.L.ByteString)
badges = force $ Map.fromList (map go [minBound .. maxBound])
  where
    go k = let b = h k in (k,(etagLBS b, b))

    h :: Badge -> BS.L.ByteString
    h BadgeUnknown = badgeRender' (parmsGray  "unknown")
    h BadgeOK      = badgeRender' (parmsGreen "available")

    parmsGreen = BadgeParams 75 75 "#555" "#4c1" "Hackage CI"
    parmsGray  = BadgeParams 75 75 "#555" "#aaa" "Hackage CI"

badgeRender :: Badge -> BS.L.ByteString
badgeRender k = case Map.lookup k badges of
                  Just (_, bs) -> bs
                  Nothing      -> error "badgeRender" -- impossible

badgeEtag :: Badge -> ETag
badgeEtag k = case Map.lookup k badges of
                  Just (etag, _) -> etag
                  Nothing        -> error "badgeEtag" -- impossible

etagLBS :: BS.L.ByteString -> ETag
etagLBS = ETag . B64.encode . BS.take (3*6) . SHA256.hashlazy

data SVG

instance Accept SVG where
    contentType _ = "image/svg+xml;charset=utf-8"

instance MimeRender SVG Badge where
    mimeRender _ = badgeRender

instance ToSchema Badge where
    declareNamedSchema _ = pure $ NamedSchema (Just "Badge") $ binarySchema

data BadgeParams = BadgeParams
    { bpWidth1, bpWidth2 :: Word
    , bpFill1, bpFill2   :: String
    , bpText1, bpText2   :: String
    }

-- | Quick'n'dirty SVG generation
badgeRender' :: BadgeParams -> BS.L.ByteString
badgeRender' BadgeParams{..}
  = BS.L.fromStrict $ T.encodeUtf8 $ T.pack $ map (\case { '\'' -> '"'; c -> c }) $ mconcat
  [ "<svg xmlns='http://www.w3.org/2000/svg' width='", ws, "' height='20'>"
  , "<linearGradient id='a' x2='0' y2='100%'>"
  , "<stop offset='0%' stop-opacity='.1' stop-color='#aaa'/>"
  , "<stop offset='100%' stop-opacity='.1'/>"
  , "</linearGradient>"
  , "<clipPath id='x'><rect rx='3' width='", ws, "' height='20'/></clipPath>"
  , "<g clip-path='url(#x)'>"
  , "<rect width='", show bpWidth1, "' height='20' fill='", bpFill1, "'/>"
  , "<rect x='", show bpWidth1, "' width='", show bpWidth2, "' height='20' fill='", bpFill2, "'/>"
  , "<rect width='", ws, "' height='20' fill='url(#a)'/>"
  , "</g>"
  , "<g fill='#fff' text-anchor='middle' font-family='DejaVu Sans,Verdana,Geneva,sans-serif' font-size='11'>"
  , "<text x='", tx1 ,"' y='15' fill='#010101' fill-opacity='.3'>", bpText1, "</text>"
  , "<text x='", tx2,"' y='15' fill='#010101' fill-opacity='.3'>", bpText2, "</text>"
  , "<text x='", tx1,"' y='14'>", bpText1, "</text>"
  , "<text x='", tx2,"' y='14'>", bpText2, "</text>"
  , "</g>"
  , "</svg>"
  ]
  where
    w = bpWidth1 + bpWidth2
    ws = show w

    tx1 | odd bpWidth1  = show ((bpWidth1+2) `quot` 2) ++ ".5"
        | otherwise     = show ((bpWidth1+2) `quot` 2)

    tx2 | odd bpWidth2  = show (w-((bpWidth2+2) `quot` 2)-1) ++ ".5"
        | otherwise     = show (w-((bpWidth2+2) `quot` 2))
