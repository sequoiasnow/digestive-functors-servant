
module Text.Digestive.Servant.Cookie
  ( ReqCookie      -- Intentionally omit constructors
  , CookieList     -- Intentionally omit constructors
  , OneCookie
  , ReqCookies
  , unReqCookie
  , HasCookie(..) )
where

import           Data.List          (lookup)
import           Data.Proxy
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           GHC.TypeLits
import           Servant.API
import           Web.Cookie



newtype ReqCookie (name :: Symbol) v = ReqCookie { unReqCookie :: v }

-- | An analog to @HList@ which avoids name collision. In practice the
-- @HasCookie@ class should be sufficient for type annotation instead of using
-- the structure directly.
data CookieList xs where
  CookieListEmpty :: CookieList '[]
  CookieListCons :: ReqCookie h x -> CookieList xs -> CookieList (ReqCookie h x ': xs)

-- | Convenience type for instances where we only care about one cookie. Unless
-- it is absolutely guaranteed that we are using only one cookie, use the
-- @HasCookie@ typeclass instead.
type OneCookie (name :: Symbol) (v :: *) = '[ReqCookie name v]

-- | Convenience type, requires the cookie headers.
type ReqCookies xs = Header' '[Required] "Cookie" (CookieList xs)

instance FromHttpApiData (CookieList '[]) where
  parseUrlPiece = parseHeader . T.encodeUtf8
  parseHeader _ = Right CookieListEmpty

instance ( FromHttpApiData (CookieList xs)
         , KnownSymbol name
         , FromHttpApiData x )
  => FromHttpApiData (CookieList (ReqCookie name x ': xs )) where
  parseUrlPiece = parseHeader . T.encodeUtf8
  parseHeader raw = do
    let key = T.encodeUtf8 $ T.pack $ symbolVal $ Proxy @name
    val <- maybeToRight "Could not parse cookie " $
      lookup key $ parseCookies raw

    val' <- parseHeader val

    -- Since the existing headers can be extracted from the given data, do
    -- so. This does mean that we repeatedly call @parseCookies@ for each
    -- element in the list. TODO: Extract the behavior into an independent
    -- class to avoid this repetition.
    alreadyParsed <- parseHeader raw
    return $ ReqCookie val' `CookieListCons` alreadyParsed

-- | Check if the cookie list contains a parable @ReqCookie@. This allows easy
-- extraction of relevant cookies. (Using TypeApplications)
--  > getCookie cookies $ ReqCookie @"cookie-name"
class HasCookie name v cookies where
  getCookie :: CookieList cookies -> ReqCookie name v

instance {-# OVERLAPPABLE #-} HasCookie name v (ReqCookie name v ': rest) where
  getCookie (CookieListCons x _) = x

instance {-# OVERLAPPABLE #-} HasCookie name v rest =>
  HasCookie name v (first ': rest) where
  getCookie (CookieListCons first rest) = getCookie rest


-- conveniance
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a Nothing  = Left a
maybeToRight _ (Just b) = Right b
