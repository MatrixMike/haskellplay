{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module Module_1637009548_87901 where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.KeyMap as J
import qualified Data.Aeson.Key as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.RawString.QQ
import qualified Text.XML.Generator as X

type Result = J.Parser

stop :: Result a
stop = fail "Oops!"

as :: Eq a => J.Parser a -> a -> J.Parser ()
as p s = do
    res <- p
    if res == s
        then pure ()
        else stop

asString :: J.Parser String -> String -> J.Parser ()
asString = as

docTag :: J.Object -> Result ()
docTag o = o J..: "tag" `asString` "doc"

mkInfo :: J.Value -> Result X.DocInfo
mkInfo v = flip (J.withObject "DocInfo") v \o -> do
    s    <- o J..: "standalone"
    t    <- o J..: "docType"
    pre  <- o J..: "preMisc" >>= jsonToXml
    post <- o J..: "postMisc" >>= jsonToXml
    pure (X.DocInfo s t pre post)

docInfo :: J.Object -> Result X.DocInfo
docInfo o = join $ (mkInfo <$> o J..: "info") <|> pure (pure X.defaultDocInfo)

mkAttribute :: J.Object -> Result (X.Xml X.Attr)
mkAttribute o = do
    n <- o J..: "name"
    t <- o J..: "content"
    pure (X.xattr n t)

mkAttribute' :: (J.Key, J.Value) -> Result (X.Xml X.Attr)
mkAttribute' (k, v) = do
    c <- J.parseJSON v
    pure (X.xattr (J.toText k) c)

mkAttributes :: J.Value -> Result [X.Xml X.Attr]
mkAttributes a@(J.Array  _) = mapM mkAttribute  =<< J.parseJSON a
mkAttributes   (J.Object o) = mapM mkAttribute' (J.toList o)
mkAttributes _              = stop

mkElem :: J.Object -> Result (X.Xml X.Elem)
mkElem o = do
    t <- o J..: "tag"
    a <-      mkAttributes =<< o J..:? "attributes" J..!= J.Array mempty
    c <- mapM mkElem       =<< o J..:? "children"   J..!= []
    pure $ X.xelem t (X.xattrs a, X.xelems c)

docTopLevelElement :: J.Object -> Result (X.Xml X.Elem)
docTopLevelElement o = o J..: "elem" >>= mkElem

jsonToXml :: J.Value -> Result (X.Xml X.Doc)
jsonToXml = \case
    (J.Object o) -> do
        docTag o
        e <- docTopLevelElement o
        i <- docInfo o
        pure $ X.doc i e

    _ -> stop


-- TESTING:


sampleAeson :: BL.ByteString 
sampleAeson = [r|
        {
            "tag": "doc",
            "elem": {
                "tag": "foobar",
                "attributes": [{"name": "bla", "content": "alskjhdflkasjhdf"}],
                "children": [{"tag": "plz", "attributes": {"a": "b", "c": "d"}}]
            }
        }
    |]

eitherP :: (Monad m, MonadFail m) => Either String r -> m r
eitherP = either fail pure


testJsonToXml :: BL.ByteString -> IO B.ByteString
testJsonToXml s = case J.parse (\() -> eitherP (J.eitherDecode s) >>= jsonToXml) () of
    J.Error err -> print @String err >> pure "Oops!"
    J.Success xml -> let rendered = X.xrender @_ @B.ByteString xml
        in B.putStrLn rendered >> pure rendered


-- >>> testJsonToXml sampleAeson
-- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<foobar bla=\"alskjhdflkasjhdf\"\n></foobar\n>"

-- >>> main
-- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n<people\n><person age=\"32\"\n>Stefan</person\n><person age=\"4\"\n>Judith</person\n></people\n>"


main :: IO B.ByteString 
main = B.putStrLn rendered >> pure rendered
    where
    rendered = X.xrender @_ @B.ByteString xml
    people = [("Stefan", "32"), ("Judith", "4")]
    xml =
        X.doc X.defaultDocInfo $
            X.xelem "people" $
                X.xelems $ map (\(name, age) -> X.xelem "person" (X.xattr "age" age X.<#> X.xtext name)) people
