{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai.Middleware.RequestLogger
import           Prelude                              hiding (div, head)
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5                     hiding (html, main, style,
                                                       text)
import           Text.Blaze.Html5.Attributes          (charset, class_, href,
                                                       rel, src, style, type_)
import           Web.Scotty                           (get, html, middleware,
                                                       notFound, scotty)


render :: Html -> Html
render body = do
  websiteHead
  body
  websiteFooter


main :: IO ()
main = scotty 3000 $ do
  middleware logStdout

  get "/" $ do
    html . renderHtml $ render $ ""

  notFound $ do
    html . renderHtml $ do
      text "Error\nThere is no such route"


websiteHead :: Html
websiteHead = do
  head $ do
    meta ! charset "UTF-8"
    title "acorneroftheweb"
    link ! rel "stylesheet" ! href "https://unpkg.com/dracula-ui@latest/styles/dracula-ui.css" ! type_ "text/css"


websiteFooter :: Html
websiteFooter = do
  footer ! style "position: fixed; bottom: 10px;" ! class_ "drac-box drac-card drac-bg-purple drac-p-sm" $
    div ! class_ "drac-text drac-line-height drac-text-black" $ do
    "This website is created by Nor Führ, source code is on "
    a ! href "https://github.com/The1Penguin/acorneroftheweb" ! class_ "drac-text-pink--hover" $ "github"
    "."

index :: Html
index = undefined
