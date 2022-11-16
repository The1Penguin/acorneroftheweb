{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai.Middleware.RequestLogger
import           Prelude                              hiding (div, head)
import           Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html4.FrameSet            (center)
import           Text.Blaze.Html5                     hiding (html, main, param,
                                                       style, text)
import           Text.Blaze.Html5.Attributes          (action, charset, class_,
                                                       hidden, href, method,
                                                       name, placeholder, rel,
                                                       src, style, type_)
import           Web.Scotty                           (get, html, middleware,
                                                       notFound, param, scotty)


render :: Html -> Html
render content = do
  websiteHead
  content
  websiteFooter


main :: IO ()
main = scotty 3000 $ do
  middleware logStdout

  get "/" $ do
    html . renderHtml $
      render $ do
        body ! class_ "drac-bg-black" $ center  index

  notFound $ do
    html . renderHtml $ do
      p "Error"
      p "There is no such route"


websiteHead :: Html
websiteHead = do
  head $ do
    meta ! charset "UTF-8"
    title "acorneroftheweb"
    link ! rel "stylesheet" !
      href "https://unpkg.com/dracula-ui@latest/styles/dracula-ui.css" !
      type_ "text/css"


websiteFooter :: Html
websiteFooter = do
  footer ! style "position: fixed; bottom: 10px;" !
    class_ "drac-box drac-card drac-bg-purple drac-p-sm" $
    div ! class_ "drac-text drac-line-height drac-text-black" $ do
    "This website is created by Nor Führ, source code is on "
    a !
      href "https://github.com/The1Penguin/acorneroftheweb" !
      class_ "drac-anchor drac-text drac-text-black drac-text-pink--hover" $
      "github"
    "."

index :: Html
index  = do
  header ! class_ "drac-btn drac-bg-purple drac-btn-lg drac-m-sm" $
    a ! href "/" ! class_ "drac-anchor drac-anchor drac-text drac-text-black drac-text-pink--hover" $ "A corner of the web"
  p ! class_ "drac-text drac-line-height drac-text-white" $ "Hello and welcome to this corner of the web."
  embed ! src "https://github-readme-stats.vercel.app/api?username=The1Penguin&show_icons=true&theme=dracula"
