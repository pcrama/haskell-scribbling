{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Home (homeView, newReadingInputFormView, readingsView) where

import           Client.CSS                  (layoutCss)
import           Control.Monad               (forM_)
import           Data.Text.Lazy              (toStrict)
import           Data.Time                   (Year, MonthOfYear, DayOfMonth)
import           Prelude                     hiding (div, head, id)
import           Text.Blaze.Html             (Html, toHtml, toValue)
import           Text.Blaze.Html5            (a, body, button,
                                              dataAttribute, div, docTypeHtml,
                                              form, h1, head, input, label,
                                              li, link, meta, option, p, script, style,
                                              title, ul, (!), select, table, tbody, td, th, thead, tr)
import           Text.Blaze.Html5.Attributes (action, charset, class_, content, href,
                                              httpEquiv, for, id, media, method,
                                              name, placeholder, rel, src, type_, value)
import           Views.Utils                 (blaze, pet)
import           Web.Scotty                  (ActionM)
import           Lib.SimpleReading           (SimpleReading(..), ReadingMeter(..))

layout :: Html -> Html -> Html
layout t b = docTypeHtml $ do
           pet "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
           pet "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
           pet "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
           pet "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
           head $ do
             title t
             meta ! charset "utf-8"
             meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
             meta ! name "description" ! content "Inspire Text"
             meta ! name "viewport" ! content "width=device-width"
             link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! rel  "stylesheet" ! media "screen"
             style $ pet $ toStrict layoutCss
           body $ do
             navBar >> b
             script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
             script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

homeView :: ActionM ()
homeView = blaze $ layout "home" $ do
             div ! class_ "container" $ do
               div ! class_ "jumbotron" $ do
                 h1 "Scotty Starter"
                 p "Welcome to the Scotty Starter template, equipped with Twitter Bootstrap 3.0 and HTML5 boilerplate"
                 p $ do a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "#navbar" $ "Facebook"
                        a ! class_ "btn btn-lg btn-danger" ! id "gmail" ! href "#navbar" $ "Gmail"

navBar :: Html
navBar = div ! class_ "navbar navbar-default navbar-static-top" $ div ! class_ "container" $ do
           div ! class_ "navbar-header" $ do
             button ! type_ "button"
                    ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
               a ! class_ "navbar-brand" ! href "#" $ "λ"
           div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav" $ do
             li ! class_ "active" $ a ! href "#" $ "Home"
             li $ a ! href "#about" $ "About"
             li $ a ! href "#contact" $ "Contact"


readingsView :: [SimpleReading] -> ActionM ()
readingsView readings = blaze $ layout "Readings" $ do
  div ! class_ "container" $ do
    readingsTable readings
  where readingsTable [] = "No meter readings yet"
        readingsTable rs = table $ do
          thead $ tr $ th "When" >> th "What" >> th "How much"
          tbody $ do
            forM_ rs $ \SimpleReading { readingTime, readingMeter, readingValue } -> tr $ do
              th (toHtml $ show readingTime) >> td (toHtml $ show readingMeter) >> td (toHtml $ show readingValue)


newReadingInputFormView :: Year -> MonthOfYear -> DayOfMonth -> ActionM ()
newReadingInputFormView year month day = blaze $ layout "New Reading" $ do
  div ! class_ "container" $ do
    form ! method "POST" ! action "/readings/" $ do
      div ! class_ "form-row" $ do
        label ! for "year" $ "Year"
        input `idname` "year" ! ivalue year ! type_ "number"
        label ! for "month" $ "Month"
        input `idname` "month" ! ivalue month ! type_ "number"
        label ! for "day" $ "Day"
        input `idname` "day" ! ivalue day ! type_ "number"
      div ! class_ "form-row" $ do
        label ! for "meter" $ "Meter"
        select `idname` "meter" $ do
          forM_ [minBound..maxBound] meterIdOption
      div ! class_ "form-row" $ do
        label ! for "value" $ "Value"
        input `idname` "value" ! value "0" ! type_ "number"  -- TODO: can't enter a float in my browser?
      div ! class_ "form-row" $ do
        label ! for "comment" $ "Comment"
        input `idname` "comment" ! placeholder "Type anything you want here, it will be ignored"
      div ! class_ "form-row" $ do
        input ! type_ "submit" ! value "Submit"
  where x `idname` s = x ! id s ! name s
        ivalue x = value . toValue $ x
        meterName Pv2022 = "Panneaux placés en 2022"
        meterName Pv2013 = "Panneaux placés en 2013"
        meterName ElectricConsumption = "Compteur Électricité"
        meterName GazConsumption = "Compteur Gaz"
        meterName WaterConsumption = "Compteur Eau"
        meterIdOption mi = option ! ivalue (show mi) $ meterName mi
