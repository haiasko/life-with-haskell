module Handler.MyHome where

import Import
-- import Yesod.Markdown

getMyHomeR :: Handler Html
getMyHomeR = do
--    content <- liftIO $ fmap markdownToHtml (markdownFromFile "README.md")

--    defaultLayout $ toWidget content
    defaultLayout $(widgetFile "myhome")
