{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}


import           Control.Applicative ((<$>), (<*>))
import qualified Web.ClientSession   as CS
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)
import Yesod

import Data.Default
import           Data.Text (Text)
import Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Text             (encodeToLazyText )
import Data.Text.Encoding 
import Data.Text.Lazy    as TL         (toStrict)

import Checkers

data Links = Links
instance Yesod Links where    
        makeSessionBackend _ = do
            backend <- defaultClientSessionBackend (60*40) "keyfile.aes"
            return $ Just backend
        
mkYesod "Links" [parseRoutes|
/ HomeR GET
/Board BoardR GET
/Moves MovesR GET
/ResetBoard ResetBoardR GET
/MakeMove/xf/#Int/yf/#Int/xt/#Int/yt/#Int MakeMoveR GET
|]

getHomeR :: Handler Html
getHomeR =  do
    currentBoard <- lookupSession "Game"
    case currentBoard of
        Nothing -> do 
            setSession "Game" (TL.toStrict $ encodeToLazyText $ toJSON $ getStartGame)
            defaultLayout  $(widgetFileNoReload def "homepage")
        Just board ->
           defaultLayout  $(widgetFileNoReload def "homepage")
     
	 
getMakeMoveR :: Int -> Int -> Int -> Int -> Handler Value
getMakeMoveR xf yf xt yt= do 
    let moveToMake = (Move (xf, yf) (xt, yt))
    currBoard <- lookupSession  "Game";
    case currBoard of
        Nothing -> do
            let newGame = preformMove getStartGame moveToMake
            setSession "Game" (TL.toStrict $ encodeToLazyText $ toJSON $  newGame)
            returnJson $ object ["winner" .= theWinner newGame,"winReason" .= winReason newGame, "turn" .= getPlayerTurn newGame]
        Just board -> do
            let Just x = (decode $ (BSL.fromStrict (encodeUtf8 (board))) :: Maybe GameJSON)
            let newGame =  preformMove (getGameFromGameJSON x) moveToMake
            setSession "Game" (TL.toStrict $ encodeToLazyText $ toJSON $  newGame)
            returnJson $ object ["winner" .= theWinner newGame,"winReason" .= winReason newGame, "turn" .= getPlayerTurn newGame]
      

getResetBoardR :: Handler Value
getResetBoardR = do 
    let newGame = getStartGame
    setSession "Game" (TL.toStrict $ encodeToLazyText $ toJSON $ newGame)
    returnJson $ object ["winner" .= theWinner newGame, "turn" .= getPlayerTurn newGame]
 

getBoardR :: Handler Value
getBoardR = do 
    currBoard <- lookupSession  "Game";
        case currBoard of
            Nothing -> 
                return $ toJSON (getBoardFromGame getStartGame)
            Just board -> do
                let Just x = (decode $ (BSL.fromStrict (encodeUtf8 (board))) :: Maybe GameJSON)
                return $ toJSON (getBoardFromGame  (getGameFromGameJSON x))
 

getMovesR :: Handler Value
getMovesR = do
    currBoard <- lookupSession  "Game";
        case currBoard of
            Nothing -> 
                return $ toJSON (getGameMoves getStartGame)
            Just board -> do
                let Just x = (decode $ (BSL.fromStrict (encodeUtf8 (board))) :: Maybe GameJSON)
                return $ toJSON (getGameMoves  (getGameFromGameJSON x))

-- getMakeMoveR :: Int -> Int -> Int -> Int -> Handler Value
-- getMakeMoveR xf yf xt yt= do 
            -- game = preformMove game (Move (xf, yf) (xt, yt))
            -- getInBoundR xf xt
       
-- getCurrentGame ::  Game -> Game ()
-- getCurrentGame = do {
    -- currBoard <- lookupSession  "Game";
    -- (case currBoard of
        -- Nothing -> 
            -- return getStartGame
        -- Just board -> 
            -- return getStartGame)
    -- }
            -- let Just x = (decode $ toStrict $ encodeUtf8 (currBoard) :: Maybe GameJSON)
            -- getGameFromGameJSON x
             
             
             

 
main = warp 3000 Links