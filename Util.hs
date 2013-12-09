{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Util where

import Import
import Prelude
import Data.Maybe
import Data.Text.Internal

--convert Text to Int value
getUserId ::  Maybe Text -> Maybe Int
getUserId mT  =  case mT of
                    Just x -> Just (read  (showText x))
                    Nothing -> Nothing  


getUserId' ::  Key User
getUserId'  =   Key{unKey = PersistInt64 1 }             
     


convertToTextKey' :: Maybe Text -> PersistValue
convertToTextKey' mI= case mI of 
                     Just x  ->  PersistText x
                     Nothing ->  PersistText ""
                     
isPartOfReciept:: [Entity ReceiptUser] -> Maybe Text -> Bool
isPartOfReciept em (Just t) = length list > 0
                              where list= filter (\x->  toString (unKey (receiptUserDebtorId (entityVal x))) == show t)     em


toString:: PersistValue->String  
toString (PersistText  t)= show t    
toString (PersistInt64 t')= "\""++(show t')++ "\""   




getAllPayements:: Handler [((Entity Payment, Entity User, Entity User), (Entity Payment, Entity ReceiptUser))]
getAllPayements=do records <- runDB $ do
                                         payments <- selectList [] [Desc PaymentTimestamp]
                                         users    <- selectList [] []
                                         return $ joinTables3 paymentFrom paymentTo payments users users

                 
                   let payments'=  map (\(x,_,_)-> x ) records
                   paAndRecords <- runDB $ do  p    <- selectList [] []
                                               return $ joinTables paymentReceiptuserline  payments' p                  
                   
                   let alltogether = zip records paAndRecords
                                      
                   return alltogether

getUsers::[Filter User]-> [SelectOpt User]-> Handler [Entity User] 
getUsers a b = do runDB $ selectList a b                                                  

getAllPayementsWhere::[Entity Payment] -> Handler [((Entity Payment, Entity User, Entity User), (Entity Payment, Entity ReceiptUser))]
getAllPayementsWhere eP=do records <- runDB $ do
                                         
                                         users    <- selectList [] []
                                         return $ joinTables3 paymentFrom paymentTo eP users users

                 
                           let payments'=  map (\(x,_,_)-> x ) records
                           paAndRecords <- runDB $ do  p    <- selectList [] []
                                                       return $ joinTables paymentReceiptuserline  payments' p                  
                   
                           let alltogether = zip records paAndRecords
                                      
                           return alltogether                                                        
                                           