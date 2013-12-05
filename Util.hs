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