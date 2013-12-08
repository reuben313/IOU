{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Receipt where

import Import
import Data.List (head)
import Data.Maybe
import Data.Time
import Data.Text.Internal
import Util
import Handler.HandlerHelpers.UserHelper
import Handler.HandlerHelpers.ReceiptHelper
import Handler.HandlerHelpers.PaymentHelper




 

-- List all receipts
getAllReceiptsR :: Handler Html
getAllReceiptsR = do
                  userId<-  lookupSession "_ID"
                  let userKey = convertToTextKey' userId 
                  let userKey' =   Key{unKey = userKey}
                  (widget, enctype) <- generateFormPost (receiptForm userKey')
                
                  rep   <- runDB (selectList [] [Desc ReceiptTimestamp])
                  
                  
                  defaultLayout
                  
                                        [whamlet|
                                          
                    
                                           <h1>    
                                              $maybe _ <- userId
                                                <form id="receiptForm" method=post action=@{AllReceiptsR} enctype=#{enctype}>
                                                  ^{widget}
                                               <button onclick="submitForm()">Create an Receipt    
                                              $nothing
                                               
                                               
                                                 
                                            <table border="1">
                                               <tr>
                                                  <th>Receipt Description
                                                  <th>Time of Issue
                                                  <th>Amount
                                                  <th>View
                                                  
                                                  
                                                  
                                                  
                                                        
                                                             $forall Entity receiptUserid receiptUser <- rep                                           
                                                                <tr>
                                                                 <td>
                                                                     #{receiptReceiptIdent receiptUser}
                                                                     
                                                                 <td>
                                                                     #{show (receiptTimestamp receiptUser)}
                                                                 
                                                                 <td>
                                                                     #{receiptTotal_amount receiptUser} 
                                                                  
                                                                 <td>
                                                                     <a href=@{ReceiptR receiptUserid}>View           
                                                                     
                                                                     
                                                                                                             |]
 
postAllReceiptsR :: Handler Html
postAllReceiptsR = do     
                          userid<- lookupSession "_ID"
                          let userKey = convertToTextKey' userid 
                          let userKey' =   Key{unKey = userKey}
                          ((result, widget), enctype) <- runFormPost (receiptForm userKey')
                          
                         
                          case result of FormSuccess a    -> do receiptIde <- runDB $ insert a
                                                                redirect (ReceiptR receiptIde)
                                                             
                                         FormMissing      ->  redirect HomeR 
                                         FormFailure tx   ->  redirect AllReceiptsR
                          
                           
                                                                                                    
                                                    
                         
                                                                           
                        
                          
                          
            
                          
                        
                          
                          
                          
                         
                          
                       
   
    
    
                          

-- Display a receipt
getReceiptR :: ReceiptId -> Handler Html
getReceiptR receiptId = do  userId<-  lookupSession "_ID"
                            let userKey = convertToTextKey' userId
                            let userKey' =   Key{unKey = userKey}
                            vb<- runDB $ selectFirst [ReceiptId ==. receiptId ] []
                            let isEven = case vb of Just (Entity x y) -> case (receiptReceiptType y) of "even"     -> True 
                                                                                                        "not even" -> False
                                                    _                 -> False
                            debtors <- runDB $ selectList [ReceiptUserReceipt_Id ==. receiptId ] []
                            userInfo<- runDB $ selectFirst[UserId==.   (Key{unKey=userKey})] []
                            let userInfo' = userIdent $ entityVal $ fromJust userInfo 
                            (widget, enctype) <- generateFormPost (receiptuserForm receiptId  isEven )
                            let x= vb
                            let y = entityVal (fromJust x)
                            let userIde = receiptRecieptUserId y
                             
                                                      
                                               
                             
                            
                                
                                               
                         
                           
                            let chek = if toString userKey ==  toString (unKey  userIde ) then Just True else Nothing 
                            let chek' =if isPartOfReciept debtors userId then Just True else Nothing
                            let chek''' x =  toString userKey ==  toString (unKey  x ) 
                            
                         
                            defaultLayout
                                       [whamlet|
                                               
                                                $forall Entity receiptUserid receiptUser <- userInfo  
                                                 <table border="1">
                                                  <tr>
                                                    <td>Is created by #{ userInfo'}
                                                    <td>
                                                 $maybe _ <- chek  
                                                    $forall Entity receiptUserid receiptUser <- vb 
                                                      <form method=post action=@{ReceiptR receiptUserid} enctype=#{enctype}>
                                                        ^{widget}
                                                        <button>Create an Receipt 
                                                                    
                                                 $nothing     
                                                                     
                                              <table border="1">
                                               <tr>
                                                  <th>Transaction Description
                                                  <th>Amount
                                                  <th>Status
                                                  <th>Action
                                                     
                                                     $forall Entity receiptUserid receiptUser <- debtors                                         
                                                                 <tr>
                                                                  <td>
                                                                      #{receiptUserReceipt_userIdent receiptUser}
                                                                     
                                                                  <td>
                                                                      #{receiptUserAmount receiptUser}
                                                                  
                                                                  <td>
                                                                      #{receiptUserStatus receiptUser} 
                                                                 
                                                                   <td> 
                                                                         $if chek''' (receiptUserDebtorId receiptUser)
                                                                           <a href=@{PayPaymentsR receiptUserid}>pay my Bill
                                                                         $else
                                                                       
                                                                          
                                                                          
                                                        
                                                                          
                                                  |]
                                                  
                                                  
                                                                  
                                                                     
                                                                     
           
                                                                        {- $maybe _ <- chek'
                                                                          <a href=@{PayPaymentsR receiptUserid}>pay my Bill 
                                                                         $nothing                                               
                                                      -}
                                                       

postReceiptR :: ReceiptId -> Handler Html
postReceiptR receiptId = do   
                            
                            -- user <- runDB $ selectFirst[UserIdent ==. debtorId content] []
                           --  let x = user
                          --   let key  = entityKey (fromJust x)
                             userid<- lookupSession "_ID"
                             let userKey = convertToTextKey' userid
                             let userKey' =   Key{unKey = userKey}
                             resceipt' <- runDB $ get404 receiptId
                             receiptsusers' <- runDB $ selectList [ReceiptUserReceipt_Id==. receiptId ][]
                             let isEven = case resceipt' of    y -> case (receiptReceiptType y) of "even"     -> True 
                                                                                                   "not even" -> False
                                                               _                  -> False
                             ((result, widget), enctype) <- runFormPost (receiptuserForm receiptId isEven) 
                             let result' = case result of FormSuccess a -> Just a
                                                          FormFailure t -> Nothing
                                                          FormMissing   -> Nothing 
                            
                             case result' of Nothing -> do  redirect HomeR
                                             Just fs -> case (receiptReceiptType resceipt') of "not even" ->  do  runDB $ insert  fs
                                                                                                                  runDB $ update receiptId [ReceiptTotal_amount +=. (receiptUserAmount fs)]
                                                                                                                  redirect (ReceiptR receiptId)
                                                                                                                  
                                                                                               "even"    ->  do  rsId <-runDB $ insert  fs
                                                                                                                 let updatevalues' =  batchUpdateValues receiptsusers' (receiptTotal_amount resceipt' )  
                                                                                                                 case rsId of x' -> do  runDB $ updateWhere [ReceiptUserReceipt_Id==. receiptId] [ReceiptUserAmount=.  updatevalues']
                                                                                                                              _  -> redirect (ReceiptR receiptId)  
                                                                                                                 --runDB $ updateWhere [ReceiptUserReceipt_Id==. receiptId] [ReceiptUserAmount=.  updatevalues']
                                                                                                                 redirect (ReceiptR receiptId)      
                                             
                                             
                                             
                                            
                                                                    
                             
                                     
                             
                                                           

batchUpdateValues::[Entity ReceiptUser]-> Double ->Double
batchUpdateValues users totalAmount =devide
                                   
                                    where devide     = splitEvenly (length  users)  totalAmount  
                                          
                                               



                            
splitEvenly:: Int  -> Double -> Double
splitEvenly factor totalvalue = (totalvalue /fromIntegral (factor+1)   )::Double

                                                      
                          