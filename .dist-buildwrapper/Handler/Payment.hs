{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Payment where

import Import
import Data.Maybe
import Util
import Data.Text (unpack,toLower)

import Handler.HandlerHelpers.PaymentHelper


getPaymentsR :: Handler Html
getPaymentsR = do yu<- getAllPayements
                  let alltogether = yu
                  
                  defaultLayout
                        
                       [whamlet|
                          
                                    
                                   <table border="1">
                                    <tr>
                                     <th>payment Description
                                     <th>From
                                     <th>To
                                     <th>amount
                                      
                                      $forall ((a,b,c),(d,e))  <- alltogether                                 
                                       <tr>
                                       <td>#{paymentPaymentIdent (entityVal a)}
                                       <td>#{userIdent (entityVal b)}
                                       <td>#{userIdent (entityVal c)}
                                       <td>#{receiptUserAmount (entityVal e)}  
                                                                                |] 
    
   
                 
                           
                           
                          
                                
                               
                                                  
                                                  
                                                  
               
               
   

              

postPaymentsR :: Handler Html
postPaymentsR =do error "wil not use"
    

    




          
                                 
                                                            
                                                          
                              
                                   
                                    
getPayPaymentsR:: ReceiptUserId-> Handler Html
getPayPaymentsR id  = do  userid<- lookupSession "_ID"
                          let userKey = convertToTextKey' userid 
                          let userKey' =   Key{unKey = userKey}
                         
                          userReceipt<- runDB $ selectFirst [ReceiptUserId ==. id,ReceiptUserDebtorId==.userKey'] []
                          case userReceipt of Nothing           -> redirect HomeR
                                              Just (Entity x y) -> do let isval =  entityVal $ fromJust userReceipt
                                                                      let uR =receiptUserReceipt_Id $  entityVal $ fromJust userReceipt
                                                                      receipt <-    runDB $ selectFirst [ReceiptId ==. uR] [] 
                                                                      let rec = receiptRecieptUserId $  entityVal $ fromJust receipt
                                                                      let debtorId=receiptUserDebtorId $  entityVal $ fromJust userReceipt
                                                                      let bla = case ( unpack (toLower $ receiptUserStatus isval) ) of "unpaid" -> Nothing
                                                                                                                                       "paid"  ->  Just True
                                                                          
                                                                      (res, enctypee)  <- generateFormPost  (form' rec debtorId id) 
                                                                      defaultLayout
                                                                        [whamlet|
                                                                                   
                                                                                    <table border="1">
                                                                                     
                                                                                      <tr>
                                                                                        $maybe _ <- bla
                                                                                            <p>THIS TRANSACTION HAS ALREADY TOOK PLACE  
                                                                                             
                                                                                        $nothing
                                                                                              
                                                                                               
                                                                                            <form method=post action=@{PayPaymentsR id} enctype=#{enctypee}>
                                                                                               ^{res}
                                                                                                 <button>Pay
                                                                                              
                                                                                              |] 
                         
                                              
                          
                                                          
                                                           
                       
                                                  
                                                                     
postPayPaymentsR :: ReceiptUserId -> Handler Html 
postPayPaymentsR id=do       userid <- lookupSession "_ID"
                             let   userKey = convertToTextKey' userid
               
                             userReceipt<- runDB $ selectFirst [ReceiptUserId ==. id] []
                             let uR =receiptUserReceipt_Id $  entityVal $ fromJust userReceipt
                             receipt <-    runDB $ selectFirst [ReceiptId ==. uR] [] 
                             let rec = receiptRecieptUserId $  entityVal $ fromJust receipt
                             let debtorId=receiptUserDebtorId $  entityVal $ fromJust userReceipt
                             let receiptide =  receiptUserReceipt_Id $  entityVal $ fromJust userReceipt
                             ((res, widget), enctypee) <- runFormPost  (form' rec debtorId id ) 
                             case res of FormSuccess a -> do runDB $ insert a
                                                             runDB $ update id [ReceiptUserStatus=."paid"]
                                                             redirect HomeR
                                         FormFailure t ->    redirect HomeR
                                         FormMissing   ->    redirect HomeR
                            
                             
                                    
                               
                                                    
                                                              
                                                               
                                                            
                                                              
                                                                                                                             


    