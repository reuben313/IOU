{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Payment where

import Import
import Data.List (head)
import Data.Maybe
import Util
import Data.Text (unpack,toLower)

import Handler.HandlerHelpers.PaymentHelper


getPaymentsR :: Handler Html
getPaymentsR = do field <- runDB $ selectList[] [Asc UserId]
                  sess <-   getSession
                  userid<- lookupSession "_ID"
                  records <- runDB $ do
                                         payments <- selectList [] [Desc PaymentTimestamp]
                                         users    <- selectList [] []
                                         return $ joinTables3 paymentFrom paymentTo payments users users

                 
                  let payments'=  map (\(x,_,_)-> x ) records
                  paAndRecords <- runDB $ do  p    <- selectList [] []
                                              return $ joinTables paymentReceiptuserline  payments' p
                  
                  let alltogether = zip records paAndRecords
    
   
                  defaultLayout
                        
                       [whamlet|
                            $maybe _ <- userid
                                    
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
                            $nothing
                                    <p>
                                        <a href=@{AuthR LoginR}>Go to the login page |]
                           
                           
                          
                                
                               
                                                  
                                                  
                                                  
               
               
   

              

postPaymentsR :: Handler Html
postPaymentsR =do error "wil not use"
    

    





getTotalvalueOfReceipt:: [Entity ReceiptUser] -> Maybe Double
getTotalvalueOfReceipt
  = foldr
      (\ a b ->
         if isNothing  b then Nothing else
           do x <- b
              let y = receiptUserAmount $ entityVal a
              return (x + y))
      (Just 0.0)
                            
                                 
                                                            
                                                          
                              
                                   
                                    
getPayPaymentsR:: ReceiptUserId-> Handler Html
getPayPaymentsR id  = do  userid<- lookupSession "_ID"
                       
                         
                          userReceipt<- runDB $ selectFirst [ReceiptUserId ==. id] []
                          let isval =  entityVal $ fromJust userReceipt
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
                             let content = case res of FormSuccess a -> Just a 
                                                       FormFailure t -> Nothing
                                                       FormMissing ->   Nothing 
                             paymentId<- runDB $ insert (fromJust content)
                                    
                             defaultLayout
                                      [whamlet|
                                              <p>#{show rec}
                                               <p>#{show debtorId}
                                              
                                               <p>Previous result: #{show content}     
                                                    
                                                              
                                                               
                                                            
                                                              
                                                              |]                                                                  


    