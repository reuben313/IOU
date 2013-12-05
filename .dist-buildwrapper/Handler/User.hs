{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import

import Data.Maybe 
  






getAllUsersR :: Handler Html
getAllUsersR = do sess <-   lookupSession "_ID"
                  field <- runDB $ selectList[] [Desc UserId]
                  
                 
                 
                
                 
                  
                 
               
                  defaultLayout
                                        [whamlet|
                                           
                                            <table border="1">
                                             <th>Email
                                              <tr>
                                                $forall Entity userid user <- field
                                                   <tr>
                                                    <td>#{userIdent user}
                                                       
                                                      
                                                       
                                                    
                                                      
                                                      |]
                                                                        
                                                                     
               
  

        
getUserR :: UserId -> Handler Html
getUserR userId =do  person         <- runDB $ get404 userId
                     receipts       <- runDB $  selectList [ReceiptRecieptUserId ==. userId] []
                     debtorReceipts <- runDB $  selectList [ReceiptUserDebtorId ==. userId,ReceiptUserStatus==. "unPaid"] []
                     paymentsDone   <- runDB $ selectList  [PaymentFrom==. userId][]
                     let o = unKey userId
                     defaultLayout
                           [whamlet|
                                            <table border="1">
                                             <th> Description
                                             <th> Amount 
                                             <th> Status
                                              <tr>
                                               $forall Entity rId receipt <- receipts
                                                <td> {# ReceiptReceiptIdent receipts} 
                                                <td> {# ReceiptTotal_amount receipts}
                                                <td> {# Rec}
                                            
                                            <ul>
                                               
                                                    <li>
                                                       #{show o}|]
                                                                        
    
               

    
    
    
    

        
