{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import
import Util 
 
  






getAllUsersR :: Handler Html
getAllUsersR = do 
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
                     receipts       <- runDB $  selectList [ReceiptRecieptUserId ==. userId] [Desc ReceiptTimestamp]
                     
                     
                     let filtersReceiptuser=   foldl (\x y-> (||.) x [ReceiptUserReceipt_Id==.(entityKey y)]  ) [] receipts
                     debtorReceipts   <- runDB $ do  lop  <- selectList filtersReceiptuser []
                                                     p    <- selectList [] []
                                                     return $   joinTables receiptUserDebtorId  lop p
                                                           
                                                      
                                                   
                     creditReceipts <- runDB $  selectList [ReceiptUserDebtorId ==. userId] []
                     paymentsDone   <- runDB $ selectList  ((||.)[PaymentFrom==. userId][PaymentTo==. userId])[Desc PaymentTimestamp]
                     let paymentSort = foldl (\ x@(a,b) ey@(Entity _ y)->  if paymentFrom y == userId then if paymentTo y == userId then(a++[ey],b++[ey]) else (a++[ey],b) else (a,b++[ey])   )  ([],[])  paymentsDone 
                     paymentsFrom   <- getAllPayementsWhere $  fst paymentSort
                     paymentsTo     <- getAllPayementsWhere $  snd paymentSort
                     userId'<-  lookupSession "_ID"
                     let userKey = convertToTextKey' userId'
                     let chek''' rux =  toString userKey ==  toString (unKey $ receiptUserDebtorId rux ) && receiptUserStatus rux /= "paid"
                     
                  
                     
                     
                     
                     
                     
                     defaultLayout
                           [whamlet|
                                            <p>Receipts  entered
                                             <table border="1">
                                              <th> Description
                                              <th> Amount 
                                              <th> View #{show (null  receipts)}
                                              
                                               $forall Entity rId receipt <- receipts
                                                 <tr>
                                                  <td> #{ receiptReceiptIdent receipt} 
                                                  <td> #{ receiptTotal_amount receipt}
                                                  <td> <a href=@{ReceiptR rId}>View
                                            
                                            
                                            
                                           
                                          <p> Creditors
                                            <table border="1">
                                             <th> Description
                                             <th> Amount
                                             <th> Status 
                                             <th> Action
                                              
                                               $forall Entity ruId receiptuser <- creditReceipts
                                                 <tr>
                                                  <td> #{ receiptUserReceipt_userIdent receiptuser} 
                                                  <td> #{ receiptUserAmount receiptuser}
                                                  <td> #{ receiptUserStatus receiptuser}
                                                 
                                                  <td> 
                                                       $if chek''' receiptuser
                                                         <a href=@{PayPaymentsR ruId}>pay my bill              
                                                       $else
                                                  
                                            
                                            
                                       
                                       
                                       
                                       
                                        <p> Debtors
                                          <table border="1">
                                             <th> Description
                                             <th> User
                                             <th> Amount 
                                             <th>Status
                                             <th> View Reciept
                                             
                                                    $if (null  receipts)
                                                       
                                                  
                                                    $else
                                                       $forall (Entity rId receiptU,Entity uId user) <- debtorReceipts
                                                                         <tr>
                                                                          <td>#{receiptUserReceipt_userIdent receiptU }
                                                                          <td>#{userIdent user }
                                                                          <td>#{receiptUserAmount receiptU}
                                                                          <td>#{receiptUserStatus receiptU}
                                                                          <td><a href=@{ReceiptR (receiptUserReceipt_Id receiptU)}> View 
                                    
                                            
                                         
                                          <p> payment from
                                          <table border="1">
                                             <th> Description
                                             <th> From
                                             <th> To 
                                             <th> Amount
                                             
                                             
                                              
                                               
                                                 $forall ((a,b,c),(d,e))  <-  paymentsTo                                 
                                                       <tr>
                                                       <td>#{paymentPaymentIdent (entityVal a)}
                                                       <td>#{userIdent (entityVal b)}
                                                       <td>#{userIdent (entityVal c)}
                                                       <td>#{receiptUserAmount (entityVal e)} 
                                                                                                                  
                                                                
                                       
                                       
                                         <p> payment to
                                          <table border="1">
                                             <th> Description
                                             <th> From
                                             <th> To 
                                             <th> Amount
                                             
                                              
                                               
                                                 $forall ((a,b,c),(d,e))  <- paymentsFrom                                
                                                       <tr>
                                                       <td>#{paymentPaymentIdent (entityVal a)}
                                                       <td>#{userIdent (entityVal b)}
                                                       <td>#{userIdent (entityVal c)}
                                                       <td>#{receiptUserAmount (entityVal e)}
               

                                                                                                             |]
    
    
    

        
