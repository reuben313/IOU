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


receiptForm :: Form Receipt
receiptForm = renderDivs $ error "receiptForm not implemented yet!"

{-
receiptUserForm :: ReceiptId -> Form ReceiptUser
receiptUserForm receiptId = renderDivs $ ReceiptUser
                            <$> areq textField "Title" Nothing
                            <*> pure receiptId
                            <*> areq textField "Contents" Nothing
                            <*> users
                            <*> areq doubleField "Title" Nothing
                            <*> areq textField "Title" Nothing
                            
                            
 
                            where
                                -- Retrieve a list of users from the database that we can use
                                -- to populate a list selection box.
                                users = do
                                    entities <- runDB (selectList [] [Asc UserIdent])
                                    optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                                                 , entityKey user             )) entities
 -}
 

-- List all receipts
getAllReceiptsR :: Handler Html
getAllReceiptsR = do
                  userId<-  lookupSession "_ID"
                  (widget, enctype) <- generateFormPost personForm
                
                  rep   <- runDB (selectList [] [Desc ReceiptTimestamp])
                  
                  
                  defaultLayout
                                        [whamlet|
                                           <h1>    
                                              $maybe _ <- userId
                                                <form method=post action=@{AllReceiptsR} enctype=#{enctype}>
                                                  ^{widget}
                                                   <button>Create an Receipt
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
postAllReceiptsR = do     time <- liftIO getCurrentTime
                          userid<- lookupSession "_ID"
                          ((result, widget), enctype) <- runFormPost personForm
                        
                          let userKey = convertToTextKey' userid 
                          let userKey' =   Key{unKey = userKey}
                          let content = case result of FormSuccess a -> a
                          receiptIde <- runDB $ insert $ Receipt (name content) userKey'   time 0.0 
                                                                           
                        
                          
                          
                         
                          
                        
                          
                          
                          
                         
                          
                       
   
    
    
                          defaultLayout
                                
                               [whamlet|
                                                                         
                                        $maybe _ <- userid
                                            
                                            
                                             <a href=/1>Go to the login page 
                                                
                                        $nothing
                                            <p>
                                                <a href=@{AuthR LoginR}>Go to the login page |]

-- Display a receipt
getReceiptR :: ReceiptId -> Handler Html
getReceiptR receiptId = do  userId<-  lookupSession "_ID"
                            let userKey = convertToTextKey' userId
                            vb<- runDB $ selectFirst [ReceiptId ==. receiptId ] []
                            debtors <- runDB $ selectList [ReceiptUserReceipt_Id ==. receiptId ] []
                            userInfo<- runDB $ selectFirst[UserId==.   (Key{unKey=userKey})] []
                            (widget, enctype) <- generateFormPost receiptuserForm
                            let x= vb
                            let y = entityVal (fromJust x)
                            let userIde = receiptRecieptUserId y
                             
                                                      
                                               
                             
                            
                                
                                               
                         
                           
                            let chek = if toString userKey ==  toString (unKey  userIde ) then Just True else Nothing 
                            let chek' =if isPartOfReciept debtors userId then Just True else Nothing
                            
                            
                         
                            defaultLayout
                                       [whamlet|
                                               
                                                $forall Entity receiptUserid receiptUser <- userInfo  
                                                 <table border="1">
                                                  <tr>
                                                    <td>Is created by #{userIdent receiptUser}
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
                                                                         $maybe _ <- chek'
                                                                          <a href=@{PayPaymentsR receiptUserid}>pay my Bill 
                                                                         $nothing     
                                                        
                                                                          
                                                  |]
                                                  
                                                  
                                                                  
                                                                     
                                                                     
                                                    
                                                      
                                                       

postReceiptR :: ReceiptId -> Handler Html
postReceiptR receiptId = do  ((result, widget), enctype) <- runFormPost receiptuserForm 
                             let content = case result of FormSuccess a -> a
                             user <- runDB $ selectFirst[UserIdent ==. billedTo content] []
                             let x = user
                             let key  = entityKey (fromJust x)
                             userid<- lookupSession "_ID"
                             let userKey = convertToTextKey' userid
                             receptId <-case user of Just a -> (runDB $ insert $ ReceiptUser (  receipt_userIdent content ) receiptId  key (amount content) "unpaid") 
                             upd<-runDB $  update receiptId [ReceiptTotal_amount +=. (amount content)]
                             defaultLayout
                                
                               [whamlet|
                                       
                                         <a href=@{AllReceiptsR}>Back to Receipt                                 
                                        
                                         |]
