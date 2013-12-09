module Handler.HandlerHelpers.ReceiptHelper where

import Import



                        
 

receiptuserForm :: ReceiptId ->Bool -> Form ReceiptUser
receiptuserForm rID isEven = renderDivs $ ReceiptUser
    <$> areq textField "Receipt Description" Nothing
    <*> pure rID
    <*> areq (selectField users)  "billedTo" Nothing
    <*> assignCorrectField isEven
    <*> pure "unpaid"
 
                  where users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> (    userIdent $ entityVal user
                                                                  ,  entityKey user            )) entities
                        
                        assignCorrectField v = if v then pure 0.0 else areq doubleField "Amount" Nothing
                      






    
 
receiptForm:: UserId-> Form Receipt
receiptForm uId = renderDivs $ Receipt
              <$>areq textField "Title" Nothing
              <*> pure uId
              <*> lift (liftIO getCurrentTime)
              <*> areq amountChek (FieldSettings  {fsId= Just "receiptfield",fsLabel= "Total Amount",fsName=Just "f3",fsTooltip=Just "",fsAttrs=[]}) (Just 0.0)
              <*> areq (selectFieldList options)  (FieldSettings  {fsId= Just "receipType",fsLabel= "Reciept Type",fsName=Just "f4",fsTooltip=Just "",fsAttrs=[("onchange","processAmount()")]}) Nothing
              
              
                where options:: [(Text, Text)]
                      options = [("even","even"),("not even","not even")]
                      
                      errorMessage :: Text
                      errorMessage = "negative Amount is not permited "

                      amountChek = checkBool (>= 0.0) errorMessage  doubleField    