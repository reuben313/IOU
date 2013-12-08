module Handler.HandlerHelpers.UserHelper where

import Import
 




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

                      
                      
              
 