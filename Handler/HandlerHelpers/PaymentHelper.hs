module Handler.HandlerHelpers.PaymentHelper where

import Import

data PaymentDTO = PaymentDTO {paymentDesc::Text,from::UserId,to::UserId,receiptLine::ReceiptUserId,payementDate::UTCTime}
                               deriving(Show)

data LightPaymentDTO = LightPaymentDTO {paymentDesc' ::Text}
    


  
   
form :: UserId->UserId -> ReceiptUserId -> Form PaymentDTO
form fromm too recLine  = renderDivs $ PaymentDTO 
                              <$> areq textField "Title" Nothing
                              <*> pure fromm
                              <*> pure too
                              <*> pure recLine
                              <*> lift (liftIO getCurrentTime)
                              
                              
                               


paymentForm :: Html -> MForm Handler (FormResult LightPaymentDTO, Widget)
paymentForm = renderDivs $ LightPaymentDTO
                        <$> areq textField "Receipt Description" Nothing
    

form' :: UserId->UserId -> ReceiptUserId -> Form Payment
form' fromm too recLine  = renderDivs $ Payment 
                              <$> areq textField "Title" Nothing
                              <*> lift (liftIO getCurrentTime)
                              <*> pure fromm
                              <*> pure too
                              <*> pure recLine
                              
                              
                        