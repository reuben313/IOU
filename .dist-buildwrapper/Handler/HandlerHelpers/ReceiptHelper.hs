module Handler.HandlerHelpers.ReceiptHelper where

import Import


data ReceiptUserDTO = ReceiptUserDTO  { receipt_userIdent:: Text
                                        ,amount :: Double,
                                        billedTo :: Text
                                        
                                        
                                      }deriving(Show)






receiptuserForm :: Html -> MForm Handler (FormResult ReceiptUserDTO, Widget)
receiptuserForm = renderDivs $ ReceiptUserDTO
    <$> areq textField "Receipt Description" Nothing
    <*> areq doubleField "Amount" Nothing
    <*> areq (selectField users)  "billedTo" Nothing
 
                  where users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                                                  , userIdent $ entityVal user             )) entities
                        
 

receiptuserForm' :: ReceiptId ->UserId -> Form ReceiptUser
receiptuserForm' rID uID = renderDivs $ ReceiptUser
    <$> areq textField "Receipt Description" Nothing
    <*> pure rID
    <*> areq (selectField users)  "billedTo" Nothing
    <*> areq doubleField "Amount" Nothing
    <*> pure "unpaid"
 
                  where users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> (    userIdent $ entityVal user
                                                                  ,  entityKey user            )) entities
                        
                        selectme :: [(Text, UserId)]
                        selectme = undefined




receiptuserForm'' :: UserId-> Form TestAs
receiptuserForm''  uID = renderDivs $ TestAs
    <$> pure "HAH"
 
                  where         
                                  users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                                                  , entityKey user           )) entities
                        
    {-
    receipt_userIdent Text
    receipt_Id ReceiptId 
    debtorId UserId 
    amount  Double
    status  Text
    deriving Show
    
    
    -}
    


userAForm ::  AForm Handler TestAs
userAForm        =TestAs
                <$> ( areq (selectField users) "Model" Nothing)
                 where         
                                  users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                                                  , userIdent $ entityVal user           )) entities
    
    