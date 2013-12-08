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
                      




receiptuserForm'' :: UserId-> Form TestAs
receiptuserForm''  uID = renderDivs $ TestAs
    <$> areq textField ( FieldSettings  {fsId= Just "myId",fsLabel= "hello"}) Nothing
 
                  where         
                                  users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                                                  , entityKey user           )) entities
                        
 
    


userAForm ::  AForm Handler TestAs
userAForm        =TestAs
                <$> ( areq (selectField users) "Model" Nothing)
                 where         
                                  users = do
                                  entities <- runDB (selectList [] [Asc UserIdent])
                                  optionsPairs $ map (\user -> ( userIdent $ entityVal user
                                                                  , userIdent $ entityVal user           )) entities
    
    