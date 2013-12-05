module Handler.HandlerHelpers.UserHelper where

import Import

data UserDto = UserDto {name:: Text}
      deriving(Show)

personForm :: Html -> MForm Handler (FormResult UserDto , Widget)
personForm = renderDivs $ UserDto <$> areq textField "Title" Nothing
