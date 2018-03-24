module Data.Zya.Utils.ComponentDetails
  (
    ComponentName(..)
  )
 where 

import Data.Text 
class ComponentName a where 
  componentName :: a -> Text