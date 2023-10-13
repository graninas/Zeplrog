module ZP.Domain.Dynamic.Model.Object where

import ZP.Prelude

import ZP.Domain.Dynamic.Model.Common
import ZP.Domain.Dynamic.Model.Property

import qualified Data.Map as Map


-- Objects are active properties with extra game info.

-- | Object ID
newtype ObjectId = ObjectId Int

-- | Active object. Always based on some top-level property.
data Object = Obj
  { objectId   :: ObjectId
  -- ^ Object ID
  , objectProp :: Property
  -- ^ Dynamic property of the object
  }
