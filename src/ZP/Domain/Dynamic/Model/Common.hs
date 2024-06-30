module ZP.Domain.Dynamic.Model.Common
  (module X
  ) where

import ZP.Domain.Static.Model as X
  (DValue (..), TagName, CustomTag(..), Tag,
  IntTag, StringTag, BoolTag, PathTag, TagTag,
  EssenceTag, PairIntIntTag,
  DEssence, DEssencePath(..),
  tagToString, tagName, mkIntPairValue, mkIntValue)
