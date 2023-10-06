module ZP.Gloss.Types where

import ZP.Prelude

import ZP.Types

-- X is horizontal, grows to right
-- Y is vertical, grows to up
-- (-x, -y) is the left bottom corner
-- (0, 0) is in the center of the window

newtype GlossBaseShift  = GlossBaseShift (Float, Float)
newtype GlossCoords     = GlossCoords (Float, Float)
newtype GlossWindowSize = GlossWindowSize Coords
newtype GlossWindowPosition = GlossWindowPosition Coords
