module ZP.TestData.GraphBuilder where

import ZP.Prelude
import ZP.Types
import ZP.Game.Types
import ZP.Game.Logic
import ZP.AI.Types
import ZP.AI.StaticKnowledge
import ZP.TestData.KnowledgeBase.Essences
import ZP.TestData.KnowledgeBase.Common

import qualified Data.Map as Map
import qualified Data.Set as Set


buildStaticPropertiesByType
  :: (PropertyType, [StaticProperty])
  -> STM (PropertyType, [String], [String])
buildStaticPropertiesByType (pType, props) = do
  strs :: [(String, [String])] <- mapM buildStaticPropertyNode props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)



buildValueNode :: String -> PropertyValue -> STM [String]
buildValueNode parentName val = do
  let toValueNodeArrStyle = " [arrowhead=dot];"
  case val of
    NoValue -> pure []
    PairValue v1 v2 -> do
      let chName1 = parentName <> "-pair1"
      let chName2 = parentName <> "-pair2"
      lStrs <- buildValueNode chName1 v1
      rStrs <- buildValueNode chName2 v2
      pure $
        [ parentName <> " -> " <> chName1 <> toValueNodeArrStyle
        , parentName <> " -> " <> chName1 <> toValueNodeArrStyle
        ] <> lStrs <> rStrs
    IntValue v -> do
      let chName = parentName <> "-int: " <> show v
      pure [parentName <> " -> " <> chName <> toValueNodeArrStyle]
    EssenceValue d (Essence e) -> do
      let chName = parentName <> "-esscence: " <> e <> "-" <> d
      pure [parentName <> " -> " <> chName <> toValueNodeArrStyle]
    StaticPropertyValue sProp -> do
      let Essence ess = essence sProp
      let chName = parentName <> "-sprop: " <> ess
      pure [parentName <> " -> " <> chName <> toValueNodeArrStyle]
    TargetValue v -> do
      let chName = parentName <> "-target"
      tStrs <- buildValueNode chName v
      pure $
        [ parentName <> " -> " <> chName <> toValueNodeArrStyle
        ] <> tStrs
    _ -> error " values not implemented"


buildStaticPropertyNode :: StaticProperty -> STM (String, [String])
buildStaticPropertyNode StaticProperty{staticPropertyId, staticPropertyValue, essence, staticProperties} = do
  let Essence ess = essence
  let StaticPropertyId sPId = staticPropertyId
  let name = "\"S:" <> show sPId <> ":" <> ess <>"\""
  let nodeStyle = "node [shape=Mcircle];"

  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM buildStaticPropertiesByType $ Map.toList staticProperties

  valStrings <- buildValueNode name staticPropertyValue

  let xs = join $ map (f name) propsByTypeStrings
  pure (name, [nodeStyle] <> [""] <> xs <> [""] <> valStrings)
  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = thisNodeName <> " -> " <> targetName <> " [label=\"" <> pType <> "\"];"


buildPropertiesByType
  :: (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [String], [String])
buildPropertiesByType (pType, psVar) = do
  props <- readTVar psVar
  strs :: [(String, [String])] <- mapM buildActivePropertyNode props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildActivePropertyNode :: ActiveProperty -> STM (String, [String])
buildActivePropertyNode ActiveProperty{..} = do
  (sPropName, sPropStrings) <- buildStaticPropertyNode staticProperty

  let nodeStyle = "node [shape=circle];"
  let toSPropArrStyle  = " [arrowhead=onormal];"

  let StaticProperty {essence} = staticProperty
  let Essence ess = essence

  let ActivePropertyId propId = activePropertyId
  let name = "\"A:" <> show propId <> ":" <> ess <>"\""

  let toOwnStaticProp = name <> " -> " <> sPropName <> toSPropArrStyle

  props <- readTVar propertiesVar
  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM buildPropertiesByType $ Map.toList props

  let xs = join $ map (f name) propsByTypeStrings

  pure (name, sPropStrings <> [""] <> [nodeStyle, toOwnStaticProp] <> [""] <> xs)

  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = thisNodeName <> " -> " <> targetName <> " [label=\"" <> pType <> "\"];"


buildActingObjectNode :: (ActingObjectId, ActingObject) -> STM [String]
buildActingObjectNode (_, ActingObject {..}) = do
  (rootPropNode, strings) <- buildActivePropertyNode rootProperty

  let nodeStyle = "node [shape=box];"
  let arrStyle  = " [arrowhead=onormal];"
  let ActingObjectId oId = actingObjectId
  let ActingObjectName aName = actingObjectName
  let row = "\"AO:" <> show oId <> ":" <> aName <> "\" -> " <> rootPropNode <> arrStyle

  pure $ strings <> [""] <> [ nodeStyle, row ]

buildGraph :: ZPNet -> STM [String]
buildGraph ZPNet{actingObjects} = do
  let start =
       [ "digraph finite_state_machine {"
       , "graph [ dpi = 600 ];"
       , "size=\"8,5\";"
       ]
  let end = [ "}" ]

  middles <- mapM buildActingObjectNode $ Map.toList actingObjects

  pure $ start <> join middles <> end
