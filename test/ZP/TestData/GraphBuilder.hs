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


quoted str = "\"" <> str <> "\""

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
        [ quoted parentName <> " -> " <> quoted chName1 <> toValueNodeArrStyle
        , quoted parentName <> " -> " <> quoted chName1 <> toValueNodeArrStyle
        ] <> lStrs <> rStrs
    IntValue v -> do
      let chName = parentName <> "-int: " <> show v
      pure [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle]
    PositionValue pos -> do
      let chName = parentName <> "-pos: " <> show pos
      pure [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle]
    EssenceValue d (Essence e) -> do
      let chName = parentName <> "-esscence: " <> e <> "-" <> d
      pure [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle]
    StaticPropertyValue sProp -> do
      let Essence ess = essence sProp
      let StaticPropertyId sPId = staticPropertyId sProp
      let sPropName = "S:" <> show sPId <> ":" <> ess
      pure [ quoted parentName <> " -> " <> quoted sPropName <> toValueNodeArrStyle]
    TargetValue v -> do
      let chName = parentName <> "-target"
      tStrs <- buildValueNode chName v
      pure $
        [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle
        ] <> tStrs
    _ -> error " values not implemented"

buildStaticPropertiesByType
  :: (PropertyType, [StaticProperty])
  -> STM (PropertyType, [String], [String])
buildStaticPropertiesByType (pType, props) = do
  strs :: [(String, [String])] <- mapM buildStaticPropertyNode props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildStaticPropertyNode :: StaticProperty -> STM (String, [String])
buildStaticPropertyNode StaticProperty{staticPropertyId, staticPropertyValue, essence, staticProperties} = do
  let Essence ess = essence
  let StaticPropertyId sPId = staticPropertyId
  let thisNodeName = "S:" <> show sPId <> ":" <> ess

  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM buildStaticPropertiesByType $ Map.toList staticProperties

  thisNodeValues <- buildValueNode thisNodeName staticPropertyValue

  let thisNodeChildrenProps = filter (not . null) $ join $ map (f thisNodeName) propsByTypeStrings
  let rootProps             = filter (not . null) $ join $ map (\(_, _, xs) -> xs) propsByTypeStrings

  pure (thisNodeName,
    rootProps
    <> [""]
    <> thisNodeChildrenProps
    <> [""]
    <> thisNodeValues
    )
  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


buildActivePropertiesByType
  :: (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [String], [String])
buildActivePropertiesByType (pType, psVar) = do
  props <- readTVar psVar
  strs :: [(String, [String])] <- mapM buildActivePropertyNode props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildActivePropertyNode :: ActiveProperty -> STM (String, [String])
buildActivePropertyNode ActiveProperty{..} = do
  let toSPropArrStyle  = " [arrowhead=onormal];"
  let StaticProperty {staticPropertyId, essence} = staticProperty
  let StaticPropertyId sPId = staticPropertyId
  let Essence ess = essence
  let ActivePropertyId propId = activePropertyId
  let thisNodeName = "A:" <> show propId <> ":" <> ess <>""
  let staticPropName = "S:" <> show sPId <> ":" <> ess
  let toSPropRow = quoted thisNodeName <> " -> " <> quoted staticPropName <> toSPropArrStyle

  props <- readTVar propertiesVar
  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM buildActivePropertiesByType $ Map.toList props

  let thisNodePointsToChildren = join $ map (f thisNodeName) propsByTypeStrings
  let childrenStrings          = join $ map (\(_, _, xs) -> xs) propsByTypeStrings

  propValue <- readTVar propertyValueVar
  thisNodeValues <- buildValueNode thisNodeName propValue

  pure (thisNodeName,
    childrenStrings
    <> [""]
    <> [toSPropRow]
    <> thisNodePointsToChildren
    <> [""]
    <> thisNodeValues
    )

  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


buildActingObjectNode :: (ActingObjectId, ActingObject) -> STM [String]
buildActingObjectNode (_, ActingObject {..}) = do
  (rootPropNodeName, activePropStrings) <- buildActivePropertyNode rootProperty

  let actingObjectNodeStyle = "node [shape=box];"
  let activePropsNodeStyle = "node [shape=circle];"
  let arrStyle  = " [arrowhead=onormal];"
  let ActingObjectId oId = actingObjectId
  let ActingObjectName aName = actingObjectName
  let name = "AO:" <> show oId <> ":" <> aName
  let row = quoted name <> " -> " <> quoted rootPropNodeName <> arrStyle

  pure $
    [ activePropsNodeStyle ]
    <> activePropStrings
    <> ["", actingObjectNodeStyle, row ]

buildGraph :: ZPNet -> STM [String]
buildGraph ZPNet{knowledgeBase, actingObjects} = do
  let start =
       [ "digraph finite_state_machine {"
       , "graph [ dpi = 600 ];"
       , "size=\"8,5\";"
       ]
  let end = [ "}" ]

  let staticPropsNodeStyle = "node [shape=Mcircle];"

  let KnowledgeBase {staticProperties} = knowledgeBase

  staticPropsStrings'   <- mapM buildStaticPropertyNode staticProperties
  let staticPropsStrings = join $ map snd staticPropsStrings'

  actingObjectsStrings <- mapM buildActingObjectNode $ Map.toList actingObjects

  pure $ start
    <> [ staticPropsNodeStyle ]
    <> staticPropsStrings
    <> [""]
    <> join actingObjectsStrings
    <> end
