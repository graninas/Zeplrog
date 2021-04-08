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

type NodeUID = TVar Int

getNodeUID :: NodeUID -> STM Int
getNodeUID uidVar = do
  v <- readTVar uidVar
  writeTVar uidVar $ v + 1
  pure v

quoted :: String -> String
quoted str = "\"" <> str <> "\""

buildValueNode :: String -> PropertyValue -> STM [String]
buildValueNode parentName val = do
  let toValueNodeArrStyle = " [arrowhead=dot];"
  case val of
    NoValue -> pure []
    PairValue v1 v2 -> do
      let chName1 = parentName <> "-1"
      let chName2 = parentName <> "-2"
      lStrs <- buildValueNode chName1 v1
      rStrs <- buildValueNode chName2 v2
      pure $
        [ quoted parentName <> " -> " <> quoted chName1 <> toValueNodeArrStyle
        , quoted parentName <> " -> " <> quoted chName2 <> toValueNodeArrStyle
        ] <> lStrs <> rStrs
    IntValue v -> do
      let chName = parentName <> " " <> show v
      pure [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle]
    PositionValue pos -> do
      let chName = parentName <> " " <> show pos
      pure [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle]
    EssenceValue d (Essence e) -> do
      let chName = parentName <> "-" <> e -- <> "-" <> d
      pure [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle]
    StaticPropertyValue sProp -> do
      let Essence ess = essence sProp
      let StaticPropertyId sPId = staticPropertyId sProp
      let sPropName = "S:" <> show sPId <> ":" <> ess
      pure [ quoted parentName <> " -> " <> quoted sPropName <> toValueNodeArrStyle]
    TargetValue v -> do
      let chName = parentName <> "-"
      tStrs <- buildValueNode chName v
      pure $
        [ quoted parentName <> " -> " <> quoted chName <> toValueNodeArrStyle
        ] <> tStrs
    _ -> error " values not implemented"

buildValueNodeAlg2 :: NodeUID -> PropertyValue -> STM (String, [String])
buildValueNodeAlg2 uidVar val = do
  let toValueNodeArrStyle = " [arrowhead=dot];"
  uid <- getNodeUID uidVar
  let mkUName n = n <> "/" <> show uid
  let mkQUName n = quoted $ mkUName n
  case val of
    NoValue -> pure (mkUName "no value", [])
    PairValue v1 v2 -> do
      (chName1, lStrs) <- buildValueNodeAlg2 uidVar v1
      (chName2, rStrs) <- buildValueNodeAlg2 uidVar v2
      pure (mkUName "pair",
        [ mkQUName "pair" <> " -> " <> quoted chName1 <> toValueNodeArrStyle
        , mkQUName "pair" <> " -> " <> quoted chName2 <> toValueNodeArrStyle
        ] <> lStrs <> rStrs
        )
    IntValue v -> do
      let chName = show v
      pure (mkUName "int",
          [ mkQUName "int" <> " -> " <> quoted chName <> toValueNodeArrStyle]
        )
    PositionValue pos -> do
      let chName = show pos
      pure (mkUName "pos",
        [ mkQUName "pos" <> " -> " <> quoted chName <> toValueNodeArrStyle]
        )
    EssenceValue d (Essence e) -> do
      let chName = e
      pure (mkUName "essence",
        [ mkQUName "essence" <> " -> " <> quoted chName <> toValueNodeArrStyle]
        )
    StaticPropertyValue sProp -> do
      let Essence ess = essence sProp
      let StaticPropertyId sPId = staticPropertyId sProp
      let sPropName = "S:" <> show sPId <> ":" <> ess
      pure (mkUName "sProp",
          [ mkQUName "sProp" <> " -> " <> quoted sPropName <> toValueNodeArrStyle]
          )
    TargetValue v -> do
      (chName, tStrs) <- buildValueNodeAlg2 uidVar v
      pure (mkUName "target",
           [ mkQUName "target" <> " -> " <> quoted chName <> toValueNodeArrStyle
           ] <> tStrs
          )
    _ -> error " values not implemented"

buildStaticPropertiesByType
  :: NodeUID
  -> (PropertyType, [StaticProperty])
  -> STM (PropertyType, [String], [String])
buildStaticPropertiesByType uidVar (pType, props) = do
  strs :: [(String, [String])] <- mapM (buildStaticPropertyNode uidVar) props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildStaticPropertyNode :: NodeUID -> StaticProperty -> STM (String, [String])
buildStaticPropertyNode uidVar StaticProperty{staticPropertyId, staticPropertyValue, essence, staticProperties} = do
  let toValueNodeArrStyle = " [arrowhead=dot];"
  let Essence ess = essence
  let StaticPropertyId sPId = staticPropertyId
  let thisNodeName = "S:" <> show sPId <> ":" <> ess

  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM (buildStaticPropertiesByType uidVar) $ Map.toList staticProperties

  (valuesRootName, thisNodeValues) <- buildValueNodeAlg2 uidVar staticPropertyValue
  let thisNodeToValuesRow = quoted thisNodeName <> " -> " <> quoted valuesRootName <> toValueNodeArrStyle

  let thisNodeChildrenProps = filter (not . null) $ join $ map (f thisNodeName) propsByTypeStrings
  let rootProps             = filter (not . null) $ join $ map (\(_, _, xs) -> xs) propsByTypeStrings

  pure (thisNodeName,
    rootProps
    <> [""]
    <> thisNodeChildrenProps
    <> ["", thisNodeToValuesRow, ""]
    <> thisNodeValues
    )
  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


buildActivePropertiesByType
  :: NodeUID
  -> (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [String], [String])
buildActivePropertiesByType uidVar (pType, psVar) = do
  props <- readTVar psVar
  strs :: [(String, [String])] <- mapM (buildActivePropertyNode uidVar) props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildActivePropertyNode :: NodeUID -> ActiveProperty -> STM (String, [String])
buildActivePropertyNode uidVar ActiveProperty{..} = do
  let toSPropArrStyle  = " [arrowhead=onormal];"
  let toValueNodeArrStyle = " [arrowhead=dot];"
  let StaticProperty {staticPropertyId, essence} = staticProperty
  let StaticPropertyId sPId = staticPropertyId
  let Essence ess = essence
  let ActivePropertyId propId = activePropertyId
  let thisNodeName = "A:" <> show propId <> ":" <> ess <>""
  let staticPropName = "S:" <> show sPId <> ":" <> ess
  let toSPropRow = quoted thisNodeName <> " -> " <> quoted staticPropName <> toSPropArrStyle

  props <- readTVar propertiesVar
  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM (buildActivePropertiesByType uidVar) $ Map.toList props

  let thisNodePointsToChildren = join $ map (f thisNodeName) propsByTypeStrings
  let childrenStrings          = join $ map (\(_, _, xs) -> xs) propsByTypeStrings

  propValue <- readTVar propertyValueVar
  (valuesRootName, thisNodeValues) <- buildValueNodeAlg2 uidVar propValue
  let thisNodeToValuesRow = quoted thisNodeName <> " -> " <> quoted valuesRootName <> toValueNodeArrStyle

  pure (thisNodeName,
    childrenStrings
    <> [""]
    <> [toSPropRow]
    <> thisNodePointsToChildren
    <> ["", thisNodeToValuesRow, ""]
    <> thisNodeValues
    )

  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


buildActingObjectNode :: NodeUID -> (ActingObjectId, ActingObject) -> STM [String]
buildActingObjectNode uidVar (_, ActingObject {..}) = do
  (rootPropNodeName, activePropStrings) <- buildActivePropertyNode uidVar rootProperty

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

  nodeUIDVar <- newTVar 0

  staticPropsStrings'   <- mapM (buildStaticPropertyNode nodeUIDVar) staticProperties
  let staticPropsStrings = join $ map snd staticPropsStrings'

  actingObjectsStrings <- mapM (buildActingObjectNode nodeUIDVar) $ Map.toList actingObjects

  pure $ start
    <> [ staticPropsNodeStyle ]
    <> staticPropsStrings
    <> [""]
    <> join actingObjectsStrings
    <> end
