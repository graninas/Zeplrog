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
import qualified Data.List as List

type NodeUID = TVar Int
type NodeStyle = String
type NodeName = String
type NodeStyles = TVar (Map.Map NodeStyle [NodeName])

toSPropArrStyle     = " [arrowhead=onormal];"
toValueNodeArrStyle = " [arrowhead=dot];"
actingObjectNodeStyle = "node [shape=box];"
activePropsNodeStyle = "node [shape=circle];"
actingObjectToActivePropStyle  = " [arrowhead=onormal];"
staticPropsNodeStyle = "node [shape=Mcircle];"
valueNodeStyle = "node [shape=plaintext];"

getNodeUID :: NodeUID -> STM Int
getNodeUID uidVar = do
  v <- readTVar uidVar
  writeTVar uidVar $ v + 1
  pure v

quoted :: String -> String
quoted str = "\"" <> str <> "\""

addNodeStyle :: NodeStyles -> NodeStyle -> NodeName -> STM ()
addNodeStyle stylesVar style node = do
  styles <- readTVar stylesVar
  case Map.lookup style styles of
    Just nodes -> writeTVar stylesVar $ Map.insert style (quoted node : nodes) styles
    Nothing    -> writeTVar stylesVar $ Map.insert style [quoted node] styles

whenNotNull' [] _ = []
whenNotNull' _ res = res

buildValueNodeAlg2 :: NodeUID -> NodeStyles -> PropertyValue -> STM (String, [String])
buildValueNodeAlg2 uidVar stylesVar val = do
  uid <- getNodeUID uidVar
  let mkUName n = n <> "/" <> show uid
  let mkQUName n = quoted $ mkUName n
  (nName, strs) <- case val of
    NoValue -> pure ("", [])
    PairValue v1 v2 -> do
      (chName1, lStrs) <- buildValueNodeAlg2 uidVar stylesVar v1
      (chName2, rStrs) <- buildValueNodeAlg2 uidVar stylesVar v2
      pure (mkUName "pair",
        [ whenNotNull' chName1 $ mkQUName "pair" <> " -> " <> quoted chName1 <> toValueNodeArrStyle
        , whenNotNull' chName2 $ mkQUName "pair" <> " -> " <> quoted chName2 <> toValueNodeArrStyle
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
      (chName, tStrs) <- buildValueNodeAlg2 uidVar stylesVar v
      pure (mkUName "target",
           [ whenNotNull' chName $ mkQUName "target" <> " -> " <> quoted chName <> toValueNodeArrStyle
           ] <> tStrs
          )
    ConditionValue -> do
      pure (mkUName "condition", [])
    StateValue v -> do
      (chName, tStrs) <- buildValueNodeAlg2 uidVar stylesVar v
      pure (mkUName "state",
           [ whenNotNull' chName $ mkQUName "state" <> " -> " <> quoted chName <> toValueNodeArrStyle
           ] <> tStrs
          )
    ListValue vals -> do
      let thisNodeQUName = mkQUName "list"
      let f' (chName, tStrs) = [thisNodeQUName <> " -> " <> quoted chName <> toValueNodeArrStyle]
                               <> tStrs
      xs <- mapM (buildValueNodeAlg2 uidVar stylesVar) vals
      let rows = join $ map f' xs
      pure (mkUName "list", rows)
  when (not $ null nName) $ addNodeStyle stylesVar valueNodeStyle nName
  pure (nName, strs)

buildStaticPropertiesByType
  :: NodeUID
  -> NodeStyles
  -> (PropertyType, [StaticProperty])
  -> STM (PropertyType, [String], [String])
buildStaticPropertiesByType uidVar stylesVar (pType, props) = do
  strs :: [(String, [String])] <- mapM (buildStaticPropertyNode uidVar stylesVar) props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildStaticPropertyNode :: NodeUID -> NodeStyles -> StaticProperty -> STM (String, [String])
buildStaticPropertyNode uidVar stylesVar StaticProperty{staticPropertyId, staticPropertyValue, essence, staticProperties} = do
  let Essence ess = essence
  let StaticPropertyId sPId = staticPropertyId
  let thisNodeName = "S:" <> show sPId <> ":" <> ess

  addNodeStyle stylesVar staticPropsNodeStyle thisNodeName

  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM (buildStaticPropertiesByType uidVar stylesVar) $ Map.toList staticProperties

  (valuesRootName, thisNodeValues) <- buildValueNodeAlg2 uidVar stylesVar staticPropertyValue
  let thisNodeToValuesRow =
        if null valuesRootName then []
          else quoted thisNodeName <> " -> " <> quoted valuesRootName <> toValueNodeArrStyle

  let thisNodeChildrenProps = filter (not . null) $ join $ map (f thisNodeName) propsByTypeStrings
  let rootProps             = filter (not . null) $ join $ map (\(_, _, xs) -> xs) propsByTypeStrings

  pure (thisNodeName,
    rootProps
    <> thisNodeChildrenProps
    <> [thisNodeToValuesRow]
    <> thisNodeValues
    )
  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


buildActivePropertiesByType
  :: NodeUID
  -> NodeStyles
  -> (PropertyType, TVar [ActiveProperty])
  -> STM (PropertyType, [String], [String])
buildActivePropertiesByType uidVar stylesVar (pType, psVar) = do
  props <- readTVar psVar
  strs :: [(String, [String])] <- mapM (buildActivePropertyNode uidVar stylesVar) props
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildActivePropertyNode :: NodeUID -> NodeStyles -> ActiveProperty -> STM (String, [String])
buildActivePropertyNode uidVar stylesVar ActiveProperty{..} = do
  let StaticProperty {staticPropertyId, essence} = staticProperty
  let StaticPropertyId sPId = staticPropertyId
  let Essence ess = essence
  let ActivePropertyId propId = activePropertyId
  let thisNodeName = "A:" <> show propId <> ":" <> ess <>""

  addNodeStyle stylesVar activePropsNodeStyle thisNodeName

  let staticPropName = "S:" <> show sPId <> ":" <> ess
  let toSPropRow = quoted thisNodeName <> " -> " <> quoted staticPropName <> toSPropArrStyle

  props <- readTVar propertiesVar
  propsByTypeStrings :: [(PropertyType, [String], [String])] <-
    mapM (buildActivePropertiesByType uidVar stylesVar) $ Map.toList props

  let thisNodePointsToChildren = join $ map (f thisNodeName) propsByTypeStrings
  let childrenStrings          = join $ map (\(_, _, xs) -> xs) propsByTypeStrings

  propValue <- readTVar propertyValueVar
  (valuesRootName, thisNodeValues) <- buildValueNodeAlg2 uidVar stylesVar propValue
  let thisNodeToValuesRow =
        if null valuesRootName then []
          else quoted thisNodeName <> " -> " <> quoted valuesRootName <> toValueNodeArrStyle

  pure (thisNodeName,
    childrenStrings
    <> [toSPropRow]
    <> thisNodePointsToChildren
    <> [thisNodeToValuesRow]
    <> thisNodeValues
    )

  where
    f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
    f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


buildActingObjectNode :: NodeUID -> NodeStyles -> (ActingObjectId, ActingObject) -> STM [String]
buildActingObjectNode uidVar stylesVar (_, ActingObject {..}) = do
  (rootPropNodeName, activePropStrings) <- buildActivePropertyNode uidVar stylesVar rootProperty
  let ActingObjectId oId = actingObjectId
  let ActingObjectName aName = actingObjectName
  let name = "AO:" <> show oId <> ":" <> aName
  let thisNodeToRootProp = quoted name <> " -> " <> quoted rootPropNodeName <> actingObjectToActivePropStyle

  addNodeStyle stylesVar actingObjectNodeStyle name

  pure $ activePropStrings <> [ thisNodeToRootProp ]

buildGraph :: ZPNet -> STM [String]
buildGraph ZPNet{knowledgeBase, actingObjects} = do
  let start =
       [ "digraph finite_state_machine {"
       , "graph [ dpi = 600 ];"
       , "size=\"8,5\";"
       ]
  let end = [ "}" ]
  let KnowledgeBase {staticProperties} = knowledgeBase

  nodeUIDVar <- newTVar 0
  stylesVar <- newTVar Map.empty

  staticPropsStrings' <- mapM (buildStaticPropertyNode nodeUIDVar stylesVar) staticProperties
  let staticPropsStrings = join $ map snd staticPropsStrings'

  actingObjectsStrings' <- mapM (buildActingObjectNode nodeUIDVar stylesVar) $ Map.toList actingObjects
  let actingObjectsStrings = join actingObjectsStrings'

  styles <- readTVar stylesVar
  let nodeStyles = join $ map toStyles $ Map.toList styles

  pure $ start
    <> nodeStyles
    <> staticPropsStrings
    <> actingObjectsStrings
    <> end

  where
    toStyles (style, nodes) = ["{", style] <> nodes <> ["}"]
