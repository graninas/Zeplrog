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
type JointPointName = String

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

buildStaticValueNode
  :: NodeUID
  -> NodeStyles
  -> TVar StaticPropertyValue
  -> STM (String, [String])
buildStaticValueNode uidVar stylesVar valVar = do
  uid <- getNodeUID uidVar
  uid2 <- getNodeUID uidVar
  val <- readTVar valVar
  let mkUName n = n <> "/" <> show uid
  let mkQUName n = quoted $ mkUName n

  (nName, strs) <- case val of
    NoStaticValue -> pure ("", [])
    MaterializableStateValue d (DirectMaterialization sProp) -> do
      let Essence ess = essence sProp
      let StaticPropertyId sPId = staticPropertyId sProp
      let sPropName = "S:" <> show sPId <> ":" <> ess

      let nName = mkUName "sProp"
      let jointPointName = "●/" <> show uid2

      addNodeStyle stylesVar valueNodeStyle nName
      addNodeStyle stylesVar valueNodeStyle jointPointName

      pure (nName,
          [ quoted nName          <> " -> " <> quoted jointPointName <> toValueNodeArrStyle
          , quoted jointPointName <> " -> " <> quoted sPropName <> toValueNodeArrStyle
          ])
    MaterializableStateValue d (SharedMaterialization sProp) -> do
      let Essence ess = essence sProp
      let StaticPropertyId sPId = staticPropertyId sProp
      let sPropName = "S:" <> show sPId <> ":" <> ess

      let nName = mkUName "mat value"
      let jointPointName = "○/" <> show uid2

      addNodeStyle stylesVar valueNodeStyle nName
      addNodeStyle stylesVar valueNodeStyle jointPointName

      pure (nName,
          [ quoted nName          <> " -> " <> quoted jointPointName <> toValueNodeArrStyle
          , quoted jointPointName <> " -> " <> quoted sPropName <> toValueNodeArrStyle
          ])

  pure (nName, strs)

buildValueNode :: NodeUID -> NodeStyles -> PropertyValue -> STM (String, [String])
buildValueNode uidVar stylesVar val = do
  uid <- getNodeUID uidVar
  let mkUName n = n <> "/" <> show uid
  let mkQUName n = quoted $ mkUName n
  (nName, strs) <- case val of
    NoValue -> pure ("", [])
    PairValue v1 v2 -> do
      (chName1, lStrs) <- buildValueNode uidVar stylesVar v1
      (chName2, rStrs) <- buildValueNode uidVar stylesVar v2
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
    EssenceValue descr (Essence e) -> do
      let chName = e
      pure (mkUName ("essence " <> descr),
        [ mkQUName ("essence" <> descr) <> " -> " <> quoted chName <> toValueNodeArrStyle]
        )
    StaticPropertyValue sProp -> do
      let Essence ess = essence sProp
      let StaticPropertyId sPId = staticPropertyId sProp
      let sPropName = "S:" <> show sPId <> ":" <> ess
      pure (mkUName "value",
          [ mkQUName "value" <> " -> " <> quoted sPropName <> toValueNodeArrStyle]
          )
    TargetValue v -> do
      (chName, tStrs) <- buildValueNode uidVar stylesVar v
      pure (mkUName "target",
           [ whenNotNull' chName $ mkQUName "target" <> " -> " <> quoted chName <> toValueNodeArrStyle
           ] <> tStrs
          )
    ConditionValue -> do
      pure (mkUName "condition", [])
    -- MaterializableStateValue descr sProp -> do
    --   let Essence ess = essence sProp
    --   let StaticPropertyId sPId = staticPropertyId sProp
    --   let sPropName = "S:" <> show sPId <> ":" <> ess
    --   pure (mkUName $ "mat sProp " <> descr,
    --       [ mkQUName ("mat sProp " <> descr) <> " -> " <> quoted sPropName <> toValueNodeArrStyle]
    --       )
    ListValue vals -> do
      let thisNodeQUName = mkQUName "list"
      let f' (chName, tStrs) = [thisNodeQUName <> " -> " <> quoted chName <> toValueNodeArrStyle]
                               <> tStrs
      xs <- mapM (buildValueNode uidVar stylesVar) vals
      let rows = join $ map f' xs
      pure (mkUName "list", rows)
  when (not $ null nName) $ addNodeStyle stylesVar valueNodeStyle nName
  pure (nName, strs)

buildStaticPropertiesByType
  :: NodeUID
  -> NodeStyles
  -> (PropertyType, [MaterializationLink])
  -> STM (PropertyType, [String], [String])
buildStaticPropertiesByType uidVar stylesVar (pType, matLinks) = do
  strs :: [(String, [String])] <- mapM (buildMaterializationLinkNode uidVar stylesVar) matLinks
  let names = map fst strs
  pure (pType, names, join $ map snd strs)


buildMaterializationLinkNode :: NodeUID -> NodeStyles -> MaterializationLink -> STM (String, [String])
buildMaterializationLinkNode uidVar stylesVar matLink = do
  uid <- getNodeUID uidVar
  let (jointPointName, sProp) = case matLink of
        SharedMaterialization sProp -> ("○/" <> show uid, sProp)
        DirectMaterialization sProp -> ("●/" <> show uid, sProp)
  addNodeStyle stylesVar valueNodeStyle jointPointName
  buildJointedStaticPropertyNode uidVar stylesVar jointPointName sProp


buildJointedStaticPropertyNode :: NodeUID -> NodeStyles -> JointPointName -> StaticProperty -> STM (String, [String])
buildJointedStaticPropertyNode uidVar stylesVar jointPointName
  StaticProperty{staticPropertyId, staticPropertyValueVar, essence, staticProperties} = do
    let Essence ess = essence
    let StaticPropertyId sPId = staticPropertyId
    let thisNodeName = "S:" <> show sPId <> ":" <> ess

    addNodeStyle stylesVar staticPropsNodeStyle thisNodeName

    propsByTypeStrings :: [(PropertyType, [String], [String])] <-
      mapM (buildStaticPropertiesByType uidVar stylesVar)
        $ Map.toList staticProperties

    (valuesRootName, thisNodeValues) <- buildStaticValueNode uidVar stylesVar staticPropertyValueVar
    let thisNodeToValuesRow =
          if null valuesRootName then []
            else quoted thisNodeName <> " -> " <> quoted valuesRootName <> toValueNodeArrStyle

    let jointPointToNodeRow = quoted jointPointName <> " -> " <> quoted thisNodeName <> toSPropArrStyle

    let thisNodeChildrenProps = filter (not . null) $ join $ map (f thisNodeName) propsByTypeStrings
    let rootProps             = filter (not . null) $ join $ map (\(_, _, xs) -> xs) propsByTypeStrings

    pure (jointPointName,
      [jointPointToNodeRow]
      <> rootProps
      <> thisNodeChildrenProps
      <> [thisNodeToValuesRow]
      <> thisNodeValues
      )
    where
      f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
      f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"

-- buildStaticPropertyNode :: NodeUID -> NodeStyles -> StaticProperty -> STM (String, [String])
-- buildStaticPropertyNode uidVar stylesVar
--   StaticProperty{staticPropertyId, staticPropertyValueVar, essence, staticProperties} = do
--     let Essence ess = essence
--     let StaticPropertyId sPId = staticPropertyId
--     let thisNodeName = "S:" <> show sPId <> ":" <> ess
--
--     addNodeStyle stylesVar staticPropsNodeStyle thisNodeName
--
--     propsByTypeStrings :: [(PropertyType, [String], [String])] <-
--       mapM (buildStaticPropertiesByType uidVar stylesVar)
--         $ Map.toList staticProperties
--
--     (valuesRootName, thisNodeValues) <- buildStaticValueNode uidVar stylesVar staticPropertyValueVar
--     let thisNodeToValuesRow =
--           if null valuesRootName then []
--             else quoted thisNodeName <> " -> " <> quoted valuesRootName <> toValueNodeArrStyle
--
--     let thisNodeChildrenProps = filter (not . null) $ join $ map (f thisNodeName) propsByTypeStrings
--     let rootProps             = filter (not . null) $ join $ map (\(_, _, xs) -> xs) propsByTypeStrings
--
--     pure (thisNodeName,
--       rootProps
--       <> thisNodeChildrenProps
--       <> [thisNodeToValuesRow]
--       <> thisNodeValues
--       )
--     where
--       f thisNodeName (PropertyType pType, targetNames, _) = map (f' thisNodeName pType) targetNames
--       f' thisNodeName pType targetName = quoted thisNodeName <> " -> " <> quoted targetName <> " [label=\"" <> pType <> "\"];"


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
  (valuesRootName, thisNodeValues) <- buildValueNode uidVar stylesVar propValue
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
  let KnowledgeBase {materializationLinks} = knowledgeBase

  nodeUIDVar <- newTVar 0
  stylesVar <- newTVar Map.empty

  staticPropsStrings' <- mapM (buildMaterializationLinkNode nodeUIDVar stylesVar) materializationLinks
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
