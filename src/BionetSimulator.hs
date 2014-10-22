module BionetSimulator where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map

process = id

-- Types and Data
-----------------

type Node = String

type EdgeType = String

data Edge = Edge { source :: Node
                 , dest :: Node
                 , edgeType :: EdgeType
                 } deriving (Show)

type InteractionType = String

-- Map of node to interaction type + parent set.
type Net = Map.Map Node (Either (InteractionType, [Node]) [(InteractionType, Node)])

-- Map of node to expression level
type Value = Double
type Values = Map.Map Node Value
type InteractionSpec = InteractionType -> [Value] -> Value

-- Constructors from strings
-- Take in cytoscape-like description of network, and get out list-of-edges description of network
--------------------------------------------------------------------------------------------------

mkLOE :: String -> [Edge]
mkLOE = (map mkEdge) . lines

mkEdge :: String -> Edge
mkEdge inStr = Edge { source = src, dest = dst, edgeType = et } where
    (src : et : dst :_) = words inStr

-- Turn list of edges into a "network" representation (Map based).
mkNet :: [Edge] -> Net
mkNet [] = Map.empty
mkNet (edge:rest) = Map.unionWith parentMerge (netFromEdge edge) (mkNet rest) where

    parentMerge (Left ("", [])) x = x
    parentMerge (Left (it1, parents1)) (Left (it2, parents2)) =
        if it1 == it2 || it2 == "" then
            -- I'm just concatenating parent lists. Don't know if I should union instread.
            Left (it1, parents1++parents2) else
            parentMerge (rightify (it1, parents1)) (rightify (it2, parents2))
    parentMerge (Left x) (Right l) = parentMerge (rightify x) (Right l)
    parentMerge y (Left x) = parentMerge (Left x) y
    parentMerge (Right l1) (Right l2) = Right $ l1++l2

    rightify (it, parents) = Right $ zip (repeat it) parents

    netFromEdge edge = Map.insert (dest edge) (Left ((edgeType edge),[source edge])) tmpMap where
        tmpMap = Map.singleton (source edge) (Left ([],[]))

-- Utility to find mixed interactions
mixedInteractions :: Net -> [([(InteractionType, Node)], Node)]
mixedInteractions net = map cleanup $ filter hasRight $ Map.assocs net where

    hasRight (_, Right _) = True
    hasRight _ = False

    cleanup (node, Right l) = (l, node)

-- Running a computation really consists of querying a values map from a network and an interaction spec.
mkValues :: Net -> InteractionSpec -> Values
mkValues net intspec = values where
    values = Map.map evaluator net
    evaluator (Left (it, parents)) = intspec it (map (values Map.!) parents)
    evaluator (Right list) = 0 --TODO: Make meaningful!

