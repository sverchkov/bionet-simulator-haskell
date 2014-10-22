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
type Net = Map.Map Node (Either InteractionType [InteractionType], [Node])

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

    parentMerge (it1, parents1) (it2, parents2) = (typeMerge it1 it2, (List.union parents1 parents2))

    typeMerge (Left a) (Left b) =
        if a == b || b == "" then Left a else
            if a == "" then Left b else Right [a,b]
    typeMerge (Left a) (Right l) = Right (a:l)
    typeMerge (Right l) (Left a) = Right (a:l)
    typeMerge (Right l1) (Right l2) = Right (l1++l2) 

    netFromEdge edge = Map.insert (dest edge) (Left (edgeType edge),[source edge]) tmpMap where
        tmpMap = Map.singleton (source edge) (Left [],[])

-- Utility to find mixed interactions
mixedInteractions :: Net -> [([InteractionType], Node)]
mixedInteractions net = map cleanup $ filter hasRight $ Map.assocs net where

    hasRight (_, (Right _, _)) = True
    hasRight _ = False

    cleanup (node, (Right l, parents)) = (l, node)
