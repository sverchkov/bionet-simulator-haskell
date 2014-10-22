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

type Net = Map.Map Node (InteractionType, [Node]) -- Map of node to interaction type + parent set.

-- Constructors from strings
-- Take in cytoscape-like description of network, and get out list-of-edges description of network
--------------------------------------------------------------------------------------------------

mkLOE :: String -> [Edge]
mkLOE = (map mkEdge) . lines

mkEdge :: String -> Edge
mkEdge inStr = Edge { source = src, dest = dst, edgeType = et } where
    (et : src : dst :_) = words inStr

-- Turn list of edges into a "network" representation (Map based).
mkNet :: [Edge] -> Net
mkNet [] = Map.empty
mkNet (edge:rest) = Map.unionWith parentMerge (netFromEdge edge) (mkNet rest) where
    parentMerge (it1, parents1) (it2, parents2) = (it1, (List.union parents1 parents2))
    netFromEdge edge = Map.insert (dest edge) ((edgeType edge),[source edge]) tmpMap where
        tmpMap = Map.singleton (source edge) ([],[])

