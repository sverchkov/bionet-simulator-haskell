module BionetSimulator where

import Data.List.Split

process = id

-- Types and Data
-----------------

type Node = String

type EdgeType = String

data Edge = Edge { source :: Node
                 , dest :: Node
                 , edgeType :: EdgeType
                 } deriving (Show)

-- Constructors from strings
-- Take in cytoscape-like description of network, and get out list-of-edges description of network
--------------------------------------------------------------------------------------------------

mkLOE :: String -> [Edge]
mkLOE = (map mkEdge).lines

mkEdge :: String -> Edge
mkEdge inStr = Edge { source = src, dest = dst, edgeType = et } where
    (et : src : dst :_) = words inStr
