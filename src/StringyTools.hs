module StringyTools where

import qualified BionetSimulator as BS

data Edge = Edge { source :: BS.Node
                 , dest :: BS.Node
                 , edgeType :: BS.EdgeType
                 , polarity :: Maybe BS.Polarity
                 } deriving (Show, Eq)

-- Constructors from strings
----------------------------

-- Take in cytoscape-like description of network, and get out list-of-edges description of network
mkLOE :: String -> [Edge]
mkLOE = (map mkEdge) . lines

mkEdge :: String -> Edge
mkEdge inStr = Edge { source = src, dest = dst, edgeType = et, polarity = Nothing } where
    (src : et : dst :_) = words inStr

-- Turn list of edges into a "network" representation (Map based).
mkNet :: [Edge] -> BS.Net
mkNet [] = BS.empty
mkNet (edge:rest) = (mkNet rest) BS.++ (source edge, (edgeType edge, polarity edge), dest edge)

-- Turn integaction spec file into interaction spec
readInteractionSpec :: String -> BS.InteractionSpec
readInteractionSpec _ = BS.polarityInteractionSpec -- Default stand-in.
