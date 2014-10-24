import BionetSimulator

data Edge = Edge { source :: Node
                 , dest :: Node
                 , edgeType :: EdgeType
                 , polarity :: Polarity
                 } deriving (Show, Eq)

-- Constructors from strings
----------------------------

-- Take in cytoscape-like description of network, and get out list-of-edges description of network
mkLOE :: String -> [Edge]
mkLOE = (map mkEdge) . lines

mkEdge :: String -> Edge -- OLD. TODO: Update when we figure out right input file format
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

-- Turn integaction spec file into interaction spec
readInteractionSpec :: String -> InteractionSpec
readInteractionSpec _ = f where f _ = sum -- Stand-in. Need to fill in deets.
