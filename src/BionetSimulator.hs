module BionetSimulator where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

-- Types and Data
-----------------

type Node = String

type EdgeType = String

-- Some typeclass decisions for (edge) polarity:
-- not Ord (b/c I don't think it makes sense to compare polarities w/ gt/lt)
-- not Bounded (same reasoning, no sense in thinking one is smaller than the other)
-- not Enum (Don't think it makes sense to think of one polarity as having the other as a predecessor)
data Polarity = Activating | Inhibiting deriving (Eq, Show, Read)

-- Probably makes sense to couple polarity in the interaction type
type InteractionType = (EdgeType, Maybe Polarity)

-- A net is a map of node to a list of interaction type - parent pairs
type Net = Map.Map Node [(InteractionType, Node)]

-- Map of node to expression level
type Value = Double
type Values = Map.Map Node Value
type InteractionSpec = InteractionType -> Value -> Value
-- Local computation = child -> [parent, parent value, interaction] -> child value
type LocalComputation = Node -> [(Node, Value, InteractionType)] -> Value

-- Network building
-------------------

-- Empty net
empty :: Net
empty = Map.empty

-- Add an edge
(+-) :: Net -> (Node, InteractionType, Node) -> Net
original +- (src,it,dest) = if dest `Map.member` original
    then Map.insert dest ((it,src):original Map.! dest) original
    else Map.insert dest [(it,src)] original

-- Add a node
(+.) :: Net -> Node -> Net
original +. node = if node `Map.member` original
    then original
    else Map.insert node [] original

-- Network inspection
---------------------

-- List Nodes
nodes :: Net -> [Node]
nodes = Map.keys

-- Get incoming arcs for node
getArcs :: Net -> Node -> [(InteractionType, Node)]
getArcs = (Map.!)

-- List all interaction types
edgeTypes :: Net -> [EdgeType]
edgeTypes net = foldl List.union [] $ map cleanup $ Map.assocs net where
    cleanup (_, list) = map (fst.fst) list

-- General computation framework
--------------------------------

-- Running a computation really consists of querying a values map from a network and an interaction spec.
compute :: Net -> LocalComputation -> Values
compute net computation = values where

    values = Map.mapWithKey comp net

    comp node arcs = computation node (map addValue arcs)

    addValue (interaction, parent) = (parent, values Map.! parent, interaction)

