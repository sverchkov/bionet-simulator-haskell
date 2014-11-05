module BionetSimulator where

import qualified Data.List as List
import qualified Data.Map.Strict as Map

process = id

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
type LinkFn = [Value] -> Value

-- Network building
-------------------

-- Empty net
empty :: Net
empty = Map.empty

-- Add an edge
(++) :: Net -> (Node, InteractionType, Node) -> Net
original ++ (src,it,dest) = if dest `Map.member` original
    then Map.insert dest ((it,src):original Map.! dest) original
    else Map.insert dest [(it,src)] original

-- Network inspection
---------------------

-- List all interaction types
edgeTypes :: Net -> [EdgeType]
edgeTypes net = foldl List.union [] $ map cleanup $ Map.assocs net where
    cleanup (_, list) = map (fst.fst) list

-- General computation framework
--------------------------------

-- Running a computation really consists of querying a values map from a network and an interaction spec.
computeValues :: Net -> Map.Map Node Value -> LinkFn -> InteractionSpec -> Values
computeValues net fixed link intspec = values where

    values = Map.mapWithKey evaluatorOuter net

    evaluatorOuter node list = if Map.member node fixed
        then fixed Map.! node
        else link $ map (evaluator node) list

    evaluator :: Node -> (InteractionType, Node) -> Value
    evaluator node (interaction, parent) = intspec interaction (values Map.! parent) -- Ignores the node, for now :/

-- The dummiest defaults
------------------------

-- Link function = 2 expit - 1
scaledExpit :: LinkFn 
scaledExpit list = 2/(1+exp(-sum list)) - 1

-- Interaction spec = polarity (as +/- 1) times 4
polarityInteractionSpec :: InteractionSpec
polarityInteractionSpec (_, Just Activating) = (4*)
polarityInteractionSpec (_, Just Inhibiting) = ((-4)*)
polarityInteractionSpec (_, Nothing) = \_->0
