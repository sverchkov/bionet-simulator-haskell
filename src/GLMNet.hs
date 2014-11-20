module GLMNet where
-- A Generalized Linear Model Network where the value a node takes is
-- link ( intercept + sum ( parent * weight ) )
-- This captures a fairly wide range of possible models.

import qualified Data.Map.Strict as Map
import BionetSimulator
import System.Random(RandomGen)
import Data.Random.Normal(normal)

type GLMNet = Map.Map Node (Value, Map.Map Node Value)
type LinkFn = Value -> Value

mkRandomGLM :: RandomGen g => Net -> g -> (GLMNet, g)
mkRandomGLM net rng = build Map.empty (nodes net) rng where

    build glm [] rng1 = (glm, rng1)
    build glm (node:rest) rng1 = build glm1 rest rng3 where

        glm1 = Map.insert node (intercept, weights) glm
        (intercept, rng2) = normal rng1
        (weights, rng3) = weightsFor (getArcs net node) rng2 where

            weightsFor [] rngA = (Map.empty, rngA)
            weightsFor ((interaction, parent):rest) rngA = (ws, rngC) where

                ws = Map.insert parent (weight interaction) otherWs
                (otherWs, rngB) = weightsFor rest rngA

                weight (_, Just Activating) = abs w
                weight (_, Just Inhibiting) = - (abs w)
                weight (_, Nothing) = w

                (w, rngC) = normal rngB

-- Loading from/Saving to string
prettyPrint :: GLMNet -> String
prettyPrint net = unlines $ map unwords $ map s1 $ Map.assocs net where

    s1 :: (Node, (Value, Map.Map Node Value)) -> [String]
    s1 (a,(b,c)) = foldl (++) [show a, show b] $ map s2 $ Map.assocs c

    s2 :: (Node, Value) -> [String]
    s2 (a,b) = [show a, show b]

readPretty :: String -> GLMNet
readPretty str = Map.fromList $ map (f1.words) $ lines str where

    f1 :: [String] -> (Node, (Value, Map.Map Node Value))
    f1 (node:intercept:weightPairs) = (read node, (read intercept, f2 weightPairs))

    f2 :: [String] -> Map.Map Node Value
    f2 [] = Map.empty
    f2 (n:v:rest) = Map.insert (read n) (read v) $ f2 rest

-- Turning into a LocalComputation
asComputation :: GLMNet -> LinkFn -> LocalComputation
asComputation net link node incomingArcs = link (intercept + sum terms) where
    (intercept, weightMap) = net Map.! node
    terms = [value * (weightMap Map.! parent) | (parent, value, _) <- incomingArcs]

-- The "2 expit - 1" link function
scaledExpit :: LinkFn
scaledExpit x = 2/(1+exp(-x)) - 1
