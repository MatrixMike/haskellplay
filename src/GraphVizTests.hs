module GraphVizTests where

import Data.GraphViz.Commands.IO ( readDotFile )
import Data.GraphViz.Types.Generalised ( DotEdge, DotGraph )
import Data.GraphViz.Types
    ( DotRepr(nodeInformation, edgeInformation), NodeLookup )

main :: IO ()
main = foo >>= print


-- >>> foo
-- DotGraph {strictGraph = False, directedGraph = True, graphID = Nothing, graphStatements = fromList [DN (DotNode {nodeID = "d", nodeAttributes = [Label (StrLabel "D"),Shape BoxShape]}),DE (DotEdge {fromNode = "a", toNode = "b", edgeAttributes = [Label (StrLabel "lol")]}),DE (DotEdge {fromNode = "b", toNode = "d", edgeAttributes = []}),DE (DotEdge {fromNode = "b", toNode = "d", edgeAttributes = []})]}
foo :: IO (DotGraph String)
foo = readDotFile "test/data/test.dot"


-- >>> bar
-- [DotEdge {fromNode = "a", toNode = "b", edgeAttributes = [Label (StrLabel "lol")]},DotEdge {fromNode = "b", toNode = "d", edgeAttributes = []},DotEdge {fromNode = "b", toNode = "d", edgeAttributes = []}]
bar :: IO [DotEdge String]
bar = edgeInformation True <$> foo

-- >>> baz
-- fromList [("a",(fromList [],[])),("b",(fromList [],[])),("d",(fromList [],[Label (StrLabel "D"),Shape BoxShape]))]
baz :: IO (NodeLookup String)
baz = nodeInformation True <$> foo