#!/usr/bin/env ruby

# detect cycles
# topo sort
# simplify

module ADT

class DependancyGraph < DirectedGraph
  
end

class DependancyGraph::SubsetOf < Array
  
end

class DependancyGraph::OneOf < Array
  def initialize(graph, *nodes)
    @nodes = nodes
    @graph = graph
  end
  
  # find dependancies that every node has in common
  def common
    
  end
end

DepGrah = DependancyGraph

end
