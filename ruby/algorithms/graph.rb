#!/usr/bin/env ruby
dir = File.dirname(File.expand_path(__FILE__))
$:.push(dir) unless $:.include?(dir)
require 'queue'

module ADT
  Infinity = 1.0/0.0
end

module ADT::SearchSpace
  
  # must be defined in subclass
  #def successors state
  #  ...
  #end
  
  # breadth first search
  def bfs start, &goal
    # parent doesn't ever get re-written (like dfs), so we also
    # can use it to check if we visited a node already
    parent = {start => nil}
    fringe = [start].queue
    
    # visit every node
    while u = fringe.pop
      # stop when goal has been reached
      break if goal.call(u)
      
      # each neighbor
      successors(u).each do |v|
        unless parent.keys.include? v # v hasn't been visited
          fringe << v
          
          # mark v as 'visited' and jot down it's parent in the path, this
          # shortcut won't work for dfs
          parent[v] = u
          #warn "visiting #{v}"
        else
          #warn "already visited #{v}"
        end
      end
    end
    
    parent
  end
  
  # depth first search
  def dfs start, &goal
    # in dfs we mark the parent without having yet visited a node, so
    # we cannot just check if parent.keys.include? neighbor. we must
    # keep track of visited nodes in another array
    visited = []
    parent  = {}
    fringe  = [start].stack
    
    while u = fringe.pop
      # stop when goal has been reached
      break if goal.call(u)
      
      # skip already visited node
      next if visited.include?(u)
      
      # mark as 'visited'
      visited << u
      
      successors(u).each do |v|
        unless visited.include? v # v hasn't been visited
          fringe << v
          
          # we can do bfs if we only set parent when it's not already been
          # set, and of course change fringe to a queue. if we don't simply
          # overwrite parent[v] when doing dfs, parent won't be correct
          # parent[v] = u unless parent[v]
          parent[v] = u
        end
      end
    end
    
    parent
  end
  
  # depth limited search
  def dls start, depth, &goal
  end
  
  # 
  def astar start, cost, heuristic, &goal
    # cost(a, b): compute the cost of moving a to it's neighbor b
    # heuristic(x): estimate the cost from x to goal
    
    open     = ::ADT::PQueue.new(:min)
    closed   = []
    parent   = {start => nil}
    distance = {start => 0}
    
    open.insert start, 0
    
    while u = open.pop
      # stop when goal has been reached
      break if goal.call(u)
      
      # don't consider u anymore
      closed.push u
      
      (successors u).each do |v|
        # calculate cost from u to v
        w = cost.call(u, v)
        
        unless closed.include? v or w == ::ADT::Infinity
          if open.include? v # we've seen v before
            if distance[u] + w < distance[v]
              # we found a shortcut to v through u
              
              # actual distance from start to v
              distance[v] = distance[u] + w
              parent[v]   = u
            
              # estimate total cost from start to goal through v
              open.insert v, distance[v] + heuristic.call(v)
            
            #else
            #  the previous path to v is shorter than a detour through u
            end
          
          else # we haven't seen v before
            distance[v] = distance[u] + w
            parent[v]   = u
            open.insert v, distance[v] + heuristic.call(v)
          end
          
        #else
        #  we've already eliminated v, or it is unreachable
        end
        
      end
    end
    
    parent
  end
  
  # find lowest cost path, no heuristic
  def dijkstra start, cost, &goal
    # heuristic always returns 0
    astar start, cost, lambda{|x| 0 }, &goal
  end
  
  # best first search, uses heuristic
  def best start, heuristic, &goal
    # FIXME: this is not correct
    astar start, lambda{|u,v| 0 }, heuristic, &goal
  end
  
  # optimized best first, use heuristic and limit scope
  def beam start, width, &goal
    # beam width of infinity is same as best first
    best 
  end
  
  # find a decent solution quickly, then find optimal solution
  def beamstack start, &goal
  end
  
  # iterative deepening dfs, start shallow and increase depth
  def iddfs start, &goal
  end
  
  # search from start and from goal, meet in the middle
  def bidirectional start, goal
  end
  
  # 
  def minimax start, depth
    alphabeta start, depth, -ADT::Infitity, ADT::Infinity
  end
  
  # 
  def alphabeta start, depth, alpha, beta
  end
  
  def negamax start, depth, alpha, beta
  end
  
  def negascout start, depth, alpha, beta
  end
  
  def mtdf start, f, d
  end
  
  # simulated annealing
  def anneal
  end
  
  # tabu search
  def tabu
  end
  
  def hillclimb start, &goal
  end
  
  # random restart hill climbing
  def hillclimb_restart &goal
  end
  
  # run many hillclimb searches from random start states
  def hillclimb_stochastic
  end
  
  # bootstrap aggregating
  def bagging
  end
  
  #
  def boosting
  end
  
  #
  def adaboost
  end
  
  # find lowest cost path, negative costs allowed
  def bellmanford start, cost, &goal
  end
  
end

class ADT::Graph
  include ADT::SearchSpace
  attr_reader :nodes, :edges
  
  def initialize nodes=[], edges={}
    @nodes, @edges = nodes, edges
  end
  
  def inspect
    "#{self.class.to_s.split('::')[-1]}:" <<
    " #{@edges.size} edges, #{@nodes.size} nodes"
  end
  
  def dup
  end
  
  # clear all cached algorithm results
  def uncache
    # don't reset these
    ignore = %w[@nodes @edges]
    
    # set the rest to nil
    (instance_variables-ignore).each{|v| eval "#{v}=nil"}
    
    self
  end
  
  # false, this is an undirected graph (unless subclassed)
  def directed?
    false
  end
  
  # true if this graph is undirected
  def undirected?
    !directed?
  end
  
  # relatively few connections compared to number of nodes
  def sparse?
    # number of edges is less than number of nodes squared
    nedges < (@nodes.size**2)
  end
  
  # relatively many connections compared to number of nodes
  def dense?
    !sparse?
  end
  
  # add a node... no duplicates
  def << a
    unless @nodes.include? a
      uncache
      @nodes << a
    end
  end
  
  # add nodes
  def push *args
    args.each{|a| self << a}
  end
  
  # make an edge connecting a and b
  def connect a, b, value=nil
    # TODO: are self loops okay?
    # TODO: this doesn't allow parallel edges?
    
    self << a
    self << b
    
    edge = Edge.new(a, b, value)
    
    # initialize edge list for nodes a and b
    @edges[a] ||= []
    @edges[b] ||= []
    
    # check if this edges already exists
    @edges[a] << edge unless @edges[a].include? edge
    @edges[b] << edge unless @edges[b].include? edge
    
    uncache
  end
  
  # remove connection from a to b
  def disconnect a, b
    if e = edge(a, b)
      @edges[a].delete e if @edges[a]
      @edges[b].delete e if @edges[b]
      uncache
      e # return value
    end
  end
  
  # delete node a and any edges connecting it
  def delete a
    # delete connections from node a
    @edges.delete a
    
    # delete connection to node a
    @edges.each{|x,l| l.delete_if{|e| e.include? a}}
    
    # delete node
    @nodes.delete a
  end
  
  # find the edge connecting a to b
  def edge a, b
    # works for both digraph and undirected graph
    @edges[a].each{|e| return e if e.connects? a, b}
  end
  
  # find edges to/from node a
  def edges a, direction=:both
    # this is specific to undirected graph!
    @edges[a]
  end
  
  # return nodes directly connected to node a
  def neighbors a, direction=:both
    (edges a, direction).map{|e| e.other a}
  end
  
  # override SearchSpace
  def successors a
    neighbors a, :out
  end
  
  # override SearchSpace
  def predecessors a
    neighbors a, :in
  end
  
  # number of nodes connected to node a
  def degree a, direction=:both
    (edges a, direction).size
  end
  
  # find nodes with no incoming edges
  def sources
    @nodes.select{|x| (edges x, :in).empty?}
  end
  
  # find nodes with no outgoing edges
  def sinks
    @nodes.select{|x| (edges x, :out).empty?}
  end
  
  # reverse all the edges of this graph
  def reverse!
    edges = {}
    
    @edges.each do |x,l|
      l.each do |e|
        edges[e.b] = e.reverse!
      end
    end
    
    @edges = edges
    uncache
  end
  
  # new graph with all the edges reversed
  def reverse
    dup.reverse!
  end
  
  # create adjacency matrix (hash)
  def matrix
    # cached matrix
    return @matrix if @matrix
    
    matrix = {}
    
    @edges.each do |e|
      # edge from a to b
      if matrix[e.a]
        matrix[e.a][e.b] = true
      else
        matrix[e.a] = {e.b => true}
      end
      
      # edge from b to a
      if matrix[e.b]
        matrix[e.b][e.a] = true
      else
        matrix[e.b] = {e.a => true}
      end
    end
    
    @matrix = matrix
  end
  
  # return the transitive closure of this digraph
  def warshall
    return @warshall if @warshall
    
    graph = dup
    
    # connect each node to itself
    graph.nodes.each{|x| graph.connect x, x}
    
    graph.nodes.each do |a|
      graph.nodes.each do |b|
        if graph.matrix[b][a]
          
          graph.nodes.each do |c|
            graph.connect b, c if graph.matrix[a][c]
          end
          
        end
      end
    end
    
    @warshall = graph
  end

  # return set of disjoint graph components
  def components
  end
  
  # find minimum spanning tree
  def prim
  end

  # find minimum spanning tree in O(E log E) == O(E log V)
  def kruskal
  end
  
  # find minimum spanning tree
  def reverse_delete
  end
  
  # find mimimum spanning tree in O(E log V)
  def boruvka
  end
  
  # find minimum spanning tree in nearly O(E)
  def chazelle
  end
  
end

class ADT::Graph::Edge
  attr_reader :a, :b, :value
  attr_writer :value
  
  def initialize a, b, value=nil
    @a, @b, @value = a, b, value
  end
  
  # the other node connected to node a with this edge
  def other a
    @a.hash == a.hash ? @b :
    @b.hash == a.hash ? @a : nil
  end
  
  # true if this edge connects a and b
  def connects? a, b
    (@a.hash == a.hash and @b.hash == b.hash) or
    (@a.hash == b.hash and @b.hash == a.hash)
  end
  
  # true if this edge connects node a to some other node
  def include? a
    @a.hash == a.hash or @b.hash == a.hash
  end
  
  def inspect
    value.nil? ?
      "<#{a.inspect} #{b.inspect}>" :
      "<#{value.inspect}|#{a.inspect} #{b.inspect}>"
  end
  
  # compare two edges by their nodes
  def == other
    (@a.hash == other.a.hash and @b.hash == other.b.hash) or
    (@a.hash == other.b.hash and @b.hash == other.a.hash)
  end
  
  # defined for Array#uniq
  alias eql? ==
  
  # some trickery so <a b>.hash == <b a>.hash
  def hash
    a = @a.hash
    b = @b.hash
    keys = [a, b].sort
    (keys.push value).hash
  end
  
  # reverse the direction of this edge
  def reverse!
    @a, @b = @b, @a
    self
  end
  
  # new edge with nodes reversed
  def reverse
    dup.reverse!
  end
  
end

class ADT::Graph::Path
  attr_writer :edges, :nodes
  attr_reader :edges, :nodes
  
  # create path from list of edges
  def self.edges *args
    path = allocate
    
    if args.size > 1
      # first node is in first edge but not second edge
      first = (args[0].other args[1].a) || (args[0].other args[1].b)
      last  = (args[-1].other args[-2].a) || (args[-1].other args[-2].b)
    else
      first = args.first.a
      last  = args.first.b
    end
    
    path.edges = args
    path.nodes = [first]
    
    args.each do |e|
      # next node is on the other end of the edge
      path.nodes.push e.other(path.nodes.last)
    end
    
    path
  end
  
  # create path from list of nodes
  def self.nodes *args
    path = allocate
    
    path.nodes = args
    path.edges = []
    
    if args.size > 1
      # TODO: need Graph#edge for this
    else
      # no edges
    end
  end
  
  # iterate each node
  def each_node
    @nodes.each{|n| yield n}
  end
  
  # iterate each edge
  def each_edge
    @edges.each{|e| yield e}
  end
  
  # last node in the path (finish)
  def first
    @nodes.first
  end
  
  # first node in the path (start)
  def last
    @nodes.last
  end
  
  def inspect
    "#{first.inspect} to #{last.inspect}, length: #{length}"
  end
  
  # true if path begins at the destination
  def cycle?
    first.hash == last.hash
  end
  
  # path length
  def length
    @edges.size
  end
  alias size :length
  
  # true if vertices and edges are distinct
  def simple?
    @nodes.uniq.size == @nodes.size ?
      # true if no duplicate edges
      @edges.uniq.size == @edges.size :
      false # false because of duplicate nodes
  end
  
  # true if no vertices in common (ignore end points)
  def disjoint? other
    yours = other.nodes[1..-2].map{|x| x.hash}
    mine  = @nodes[1..-2].map{|x| x.hash}
    
    # false if your path has one of my nodes
    mine.each{|a| return false if yours.include? a }
    
    # no matches
    true
  end
  
end

if true # $0 == __FILE__
  $g = ADT::Graph.new
  
  # city lon lat
  $cities = {'Atlanta'       => [ 84.23, 33.45],
             'Boston'        => [ 71.05, 42.21],
             'Chicago'       => [ 87.37, 41.50],
             'Denver'        => [105.00, 39.45],
             'Eugene'        => [123.05, 44.03],
             'Flagstaff'     => [111.41, 35.13],
             'Grand Junction'=> [108.37, 39.05],
             'Houston'       => [105.00, 34.00],
             'Indianapolis'  => [ 86.10, 39.46],
             'Jacksonville'  => [ 81.40, 30.22],
             'Kansas City'   => [ 94.35, 39.06],
             'Los Angeles'   => [118.15, 34.03],
             'Memphis'       => [ 90.03, 35.09],
             'New York'      => [ 76.58, 40.47],
             'Oklahoma City' => [ 97.28, 35.26],
             'Pittsburgh'    => [ 76.57, 40.27],
             'Quebec'        => [ 97.28, 35.26],
             'Reno'          => [119.49, 39.30],
             'San Francisco' => [122.26, 37.47],
             'Tampa'         => [ 82.27, 27.57],
             'Victoria'      => [123.21, 48.25],
             'Wilmington'    => [ 77.57, 34.14]}
  
  def deg2rad a
    # return a-degrees in radians
    (Math::PI/180.0) * (a.floor + (a%1)*100.0/60.0)
  end
  
  def xyz lon, lat
    # center is (0 0 0) north pole is (0 0 1)
    psi = deg2rad lat
    phi = deg2rad lon
    
    [Math.cos(psi)*Math.cos(phi),
     Math.cos(psi)*Math.sin(phi),
     Math.sin(psi)]
  end
  
  def distance a, b
    # eucildean distance in n-dimensional space
    Math.sqrt(a.zip(b).inject(0){|d,p| d += (p.first-p.last)**2 })
  end
  
  def earth_distance a, b
    12765.0 * Math.asin(0.5 * (distance a, b))
  end
  
  def $g.successors a
    $cities.keys.select do |b|
      a != b and (earth_distance xyz(*$cities[a]), xyz(*$cities[b])) < 1000
    end
  end

  $a = ADT::Graph.new
  $a.connect 0, 7
  $a.connect 0, 5
  $a.connect 0, 2
  $a.connect 2, 6
  $a.connect 6, 4
  $a.connect 4, 7
  $a.connect 4, 5
  $a.connect 5, 3
  $a.connect 3, 4
  $a.connect 7, 1
end
