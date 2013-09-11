#!/usr/bin/env ruby
dir = File.dirname(File.expand_path(__FILE__))
$:.push(dir) unless $:.include?(dir)
require 'graph.rb'

module ADT

class DirectedGraph < Graph
  
  # make an edge connecting a TO b (but not b TO a)
  def connect a, b, value=nil
    @nodes << a unless @nodes.include? a
    @nodes << b unless @nodes.include? b
    
    unless edge a, b
      uncache
      @edges[a] ||= {}
      @edges[a][b] = Edge.new(a, b, value)
    end
  end
  
  # disconnect edge from a to b
  def disconnect a, b
    if @edges[a]
      @edges[a].delete(b)
      uncache
    end
  end
  
  # find the edge connecting a and b
  def edge a, b
    @edges[a] and @edges[a][b]
  end
  
  # return edges exiting/entering given node
  def edges a, direction=:out
    case direction
    when :out
      # a TO somewhere else
      (@edges[a] || {}).values
    when :in
      # somewhere TO a
      @edges.map{|b,list| list.map{|x,e| e if x.hash == a.hash}}.flatten.compact
    else
      (edges a, :in).push *(edges a, :out)
    end
  end
  
  # number of in/out-bound edges connecting node a
  def degree a, direction=:out
    case direction
    when :in
      @edges.inject(0){|count,e| (e.b.hash == a.hash) ? count+1 : count}
    when :out
      @edges.inject(0){|count,e| (e.a.hash == a.hash) ? count+1 : count}
    else
      # count both incoming and outgoing edges
      super a
    end
  end
  
  # true, this is a directed graph
  def directed?
    true
  end
  
  # create adjacency matrix (hash)
  def matrix
    return @matrix if @matrix
    
    matrix = {}
    
    @edges.each do |e|
      if matrix[e.a]
        matrix[e.a][e.b] = true
      else
        matrix[e.a] = {e.b => true}
      end
    end
    
    @matrix = matrix
  end
  
end

class DirectedGraph::Edge < Graph::Edge
  alias :source :a
  alias :dest   :b
  
  # true if this edge connects a TO b
  def connects? a, b
    (@a.hash == a.hash and @b.hash == b.hash)
  end
  
  # compare two edges, override bidirectional edge ==
  def == other
    # a TO b != b TO a
    (@a.hash == other.a.hash and @b.hash == other.b.hash)
  end
  
  # defined for Array#uniq
  alias eql? ==
  
  # no trickery, <a b>.hash != <b a>.hash
  def hash
    [a, b, value].hash
  end
  
  def inspect
    value.nil? ?
      "<#{a.inspect} => #{b.inspect}>" :
      "<#{value.inspect}|#{a.inspect} => #{b.inspect}>"
  end
  
end

DiGraph = DirectedGraph

end

if $0 == __FILE__
  $u = ADT::DiGraph.new
  $net = [
    [], # list of outgoing connections
    [2],
    [3,10],
    [11],
    [3,12],
    [4],
    [5,14],
    [6,15,8],
    [16],
    
    [1,10,17],
    [18],
    [10,12],
    [13],
    [5],
    [13],
    [14,16],
    [24],
    
    [18,25],
    [],
    [11,18,20,27],
    [12],
    [20,13],
    [14,21,30],
    [22,15,24],
    [32],
    
    [],
    [25,18,34],
    [26,28],
    [20,29],
    [21,30],
    [31,38],
    [23,32,39],
    [40],
    
    [25,34,41],
    [42],
    [34,43,27],
    [35,28],
    [36,29,38],
    [],
    [38,40],
    [48],
    
    [49],
    [41,50,43],
    [44],
    [36],
    [44,37,46],
    [38,47,54],
    [39,48],
    [56],
    
    [50],
    [51,58],
    [43,59],
    [51,44,53],
    [45,54],
    [],
    [54,47],
    [55,64],
    
    [49,58],
    [59],
    [],
    [59,52,61],
    [53],
    [61,54,63],
    [55],
    [63]
  ]
  
  $net.each_with_index do |list, u|
    list.each{|v| $u.connect u, v}
  end
end
