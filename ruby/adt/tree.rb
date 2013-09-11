#!/usr/bin/env ruby

class ADT::Forest < ADT::DiGraph
end

class ADT::Tree < ADT::DiGraph
  # return direct descendants of node a
  def children a
    neighbors a, :out
  end
  
  # return direct ancestors of node a
  def parents a
    neighbors a, :in
  end
  
  def kosaraju
  end
  
  def tarjan
  end
  
  def gabow
  end
  
end

class ADT::BST < ADT::Tree
end

class ADT::Heap
  def initialize type=:min
  end
  
  def push
  end
  
  def pop
  end
  
  def min
  end
  
  def max
  end
  
  def demote e
  end
  
  def promote e
  end
  
end
