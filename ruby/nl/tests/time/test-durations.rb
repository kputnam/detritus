#!/usr/bin/env ruby
# $Hg: test-durations.rb e2b89147ca00 2008-02-06 00:55 -0600 kputnam $
require 'test/unit'
require 'mock/mock-entities'
require 'mock/mock-durations'
require '../../time/durations'

class TestDuration < Test::Unit::TestCase
  def setup
    @g = {}
    @m = Mock.new('ADT::DiGraph', Class)
    
    @m.implement(:connect) do |from, to, value|
      @g[from] ||= {}
      @g[from][to] = value
    end
    
    @m.implement(:edge) do |from, to|
    end
    
    @m.implement(:bfs) do |from| #,&block|
    end
  end
  
  def teardown
    @m.revert
  end
end
