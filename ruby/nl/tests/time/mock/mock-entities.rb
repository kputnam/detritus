#!/usr/bin/env ruby
# $Hg: mock-entities.rb e2b89147ca00 2008-02-06 00:55 -0600 kputnam $

path = File.dirname(File.expand_path(__FILE__))

module NLTime
  class Interval
  end
  class Entity < Interval
    class << self
      def nltime_implies(*args)
      end
      def nltime_memoize(*args)
      end
    end
    def initialize(*args)
    end
    
    module Graph
    end
    module Numeric
      def to_i() @value.to_i end
      def to_f() @value.to_f end
      def difference(other) self.to_i - other.to_i end
    end
  end
end

Dir.glob("#{path}/../../time/entities/*rb").each do |e|
  e = File.basename(e).split('.').first.capitalize
  k = Class.new(NLTime::Entity)
  
  unless NLTime::Entity.const_defined?(e)
    NLTime::Entity.const_set(e, k)
  end
end
