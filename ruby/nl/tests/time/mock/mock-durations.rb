#!/usr/bin/env ruby
# $Hg: mock-durations.rb e2b89147ca00 2008-02-06 00:55 -0600 kputnam $

path = File.dirname(File.expand_path(__FILE__))

module NLTime
  class Duration
    class << self
      def nltime_equals(*args)
      end
      def nltime_memoize(*args)
      end
    end
    def initialize(*args)
    end
  end
  module Graph
  end
end

Dir.glob("#{path}/../../time/durations/*rb").each do |e|
  e = File.basename(e).split('.').first.capitalize
  k = Class.new(NLTime::Duration)
  
  unless NLTime::Duration.const_defined?(e)
    NLTime::Duration.const_set(e, k)
  end
  
  e.downcase!
  Numeric.class_eval do
    define_method(e) { k.new(self) }
    alias_method(e =~ /^(.+[^a])y$/ ? "#{$1}ies" : "#{e}s", e)
  end
end
