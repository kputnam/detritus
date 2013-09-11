#!/usr/bin/env ruby
# $Hg: millenium.rb 67d3033d3644 2008-02-05 23:55 -0600 kputnam $

module NLTime

class Entity::Millenium < Entity
  include Numeric
  
  # 1st millenium: years 0001 - 1000
  def initialize(number)
    raise ArgumentError unless number.between?(0,4)
    @value = number
    super()
  end
  
  nltime_memoize {|m| m = m.to_i; [m, m] }
end

end
