#!/usr/bin/env ruby
# $Hg: century.rb d500ed9fe66b 2008-02-05 23:46 -0600 kputnam $

module NLTime

class Entity::Century < Entity
  include Numeric
  
  def self.first(millenium)
    raise ArgumentError unless millenium.kind_of?(NLTime::Entity::Millenium)
    new(1 + 10*(millenium-1))
  end
  
  # 20th century: years 1901 - 2000
  def initialize(number)
    raise ArgumentError unless number.between?(0,40)
    @value = number
    super()
  end
  
  def millenium
    Millenium.new((@value-1).div(10) + 1)
  end
  
  nltime_implies :millenium
  nltime_memoize {|c| c = c.to_i; [c, c] }
end

end
