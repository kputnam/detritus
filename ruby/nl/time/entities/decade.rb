#!/usr/bin/env ruby
# $Hg: decade.rb 7c9a4edfb297 2008-02-05 23:47 -0600 kputnam $

module NLTime

class Entity::Decade < Entity
  include Numeric
  
  def self.first(century)
    raise ArgumentError unless century.kind_of?(NLTime::Entity::Century)
    new((century - 1) * 10)
  end
  
  # 190th: years 1900 - 1909
  def initialize(number)
    raise ArgumentError unless number.between?(0,400)
    @value = number
    super()
  end
  
  def century
    Century.new(to_i.div(10) + 1)
  end
  
  nltime_implies :century
  nltime_memoize {|d| d = d.to_i; [d, d] }
end

end
