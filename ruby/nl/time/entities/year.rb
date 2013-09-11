#!/usr/bin/env ruby
# $Hg: year.rb 85c6b6949294 2008-02-06 00:40 -0600 kputnam $

module NLTime

class Entity::Year < Entity
  include Numeric
  
  # convert two-digit year to four-digit year
  def self.convert(n, year=::Time.now.year)
    return n if n.between?(1000, 9999)
    
    # TODO make range configurable
    # this is 69 years ago or 30 years ahead
    year + ((n+(69 - year.modulo(100))).modulo(100) - 69)
  end
  
  def self.first(entity)
    case entity
    when NLTime::Entity::Decade
      new(entity * 10)
    when NLTime::Entity::Century
      new((entity-1) * 100)
    when NLTime::Entity::Millenium
      new((entity-1) * 1000 + 1)
    else
      raise ArgumentError
    end
  end
  
  def initialize(number)
    raise ArgumentError unless number.between?(1000,4000)
    @value = number
    super(((common?) ? 365 : 366).days)
  end
  
  def millenium
    Millenium.new((to_i-1).div(1000) + 1)
  end
  
  def century
    Century.new(to_i.div(100) + 1)
  end
  
  def decade
    Decade.new(to_i.div(10))
  end
  
  def easter(method=3)
    # TODO clean up
    g = year % 19
    c = year.div(100)
    case method
    when 1, 2
      i = (19*g+15) % 30
      j = (year+year.div(4)) % 7
      j += 10 + ((year > 1600) ? c - 16 - (c-16).div(4) : 0) if method == 2
    when 3
      h = (c - c.div(4) - (8*c+14).div(25) + 19*g + 15) % 30
      i = h - h.div(28) * (1 - h.div(28) * 29.div(h+1)) * (21-g).div(11)
      j = (year + year.div(4) + i + 2 - c + c.div(4)) % 7
    end
    p = i - j
    d = 1 + (p + 27 + (p+6).div(40)) % 31
    m = 3 + (p + 26).div(30)
    
    NLTime::Entity::Date.civil(self, m, d)
  end
  
  # true if this is a leap year
  def leap?
    not common?
  end
  
  # true if this is not a leap year
  def common?
    # not div by 4, or div by 100 but not 400
    !to_i.modulo(4).zero? || (to_i.modulo(100).zero? && !to_i.modulo(400).zero?)
  end
  
  nltime_implies :decade, :century, :millenium
  nltime_memoize {|y| y = y.to_i; [y, y] }
end

end
