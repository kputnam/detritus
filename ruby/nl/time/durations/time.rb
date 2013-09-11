#!/usr/bin/env ruby
# $Hg: time.rb 57ae291ab1d4 2008-01-05 17:41 -0600 kputnam $

module NLTime

class Duration::Time
  # the smallest instant of time: hour, minute, second, offset
  def initialize(*args)
    @args = args
    raise ArgumentError unless hour.between?(0, 24)
    raise ArgumentError unless minute.between?(0, 59)
    raise ArgumentError unless second.between?(0, 60) and second < 60
  end
  
  # mock methods
  def hour;   @args[0].to_i end
  def minute; @args[1].to_i end
  def second; @args[2].to_f end
  def offset; @args[3]      end
  alias min minute
  alias sec second
  
  def hour=(n)   @args[0] = n end
  def minute=(n) @args[1] = n end
  def second=(n) @args[2] = n end
  def offset=(n) @args[3] = n end
  alias min= minute=
  alias sec= second=
    
  def inspect
    "#<Duration:#{self.class.to_s.split('::').last} #{@args.inspect}>"
  end
end

# TODO this isn't really a duration, move somewhere else?
class Duration::Offset
  def initialize(*args)
    @args = args
    # check -12:00 to +12:00
    raise ArgumentError unless hour.between?(-12, 14)
    raise ArgumentError unless minute.between?(0, 59)
  end
  
  # mock methods
  def hour;   @args[0].to_i end
  def minute; @args[1].to_i end
  def second; @args[2].to_f end
  alias min minute
  alias sec second
  
  def hour=(n)   @args[0] = n end
  def minute=(n) @args[1] = n end
  def second=(n) @args[2] = n end
  alias min= minute=
  alias sec= second=
    
  def inspect
    "#<Duration:#{self.class.to_s.split('::').last} #{@args.inspect}>"
  end
end

end
