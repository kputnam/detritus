#!/usr/bin/env ruby
# $Hg$
require 'kputnam/tokenizer'

module NLTime

# FormatSpec can be used to parse dates with a consistent format, ex from
# log files. The format string is similar to strftime and sprintf, and it
# is parsed to create a regex.

# Care must be taken not to use capture groups in the format string, since
# these will not be recognized by the parser, and will cause problems when
# parsing the text. Otherwise, all regex syntax is allowed in format string.

# There is not yet any way to extract info from ambiguous dates or times,
# so this is only useful if a year %Y or %y is given, and some information
# to determine a date, month, week, etc. Time information (%H %M %S %p) is
# only used when an exact date can be determined.

class FormatSpec
  attr_reader :regex
  
  FORMAT = { 'a' => ['[A-z]', 2, 9], # day name
             'b' => ['[A-z]', 3, 9], # month name
             'd' => ['[0-9]', 1, 2], # day of month
             'H' => ['[0-9]', 1, 2], # hour
             'j' => ['[0-9]', 1, 3], # day of year
             'M' => ['[0-9]', 1, 2], # minute
             'N' => ['(st|nd|rd|th)'], # ordinal suffix
             'm' => ['[0-9]', 1, 2], # month
             'o' => ['(?:[+-])[0-9]{1,2}:?[0-9]{2}'], # offset
             'p' => ['(?:a|A|p|P)(?:\.?(?:m|M\.?)?)?'], # meridiem
             'S' => ['[0-9]', 1, 2], # second
             'U' => ['[0-9]', 1, 2], # week of year
             'w' => ['[0-9]', 1, 2], # day of week
             'y' => ['[0-9]', 2, 4], # year
             'Y' => ['[0-9]', 2, 4], # year
             'Z' => nil } # timezone
  
  def initialize(format)
    regexp    = ''
    @captures = []
    @format   = format
    
    # create the whole regex
    Tokenizer.new.tokenize(format).each do |t|
      regexp << t.word
      if c = t.fetch(Symbol)
        @captures << c
      end
    end
    @regex = /#{regexp}/
    
    # compute date
    if y = @captures.index(:y) || y = @captures.index(:Y)
      if m = @captures.index(:m) || m = @captures.index(:b)
        if d = @captures.index(:d)
          @date = Proc.new do |captures|
            NLTime::Entity::Date.civil(captures[y].to_i, captures[m], captures[d].to_i)
          end
        else
          @date = Proc.new do |captures|
            NLTime::Entity::Month.new(captures[m],
                                      NLTime::Entity::Year.new(captures[y].to_i))
          end
        end
        
      elsif w = @captures.index(:U)
        if d = @captures.index(:w) || d = @captures.index(:a)
          # 
        end
      
      elsif d = @captures.index(:j)
        @date = Proc.new do |captures|
          NLTime::Entity::Date.ordinal(captures[y].to_i, captures[d].to_i)
        end
      else
        @date = Proc.new do |captures|
          NLTime::Entity::Year.new(captures[y].to_i)
        end
      end
    end
    
    # compute time
    if h = @captures.index(:H)
      if n = @captures.index(:M)
        if s = @captures.index(:S)
          @time = Proc.new do |captures|
            NLTime::Duration::Time.new(captures[h].to_i, captures[n].to_i, captures[s].to_i)
          end
        else
          #@time = ...
        end
      else
        #@time = ...
      end
    end
    
  end
  
  # return all mached dates/times
  def parse(string)
    string.scan(@regex).map do |captures|
      if @date
        date = @date.call(captures)
        if date.kind_of?(NLTime::Entity::Date) and @time
          time = @time.call(captures)
          NLTime::Entity::Time.new(time, date)
        else
          date
        end
      else
        @time.call(m.captures) if @time
      end
    end
  end
  
  def inspect
    "<FormatSpec @format=#{@format}>"
  end
end

class FormatSpec::Tokenizer < ::Tokenizer
  # directive
  tokenize(/%( |0)?(\d*)([#{FormatSpec::FORMAT.keys.join}])/) do |m|
    pad, length, field = m.captures
    pad = "(?:#{pad}*)" if pad
    
    unless length.any? and length.to_i.between?(*FormatSpec::FORMAT[field][1..2])
      if FormatSpec::FORMAT[field].size > 1
        length = "{#{FormatSpec::FORMAT[field][1..2].join(',')}}" # default
      end
    else
      length = "{#{length}}"
    end
    
    # token.word is a regex string
    Token.new("#{pad}(#{FormatSpec::FORMAT[field].first}#{length})", field.to_sym)
  end
  
  # literal % symbol
  tokenize(/%%/) do
    Token.new('%%')
  end
  
  # literal charaters
  tokenize(/[^%]+/) do |m|
    Token.new(m.to_s)
  end
end

end
