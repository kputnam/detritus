#!/usr/bin/env ruby
# $Hg: tokenize.rb 91edd4a393cb 2008-02-08 14:31 -0600 kputnam $
require 'kputnam/tokenizer'

module NLTime

class Tokenizer < Tokenizer
  # %{!"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~}
  
  apunc = Regexp.escape %{,.@='"`([\{</}
  bpunc = Regexp.escape %{./-:,}
  cpunc = Regexp.escape %{,.?!'"`)]\}>/-}
  
  # match ordinals
  tokenize(/(\s*[#{apunc}]*?)([+-]?[0-9]+)(st|nd|rd|th)([#{bpunc}]*\s*)/) do |m|
    a,b,c = m.captures[0], "#{m.captures[1]}#{m.captures[2]}", m.captures[3]
    NLTime::Token.new([a, b, c], :ordinal, m.captures[1].to_i)
  end
  
  # match numbers
  tokenize(/(\s*[#{apunc}]*?)([+-]?[0-9]+[0-9#{bpunc}]*)([#{cpunc}]*\s*)/) do |m|
    # FIXME there's a better way to do this
    a = m.captures[0]
    b, c = m.captures[1].match(/\A(.+?)([#{cpunc}]*)\z/).captures
    c << m.captures[2]
    
    token = NLTime::Token.new([a, b, c], :numeric)
    if m = b.match(/^([+-]?)(?:0*)(\d+(\.\d+)?)$/)
      if m.captures.last
        # looks like a Float
        token.tag(m.values_at(1,2).join.to_f)
      else
        # looks like an Integer
        token.tag(m.values_at(1,2).join.to_i)
      end
    end
    token
  end
  
  dpunc = Regexp.escape %{,.'"`-([\{<}
  epunc = Regexp.escape %{.'}
  fpunc = Regexp.escape %{,.'"`-)]\}>}
  
  # match words
  tokenize(/(\s*[#{dpunc}]*)([a-z]+[a-z#{epunc}]*)([#{fpunc}]*\s*)/i) do |m|
    # FIXME there's a better way to do this
    a = m.captures[0]
    b, c = m.captures[1].match(/\A(.+?)([#{cpunc}]*)\z/).captures
    c << m.captures[2]
    
    NLTime::Token.new([a, b, c], :word)
  end
  
  # munch white space
  #tokenize(/\s+/) do |m|
  #  NLTime::Token.new(m.to_s, :skip)
  #end
  
end

class Token < Token
  attr_accessor :before, :after
  
  def initialize(word, *tags)
    if word.kind_of?(Array)
      raise ArgumentError unless word.size == 3
      @before, @after, word = word[0].to_s, word[2].to_s, word[1].to_s
    else
      @before = @after = ''
    end
    super(word, *tags)
  end
  
  def prepend(*tokens)
    tokens.each do |t|
      @before = t.to_s + @before
    end
  end
  
  def append(*tokens)
    tokens.each do |t|
      @after << t.to_s
    end
  end
  
  def to_s
    "#{before}#{word}#{after}"
  end
end

end
