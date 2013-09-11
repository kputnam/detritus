#!/usr/bin/env ruby

class Tokenizer
  attr_reader :options
  
  def initialize(options={})
    @options = {:strict => false}.merge(options)
  end
  
  def tokenize(string)
    # NOTE this scans right to left
    # TODO allow recursion for context?
    
    tokens = []
    size   = string.size
    fail   = ''
    
    while not string.empty?
      catch :done do
        self.class.tokenizers.each do |regexp, block|
          if m = string.match(regexp)
            token  = block.call(m)
            unless options[:strict] or fail.empty?
              tokens << Token.new(fail, :fail)
              fail = ''
            end
            if token.kind_of?(Token)
              tokens << token
              string = string[m.to_s.size..-1]
              throw :done
            end
          end
        end
        
        raise "fail at char #{size-string.size}: #{string[0..0]}" if options[:strict]
        fail << string.slice!(0).chr
      end
    end
    tokens << Token.new(fail, :fail) unless fail.empty?
    tokens
  end
  
  class << self
    attr_reader :tokenizers
    
    def tokenize(regexp, &block)
      (@tokenizers ||= []) << [Regexp.new("\\A#{regexp}"), block]
    end
  end
end

class Token
  attr_accessor :word
  
  def initialize(word, *tags)
    @word = word
    @tags = tags
  end
  
  # add one or more tags
  def tag(*keys)
    @tags.unshift(*keys)
  end
  
  # get tags of given class
  def fetch(klass, exact=false)
    test = exact ? :instance_of? : :kind_of?
    @tags.each{|t| return t if t.send(test, klass) }; nil
  end
  
  # remove a tag of a given class, or symbol
  def untag(key, exact=false)
    test = exact ? :instance_of? : :kind_of?
    case key
    when Symbol
      @tags.delete(key)
    when Module
      @tags.reject!{|t| t.send(test, key) }
    else
      @tags.reject!{|t| t == key }
    end
  end
  
  # test if tag of given class or symbol exists
  def include?(key, exact=false)
    test = exact ? :instance_of? : :kind_of?
    case key
    when Symbol
      @tags.include?(key)
    when Module
      @tags.any?{|t| t.send(test, key) }
    else
      @tags.any?{|t| t == key }
    end
  end
  
  # remove all tags
  def clear!
    @tags.clear
  end
  
  # true if more than 0 tags exist
  def tagged?
    @tags.any?
  end
  
  def inspect
    "#<Token:#{word} #{@tags.inspect}>"
  end
end
