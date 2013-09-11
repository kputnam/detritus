#!/usr/bin/env ruby
# $Hg: reducer.rb 4aaf378b7050 2008-01-10 04:18 -0600 kputnam $

class Reducer
  attr_reader :options
  
  def initialize(options={})
    @options = {:verbose => false}.merge(options)
  end
  
  # APPLE BANANA CRANBERRY FIG GRAPE HONEYDEW KIWI LIME MANGO
  # |---- ------ --------- --- ----- -------- ---- ---- -----
  #       |----- --------- --- ----- -------- ---- ---- -----
  #              |-------- --- ----- -------- ---- ---- -----
  #                        |-- ----- -------- ---- ---- -----
  #                            |---- -------- ---- ---- -----
  def rreduce(tokens)
    # NOTE scans from right to left
    size  = tokens.size
    match = 1
    
    while match > 0  # last loop had any effect
      match = k = 0
      
      while k < tokens.size
        catch :done do
          self.class.reducers.each do |tags, block|
            # not enough tokens available
            next unless tokens.size - k >= tags.size
            
            mtokens = tokens[k, tags.size]
            
            # next unless all tags match
            next unless catch :fail do
              mtokens.zip(tags).each do |token,tag|
                case tag
                when Symbol, Module
                  throw :fail unless token.include?(tag)
                when String
                  throw :fail unless token.word == tag
                when Regexp
                  throw :fail unless token.word =~ tag
                when Proc
                  throw :fail unless tag.call(token)
                when nil
                  # matches any token
                else
                  throw :fail unless token.fetch(tag.class) == tag
                end
              end
            end
            
            object = block.call(self, *mtokens)
            if object.kind_of?(Token)
              puts "@#{k}: OK #{tags.inspect}" if options[:verbose]
              
              # restart parsing having replaced many tokens with one
              tokens[k, tags.size] = object
              # restart at beginning of array
              k = -1; match += 1
              throw :done
            else
              puts "@#{k}: no #{tags.inspect}" if options[:verbose]
            end
          end
        end
      
        k += 1
      end
      
    end
    tokens
  end
  
  # APPLE BANANA CRANBERRY FIG GRAPE HONEYDEW KIWI LIME MANGO
  # ----- ------ --------- --- ----- -------- ---- ---- ----|
  # ----- ------ --------- --- ----- -------- ---- ---|
  # ----- ------ --------- --- ----- -------- ---|
  # ----- ------ --------- --- ----- -------|
  def lreduce(tokens)
    # NOTE scans from right to left
    size  = tokens.size
    match = 1
    
    while match > 0  # last loop had any effect
      match = 0; k = tokens.size
      
      while k > 0
        catch :done do
          self.class.reducers.each do |tags, block|
            # not enough tokens available
            next if tokens.size - (k - 1) < tags.size
            mtokens = tokens[k-1, tags.size]
            
            # next unless all tags match
            next unless catch :fail do
              mtokens.zip(tags).each do |token,tag|
                case tag
                when Symbol, Module, Class
                  throw :fail unless token.include?(tag)
                when String
                  throw :fail unless token.word == tag
                when Regexp
                  throw :fail unless token.word =~ tag
                when Proc
                  throw :fail unless tag.call(token)
                when nil
                  # matches any token
                else
                  throw :fail unless token.fetch(tag.class) == tag
                end
              end
            end
            
            puts "@#{k}: #{tags.inspect}" if options[:verbose]
          
            object = block.call(*mtokens)
            if object.kind_of?(Token)
              # restart parsing having replaced many tokens with one
              tokens[k-1, tags.size] = object
              match += 1
              throw :done
            end
          end
        end
      
        k -= 1
      end
    
    end
    tokens
  end
  
  class << self
    attr_reader :reducers
    
    def reduce(*args, &block)
      (@reducers ||= []) << [args, block]
    end
  end
end
