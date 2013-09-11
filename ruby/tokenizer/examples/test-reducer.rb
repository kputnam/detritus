#!/usr/bin/env ruby
# $Hg: test-reducer.rb e114f2055668 2008-01-12 00:25 -0600 kputnam $
require 'pp'
require 'kputnam/tokenizer'
require 'kputnam/tokenizer/reducer'

class T < Tokenizer
  tokenize(/(["'])([a-z]+)\1/i) do |m|
    Token.new(m.captures[1], :string)
  end
  
  tokenize(/\:[a-z]+/i) do |m|
    Token.new(m.to_s, :symbol)
  end
  
  tokenize(/[a-z]+/i) do |m|
    Token.new(m.to_s, :identifier)
  end
  
  tokenize(/[+-]?\d+(?:\.\d+)/) do |m|
    Token.new(m.to_s, :number)
  end
  
  tokenize(/\s+/) do |m|
    Token.new(m.to_s, :skip)
  end
end

class Q < Reducer
  reduce(:skip, nil){|a,b| b }
  reduce(nil, :skip){|a,b| a }
end

class R < Reducer
  reduce(:symbol, :number) do |s,n|
    Token.new('XXZZY', :string)
  end
  
  reduce(:identifier, :string) do |a,b|
    Token.new('hola mundo', :expr)
  end
  
  reduce(:expr, :string) do |a,b|
    Token.new('ALL DONE', :stop)
  end
end

string = %{alpha "beta" :gamma 0.9855}
pp tokens = T.new.tokenize(string, true)
pp tokens = Q.new.reduce(tokens)
pp tokens = R.new.reduce(tokens)
