#!/usr/bin/env ruby
%w[tokenizer reducer].each{|r| require "#{File.dirname __FILE__}/../#{r}"}

class SimpleTokenizer < Tokenizer
  tokenize(/[=+*<>&|-]{2}|[*\/<>%^&+|-]/) do |m|
    Token.new(m.to_s, :operator)
  end
  
  tokenize(/=/) do |m|
    Token.new(m.to_s, :assign)
  end
  
  tokenize(/(\d+(?:\.\d+)?)/) do |m|
    Token.new(m.captures.first, :number)
  end
  
  tokenize(/(["'])([a-z]+)\1/) do |m|
    Token.new(m.captures[1], :string)
  end
  
  tokenize(/[a-z]+/i) do |m|
    Token.new(m.to_s, :symbol)
  end
  
  tokenize(/[;\n]+|\z/) do |m|
    Token.new(m.to_s, :stop)
  end
  
  tokenize(/,/) do
    Token.new(',', :comma)
  end
  
  tokenize(/\s+/) do |m|
    Token.new(m.to_s, :skip)
  end
  
end

class SimpleReducer < Reducer
  reduce(:skip, nil) do |skip, token|
    token
  end
  
  reduce(nil, :skip) do |token, skip|
    token
  end
  
  reduce(:number, :operator, :symbol) do |a, op, b|
    Token.new("#{a.word} #{op.word} #{b.word}", :expr)
  end
  
  reduce(:number, :operator, :number) do |a, op, b|
    Token.new("#{a.word} #{op.word} #{b.word}", :expr)
  end
  
  reduce(:symbol, :operator, :number) do |a, op, b|
    Token.new("#{a.word} #{op.word} #{b.word}", :expr)
  end
  
  reduce(:symbol, :operator, :symbol) do |a, op, b|
    Token.new("#{a.word} #{op.word} #{b.word}", :expr)
  end
  
  reduce(:number, :operator, :expr) do |n, o, e|
    Token.new("#{n.word} #{o.word} (#{e.word})", :expr)
  end
  
  reduce(:symbol, :operator, :expr) do |s, o, e|
    Token.new("#{s.word} #{o.word} (#{e.word})", :expr)
  end
  
  reduce(:number, :assign) do |n, a|
    raise "can't assign to #{n.word}"
  end
  
  reduce(:operator, :operator) do
    raise "syntax error"
  end
  
  reduce(:symbol, :comma, :symbol) do |a, _, b|
    Token.new("#{a.word}, #{b.word}", :list)
  end
  
  reduce(:symbol, :comma, :number) do |a, _, b|
    Token.new("#{a.word}, #{b.word}", :list)
  end
  
  reduce(:number, :comma, :number) do |a, _, b|
    Token.new("#{a.word}, #{b.word}", :list)
  end
  
  reduce(:number, :comma, :symbol) do |a, _, b|
    Token.new("#{a.word}, #{b.word}", :list)
  end
  
  reduce(:symbol, :comma, :list) do |a, _, b|
    Token.new("#{a.word}, #{b.word}", :list)
  end
  
  reduce(:number, :comma, :list) do |a, _, b|
    Token.new("#{a.word}, #{b.word}", :list)
  end
  
  reduce(:symbol, :list) do |a, b|
    Token.new("#{a.word}(#{b.word})", :call)
  end
  
  reduce(:symbol, :expr) do |a, b|
    Token.new("#{a.word}(#{b.word})", :call)
  end
end

string = 'b = 2 >> 9 - 1; a = 3 + b; f a, b, c; f a + b'
tokens = SimpleTokenizer.new.tokenize(string)
tokens = SimpleReducer.new.reduce(tokens)

pp tokens
