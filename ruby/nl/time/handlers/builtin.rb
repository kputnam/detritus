#!/usr/bin/env ruby
# $Hg$

NLTime.handle('Time') do |time|
  NLTime::Entity::Time.new(time.hour, time.min, time.sec,
    NLTime::Entity::Date.civil(time.year, time.month, time.mday))
end

NLTime.handle('Date') do |date|
  NLTime::Entity::Date.civil(date.year, date.month, date.mday)
end

NLTime.handle('DateTime') do |time|
  NLTime::Entity::Time.new(time.hour, time.min, time.sec,
    NLTime::Entity::Date.civil(time.year, time.month, time.mday))
end

NLTime.handle('String') do |string, *args|
  # split string into tokens
  tokens = NLTime::Tokenizer.new.tokenize(string)
  
  # remove tokens marked :skip (whitespace)
  tokens = NLTime::Reducer::Skip.new.reduce(tokens)
  
  # parse individual tokens
  tokens = NLTime::Parser.new(*args).parse(tokens)
  
  # parse combinations of tokens
  tokens = NLTime::Reducer.new(*args).reduce(tokens)
  
  # yes, this is ugly but I didn't feel like typing the word 'if' again
  ((tokens.size == 1) &&
    (tokens.first.fetch(NLTime::Entity) || tokens.first)) ||
  tokens
end
