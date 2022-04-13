#!/usr/bin/env ruby
require 'kputnam/webclient'
require 'pp'

w = WebClient.new

# ugly but true
w.debug = (ARGV.first == '-D' ? ARGV.shift : false)

begin
  hash = w.head(ARGV.shift).to_hash
  hash.each do |key,val|
    puts "#{key} => #{val}"
  end
rescue
  abort $!
end

