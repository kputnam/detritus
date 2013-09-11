#!/usr/bin/env ruby
require '/Users/kputnam/Programming/webclient/webclient.rb'

w = WebClient.new
#w.debug = true

url = 'http://www.whatismyipaddress.com/'
url = 'http://www.lawrencegoetz.com/programs/ipinfo/'

begin
  # no redirection please
  r = w.get(url, nil, nil, 1).body
  m = r.scan(/(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})/)
rescue
  abort $!
end

puts *m.flatten!.first
