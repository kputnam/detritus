#!/usr/bin/env ruby
require 'webclient.rb'
require 'thread'

def thread
  Thread.new do
    www = WebClient.new
    while url = $queue.pop
      www.get(url)
    end
  end
end

if $0 == __FILE__
  $queue   = Queue.new
  $threads = (0..2).map{thread}
  
  50.times do
    $queue.push 'http://google.com'
    $queue.push 'http://kernel.org/'
  end
  
  while !$queue.empty?
    puts "queue size: #{$queue.length}"
    sleep 1
  end
end
