#!/usr/bin/env ruby

class CacheMiss < StandardError
end

# generic cache, subclass must define 'expire' method
class Cache
  attr_accessor :size
  
  def initialize(size)
    @size = size
    @data = {}
  end
  
  # fetch a single key, raise CacheMiss if does not exist
  def fetch(key)
    if @data.has_key?(key)
      @data[key]
    else
      raise CacheMiss
    end
  end
  
  # delete given keys
  def delete(*keys)
    keys.each{|k| @data.delete(k) }
  end
  
  # store a single key/value pair
  def store(key, data)
    unless @data.has_key?(key)
      expire if @data.size + 1 > @size
    end
    @data[key] = data
  end
  
  # return cache keys
  def keys
    @data.keys
  end
  
  def inspect
    "<##{self.class} #{keys.size}/#{size}>"
  end
end

# expiration based on access time
class Cache::RU < Cache
  def initialize(*args)
    @history = []
    super(*args) # me
  end
  
  def fetch(key)
    begin
      data = super(key)
      # move to front of list
      @history.delete(key)
      @history.unshift(key)
    rescue CacheMiss
      raise CacheMiss
    end
    data
  end
  
  def store(key, data)
    # must be called in this order!
    super(key, data)
    @history.push(key) unless @history.include?(key)
    data
  end
  
  def delete(*keys)
    keys.each{|k| @history.delete(k) }
    super(*keys)
  end
  
  # return n least recently accessed keys
  def oldest(n=1)
    @history[-n..-1].reverse!
  end
  
  # return n most recently accessed keys
  def newest(n=1)
    @history[0, n]
  end
  
  alias []  fetch
  alias []= store
end

# expiration based on access frequency
class Cache::FU < Cache
  def initialize(*args)
    super(*args)
    @access = {}
  end
  
  def fetch(key)
    begin
      data = super(key) 
      @access[key] += 1
    rescue CacheMiss
      raise CacheMiss
    end
    data
  end
  
  def store(key, data)
    super(key, data)
    @access[key] ||= 0
    data
  end
  
  def delete(*keys)
    keys.each{|k| @access.delete(k) }
    super(*keys)
  end
  
  def popular(n=1)
    @access.keys.sort{|a,b| @access[b] <=> @access[a] }[0, n]
  end
  
  def unpopular(n=1)
    @access.keys.sort{|a,b| @access[a] <=> @access[b] }[0, n]
  end
  
  alias []  fetch
  alias []= store
end

# discard least frequently accessed items
class Cache::LFU < Cache::FU
  # this algorithm can accumulate an excess of once frequently used
  # objects, that are now unlikely to ever expire
  def expire; delete(*unpopular); end
end

# discard most frequently accessed items
class Cache::MFU < Cache::FU
  # this algorithm can accumulate an excess of objects that are seldom accessed
  def expire; delete(*popular); end
end

# discard least recently used items
class Cache::LRU < Cache::RU
  # this algorithm is mostly fair, and shouldn't accumulate stale objects
  def expire; delete(*oldest); end
end

# discard most recently used items
class Cache::MRU < Cache::RU
  # this algorithm will accumulate seldom used objects
  def expire; delete(*newest); end
end

# each item has an exact expiration time
class Cache::Time < Cache
end

# discard items that haven't been used within a fixed interval
class Cache::Interval < Cache  
end

# adaptive replacement cache: balances between LRU and LFU
class Cache::ARC
end

# Other things to consider
# - some items cost more to create than others
# - some items are larger and take up more cache space
# - some items expire at a certain time
# - don't cache new items unless certain number of misses previously

# http://en.wikipedia.org/wiki/Page_replacement_algorithm
