#!/usr/bin/env ruby

# print integer as 32-bit sequence
def p32(s,a='.') puts s.to_s(2).rjust(32,a) end

class IP
  def initialize(s)
    if s.is_a?(String)
      a,b,c,d = s.split('.').map{|q| q.to_i }
      @n = d + (c << 8) + (b << 16) + (a << 24)
    elsif s.is_a?(Integer)
      @n = s
    elsif s.is_a?(IP)
      @n = s.to_i
    end
  end
  def to_i
    @n
  end
  def to_s(radix=nil)
    if radix
      to_i.to_s(radix)
    else
      [0,8,16,24].map{|n| (@n >> n) & 0xff }.reverse.join('.')
    end
  end
  def cidr(bits)
    CIDR.new(self, bits)
  end
  def container(other)
    # compute minimum bitmask
    case diff = self.to_i ^ IP.new(other).to_i
    when 0 then bits = 0
    when 1 then bits = 1
    else    bits = (Math.log(diff)/Math.log(2))
    end

    # compute host mask, net mask
    host = (2**bits.ceil) - 1
    mask = 2**32 - host - 1

    CIDR.new(IP.new(to_i & mask), 32-bits.ceil)
  end
  def ==(other)
    IP.new(other).to_i == self.to_i
  end
end
class CIDR
  attr_reader :min, :max, :mask

  # CIDR.new('192.168.1.1', 16)
  # CIDR.new('192.168.1.1/16')
  # CIDR.new('192.168')
  def initialize(network, cidr=nil)
    if cidr.nil? 
      if network.include? '/'
        network, cidr = network.split('/')
      else
        # remove trailing .0 subnets
        network, _ = network.split(/(:?\.0+)*$/)

        # compute cidr
        network = network.split('.')
        cidr = 8 * (network.size)

        # pad with 0's as necesarry
        pad = ['0'] * (4 - network.size)
        network = network.push(*pad).join('.')
      end
    end

    @min, @mask = IP.new(network), cidr.to_i
    @max = IP.new(@min.to_i + 2**(32-@mask) - 1)
  end
  def to_s
    "#{@min}/#{@mask}"
  end
  def include?(ip)
    ip = IP.new(ip) and (@max.to_i >= ip.to_i) and (@min.to_i <= ip.to_i)
  end
end

if __FILE__ == $0
  if ARGV[1][0] == ?/
    puts CIDR.new(ARGV[0], ARGV[1][1..-1])
  else
    puts IP.new(ARGV[0]).container(IP.new(ARGV[1]))
  end
end
