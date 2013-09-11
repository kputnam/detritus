#!/usr/bin/env ruby

# Interface for MacOS X's scutil
module SystemConfiguration
  # list keys (matching pattern if given)
  def self.list(pattern=nil)
    unless @list
      data = ''
      Open3.popen3('scutil') do |i,o,e|
        i.puts 'list'
        i.close
        data = o.read
      end
      @list = data.scan(/^\s+subKey .*= (.+)$/).flatten
    end
    if pattern
      # assume pattern is a prefix search if it's not a regex
      pattern = /^#{Regexp.escape(pattern)}/ unless pattern.is_a?(Regexp)
      @list.select{|x| x =~ pattern }
    else
      @list
    end
  end
  
  # lookup sc key
  def self.[](key)
    @cache ||= {}
    unless @cache[key]
      data = ''
      Open3.popen3('scutil') do |i,o,e|
        i.puts "show #{key}"
        i.close
        data = o.read
      end
      @cache[key] = parse(data)
    end
    @cache[key]
  end
  
  # parse scutil output into ruby hash/array
  def self.parse(data, mode=nil, count=0)
    return parse(data.split("\n")) unless data.is_a?(Array)
    
    while line = data.shift
      # trim trailing and leading whitespace
      line.strip!
      count += 1
      
      unless mode
        # get next data type
        unless m = line.match(/^<(.+)> \{$/)
          #raise "cannot parse line #{count}: #{line}"
          return (line == 'No such key') ? nil : line
        end
        mode = m.captures.first
      else
        case mode
        when 'dictionary'
          store = {}
        when 'array'
          store = []
        else
          raise "unknown data type #{mode}"
        end unless store
        
        unless m = line.match(/^(.+?) : (.+?)( \{)?$/)
          unless line == '}'
            unless m = line.match(/^(.+?) :$/)
              raise "cannot parse line #{count}: #{line.inspect}"
            else
              # no value?
            end
          else
            return store
          end
        end
        
        key, value, nest = m.captures
        key = key.to_i if mode == 'array'
        
        if nest == ' {' and n = value.match(/^<(.+)>$/)
          # begin new nested data
          store[key] = parse(data, n.captures.first, count)
        else
          store[key] = value
        end
      end
    end
    store
  end
  
  # convert to ruby boolean value
  def self.boolean(data)
    (data.to_s =~ /^(1|true|t)$/i) ? true : false
  end
  
  # set network location
  def self.location=(location)
    # system('scutil location')
  end
  
  # current location only
  def self.location
    @location ||= SC::Location.new(self['Setup:/']['UserDefinedName'])
  end
  
  # current primary interface
  def self.interface(id=nil)
    if x = self['State:/Network/Global/IPv4']
      x = x['PrimaryInterface']
    end
    SC::Interface.new(id||x)
  end
  
  # current primary service
  def self.service(id=nil)
    if x = self['State:/Network/Global/IPv4']
      x = x['PrimaryService']
    end
    SC::Service.new(id||x)
  end
  
  # FIXME doesn't belong
  def self.publicaddr
    if $IPURL and cmd('scutil -r putnamcabinets.com') == "Reachable\n" and !@ip
      begin
        @w ||= WebClient.new
        Timeout.timeout(1) {@ip = @w.get($IPURL).body[0..60]}
      rescue TimeoutError
        nil
      end
    end
    @ip
  end
end

require "observer"

class SystemConfiguration::Monitor
  include Observable
  
  def initialize(*keys)
  end

  # add watch key
  def <<(*keys)
    # n.add key ["pattern"]
  end

  # remove watch key
  def remove(*keys)
    # n.remove key ["pattern"]
  end

  # begin monitoring for updates
  def start
    # n.watch [verbose]
    # notify_obsevers(*args)
  end

  def pause
    # n.cancel
  end
end

# current Location contains Services (enabled/disabled)
# each Service corresponds to an Interface

# each 'network port' like built-in ethernet, airport, etc is a service
class SystemConfiguration::Service
  attr_reader :service_id
  def initialize(id)
    if @service_id = id
      @path  = "/Network/Service/#{@service_id}"
      @setup = SC.list("Setup:#{@path}")
      @state = SC.list("State:#{@path}")
    else
      @path  = 'xxx'
      @setup = @state = []
    end
  end
  def name
    SC["Setup:#{@path}/#{@service_id}"]['UserDefinedName']
  end
  def enabled?
    # is the device checked in Network Port Configuration?
    @setup.any?
  end
  def disabled?
    not enabled?
  end
  def active?
    # is the device plugged in and turned on?
    @state.any?
  end
  def inactive?
    not active?
  end
  def interface
    SC::Interface.new(SC["Setup:#{@path}/Interface"]['DeviceName'])
  end
  def wireless?
    SC["Setup:#{@path}/Interface"]['Hardware'] == 'AirPort'
  end
  def ethernet?
    SC["Setup:#{@path}/Interface"]['Type'] == 'Ethernet'
  end
  def macaddr(stop=false)
    SC["Setup:#{@path}/Ethernet"]['MACAddress'] ||
      (stop ? nil : interface.macaddr(true))
  end
  def addresses
    SC["State:#{@path}/IPv4"]['Addresses']
  end
  def netmasks
    SC["State:#{@path}/IPv4"]['SubnetMasks']
  end
  def router
    SC["State:#{@path}/IPv4"]['Router']
  end
  def broadcasts
    interface.broadcasts
  end
  def dnsservers
    if x = SC["State:#{@path}/DNS"]
      x['ServerAddresses'] || []
    else
      []
    end
  end
  def domain
    if x = SC["State:#{@path}/DNS"]
      x['DomainName']
    end
  end
  alias to_s name
  alias activated? active?
end

# physical hardware device?
class SystemConfiguration::Interface
  attr_reader :name
  def initialize(name)
    if name
      @name  = name
      @path  = "/Network/Interface/#{name}"
      @setup = SC.list("Setup:#{@path}")
      @state = SC.list("State:#{@path}")
    else
      @name  = 'no name'
      @path  = 'xxx'
      @setup = @state = []
    end
  end
  def active?
    SC.boolean(SC["State:/Network/Interface/#{name}/Link"]['Active'])
  end
  def macaddr(stop=false)
    SC["Setup:#{@path}/AirPort"]['MACAddress'] ||
      (stop ? nil : service.macaddr)
  end
  def wireless?
    SC["State:#{@path}/AirPort"].is_a?(Hash)
  end
  def ssid
    return unless wireless?
    SC["State:#{@path}/AirPort"]['SSID']
  end
  def bssid
    return unless wireless?
    mac = SC["State:#{@path}/AirPort"]['BSSID']
    mac[/^<data> 0x([0-9a-f]+)$/i, 1].scan(/.{2}/).join(':').downcase
  end
  def broadcasts
    SC["State:#{@path}/IPv4"]['BroadcastAddresses']
  end
  alias to_s name
end

# only able to represent current location
class SystemConfiguration::Location
  attr_reader :name
  def initialize(name)
    @name = name
  end
  def hostname
    SC['Setup:/Network/HostNames']['LocalHostName']
  end
  def services
    unless @services
      @services = SC['Setup:/Network/Global/IPv4']['ServiceOrder']
      @services.map!{|id| SC.service(id) }
    end
    @services
  end
  alias to_s name
end

SC = SystemConfiguration unless defined? SC

