#!/usr/bin/env ruby

# FIXME: something is leaking memory everytime #get (or #post ?) is called
# and beware res.body.inspect creates a new copy of res.body

# TODO: resolve hostnames so google.com and www.google.com don't create
# separate connections (unless they really are different IPs)

# TODO: allow limit on number of keepalive connections, expire by LRU or
# run thread to expire connections after X minutes

# TODO: read keepalive timeout from response header and expire old connections

# FIXME: proxy keepalive connections seem messed up? should these be kept
# open at all?

# FIXME: post maybe shouldn't be re-posted for 403 redirect?

# TODO: implement cache/e-tag/last-modified in a superclass?

require 'uri'
require 'cgi'
require 'net/http'
require 'net/https'

class WebClient
  attr_reader :cookies, :history, :future, :proxies, :keepalive
  attr_accessor :debug, :agent

  REDIRECT_LIMIT = 10
  
  def initialize(keepalive=false)
    @proxies   = {'DIRECT' => Net::HTTP}
    @keepalive = {} if keepalive
    #@agent     = 'Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en)'
    @agent     = 'Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en) ' <<
                 'AppleWebKit/523.12.2 (KHTML, like Gecko) ' <<
                 'Version/3.0.4 Safari/523.12.2'
    @cookies   = CookieJar.new
    @history   = []
    @future    = []
    @cert      = false
    @debug     = false
    @pac       = nil
    
    read_env_proxy
  end
  
  def inspect
    "#<WebClient #{@cookies.cookies.size} cookies @agent=#{@agent}>"
  end
  
  def get(uri, data=nil, proxy=nil, limit=REDIRECT_LIMIT)
    raise "HTTP redirect too deep" unless limit > 0
    uri = mkuri(uri)
    
    puts "GET: #{uri.to_s}" if @debug
    http  = keep_alive uri, proxy
    query = uri.path
    
    if uri.query and uri.query.any?
      query << "?#{uri.query}"
      query << "&#{data}" if data
    else
      query << "?#{data}" if data and data.any?
    end
    
    res = http.get(query, mkheaders(uri))
    res['uri'] = uri
    
    storecookies(uri, res)
    @history << uri
    @history.shift if @history.size > 10
    
    case res
    when Net::HTTPRedirection
      puts "Redirection: #{res['location']}" if @debug
      res = self.get(res['location'], data, proxy, limit-1)
    end
    
    res
  end
  
  def post(uri, data=nil, proxy=nil, limit=REDIRECT_LIMIT)
    raise "HTTP redirect level too deep" unless limit > 0
    uri = mkuri(uri)
    
    puts "POST: #{uri.to_s}" if @debug
    http  = keep_alive uri, proxy
    query = uri.path
    query << "?#{uri.query}" if uri.query
    
    res = http.post(query, data, mkheaders(uri))
    res['uri'] = uri
    
    storecookies(uri, res)
    @history << uri
    @history.shift if @history.size > 10

    case res
    when Net::HTTPRedirection
      puts "Redirection: #{res['location']}" if @debug
      res = get(res['location'], nil, proxy, limit-1)
    end
    
    res
  end
  
  def head(uri, proxy=nil, limit=REDIRECT_LIMIT)
    raise "HTTP redirect too deep" unless limit > 0
    uri = mkuri(uri)
    
    puts "HEAD: #{uri.to_s}" if @debug
    http  = keep_alive uri, proxy
    query = uri.path
    query << "?#{uri.query}" if uri.query
    
    res = http.head(query, mkheaders(uri))
    
    storecookies(uri, res)
    @history << uri
    @history.shift if @history.size > 10
    
    case res
    when Net::HTTPRedirection
      puts "Redirection: #{res['location']}" if @debug
      res = self.head(res['location'], proxy, limit-1)
    end
    
    res['uri'] = uri
    res
  end
  
  def put(uri)
  end
  
  def proppatch(uri)
  end
  
  def lock(uri)
  end
  
  def unlock(uri)
  end
  
  def options(uri)
  end
  
  def propfind(uri)
  end
  
  def delete(uri)
  end
  
  def move(uri)
  end
  
  def copy(uri)
  end
  
  def mkcol(uri)
  end
  
  def trace(uri)
  end
  
  def certify_ssl_peer=(status)
    # @cert = status would probably be fine
    @cert = (status != false and !status.nil?)
  end

  def debug?
    @debug
  end
  
  def mkuri(uri)
    #uri << '/' unless uri.is_a?(URI) or uri[-1].chr == '/'
    uri = URI.parse(uri) unless uri.is_a?(URI)
    
    if uri.path.nil? or uri.path.empty?
      uri.path = '/'
    end
    
    case uri
    when URI::HTTP, URI::HTTPS
    when URI::Generic # missing server, port
      
      # FIXME this requires correct history using BACK/FORWARD
      newuri       = @history.last.dup
      newuri.query = uri.query
      
      if uri.path[0].chr == '/'
        # absolute path
        newuri.path = uri.path
      else
        # relative to previous path?
        newuri.path = ::File.dirname(newuri.path).chomp('/') << "/#{uri.path}"
      end
      
      # fixed uri
      uri = URI.parse(newuri.to_s)
    else
    end
    
    uri
  end
  
  def basic_auth(user, pass)
    @auth = :basic
    @authuser = user
    @authpass = pass
  end

  def no_auth
    @auth = @authuser = @authpass = nil
  end
  
  private
  
  def mkheaders(uri)
    headers = {}
    headers['User-Agent'] = @agent
    headers['Connection'] = 'Keep-Alive'
    headers['Keep-Alive'] = '300'
    headers['Accept'] = 'text/xml,application/xml,application/xhtml+xml,' <<
                        'text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5'
    
    #headers['Accept']     = 'text/xml,application/xml'   <<
    #  'application/xhtml+xml,text/html;q=0.9,text/plain' <<
    #  ';q=0.8,image/png,*/*;q=0.5'
    headers['Accept-Language']   = 'en-us,en;q=0.5'
    #headers['Accept-Encoding']  = 'gzip,deflate'
    #headers['Accept-Charset']   = 'ISO-8859-1,utf-8;q=0.7,*;q=0.7'
    #headers['Cache-Control']    = 'max-age=0'
    
    # FIXME: shouldn't have referer unless actually referred
    headers['Referer'] = @history.last.to_s unless @history.empty?
    
    cookies = sendcookies(uri)
    #puts "No cookies!" if cookies.empty? and @debug
    
    headers['Cookie'] = cookies unless cookies.empty?
    
    case @auth
    when :basic
      token = "#{@authuser}:#{@authpass}"
      headers['Authorization'] = "Basic #{[token].pack('m')}"
    end
    
    headers
  end

  def sendcookies(uri)
    @cookies.retrieve(uri).map do |c|
      puts "Cookie: #{c.name}" if @debug
      "#{c.name}=#{c.value}"
    end.join('; ')
  end

  def storecookies(uri, response)
    @cookies.store(uri, response['set-cookie'])
  end
  
  def select_proxy(uri)
    if @proxies['DEFAULT']
      proxy = 'DEFAULT'
    elsif @pac
      # TODO: test connection to proxy, use first working proxy
      list = @pac.call(uri).split(';')
      item = list.first.split(/\s+/)
      case item.first
      when 'PROXY'  
        proxy = item.last
        
        unless @proxies[proxy]
          host, port = item.last.split(':')
          @proxies[proxy] = Net::HTTP.Proxy(host, port.to_i)
        end
      else
        proxy = 'DIRECT'
      end
    else
      proxy = 'DIRECT'
    end
    
    @keepalive[proxy] ||= {} if @keepalive
    
    proxy
  end
  
  def fetch_pac(address=nil)
    unless address
      # use web proxy auto-discovery (wpad)
      me = Socket.gethostname.split('.')
      
      while me.size > 1
        me.shift # remove next most local component
        url = "http://wpad.#{me.join('.')}/wpad.dat"
        res = get url, nil, 'DIRECT'
      
        # check the MIME type
        if res and res['Content-Type'] == 'application/x-ns-proxy-autoconfig'
          pac = res
          break
        end
      end
    else
      # use given address
      res = get address, nil, 'DIRECT'
      pac = res if res
    end
    
    #if pac
    #  require 'js'
    #  @pac = PACFile.generate(res)
    #end
  end
  
  def read_env_proxy
    uri = ENV['http_proxy'] || ENV['HTTP_PROXY'] || return
    uri = URI.parse(uri) || return
    uri.is_a?(URI::HTTP) || uri.is_a?(URI::HTTPS) || return
    
    if uri.path.size <= 1
      # no 'file' in the uri
      @proxies['DEFAULT'] = Net::HTTP::Proxy(uri.host, uri.port,
                                             uri.user, uri.password)
    else
      # proxy uri has a file, it's probably a PAC?
      fetch_pac uri
    end
  end
  
  def keep_alive(uri, proxy=nil)
    proxy = select_proxy uri unless proxy
    puts "Selected proxy '#{proxy}' for this URI" if @debug
    
    #unless @proxies[proxy].proxy_class?
      # if we're not connecting through a proxy, we'll keep the connections
      # open to each host we connect to and reuse them if possible
      key = "#{uri.scheme}://#{uri.host}:#{uri.port}"
    #else
      # since we're connecting with a proxy, we only need to keep the proxy
      # connection open. hopefully the proxy does its own keepalives
      #key = rand(2)
    #end
    
    unless @keepalive
      puts "Starting new connection on #{key}" if @debug
      
      if uri.scheme == 'https'
        # use_ssl has to be called after start
        connection = @proxies[proxy].new(uri.host, uri.port)
        connection.use_ssl = true
        connection.verify_mode = OpenSSL::SSL::VERIFY_NONE unless @cert
        connection.start
      else
        connection = @proxies[proxy].start(uri.host, uri.port)
      end
      return connection
    end
    
    unless @keepalive[proxy][key]
      puts "Starting new connection on #{key}" if @debug
      
      if uri.scheme == 'https'
        # use_ssl has to be called after start
        @keepalive[proxy][key] = @proxies[proxy].new(uri.host, uri.port)
        @keepalive[proxy][key].use_ssl = true
        unless @cert
          @keepalive[proxy][key].verify_mode = OpenSSL::SSL::VERIFY_NONE
        end
        @keepalive[proxy][key].start
      else
        @keepalive[proxy][key] = @proxies[proxy].start(uri.host, uri.port)
      end
      connection = @keepalive[proxy][key]
      
    else
      connection = @keepalive[proxy][key]
      
      # see /opt/local/lib/ruby/1.8/net/http.rb
      # to_io is called because HTTPS's @socket is an OpenSSL::SSL::SSLSocket
      io = connection.instance_eval{@socket}.io.to_io
      
      begin
        # we're assuming all the data was read from the last request,
        # so reading 1 byte shouldn't really return any data
        c = io.recv_nonblock(1)
      rescue Errno::EAGAIN
        # still connected
        puts "Reusing connection #{key}" if @debug
      rescue
        # some other exception, reconnect
        puts "Reconnecting to #{key} (#1) #{$!}" if @debug
        connection.finish
        connection.start
      ensure
        if c
          # normal HTTP sockets c is empty string '' if disconnected
          # for HTTPS sockets, c is gibberish (?) if disconnected
          # when c is nil, we're still connected
          puts "Reconnecting to #{key} (#2)" if @debug
          connection.finish
          connection.start
        end
      end
    end
    
    connection
  end
  
end

class WebClient::CookieJar
  attr :cookies
  
  def initialize
    @cookies = {}
  end
  
  def domainmatch(a, b)
    # does b match a?.. a must be less specific than b
    m = a.downcase
    n = b.downcase
    
    p = m.reverse.chomp('.').reverse.split('.')
    q = n.split('.')
    
    (m == n) or
      # here match('example.com', 'www.example.com')  => false
      #  but match('.example.com', 'www.example.com') => true
      
      #(m[0].chr == '.' and n[(n.size - m.size)..-1] == m) or
      #(m[0].chr == '.' and m[1..-1] == n)
      
      # no prefix . required to match subdomains
      (q.size >= p.size and q[q.size-p.size..-1] == p)
  end
  
  def pathmatch(a, b)
    # does b match a?.. a must be less specific than b
    
    # ensure only 1 slash at the end of each path
    m = a.downcase.chomp('/') << '/'
    n = b.downcase.chomp('/') << '/'
    
    (m == n) or (n[0..m.size-1] == m)
  end
  
  def expired?(timestamp)
    # TODO: parse timestamp and return true if Time.now >= timestamp
    # Wdy, DD-Mon-YY HH:MM:SS GMT
    
    return false if timestamp.nil? or timestamp.empty?
  end
  
  def store(uri, cookiestr)
    return if cookiestr.nil? or cookiestr.empty?
    
    # ugly hack!
    cookiestr.gsub!(/expires=([a-z]+),/i, 'expires=\1.')
    cookiestr.split(/,\s*/).each do |cstr|
      domain  = uri.host
      path    = uri.path
      expires = nil
      name    = nil
      value   = nil
      
      # parse each key=value pair for this cookie
      cstr.split(/;\s*/).each do |kp|
        _n, _v = kp.split('=', 2)
      
        case _n
        when 'domain'
          domain = _v
        when 'path'
          path = _v
        when 'expires'
          _v.gsub!(/^([a-z]+)\./i, '\1,')
          expires = _v
        else
          name  = _n
          value = _v
        end
      end
      
      # TODO reject from x.y.foo.com when Domain=.foo.com
      #      accept from y.foo.com when Domain=.foo.com
      
      if pathmatch(path, uri.path) and domainmatch(domain, uri.host)
        cookie = WebClient::Cookie.new(name, value, domain, path, expires)
        @cookies[domain]       = {} if @cookies[domain].nil?
        @cookies[domain][name] = cookie
      end
    end
    
    nil
  end
  
  # return array of WebClient::Cookie objs to send to server
  def retrieve(uri)
    # find domains that match first
    domains = @cookies.keys.select{|d| domainmatch(d, uri.host)}
    cookies = []
    
    domains.each do |d|
      @cookies[d].each do |n, c|
        if pathmatch(c.path, uri.path)
          if !expired?(c.expires)
            cookies << c
          else
            # delete expired cookie
            @cookies[d].delete(n)
          end
        end
      end
    end
    
    cookies
  end
  
  def to_s
    contents = @cookies.map do |domain,hash|
      data = hash.keys.sort.map{|name| "  #{name} = #{hash[name].value}"}
      data = data.join("\n")
      
      "#{domain}\n#{data}"
      
    end.join("\n\n")
    
    "CookieJar Contents:\n#{contents}\n"
  end
  
  def purge
    @cookies = {}
  end
end

class WebClient::Cookie
  attr_accessor :name, :value, :domain, :path, :expires, :version
  
  def initialize(name, value, domain, path, expires, version=1)
    @name    = name
    @value   = value
    @domain  = domain
    @path    = path
    @expires = expires
  end
end

module WebClient::PAC
  def self.generate(res)
    # return Proc
  end
end

class WebClient::Response
  def code
    # 302
  end
  
  def message
    # OK
  end
  
  def http_version
    # 1.1
  end
end

class WebClient::File
  attr_reader :content
  
  def initialize(content, response)
    @content  = content
    @response = response
  end
  
  def store(path)
    
  end
end

class WebClient::Document < WebClient::File
  alias :body :content
  
  # requisites
  # doctype
  # title
  # styles
  # scripts
  # meta
  # forms
  # links
  # images
  # frames
  # tables
  # lists
  # text
  # comments
  # scripts
  # to_s
  
  # click(pattern)
end

class WebClient::Form
  def initialize(e)
  end
  
  # passwords
  # texts
  # textareas
  # files
  # buttons
  # images
  # radio
  # check
  # dropdown
  # fields
  
  # submit
  # reset
end

