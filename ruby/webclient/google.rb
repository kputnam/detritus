#!/usr/bin/env ruby
# There's probably some trendy XML/SOAP/REST interface that should be
# used instead of screen scraping, but this does the job

libs = %w[kputnam/webclient kputnam/adt/cache hpricot]
begin
  while l = libs.shift; require l; end
rescue LoadError
  require 'rubygems'
  require l; libs.each{|l| require l }
end

class Google
  attr_reader :count, :query, :batch
  
  class << self
    def search(string, w)
      new(string, w)
    end
    
    def escape(string)
      string.gsub(/[^a-z0-9*.+-]/i){|c| "%#{c[0].to_s(16)}" }
    end
  end
  
  def initialize(string, w)
    @lang  = "en"
    @query = self.class.escape(string)
    @count = -1
    @batch = 25
    @agent = w
    @cache = Cache::LRU.new(3)
    
    # be friendly
    @agent.head('http://google.com')
  end
  
  def each(&block)
    lazy_init
    
    start = 0
    data  = fetch_page(start)
    
    while start < @count
      unless res = data.at('div[@class=g]')
        # no results on this page
        break
      end
      
      while res
        # yield each result on this page
        yield Google::Result.new(res)
        
        # skip non-result siblings
        while res = res.next_sibling
          break if res.name == 'div' and res[:class] == 'g'
        end
      end
      
      # continue to next page
      start += @batch
      data   = fetch_page(start)
    end
  end
  
  def [](n)
    lazy_init
    
    return nil unless n < @count
    
    start = @batch*(n/@batch.to_f).floor
    data  = fetch_page(start)
    
    if res = data.at("div[@class=g]:nth(#{n-start})")
      Google::Result.new(res)
    end
  end
  
  def inspect
    "#<#{self.class} #{count} results (#{batch} pp) " <<
      "@query=#{@query} @cache=#{@cache.inspect}>"
  end
  
  private
  
  def fetch_page(start=0)
    begin
      return @cache[start]
    rescue CacheMiss
    end
    
    url  = "http://google.com/search?q=#{@query}&hl=#{@lang}"
    url << "&start=#{start}"
    url << "&num=#{@batch}" if @batch != -1 and @batch != 10
    url << "&sa=N&filter=0"
    
    unless (res = @agent.get(url)).kind_of?(Net::HTTPOK)
      raise "Unexpected #{res.inspect}"
    end
    
    @cache[start] = Hpricot(res.body)
  end
  
  def lazy_init
    if @count == -1
      # fetch only 1 result per page
      url  = "http://google.com/search?hl=#{@lang}&q=#{@query}"
      url << "&btnG=Google+Search"
      
      unless (res = @agent.get(url)).kind_of?(Net::HTTPOK)
        raise "Unexpected #{res.inspect}"
      end
      
      data = Hpricot(res.body)
      
      if data.at('title').inner_text =~ /forbidden/i
        # ugh! fun time
        str, tmp, form = forbidden(data)
        
        File.unlink(tmp)
        
        unless str
          raise "Dammit Jim!"
        end
        
        res = captcha(str, form)
        unless res['location']
          raise "Dammit Jim!"
        end
        
        unless (res = @agent.get(url)).kind_of?(Net::HTTPOK)
          raise "Unexpected #{res.inspect}"
        end
      
        data = Hpricot(res.body)
        if data.at('title').inner_text =~ /forbidden/i
          raise "Dammit Jim!"
        end
      end
      
      stats = data.search('table[@class="t bt"]/tr/td[@nowrap]').last
      a, b, count, query, time = stats.search('b').map{|e| e.inner_text }
      
      # total number of results
      @count = count.gsub(/[^\d]+/, '').to_i
      
      # number of results per page
      @batch = b.gsub(/[^\d]+/, '').to_i if @batch == -1
    end
  end
  
  def forbidden(data)
    if form = data.at('form')
      # find captcha image
      img = form.search('img').find{|e| e['src'] =~ /^\/sorry\/image?/ }
      abort "Couldn't find the CAPTCHA img" unless img
      
      res = @agent.get(img['src'])
      unless res['content-type'] =~ /^image\/(.+)$/
        abort "CAPTCHA content-type is `#{res['content-type']}'"
      end
      
      # save image and open
      tmp = "/tmp/#{Time.now.to_i}.#{$1}"
      File.open(tmp, 'w'){|f| f.write(res.body) }
      system("open '#{tmp}'")
      
      print "Enter CAPTCHA string from opened image: "
      string = $stdin.gets
      
      return string.strip, tmp, form
    end
  end
  
  def captcha(string, form)
    data   = {}
    action = form['action']
    method = form['method'].downcase
    
    form.search('input').each do |e|
      data[e['name']] = e['value']
      
      if e['name'] =~ /captcha/i
        data[e['name']] = string
      end
    end
    res = @agent.send(method, action, data.map{|k,v| "#{k}=#{v}" }.join('&'))
  end
  
end

# TODO 'View as HTML'
# TODO read [PDF], [DOC] etc
# TODO read File Format: PDF/Adobe Acrobat
# TODO read summary
# TODO read excerpts

class Google::Result
  attr_reader :title, :summary, :excerpts, :pagesize, :url, :cache
  
  def initialize(div)
    @div = div
    
    unless title = div.at('h2/a')
      puts div.to_s
      raise "NO H2/A!?"
    end
    
    @url   = title[:href]
    @title = title.inner_text
    
    if span = div.at('span[@class="a"]')
      @pagesize = span.inner_text[/- (.+) -$/, 1]
    end
    
    if body = div.at('table/tr/td[@class="j"]')
      cache  = body.at('a[text()*="Cached"]')
      @cache = cache[:href] if cache
    end
    
    @summary = body
  end
  
  def succ
    @div.next_sibling
  end
  
  def prev
    @div.previous_sibling
  end
  
  def inspect
    "#<#{self.class} #{@title.inspect}>"
  end
end
