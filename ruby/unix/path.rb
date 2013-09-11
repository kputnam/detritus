#!/usr/bin/env ruby

class Path
  def initialize(*paths)
    @cache  = {}
    @search = (paths.any?) ? paths : ENV['PATH'].split(':')
    @search.map!{|x| File.expand_path(x) }
  end
  
  def clear
    @search.clear
  end
  
  def to_a
    @search.dup
  end
  
  def include?(path)
    @search.include? File.expand_path(path)
  end
  
  def push(*paths)
    paths.each{|x| @search.push File.expand_path(x) }
  end
  
  def unshift(*paths)
    paths.each{|x| @search.unshift File.expand_path(x) }
  end
  
  def remove(*paths)
    paths.each{|x| @search.delete File.expand_path(x) }
  end
  
  def cleanup
    # uncache failures
    @cache.reject!{|k,v| v.nil? }
  end
  
  def uncache(cmd=nil)
    cmd ? @cache.delete(cmd) : @cache.clear
  end
  
  def [](cmd)
    # TODO allow glob or regex here: path[/^[a-z]*sh$/i]
    unless @cache.include?(cmd)
      @cache[cmd] = nil
      @search.each do |dir|
        begin
          path = "#{dir}/#{cmd}"
          if File.executable?(path)
            @cache[cmd] = path
            break
          end
        rescue Errno::EACCES
        end
      end
    end
    @cache[cmd]
  end
  
  def []=(cmd, path)
    @cache[cmd] = path
  end
  
  def glob(pattern)
    match = []
    @search.each do |dir|
      begin
        match << (Dir.glob("#{dir}/#{pattern}").select{|x| File.executable? x })
      rescue Errno::EACCES
      end
    end
    match.flatten.compact
  end
  
  def match(pattern)
    # TODO find executables by regexp
  end
  
  alias <<     push
  alias search glob
end
