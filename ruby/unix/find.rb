#!/usr/bin/env ruby

module Find
  DEFAULTS = Hash.new[
    # block to allow things like accessing dir contents in alphabetical order
    :sort => proc{0},
    
    # forward exceptions to caller
    :exceptions => false,
    
    # depth-first traversal (:dfs) or breadth-first traversal (:bfs)
    :mode => :dfs, 
    
    # do not cross into other file systems
    :onefs => false,
    
    # follow symlinks
    :follow => false
  ]
  
  class << self
    def traverse(*paths)
      # makes it easy to traverse(File.glob('../*.o), File.glob('*.c'))
      paths.flatten!
      
      options = DEFAULTS
      options = options.merge(paths.pop) if paths.last.kind_of?(Hash)
      
      method = {:dfs=>:unshift, :bfs=>:push}.fetch(options[:mode])
      fs = nil; followed = {}
      
      while path = paths.shift
        begin # don't yield unless everything checks out
          stat = File.lstat(path)
          # do not cross into another filesystem
          next if options[:onefs] and (fs ||= stat.dev) != stat.dev
          
          if stat.symlink?
            # don't follow any symlinks
            next unless options[:follow]
            followed[stat.dev] ||= []
            # already followed this symlink
            next if followed[stat.dev].include?(stat.ino)
            # remember following this symlink
            followed[stat.dev] << stat.ino
          end
        rescue Errno::EACCES
          raise $! if options[:exceptions]
        end
        
        catch :prune do
          yield path
          
          begin # this is skipped if block threw a :prune
            Dir.entries(path).sort!(&options[:sort]).each do |f|
              next if f == '.' or f == '..'
              # queue directory contents
              paths.send(method, File.join(path, f))
            end if File.directory?(path)
          rescue Errno::EACCES, Errno::ENOENT
            raise $! if options[:exceptions]
          end
        end
        
      end
    end
    
    def prune
      throw :prune
    end
    
    # true if block never returns false
    def all?(*paths)
      !catch(:done){ traverse(*paths){|f| throw(:done, true) unless yield f }}
    end
    
    # true if block never returns true
    def none?(*paths, &b)
      not any?(*paths, &b)
    end
    
    # returns true (stops processing elements) once block returns true
    def any?(*paths)
      catch(:done){ traverse(*paths){|f| throw(:done, true) if yield f }}
    end
    
    # apply block to each element
    def collect(*paths)
      match = []; traverse(*paths){|f| match << yield(f) }; match
    end
    
    # returns first element where block returns true
    def detect(*paths)
      catch(:done){ traverse(*paths){|f| throw(:done, f) if yield f }}
    end
    
    # return an array containing each element
    def entries(*paths)
      # call block to allow pruning, but ignore block return value
      collect(*paths){|f| yield f; f }
    end
    
    # return true and false list of elements
    def partition(*paths)
      t = []; f = []
      traverse(*paths){|x| next t << f if yield f; f << x }
      [t, f]
    end
    
    # return elements where block returns false
    def reject(*paths)
      match = []; traverse(*paths){|f| match << f unless yield f }; match
    end
    
    # return elements where block returns true
    def select(*paths)
      match = []; traverse(*paths){|f| match << f if yield f }; match
    end
    
    alias skip prune
    alias each traverse
    alias map collect
    alias find detect
    alias to_a entries
  end
end

