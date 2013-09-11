#!/usr/bin/env ruby
# $Hg: mock.rb e2b89147ca00 2008-02-06 00:55 -0600 kputnam $

# Groovy is an example of a modern language that supports selector
# namespaces, which they call Categories. Categories allow you to apply a
# set of metaclass changes to one (or more?) class on a specific thread
# within a specific scope. So you can add behavior to String for a block
# of code and all code it calls, and when the block finishes the behavior
# disappears.

class Mock
  attr_accessor :trace
  
  # Mock.new('Child', Class.new(Parent))
  #   
  
  # Mock.new(Parent, Class.new)
  #   
  
  # Mock.new(Parent)
  #   
  
  def initialize(klass, from=nil)
    @methods = []
    @aliases = {}
    @former  = nil
    @destroy = false
    
    unless klass.is_a?(Module)
      # create new object
      @destroy = klass.to_s
      klass = Object.const_set(klass.to_s, from.clone)
    else
      if from.is_a?(Module)
        # shadow
        @former = klass
        klass = Object.const_set(klass.to_s, from)
      end
    end
    @klass = klass
  end
  
  def implement(method, &block)
    # only if we're not redefining a mock method
    if @klass.instance_methods.include?(method.to_s)
      @klass.class_eval{ alias_method("__mocked__#{method}", method) }
      @aliases[method] = "__mocked__#{method}"
    end unless @methods.include?(method)
    
    @klass.class_eval{ define_method(method, block) }
    @methods << method
  end
  
  def stub(method, value)
    implement(method, &proc{ value })
  end
  
  def revert
    if @former
      Object.const_set(@former.to_s, @former)
      @destroy = @klass = @former = nil
      return
    end
    
    if @destroy
      d = @destroy
      Object.class_eval{ remove_const(d) }
      @destroy = @klass = @former = nil
      return
    end
    
    while m = @methods.pop
      @klass.class_eval{ remove_method(m) }
    end
    @aliases.each do |name,o|
      @klass.class_eval{ alias_method(name, o); remove_method(o) }
      @aliases.delete(name)
    end
  end
  
  def new(*args)
    @klass.new(*args)
  end
end
