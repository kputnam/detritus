
# don't verify SSL cert
module OpenURI
  module Net
    def self.const_missing(name)
      ::Net.const_get(name)
    end
    def self.method_missing(name,*args,&block)
      ::Net.send(name,*args,&block)
    end
    class HTTP < ::Net::HTTP
      def verify_mode=(foo)
      end
    end
  end
end

# shut up!
class Net::HTTP
  def warn(*args)
    super unless args.first == "warning: peer certificate won't be verified in this SSL session"
  end
end

