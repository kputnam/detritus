require "thread"
require "socket"

class Object
  def bind; yield self; end
  def tap; yield self; self; end
end

module FtpServer
  class Storage
    attr_reader :cwd
    attr_accessor :type, :rest, :storage

    def initialize(root = "/")
      @cwd, @type, @rest = root, :ascii, 0
      @storage = Hash["welcome.msg" => "hello!"].tap{|fs| fs[""] = fs }
    end

    def list(path = @cwd)
      resolve(path).map do |name,v|
        if name == ""
          "" # skip root directory
        else
          v.is_a?(Hash) ?
            "drwx-----  1 user user #{v.size} 2000-01-01 00:00 #{name}\r\n" :
            "-rw------  1 user user #{v.size} 2000-01-01 00:00 #{name}\r\n"
        end
      end.join
    end

    def entries(path = @cwd)
      resolve(path).keys.map{|name| "#{name}\r\n" }
    end

    def read(path)
      encode(resolve(path).slice(@rest .. -1))
    end

    def write(path, data)
      resolve(dirname(path)).store(basename(path), encode(data))
    end

    def chdir(path)
      @cwd = canonicalize(path)
    end

    def mkdir(path)
      resolve(dirname(path)).store(basename(path), Hash.new)
    end

    def remove(path)
      unless canonicalize(path) == "/"
        resolve(dirname(path)).delete(basename(path))
      end
    end

    def rename(from, to)
      resolve(dirname(to)).store(basename(to), remove(from))
    end

    def readable?(path)
      not resolve(path).nil?
    end

    def exists?(path)
      not resolve(path).nil?
    end

    def writable?(path) # writable file
      traversable?(dirname(path)) and not resolve(path).is_a?(Hash)
    end

    def traversable?(path)
      resolve(path).is_a?(Hash)
    end

    def creatable?(path)
      resolve(path).nil? and resolve(dirname(path)).is_a?(Hash)
    end

    def removable?(path)
      case entry = resolve(path)
      when Hash; entry.empty?
      else entry # files are removable, nil is not
      end
    end

  private

    def canonicalize(path)
      File.expand_path(path[0,1] == "/" ? path : "#{@cwd}/#{path}").gsub(/^\/+/, "/")
    end

    def resolve(path)
      canonicalize(path).split("/").inject(storage){|dir, entry| dir[entry] if dir }
    end

    def dirname(path)
      File.dirname(canonicalize(path))
    end

    def basename(path)
      File.basename(path)
    end

    def encode(data)
      @mode == :binary ? data : data.gsub("\r\n", "\n").gsub(/[\200-\377]/) {|c| (c[0] - 0200).chr }
    end
  end

  class Multiplexer
    attr_reader :observers, :thread

    def initialize
      @mutex     = Mutex.new
      @observers = Hash.new
    end

    def start
      @thread ||= Thread.new { main }
    end

    def stop
      @stopped = true
      @thread.join
      @observers.each{|socket, o| o.call(:cleanup, socket) }
    end

    # observer/callback.call(event, socket) where event is either :select or :cleanup
    def connect(socket, observer = nil, &callback)
      @mutex.synchronize { @observers[socket] = (observer || callback) }
    end

    def disconnect(socket)
      @mutex.synchronize { @observers.delete(socket).bind{|o| o.call(:cleanup, socket) if o }}
    end

  private

    def main
      while not @stopped
        ready, = IO.select(@observers.keys, nil, nil, 0.10)
        ready.each{|s| @observers[s].bind{|o| o.call(:select, s) if o }} if ready
      end
    end
  end

  #
  # ftp = FtpServer::Server.new
  #
  # No SSL
  #   ftp.listen(TCPServer.new(21))
  #
  # SSL Setup
  #   context = OpenSSL::SSL::SSLContext.new
  #   context.cert = OpenSSL::X509::Certificate.new(File.read("server.crt"))
  #   context.key  = OpenSSL::PKey::RSA.new(File.read("server.key"))
  #   context.verify_mode = OpenSSL::SSL::VERIFY_NONE
  #
  # Implicit SSL
  #   socket = OpenSSL::SSL::SSLServer.new(TCPServer.new(990), context)
  #   socket.start_immediately = true
  #   ftp.listen(socket)
  #
  # Explicit SSL
  #   socket = OpenSSL::SSL::SSLServer.new(TCPServer.new(21), context)
  #   socket.start_immediately = false
  #   ftp.listen(socket)
  #
  class Server
    attr_reader :mux, :sessions

    def initialize(mux = Multiplexer.new)
      @storage, @mux, @sessions = storage, mux, []
    end

    def listen(socket)
      @mux.connect(socket, self)
      @mux.start
    end

    # multiplexer callback
    def call(event, socket)
      case event
      when :select
        # client connected to a listening socket
        @sessions << ControlChannel.new(@mux, socket.accept, storage)
      when :cleanup
        # close the listening socket
        socket.close rescue nil
      end
    rescue
      warn "#{$!.class}: #{$!.message}\n\t" + $!.backtrace.join("\n\t")
    end

    def storage
      Storage.new # each session gets a private file system
    end
  end

  module Commands
    def noop
      "200 awkward silence"
    end

    def quit
      @mux.disconnect(@socket)
    end

    def user(username)
      "331 what's the password, mac?"
    end

    def pass(password)
      "230 close enough"
    end

    def pwd
      "257 #{@storage.cwd.inspect}"
    end

    def cwd(path)
      if @storage.traversable?(path)
        @storage.chdir(path)
        "200 success"
      else
        "500 not permitted"
      end
    end

    def mkd(path)
      if @storage.creatable?(path)
        @storage.mkdir(path)
        "257 directory created"
      else
        "550 not permitted"
      end
    end

    def rmd(path)
      if @storage.removable?(path)
        @storage.remove(path)
        "250 success"
      else
        "550 not permitted"
      end
    end

    def dele(path)
      if @storage.removable?(path)
        @storage.remove(path)
        "250 success"
      else
        "550 not permitted"
      end
    end

    def rnfr(path)
      if @storage.renamable?(path)
        @rnfr = path
        "350 ok, where to?"
      else
        "550 not permitted"
      end
    end

    def rnto(path)
      if @storage.renamable?(@rnfr) and (@storage.creatable?(path) or @storage.writable?(path))
        @storage.rename(@rnfr, path)
        @rnfr = nil
        "250 rename successful"
      else
        "550 not permitted"
      end
    end

    def list(path = @storage.cwd)
      return "550 not permitted" unless @storage.readable?(path)
      return "425 use port or pasv first" unless @port
      transfer {|io| io.write(@storage.list(path)) }
    end

    def nlst(path)
      return "550 not permitted" unless @storage.readable?(path)
      return "425 use port or pasv first" unless @port
      transfer {|io| io.write(@storage.entries(path)) }
    end

    def retr(path)
      return "550 not permitted" unless @storage.readable?(path)
      return "425 use port or pasv first" unless @port
      transfer {|io| io.write(@storage.read(path)) }
    end

    def stor(path)
      return "500 not permitted" unless @storage.creatable?(path)
      return "425 use port or pasv first" unless @port
      transfer {|io| @storage.write(path, io.read) }
    end

    def rest(index)
      if @storage.type == :ascii
        "501 not permitted in ascii mode"
      else
        @storage.rest = index.to_i
        "350 restarting at #{@storage.rest}, send store or retr to transfer"
      end
    end

    def stru(flag)
      flag.downcase == "f" ?
        "200 success" :
        "504 unrecognized argument"
    end

    def type(flag)
      case flag.downcase
      when "a"; @storage.type = :ascii;  "200 ascii mode"
      when "i"; @storage.type = :binary; "200 binary mode"
      else "504 unrecognized argument"
      end
    end

    def mode(flag)
      flag.downcase == "s" ?
        "200 success" :
        "504 unrecognized argument"
    end

    # +transfer+ will wait for peer to connect to listening socket
    def pasv
      unless @port and @port.first == :passive
        @port = [:passive, mk_passive]
      end

      port, *host = @port.last.getsockname.unpack("@2nCCCC")
      "227 entering passive mode (#{host.join(',')},#{port >> 8},#{port & 0xff})"
    end

    # +transfer+ will first connect to peer
    def port(spec)
      parts = spec.split(",").map(&:to_i)
      host  = parts.first(4)
      port  = parts[5] + (parts[4] << 8)

      if port.between?(1024, 65535) and host == @socket.getpeername.unpack("@4CCCC")
        # close the listening passive socket
        @mux.disconnect(@port.last) if @port and @port.first == :passive
        @port = [:active, host.join("."), port]
        "200 duly noted"
      else
        "501 invalid address"
      end
    end

    def auth(mode)
      if @socket.is_a?(OpenSSL::SSL::Socket)
        if @socket.state.nil?
          case mode.downcase
          when "SSL", "TLS"
            begin
              @socket.accept
              "234 success"
            rescue; "550 handshake failed" end
          else "500 bad argument"          end
        else "500 already encrypted"       end
      else "500 ssl not available"         end
    end

    def prot(mode)
      case mode.downcase
      when "c"; @prot = nil   # plain text data transfers
      when "p"; @prot = :ssl  # encrypt data transfers
      else "504 mode not understood"
      end
    end

    def pbsz(size)
      size.to_i == 0 ?
        "200 success" :                  # stream mode
        "501 only PBSZ 0 is implemented" # block/chunk mode
    end

    def ccc
      "500 not implemented" # TODO: find binding for SSL_shutdown
    end

    def cdc
      "500 not implemented" # TODO: find binding for SSL_shutdown
    end
  end

  class ControlChannel
    include Commands
    attr_reader :mux, :socket, :buffer, :storage, :transcript

    def initialize(mux, session, storage)
      @mux        = mux
      @socket     = session
      @storage    = storage
      @buffer     = ""
      @transcript = []
      @socket.write "220 welcome to the world wide web\r\n"
      @mux.connect(@socket, self)
    end

    # multiplexer callback
    def call(event, socket)
      case event
      when :select
        parse(socket.readpartial(16384))
      when :cleanup
        @port.last.close if @port and @port.first == :passive
        socket.write "221 no, don't leave me!\r\n" rescue nil
        socket.close rescue nil
      end
    rescue
      warn "#{$!.class}: #{$!.message}\n\t" + $!.backtrace.join("\n\t")
      @mux.disconnect(socket)
    end

    private

    def preauth_commands
      @preauth_commands ||= %w(user pass auth)
    end

    def authenticated?
      true
    end

    def parse(buffer)
      @buffer << buffer
      commands = @buffer.split(/(?:\r\n)+/)
      @buffer  = @buffer[/\n\z/] ? "" : commands.pop
      commands.each do |cmd|
        answer = dispatch(*cmd.match(/(\w+)(?:\s+(.*))?$/).captures)
        @socket.write("#{answer}\r\n") if answer
      end
    end

    def dispatch(command, argument)
      name  = command.downcase
      entry = Hash[:command => command, :argument => argument]

      entry[:response] =
        if preauth_commands.include?(command) or authenticated?
          if respond_to?(name) and m = method(name)
            argument ? m.call(argument) : m.call
          else
            "500 unrecognized command"
          end
        else
          "530 please login first"
        end

      @transcript << entry
      entry[:response]
    end

    # establish a listening socket
    def mk_passive
      if @prot == :ssl
        OpenSSL::SSL::SSLServer.new(TCPServer.new(0), @socket.context)
      else
        TCPServer.new(0)
      end
    end

    # establish a connection to the client
    def mk_active(host, port)
      if @prot == :ssl
        OpenSSL::SSL::SSLSocket.new(TCPSocket.new(host, port), @socket.context)
      else
        TCPSocket.new(host, port)
      end
    end

    # blocks until data connection is established
    def transfer # :yields: socket
      data = @port.first == :passive ?
        @port.last.accept :
        mk_active(*@port.last(2))
      @socket.write("150 data connection established\r\n")
      yield data
      "226 end transmission"
    rescue
      "425 can't establish data connection"
    ensure
      @port, @prot = nil
      data.close rescue nil
    end
  end

end
