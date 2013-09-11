
  module CertUtil
    class Config
      attr_accessor :country, :organization, :division, :name, :effective, :expires, :type

      DEFAULTS = { :country      => 'US',
                   :organization => 'Snake Oil, Inc.',
                   :division     => 'Research & Development',
                   :name         => '127.0.0.1',
                   :type         => :server,
                   :effective    => Time.now - 3600,
                   :expires      => Time.now + 3600 }

      def initialize(*args)
        setup = (args.first.is_a?(Hash)) ? DEFAULTS.merge(args.first) : DEFAULTS
        setup.each {|name, value| send "#{name}=", value }
      end
    end

    def self.config(*args) Config.new(*args) end

    # generates a certificate authority
    def self.ca(config)
      name = [ ['C', config.country, OpenSSL::ASN1::PRINTABLESTRING],
               ['O', config.organization, OpenSSL::ASN1::UTF8STRING],
               ['OU', config.division, OpenSSL::ASN1::UTF8STRING],
               ['CN', config.name, OpenSSL::ASN1::UTF8STRING] ]

      key  = OpenSSL::PKey::RSA.new 1024
      cert = OpenSSL::X509::Certificate.new.tap do |c|
        c.subject    = OpenSSL::X509::Name.new name
        c.issuer     = OpenSSL::X509::Name.new name
        c.not_before = config.effective
        c.not_after  = config.expires
        c.public_key = key.public_key
        c.serial     = 0
        c.version    = 2
      end

      ex = OpenSSL::X509::ExtensionFactory.new
      ex.subject_certificate = cert
      ex.issuer_certificate  = cert
      cert.extensions = [ ex.create_extension('basicConstraints', 'CA:TRUE', true),
                          ex.create_extension('nsComment', 'Ruby/OpenSSL'),
                          ex.create_extension('subjectKeyIdentifier', 'hash'),
                          ex.create_extension('keyUsage', 'cRLSign,keyCertSign', true) ]
      cert.add_extension(ex.create_extension('authorityKeyIdentifier', 'keyid:always,issuer:always'))
      cert.sign(key, OpenSSL::Digest::SHA1.new)
      return cert, key
    end

    # generates a certificate signing request
    def self.request(key, config)
      name = [['C', config.country, OpenSSL::ASN1::PRINTABLESTRING],
              ['O', config.organization, OpenSSL::ASN1::UTF8STRING],
              ['OU', config.division, OpenSSL::ASN1::UTF8STRING],
              ['CN', config.name, OpenSSL::ASN1::UTF8STRING]]

      req = OpenSSL::X509::Request.new.tap do |r|
        r.version = 0
        r.subject = OpenSSL::X509::Name.new name
        r.public_key = key.public_key
        r.sign key, OpenSSL::Digest::MD5.new
      end
    end

    # generates a signed certificate
    def self.sign(csr, ca, ca_key, config)
      raise ArgumentError, "first arg must be an OpenSSL::X509::Request" unless csr.is_a? OpenSSL::X509::Request
      raise ArgumentError, "second arg must be an OpenSSL::X509::Certificate" unless ca.is_a? OpenSSL::X509::Certificate
      raise ArgumentError, "third arg must be an OpenSSL::PKey::RSA" unless ca_key.is_a? OpenSSL::PKey::RSA
      raise "CSR sign verification failed" unless csr.verify csr.public_key
      raise "CSR key length too short" if csr.public_key.n.num_bits < 1024
      raise "CSR key length too long" if csr.public_key.n.num_bits > 1024
      #raise "CSR DN doesn't match" unless csr.subject.to_a[0, x] == name
      
      cert = OpenSSL::X509::Certificate.new.tap do |c|
        c.subject    = csr.subject
        c.issuer     = ca.subject
        c.not_before = config.effective
        c.not_after  = config.expires
        c.public_key = csr.public_key
        c.serial     = 0
        c.version    = 2
      end

      constraint, usage, ext =
        case config.type
        when :server, nil
          [ 'CA:FALSE', %w[digitalSignature keyEncipherment], %w[serverAuth] ]
        when :ca
          [ 'CA:TRUE', %w[cRLSign keyCertSign], [] ]
        when :client
          [ 'CA:FALSE', %w[nonRepudiation digitalSignature keyEncipherment], %w[clientAuth emailProtection] ]
        end

      ex = OpenSSL::X509::ExtensionFactory.new
      ex.subject_certificate = cert
      ex.issuer_certificate  = ca

      cert.extensions = [ ex.create_extension('basicConstraints', constraint, true),
                          ex.create_extension('nsComment', 'Ruby/OpenSSL'),
                          ex.create_extension('subjectKeyIdentifier', 'hash'),
                          ex.create_extension('keyUsage', usage.join(',')),
                          ex.create_extension('extendedKeyUsage', ext.join(',')) ]

      cert.sign(ca_key, OpenSSL::Digest::SHA1.new)
      cert
    end

    # generate PEM-encoded key
    def self.encrypt(key, secret)
      key.export OpenSSL::Cipher::DES.new(:EDE3, :CBC), secret
    end

    # generates a certificate signed by a ca
    def self.signed_cert(ca_conf=nil, cert_conf=nil)
      ca_conf   ||= Config.new
      cert_conf ||= Config.new

      ca, ca_key = ca(ca_conf)
      key        = OpenSSL::PKey::RSA.new(1024)
      csr        = request(key, cert_conf)
      signed     = sign(csr, ca, ca_key, cert_conf)

      #caPEM = encrypt(ca_key, 'password')
      #clPEM = encrypt(key, 'secret')
      
      # ca can be used to authenticate signed
      #   key is required to present signed
      return signed, key, ca
    end
  end

