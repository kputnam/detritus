#!/usr/bin/env ruby

class File
  class << self
    def stat
    end
  end
  
  def stat
  end
end

class File::Stat
  def writable?
    #
  end
  
  def writable_real?
    #
  end
  
  def flags
    #
  end
end

class File::Stat::Flags
  UF_SETTABLE  = 0x0000ffff
  UF_NODUMP    = 0x00000001
  UF_IMMUTABLE = 0x00000002
  UF_APPEND    = 0x00000004
  UF_OPAQUE    = 0x00000008
  
  SF_SETTABLE  = 0xffff0000
  SF_ARCHIVED  = 0x00010000
  SF_IMMUTABLE = 0x00020000
  SF_APPEND    = 0x00040000
  
  # SUPER-USER ONLY:
  def arch?
    #
  end
  
  def sappnd?
    #
  end
  
  def schg?
    #
  end
  
  def sunlnk?
    #
  end
  
  # OWNER/SUPER-USER ONLY
  def opaque?
    #
  end
  
  def nodump?
    #
  end
  
  def uappnd?
    #
  end
  
  def uchg?
    #
  end
  
  def uunlnk?
    #
  end
  
  alias sappend?    sappnd?
  alias schange?    schg?
  alias simmutable? schg?
  alias sunlink?    sunlnk?
  alias uappend?    uappnd?
  alias uchange?    uchg?
  alias uimmutable? uchg?
  alias archived?   arch?
  alias uunlink?    uunlnk?
end
