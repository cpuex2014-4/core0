#!/usr/bin/ruby
# -*- coding: utf-8 -*-

File.open("in.txt", "r") do|txt|
  File.open("in.dat", "w") do|dat|
    txt.read.unpack('C*').each do|chr|
      dat.puts chr.to_s
    end
  end
end
