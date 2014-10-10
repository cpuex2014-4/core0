#!/usr/bin/ruby
# -*- coding: utf-8 -*-

File.open("out.dat", "r") do|dat|
  File.open("out.txt", "w") do|txt|
    a = dat.each_line.map {|chr| chr.chomp.to_i }
    txt.write a.pack('C*')
  end
end
