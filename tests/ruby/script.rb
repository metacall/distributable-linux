#!/usr/bin/env ruby

#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2025 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#
#	Licensed under the Apache License, Version 2.0 (the 'License');
#	you may not use this file except in compliance with the License.
#	You may obtain a copy of the License at
#
#		http://www.apache.org/licenses/LICENSE-2.0
#
#	Unless required by applicable law or agreed to in writing, software
#	distributed under the License is distributed on an "AS IS" BASIS,
#	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#	See the License for the specific language governing permissions and
#	limitations under the License.
#

# Debug load path
puts $LOAD_PATH

puts "Ruby simple test"

# TODO:
#
# Ruby 2.7
#
# + metacallcli script.rb
# #<ArgumentError: unknown encoding name: binary>
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/openssl/buffering.rb:1
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/openssl/ssl.rb:13:in `require'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/openssl/ssl.rb:13:in `<top (required)>'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/openssl.rb:21:in `require'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/openssl.rb:21:in `<top (required)>'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/net/https.rb:23:in `require'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/net/https.rb:23:in `<top (required)>'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:321:in `require'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:321:in `open_http'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:764:in `buffer_open'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:235:in `block in open_loop'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:233:in `catch'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:233:in `open_loop'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:174:in `open_uri'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:744:in `open'
# /gnu/store/6669014b1cf15njllsd0ryir5d0gp5ch-ruby-2.7.8/lib/ruby/2.7.0/open-uri.rb:50:in `open'
# eval:25:in `<module:Script>'
# eval:1:in `<main>'
# Error: Ruby evaluation failed script.rb
#
################################################################################################################
#
# Ruby 3.2 & 3.3
#
# #<NoMethodError: undefined method `make_shareable' for Ractor:Class>
# /gnu/store/75dz8nhrawcahgxnjjxpfazmkq6pmcxh-ruby-3.2.3/lib/ruby/3.2.0/uri/common.rb:21:in `<module:URI>'
# /gnu/store/75dz8nhrawcahgxnjjxpfazmkq6pmcxh-ruby-3.2.3/lib/ruby/3.2.0/uri/common.rb:15:in `<top (required)>'# /gnu/store/75dz8nhrawcahgxnjjxpfazmkq6pmcxh-ruby-3.2.3/lib/ruby/3.2.0/uri.rb:94:in `require_relative'
# /gnu/store/75dz8nhrawcahgxnjjxpfazmkq6pmcxh-ruby-3.2.3/lib/ruby/3.2.0/uri.rb:94:in `<top (required)>'
# /gnu/store/75dz8nhrawcahgxnjjxpfazmkq6pmcxh-ruby-3.2.3/lib/ruby/3.2.0/open-uri.rb:2:in `require'
# eval:23:in `require'
# eval:23:in `<module:Script>'uby/3.2.0/open-uri.rb:2:in `<top (required)>'
# eval:1:in `<main>'
#
################################################################################################################
#
# Code:
#
# require 'open-uri'
#
# URI.open('http://www.ruby-lang.org/') {|f|
# 	f.each_line {|line| p line}
# }
################################################################################################################

# TODO: Ruby cannot find gems installed by bundle
#require 'xmlsimple'
#result = XmlSimple.xml_in(html)
#p result
