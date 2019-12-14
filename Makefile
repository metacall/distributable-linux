#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2019 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#
#	Licensed under the Apache License, Version 2.0 (the "License");
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

.PHONY: all build help default

# Default target
default: all

# All targets
all:
	$(MAKE) build

# Show help
help:
	@echo 'Management commands for metacall-distributable:'
	@echo
	@echo 'Usage:'
	@echo '    make build                    Build all images for all platforms and architectures.'
	@echo '    make help                     Show verbose help.'
	@echo

# Build all images
build:
#	Build linux
	@cd linux && $(MAKE) -f Makefile

#	Build windows
	@cd windows && $(MAKE) -f Makefile

#	Build macos
	@cd macos && $(MAKE) -f Makefile

# Test targets
test:
#	Run test suite
	@cd test && $(MAKE) -f Makefile

# Empty target do nothing
%:
	@:
