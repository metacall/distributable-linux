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

.PHONY: all deps build test help default

# Default target
default: all

# All targets
all:
	@$(MAKE) deps
	@$(MAKE) build
	@$(MAKE) test

# Show help
help:
	@echo 'Management commands for metacall-distributable:'
	@echo
	@echo 'Usage:'
	@echo '    make deps         Build dependency images for caching the runtimes.'
	@echo '    make build        Build the tarball for all platforms and architectures.'
	@echo '    make test         Run integration tests for the already built tarballs.'
	@echo '    make help         Show verbose help.'
	@echo

# Build deps
deps:
	@docker stop metacall_distributable 2> /dev/null || true
	@docker rm metacall_distributable 2> /dev/null || true
	@docker build -t metacall/distributable -f Dockerfile .
	@docker run -d --privileged --name metacall_distributable metacall/distributable
	@docker exec -it metacall_distributable /metacall/scripts/deps.sh
	@docker commit metacall_distributable metacall/distributable
	@docker rm -f metacall_distributable

# Build tarball
build:
	@rm -rf out/* && touch out/.gitkeep
	@docker stop metacall_distributable 2> /dev/null || true
	@docker rm metacall_distributable 2> /dev/null || true
	@docker run --rm -v `pwd`/out:/metacall/pack --privileged --name metacall_distributable metacall/distributable /metacall/scripts/build.sh

# Test tarballs
test:
	$(eval CACHE_INVALIDATE := $(shell date +%s))
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:c -f tests/c/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:python -f tests/python/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:node -f tests/node/Dockerfile .

# Empty target do nothing
%:
	@:
