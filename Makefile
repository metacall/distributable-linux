#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2020 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

.PHONY: all base pull deps build test release help default

# Default target
default: all

# All targets
all:
	# Intel x64 Windows MinGW architecture
	#$(eval export METACALL_BUILD_TRIPLET := i686-w64-mingw32)

	# Intel/AMD x86_64 Linux architecture
	$(eval export METACALL_BUILD_TRIPLET := x86_64-linux-gnu)

	# Build and test
	@$(MAKE) clear
	@$(MAKE) base
	@$(MAKE) pull
	@$(MAKE) deps
	@$(MAKE) build
	@$(MAKE) test
	@$(MAKE) clear

	# Release the tarballs
	@$(MAKE) release

# Show help
help:
	@echo 'Management commands for metacall-distributable:'
	@echo
	@echo 'Usage:'
	@echo '    make clear        Clear all containers and images.'
	@echo '    make base         Builds base image with MetaCall scripts.'
	@echo '    make pull         Updates Guix repository to the latest version.'
	@echo '    make deps         Build dependency image for caching the runtimes.'
	@echo '    make build        Build the tarball for specified platform and architecture.'
	@echo '    make test         Run integration tests for the already built tarball.'
	@echo '    make release      Releases the tarball into GitHub.'
	@echo '    make help         Show verbose help.'
	@echo

# Clear images and containers
clear:
	# Clear the container
	@docker stop metacall_distributable 2> /dev/null || true
	@docker rm metacall_distributable 2> /dev/null || true
	# Clear the images
	@docker images | grep metacall/distributable_test | tr -s ' ' | cut -d ' ' -f 2 | xargs -I {} docker rmi metacall/distributable_test:{} 2> /dev/null || true
	@docker rmi metacall/distributable 2> /dev/null || true

# Build the base image
base:
	# Build the base image
	@docker build -t metacall/distributable -f Dockerfile .
	@echo "Done"

# Pull latest version of Guix
pull:
	# TODO
	@exit 0
	# Clear the container
	@docker stop metacall_distributable 2> /dev/null || true
	@docker rm metacall_distributable 2> /dev/null || true
	# Delete symlink to current guix profile
	@docker run --privileged --name metacall_distributable metacall/distributable rm -rf /var/guix/profiles/per-user/root/current-guix
	@docker commit metacall_distributable metacall/distributable
	@docker rm -f metacall_distributable
	# Update to latest core-updates branch
	@docker run --privileged --name metacall_distributable metacall/distributable guix pull --branch=core-updates --fallback --commit=142cd4d # TODO
	@docker commit metacall_distributable metacall/distributable
	@docker rm -f metacall_distributable
	@echo "Done"

# Build deps
deps:
	# Clear the container
	@docker stop metacall_distributable 2> /dev/null || true
	@docker rm metacall_distributable 2> /dev/null || true
	# Build dependencies
	@docker run -e METACALL_BUILD_TRIPLET=${METACALL_BUILD_TRIPLET} --privileged --name metacall_distributable metacall/distributable /metacall/scripts/deps.sh
	# Commit dependencies into the image
	@docker commit metacall_distributable metacall/distributable
	# Clear the container
	@docker rm -f metacall_distributable
	@echo "Done"

# Build tarball
build:
	# Clear the tarball
	@rm -rf out/* && touch out/.gitkeep
	# Clear the container
	@docker stop metacall_distributable 2> /dev/null || true
	@docker rm metacall_distributable 2> /dev/null || true
	# Patch the source (metacall.scm) with latest version
	@docker run -v `pwd`/source:/metacall/patch --privileged --name metacall_distributable metacall/distributable cp /metacall/patch/metacall.scm /metacall/source/metacall.scm
	@docker commit metacall_distributable metacall/distributable
	@docker rm -f metacall_distributable
	# Patch the script (build.sh) with latest version
	@docker run -v `pwd`/scripts:/metacall/patch --privileged --name metacall_distributable metacall/distributable cp /metacall/patch/build.sh /metacall/scripts/build.sh
	@docker commit metacall_distributable metacall/distributable
	@docker rm -f metacall_distributable
	# Build tarball and store it into out folder
	@docker run --rm -e METACALL_BUILD_TRIPLET=${METACALL_BUILD_TRIPLET} -v `pwd`/out:/metacall/pack --privileged --name metacall_distributable metacall/distributable /metacall/scripts/build.sh
	@echo "Done"

# Test tarball
test:
	# Generate a unique id for invalidating the cache of test layers
	$(eval CACHE_INVALIDATE := $(shell date +%s))
	# Run tests
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:cli -f tests/cli/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:c -f tests/c/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:python -f tests/python/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_test:node -f tests/node/Dockerfile .
	@echo "Done"

# Release tarballs
release:
	# TODO
	@echo "Done"

# Empty target do nothing
%:
	@:
