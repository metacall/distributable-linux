#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2024 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

.PHONY: all download base pull deps build test help default

# Default target
default: all

# All targets
all:
	@$(MAKE) clear
	@$(MAKE) base
	@$(MAKE) pull
	@$(MAKE) deps
	@$(MAKE) build
	@$(MAKE) test

# Show help
help:
	@echo 'Management commands for metacall-distributable:'
	@echo
	@echo 'Usage:'
	@echo '    make download     Downloads MetaCall latest version, extracts the checksum and updates it in metacall.scm.'
	@echo '    make base         Builds base image with MetaCall scripts.'
	@echo '    make pull         Updates Guix repository to the latest version.'
	@echo '    make deps         Build dependency images for caching the runtimes.'
	@echo '    make build        Build the tarball for all platforms and architectures.'
	@echo '    make test         Run integration tests for the already built tarballs.'
	@echo '    make clear        Clear all containers and images.'
	@echo '    make help         Show verbose help.'
	@echo

# Update MetaCall source
download:
#	Get latest tag from the repository
	$(eval LATEST_TAG := $(shell git ls-remote --tags https://github.com/metacall/core.git | sort -t '/' -k 3 -V | tail -n1 | sed 's/.*\///; s/\^{}//'))
#	Download the source and extract the checksum
	$(eval CHECKSUM := $(shell docker run --rm --privileged -it metacall/guix guix download https://github.com/metacall/core/archive/${LATEST_TAG}.tar.gz | tail -n1))
#	Get latest version
	$(eval LATEST_VERSION := $(shell printf "${LATEST_TAG}" | tail -c +2))
#	Print version and checksum
	@echo "${LATEST_VERSION} ${CHECKSUM}"
#	Update MetaCall source with latest version
	@sed -i '/(name "metacall")/!b;n;c\    (version "${LATEST_VERSION}")' source/metacall.scm
#	Update MetaCall source with checksum
	@sed -i '/(uri (string-append "https:\/\/github.com\/metacall\/core\/archive\/v" version ".tar.gz"))/!b;n;c\        (sha256 (base32 "${CHECKSUM}"))' source/metacall.scm

# Build base Docker image
base:
#	Clear the container
	@docker stop metacall_distributable_linux 2> /dev/null || true
	@docker rm metacall_distributable_linux 2> /dev/null || true
#	Build the base image
	@docker build -t metacall/distributable_linux -f Dockerfile .

# Pull latest version of Guix
pull:
#	Clear the container
	@docker stop metacall_distributable_linux 2> /dev/null || true
	@docker rm metacall_distributable_linux 2> /dev/null || true
#	Install the additional channels and pull
	@docker run --privileged --name metacall_distributable_linux metacall/distributable_linux sh -c 'guix pull'
	@docker commit metacall_distributable_linux metacall/distributable_linux
	@docker rm -f metacall_distributable_linux
	@echo "Done"

# Build deps
deps:
#	Clear the container
	@docker stop metacall_distributable_linux 2> /dev/null || true
	@docker rm metacall_distributable_linux 2> /dev/null || true
#	Patch the source (metacall.scm) with latest version
	@docker run -v `pwd`/source:/metacall/patch --privileged --name metacall_distributable_linux metacall/distributable_linux cp /metacall/patch/metacall.scm /metacall/source/metacall.scm
	@docker commit metacall_distributable_linux metacall/distributable_linux
	@docker rm -f metacall_distributable_linux
#	Patch the script (deps.sh) with latest version
	@docker run -v `pwd`/scripts:/metacall/patch --privileged --name metacall_distributable_linux metacall/distributable_linux cp /metacall/patch/deps.sh /metacall/scripts/deps.sh
	@docker commit metacall_distributable_linux metacall/distributable_linux
	@docker rm -f metacall_distributable_linux
#	Build dependencies
	@docker run --privileged --name metacall_distributable_linux metacall/distributable_linux /metacall/scripts/deps.sh
#	Commit dependencies into the image
	@docker commit metacall_distributable_linux metacall/distributable_linux
#	Clear the container
	@docker rm -f metacall_distributable_linux
	@echo "Done"

# Build tarball
build:
#	Clear the container
	@docker stop metacall_distributable_linux 2> /dev/null || true
	@docker rm metacall_distributable_linux 2> /dev/null || true
#	Patch the source (metacall.scm) with latest version
	@docker run -v `pwd`/source:/metacall/patch --privileged --name metacall_distributable_linux metacall/distributable_linux cp /metacall/patch/metacall.scm /metacall/source/metacall.scm
	@docker commit metacall_distributable_linux metacall/distributable_linux
	@docker rm -f metacall_distributable_linux
#	Patch the script (build.sh) with latest version
	@docker run -v `pwd`/scripts:/metacall/patch --privileged --name metacall_distributable_linux metacall/distributable_linux cp /metacall/patch/build.sh /metacall/scripts/build.sh
	@docker commit metacall_distributable_linux metacall/distributable_linux
	@docker rm -f metacall_distributable_linux
#	Build tarball and store it into out folder
	@docker run --rm -v `pwd`/out:/metacall/pack --privileged --name metacall_distributable_linux metacall/distributable_linux /metacall/scripts/build.sh
	@echo "Done"

# Test tarballs
test:
#	Generate a unique id for invalidating the cache of test layers
	$(eval CACHE_INVALIDATE := $(shell date +%s))
#	Run tests
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_linux_test:cli -f tests/cli/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_linux_test:c -f tests/c/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_linux_test:python -f tests/python/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_linux_test:node -f tests/node/Dockerfile .
	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_linux_test:typescript -f tests/typescript/Dockerfile .
#	TODO:
#	@docker build --build-arg CACHE_INVALIDATE=${CACHE_INVALIDATE} -t metacall/distributable_linux_test:tsx -f tests/tsx/Dockerfile .
	@echo "Done"

# Clear images and containers
clear:
#	Clear the tarball
	@rm -rf out/* && touch out/.gitkeep
#	Clear the container
	@docker stop metacall_distributable_linux 2> /dev/null || true
	@docker rm metacall_distributable_linux 2> /dev/null || true
#	Clear the images
	@docker images | grep metacall/distributable_linux_test | tr -s ' ' | cut -d ' ' -f 2 | xargs -I {} docker rmi metacall/distributable_linux_test:{} 2> /dev/null || true
	@docker rmi metacall/distributable_linux 2> /dev/null || true

# Empty target do nothing
%:
	@:
