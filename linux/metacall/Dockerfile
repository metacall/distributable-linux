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

ARG METACALL_ARCH

FROM ${METACALL_ARCH}/debian:stretch-slim AS builder_base

# Install dependencies
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		`# APT dependencies` \
		locales \
		git \
		build-essential \
		cmake \
		wget \
		apt-utils \
		apt-transport-https \
		gnupg \
		dirmngr \
		ca-certificates \
		`# Funchook dependencies` \
		autoconf \
		`# Swig dependencies` \
		libpcre3-dev \
		swig \
	&& rm -rf /var/lib/apt/lists/* \
	&& localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ARG METACALL_PATH

ENV LANG=en_US.utf8 \
	DEBIAN_FRONTEND=noninteractive \
	METACALL_PATH=${METACALL_PATH}

FROM metacall/distributable:linux-libc-${METACALL_ARCH} AS libc
FROM metacall/distributable:linux-python-${METACALL_ARCH} AS python
FROM metacall/distributable:linux-ruby-${METACALL_ARCH} AS ruby

FROM builder_base AS builder

# Copy dependencies
COPY --from=libc ${METACALL_PATH}/libc ${METACALL_PATH}/libc
COPY --from=python ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=ruby ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

# Set c flags
ENV CFLAGS=" \
	-I${METACALL_PATH}/libc/include \
	-I${METACALL_PATH}/python/include \
	-I${METACALL_PATH}/ruby/include"

# Set linker flags
ENV LDFLAGS=" \
		-fPIC \
		-L${METACALL_PATH}/libc/lib \
		-L${METACALL_PATH}/python/lib \
		-L${METACALL_PATH}/ruby/lib \
		-Wl,-rpath=${METACALL_PATH}/libc/lib \
		-Wl,-rpath=${METACALL_PATH}/python/lib \
		-Wl,-rpath=${METACALL_PATH}/ruby/lib \
		-Wl,--dynamic-linker=${METACALL_PATH}/libc/lib/ld.so"

# Create output path
RUN mkdir -p ${METACALL_PATH}/metacall

FROM builder AS builder_metacall

ARG METACALL_BUILD_TYPE
ARG METACALL_VERSION
ARG METACALL_PYTHON_VERSION
ARG METACALL_NETCORE_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_VERSION} https://github.com/metacall/core.git core \
	&& mkdir core/build \
	&& cd core/build \
	&& cmake .. \
		-DCMAKE_INSTALL_PREFIX=${METACALL_PATH}/metacall \
		-DCMAKE_BUILD_TYPE=${METACALL_BUILD_TYPE} \
		-DOPTION_BUILD_DIST_LIBS=ON \
		-DOPTION_BUILD_EXAMPLES=ON \
		-DOPTION_BUILD_LOADERS=ON \
		-DOPTION_BUILD_SCRIPTS=ON \
		-DOPTION_BUILD_LOADERS_MOCK=ON \
		-DPYTHON_EXECUTABLE=${METACALL_PATH}/python/bin/python${METACALL_PYTHON_VERSION} \
		-DOPTION_BUILD_LOADERS_PY=ON \
		-DOPTION_BUILD_SCRIPTS_PY=ON \
		-DOPTION_BUILD_LOADERS_RB=ON \
		-DOPTION_BUILD_SCRIPTS_RB=ON \
		-DDOTNET_CORE_PATH=${METACALL_PATH}/netcore/share/dotnet/shared/Microsoft.NETCore.App/${METACALL_NETCORE_VERSION}/ \
		-DOPTION_BUILD_LOADERS_CS=OFF \
		-DOPTION_BUILD_SCRIPTS_CS=OFF \
		-DOPTION_BUILD_LOADERS_JS=OFF \
		-DOPTION_BUILD_SCRIPTS_JS=OFF \
		-DOPTION_BUILD_LOADERS_NODE=OFF \
		-DOPTION_BUILD_SCRIPTS_NODE=OFF \
		-DOPTION_BUILD_LOADERS_FILE=ON \
		-DOPTION_BUILD_SCRIPTS_FILE=ON \
		-DOPTION_BUILD_PORTS=ON \
		-DOPTION_COVERAGE=OFF \
	&& make -j$(nproc) \
	&& ctest -VV -C ${METACALL_BUILD_TYPE} \
	&& make install \
	&& cd ../.. \
	&& rm -rf core

FROM scratch AS metacall

ARG METACALL_PATH

COPY --from=builder_metacall ${METACALL_PATH} ${METACALL_PATH}
