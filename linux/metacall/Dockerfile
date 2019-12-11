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

FROM metacall/distributable:linux-libc-${METACALL_ARCH} AS libc
FROM metacall/distributable:linux-python-${METACALL_ARCH} AS python
FROM metacall/distributable:linux-ruby-${METACALL_ARCH} AS ruby

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

RUN apt-get update \
	&& apt-get install -y --no-install-recommends cmake

ARG METACALL_PATH

# Create output path
RUN mkdir -p ${METACALL_PATH}/core

# Copy all dependencies
COPY --from=libc ${METACALL_PATH}/libc ${METACALL_PATH}/libc
COPY --from=python ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=ruby ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

# Set c and linker flags
ENV CFLAGS="" \
	LDFLAGS=" \
		-fPIC \
		-L${METACALL_PATH}/libc/lib \
		-L${METACALL_PATH}/python/lib \
		-L${METACALL_PATH}/ruby/lib \
		-Wl,-rpath=${METACALL_PATH}/libc/lib \
		-Wl,-rpath=${METACALL_PATH}/python/lib \
		-Wl,-rpath=${METACALL_PATH}/ruby/lib \
		-Wl,--dynamic-linker=${METACALL_PATH}/libc/lib/ld.so"

# Build MetaCall
ARG METACALL_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_VERSION} https://github.com/metacall/core.git \
	&& mkdir -p core/build \
	&& cd core/build \
	&& cmake \
		-DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_INSTALL_PREFIX=${METACALL_PATH}/core \
		-DOPTION_BUILD_SECURITY=OFF `# Disable security in order to improve libc portability / compatibility` \
		-DOPTION_BUILD_DIST_LIBS=ON \
		-DOPTION_FORK_SAFE=OFF \
		-DOPTION_BUILD_TESTS=OFF \
		-DOPTION_BUILD_SCRIPTS=OFF \
		-DOPTION_BUILD_SERIALS=ON \
		-DOPTION_BUILD_SERIALS_RAPID_JSON=ON \
		-DOPTION_BUILD_SERIALS_METACALL=ON \
		-DOPTION_BUILD_EXAMPLES=ON \
		-DOPTION_BUILD_LOADERS=ON \
		-DOPTION_BUILD_LOADERS_MOCK=ON \
		-DOPTION_BUILD_LOADERS_PY=ON \
		-DPYTHON_EXECUTABLE=${METACALL_PATH}/python/bin/python3 \
		-DPYTHON_INCLUDE_DIRS=${METACALL_PATH}/python/include \
		-DPYTHON_LIBRARIES=${METACALL_PATH}/python/lib \
		-DOPTION_BUILD_LOADERS_RB=ON \
		-DOPTION_BUILD_LOADERS_FILE=ON \
		-DOPTION_BUILD_LOADERS_NODE=OFF `# TODO` \
		-DOPTION_BUILD_LOADERS_CS=OFF `# TODO` \
		-DOPTION_BUILD_LOADERS_JS=OFF `# TODO` \
		-DOPTION_BUILD_PORTS=OFF `# TODO` \
		-DCMAKE_CXX_FLAGS=-fpermissive `# Required by Python Port (Swig)` \
		.. \
	&& make -j$(nproc) \
	&& make install \
	&& cd ../.. \
	&& rm -rf core

FROM scratch AS core

ARG METACALL_PATH

COPY --from=builder_core ${METACALL_PATH}/core ${METACALL_PATH}/core
