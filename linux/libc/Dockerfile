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

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

# Install glibc build dependencies
RUN apt-get update \
	&& apt-get install -y --no-install-recommends bison python3 python3-pip \
	&& rm -rf /var/lib/apt/lists/* \
	&& pip3 install pexpect

ARG METACALL_PATH
ARG METACALL_GLIBC_VERSION

ENV LC_ALL=POSIX \
	PATH=${METACALL_PATH}/bin:/bin:/usr/bin:/sbin:/usr/sbin

# Create output path
RUN mkdir -p ${METACALL_PATH}/libc

# Build glibc
RUN curl https://ftp.gnu.org/gnu/glibc/glibc-${METACALL_GLIBC_VERSION}.tar.bz2 --output glibc.tar.bz2 \
	&& curl https://ftp.gnu.org/gnu/glibc/glibc-${METACALL_GLIBC_VERSION}.tar.bz2.sig --output glibc.tar.bz2.sig \
	&& gpg --verify glibc.tar.bz2.sig glibc.tar.bz2 \
	&& tar -xjf glibc.tar.bz2 \
	&& mkdir -p glibc-${METACALL_GLIBC_VERSION}/build \
	&& cd glibc-${METACALL_GLIBC_VERSION}/build \
	&& ../configure \
		--prefix=${METACALL_PATH}/libc \
		--host=${METACALL_ARCH_HOST} \
		--build=$(../scripts/config.guess) \
		--enable-kernel=3.2 \
		libc_cv_forced_unwind=yes \
		libc_cv_c_cleanup=yes \
	&& make -j $(nproc) \
	&& make install \
	&& cd ../.. \
	&& rm -rf glibc-${METACALL_GLIBC_VERSION} glibc.tar.bz2 glibc.tar.bz2.sig

# Create a common link for any glibc version
RUN ln -s ${METACALL_PATH}/libc/lib/ld-${METACALL_GLIBC_VERSION}.so ${METACALL_PATH}/libc/lib/ld.so

# Test glibc
RUN echo 'int main(){}' > main.c \
	&& gcc main.c \
		-L${METACALL_PATH}/libc/lib \
		-Wl,-rpath=${METACALL_PATH}/libc/lib \
		-Wl,--dynamic-linker=${METACALL_PATH}/libc/lib/ld.so \
	&& readelf -l a.out | grep ": ${METACALL_PATH}" \
	&& rm main.c a.out

FROM scratch AS libc

ARG METACALL_PATH

COPY --from=builder ${METACALL_PATH}/libc ${METACALL_PATH}/libc
