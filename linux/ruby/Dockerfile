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

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

ARG METACALL_PATH

# Copy libc
COPY --from=libc ${METACALL_PATH}/libc ${METACALL_PATH}/libc

# Set c flags
ENV CFLAGS=" \
	-I${METACALL_PATH}/ruby/include"

# Set linker flags
ENV LDFLAGS=" \
		-fPIC \
		-L${METACALL_PATH}/libc/lib \
		-L${METACALL_PATH}/ruby/lib \
		-Wl,-rpath=${METACALL_PATH}/libc/lib \
		-Wl,-rpath=${METACALL_PATH}/ruby/lib \
		-Wl,--dynamic-linker=${METACALL_PATH}/libc/lib/ld.so"

# Create output path
RUN mkdir -p ${METACALL_PATH}/ruby

# Install zlib
FROM builder AS builder_zlib

ARG METACALL_ZLIB_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_ZLIB_VERSION} https://github.com/madler/zlib.git zlib \
	&& cd zlib \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf zlib

# Install libffi
FROM builder AS builder_libffi

ARG METACALL_LIBFFI_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_LIBFFI_VERSION} https://github.com/libffi/libffi.git libffi \
	&& cd libffi \
	&& ./autogen.sh \
	&& sed -e '/^includesdir/ s/$(libdir).*$/$(includedir)/' -i include/Makefile.in \
	&& sed -e '/^includedir/ s/=.*$/=@includedir@/' -e 's/^Cflags: -I${includedir}/Cflags:/' -i libffi.pc.in \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--disable-static \
		--disable-docs \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf libffi

# Install libssl
FROM builder AS builder_libssl

COPY --from=builder_zlib ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

ARG METACALL_LIBSSL_VERSION

RUN git clone -j8 --recursive --single-branch --branch ${METACALL_LIBSSL_VERSION} https://github.com/openssl/openssl.git libssl \
	&& cd libssl \
	&& ./config \
		--prefix=${METACALL_PATH}/ruby \
		--openssldir=${METACALL_PATH}/ruby \
		--with-zlib-include=${METACALL_PATH}/ruby/include \
		--with-zlib-lib=${METACALL_PATH}/ruby/lib \
		shared \
		no-tests \
		zlib \
		zlib-dynamic \
	&& make -j $(nproc) \
	&& make install_sw \
	&& cd .. \
	&& rm -rf libssl

# Install libncurses
FROM builder AS builder_libncurses

ARG METACALL_LIBNCURSES_VERSION

RUN curl https://ftp.gnu.org/gnu/ncurses/ncurses-${METACALL_LIBNCURSES_VERSION}.tar.gz --output ncurses.tar.gz \
	&& curl https://ftp.gnu.org/gnu/ncurses/ncurses-${METACALL_LIBNCURSES_VERSION}.tar.gz.sig --output ncurses.tar.gz.sig \
	&& gpg --verify ncurses.tar.gz.sig ncurses.tar.gz \
	&& tar -xzf ncurses.tar.gz \
	&& cd ncurses-${METACALL_LIBNCURSES_VERSION} \
	&& sed -i '/LIBTOOL_INSTALL/d' c++/Makefile.in \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--with-shared \
		--without-debug \
		--without-normal \
		--without-manpages \
		--without-ada \
		--enable-widec \
		--enable-pc-files \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf ncurses.tar.gz ncurses.tar.gz.sig ncurses-${METACALL_LIBNCURSES_VERSION}

# Install libgdbm
FROM builder AS builder_libgdbm

ARG METACALL_LIBGDBM_VERSION

RUN curl https://ftp.gnu.org/gnu/gdbm/gdbm-${METACALL_LIBGDBM_VERSION}.tar.gz --output gdbm.tar.gz \
	&& curl https://ftp.gnu.org/gnu/gdbm/gdbm-${METACALL_LIBGDBM_VERSION}.tar.gz.sig --output gdbm.tar.gz.sig \
	&& gpg --verify gdbm.tar.gz.sig gdbm.tar.gz \
	&& tar -xzf gdbm.tar.gz \
	&& cd gdbm-${METACALL_LIBGDBM_VERSION} \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--disable-static \
		--enable-libgdbm-compat \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf gdbm.tar.gz.sig gdbm.tar.gz gdbm-${METACALL_LIBGDBM_VERSION}

# Install libreadline
FROM builder AS builder_libreadline

COPY --from=builder_libncurses ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

ARG METACALL_LIBREADLINE_VERSION

RUN curl https://ftp.gnu.org/gnu/readline/readline-${METACALL_LIBREADLINE_VERSION}.tar.gz --output readline.tar.gz \
	&& curl https://ftp.gnu.org/gnu/readline/readline-${METACALL_LIBREADLINE_VERSION}.tar.gz.sig --output readline.tar.gz.sig \
	&& gpg --verify readline.tar.gz.sig readline.tar.gz \
	&& tar -xzf readline.tar.gz \
	&& cd readline-${METACALL_LIBREADLINE_VERSION} \
	&& sed -i '/MV.*old/d' Makefile.in \
	&& sed -i '/{OLDSUFF}/c:' support/shlib-install \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--disable-static \
		--enable-shared \
		--enable-multibyte \
		--with-curses \
	&& make SHLIB_LIBS="-L${METACALL_PATH}/ruby/lib -lncursesw" -j $(nproc) \
	&& make SHLIB_LIBS="-L${METACALL_PATH}/ruby/lib -lncursesw" install \
	&& cd .. \
	&& rm -rf readline-${METACALL_LIBREADLINE_VERSION}

# Install libgmp
FROM builder AS builder_libgmp

ARG METACALL_LIBGMP_VERSION

RUN apt-get update \
	&& apt-get install xz-utils -y --no-install-recommends \
	&& curl https://gmplib.org/download/gmp/gmp-${METACALL_LIBGMP_VERSION}.tar.xz --output gmp.tar.xz \
	&& curl https://gmplib.org/download/gmp/gmp-${METACALL_LIBGMP_VERSION}.tar.xz.sig --output gmp.tar.xz.sig \
	&& gpg --verify gmp.tar.xz.sig gmp.tar.xz \
	&& tar -xf gmp.tar.xz \
	&& cd gmp-${METACALL_LIBGMP_VERSION} \
	&& cp configfsf.guess config.guess \
	&& cp configfsf.sub config.sub \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--disable-static \
		--enable-shared \
		--disable-cxx \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf gmp-${METACALL_LIBGMP_VERSION}

# Install libtinfo
FROM builder AS builder_libtinfo

COPY --from=builder_libncurses ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

ARG METACALL_LIBNCURSES_VERSION
ARG METACALL_LIBTINFO_VERSION

RUN ln -s ${METACALL_PATH}/ruby/lib/libncursesw.so.${METACALL_LIBNCURSES_VERSION} ${METACALL_PATH}/ruby/lib/libtinfo.so.${METACALL_LIBTINFO_VERSION} \
	&& ln -s ${METACALL_PATH}/ruby/lib/libtinfo.so.${METACALL_LIBTINFO_VERSION} ${METACALL_PATH}/ruby/lib/libtinfo.so

# Install libyaml
FROM builder AS builder_libyaml

ARG METACALL_LIBYAML_VERSION

RUN git clone -j8 --recursive --single-branch --branch ${METACALL_LIBYAML_VERSION} https://github.com/yaml/libyaml.git libyaml \
	&& cd libyaml \
	&& ./bootstrap \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf libyaml

# Install libtcl
FROM builder AS builder_libtcl

COPY --from=builder_zlib ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

ARG METACALL_LIBTCL_VERSION

RUN export METACALL_LIBTCL_VERSION_BRANCH=$(printf '%s' "${METACALL_LIBTCL_VERSION}" | tr '.' '-') \
	&& git clone -j8 --single-branch --branch core-${METACALL_LIBTCL_VERSION_BRANCH} https://github.com/tcltk/tcl.git tcl \
	&& cd tcl/unix \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--exec-prefix=${METACALL_PATH}/ruby \
		--enable-shared \
		--enable-threads \
	&& make -j $(nproc) \
	&& sed -i \
		-e "s@^\(TCL_SRC_DIR='\).*@\1${METACALL_PATH}/ruby/include'@" \
		-e "/TCL_B/s@='\(-L\)\?.*unix@='\1${METACALL_PATH}/ruby/lib@" \
		-e "/SEARCH/s/=.*/=''/" \
		tclConfig.sh \
	&& make install \
	&& make install-private-headers \
	&& ln -rs ${METACALL_PATH}/ruby/bin/tclsh ${METACALL_PATH}/ruby/bin/$(expr substr "${METACALL_LIBTCL_VERSION}" 1 3) \
	&& cd ../.. \
	&& rm -rf tcl

# Install libtk
FROM builder AS builder_libtk

COPY --from=builder_libtcl ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

ARG METACALL_LIBTK_VERSION

# TODO

FROM builder AS builder_ruby

# Copy all dependencies
COPY --from=builder_zlib ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libffi ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libssl ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libncurses ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libgdbm ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libreadline ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libgmp ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libtinfo ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libyaml ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=builder_libtk ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

# Build Ruby
ARG METACALL_RUBY_VERSION

# Build CRuby
# TODO: Remove --without-x11 and --with-out-ext=tk,tk/* and add x11/tk dependencies
RUN apt-get update \
	&& apt-get install ruby -y --no-install-recommends \
	&& git clone -j8 --single-branch --branch v${METACALL_RUBY_VERSION} https://github.com/ruby/ruby.git ruby \
	&& cd ruby \
	&& autoconf \
	&& ./configure \
		--prefix=${METACALL_PATH}/ruby \
		--enable-shared \
		--disable-install-doc \
		--with-readline-dir=${METACALL_PATH}/ruby \
		--with-openssl-dir=${METACALL_PATH}/ruby \
		--without-x11 \
		--with-out-ext=win32,win32ole,tk,tk/* \
		CFLAGS="${CFLAGS} -D_FORTIFY_SOURCE=2 -O3 -fstack-protector-strong -Wformat -Werror=format-security" \
		LDFLAGS="-Wl,-z,relro ${LDFLAGS}" \
	&& make -j$(nproc) \
	&& make update-gems \
	&& make extract-gems \
	&& make install

FROM scratch AS ruby

ARG METACALL_PATH

COPY --from=builder_ruby ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
