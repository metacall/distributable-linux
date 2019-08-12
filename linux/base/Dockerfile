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

FROM ${METACALL_ARCH}/debian:stretch-slim AS base

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		locales \
		git \
		build-essential \
		libltdl-dev \
		gawk \
		gettext \
		texinfo \
		autoconf \
		automake \
		autopoint \
		libtool \
		libedit2 \
		curl \
		gnupg \
		ca-certificates \
	&& rm -rf /var/lib/apt/lists/* \
	&& localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
	&& curl https://ftp.gnu.org/gnu/gnu-keyring.gpg --output gnu-keyring.gpg \
	&& gpg --quiet --import gnu-keyring.gpg \
	&& rm gnu-keyring.gpg

ARG METACALL_ARCH_HOST
ARG METACALL_PATH

ENV LANG=en_US.utf8 \
	DEBIAN_FRONTEND=noninteractive \
	METACALL_ARCH_HOST=${METACALL_ARCH_HOST} \
	METACALL_PATH=${METACALL_PATH}
