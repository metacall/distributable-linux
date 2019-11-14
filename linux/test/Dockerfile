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

FROM ${METACALL_ARCH}/alpine:3.10.1 AS test

RUN apk update \
	&& apk add binutils

ARG METACALL_PATH

ENV LD_LIBRARY_PATH=${METACALL_PATH}/libc/lib:${METACALL_PATH}/python/lib:${METACALL_PATH}/ruby/lib \
	SHLIB_PATH=${METACALL_PATH}/libc/lib:${METACALL_PATH}/python/lib:${METACALL_PATH}/ruby/lib

COPY --from=libc ${METACALL_PATH}/libc ${METACALL_PATH}/libc
COPY --from=python ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=ruby ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby

# Test binaries
RUN cd ${METACALL_PATH}/python/bin \
	&& export ELFS=`ls python[0-9].[0-9]` \
	&& for elf in $ELFS; do readelf -l ${elf} | grep interpreter; done \
	&& cd ${METACALL_PATH}/ruby/bin \
	&& export ELFS=`ls ruby` \
	&& for elf in $ELFS; do readelf -l ${elf} | grep interpreter; done
