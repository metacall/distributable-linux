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

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS deps

# Copy libraries
RUN \
	export LIBS="c pthread dl util m"; \
	metacall-dist-lib /lib/${METACALL_ARCH_PATH} ${METACALL_PATH}/libc/lib 'lib${lib}\.so' && \
	export LIBS="ld-linux"; \
	metacall-dist-lib /lib/${METACALL_ARCH_PATH} ${METACALL_PATH}/libc/lib '${lib}.*\.so$'

FROM scratch AS libc

ARG METACALL_PATH

COPY --from=deps ${METACALL_PATH}/libc/lib ${METACALL_PATH}/libc/lib
