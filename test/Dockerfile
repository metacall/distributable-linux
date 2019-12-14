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
ARG METACALL_OS

FROM metacall/distributable:${METACALL_OS}-libc-${METACALL_ARCH} AS libc
FROM metacall/distributable:${METACALL_OS}-python-${METACALL_ARCH} AS python
FROM metacall/distributable:${METACALL_OS}-ruby-${METACALL_ARCH} AS ruby
FROM metacall/distributable:${METACALL_OS}-core-${METACALL_ARCH} AS core

FROM scratch AS base_test

# Image descriptor
LABEL copyright.name="Vicente Eduardo Ferrer Garcia" \
	copyright.address="vic798@gmail.com" \
	maintainer.name="Vicente Eduardo Ferrer Garcia" \
	maintainer.address="vic798@gmail.com" \
	vendor="MetaCall Inc." \
	version="0.1"

ARG METACALL_PATH

COPY --from=libc ${METACALL_PATH}/libc ${METACALL_PATH}/libc
COPY --from=python ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=ruby ${METACALL_PATH}/ruby ${METACALL_PATH}/ruby
COPY --from=core ${METACALL_PATH}/core ${METACALL_PATH}/core

FROM python:3.7.5-slim-buster AS python_test

COPY --from=base_test ${METACALL_PATH}/ ${METACALL_PATH}/

COPY test/source/test.py /

COPY test/scripts/ /scripts/

RUN cd / \
	&& sed -i "s#@METACALL_LIBRARY_PATH@#${METACALL_PATH}/lib#g" test.py \
	&& python3 test.py | grep \
		-e 'Hello World'

ENTRYPOINT ["sh", "-c"]
