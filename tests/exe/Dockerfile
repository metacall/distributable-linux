#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2025 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

FROM debian:trixie-slim AS exe_test

# Image descriptor
LABEL copyright.name="Vicente Eduardo Ferrer Garcia" \
	copyright.address="vic798@gmail.com" \
	maintainer.name="Vicente Eduardo Ferrer Garcia" \
	maintainer.address="vic798@gmail.com" \
	vendor="MetaCall Inc." \
	version="0.1"

SHELL ["/bin/bash", "-c"]

RUN apt-get update \
	&& apt-get install --no-install-recommends -y python3 nodejs ruby

COPY out/tarball.tar.gz /

RUN cd / \
	&& tar --no-same-owner --no-same-permissions -xzf tarball.tar.gz \
	&& rm tarball.tar.gz

COPY tests/scripts/ /scripts/

WORKDIR /scripts

ARG CACHE_INVALIDATE

RUN echo "Running tests ${CACHE_INVALIDATE}" \
	&& set -exo pipefail \
	&& source /gnu/etc/profile \
	&& whereis node \
	&& ls -la /usr/bin/node \
	&& /usr/bin/node port-test.js | grep \
		-e 'Node.js port works' \
	&& whereis python3 \
	&& ls -la /usr/bin/python3 \
	&& /usr/bin/python3 port-test.py | grep \
		-e 'Python port works'
	# \
	# && ruby port-test.rb | grep \
	# 	-e 'Ruby port works'

ENTRYPOINT ["sh", "-c"]
