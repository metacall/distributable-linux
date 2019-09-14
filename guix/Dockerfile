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

FROM ${METACALL_ARCH}/debian:stretch-slim AS guix

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		wget \
		xz-utils \
		locales \
		gnupg \
		ca-certificates \
	&& rm -rf /var/lib/apt/lists/*

RUN wget https://sv.gnu.org/people/viewgpg.php?user_id=15145 -qO - | gpg --import - \
	&& wget -O guix-install.sh https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh \
	&& echo -ne '\n' | yes | bash guix-install.sh \
	&& rm -rf guix-install.sh

RUN echo '#!/bin/sh' > /entrypoint.sh \
	&& echo 'source $GUIX_PROFILE/etc/profile' >> /entrypoint.sh \
	&& echo '/root/.config/guix/current/bin/guix-daemon --build-users-group=guixbuild &' >> /entrypoint.sh \
	&& echo 'DAEMON_PID=$!' >> /entrypoint.sh \
	&& echo 'function cleanup() {' >> /entrypoint.sh \
	&& echo '  kill -9 ${DAEMON_PID}' >> /entrypoint.sh \
	&& echo '}' >> /entrypoint.sh \
	&& echo 'trap cleanup SIGINT SIGTERM' >> /entrypoint.sh \
	&& echo 'exec "$@"' >> /entrypoint.sh \
	&& chmod +x /entrypoint.sh

VOLUME [ "/sys/fs/cgroup" ]

ENTRYPOINT ["bash", "/entrypoint.sh"]
CMD ["sh"]
