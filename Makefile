#
# MetaCall Kubernetes by Parra Studios
# Copyright (C) 2016 - 2017 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#

.PHONY: all build help default

# Colors
COLOR_RED=\033[0;31m
COLOR_GREEN=\033[0;32m
COLOR_OFF=\033[0m

# Default target
default: all

# All targets
all:
	$(MAKE) build

# Show help
help:
	@echo 'Management commands for metacall-distributable:'
	@echo
	@echo 'Usage:'
	@echo '    make build                    Build all images for all platforms and architectures.'
	@echo '    make help                     Show verbose help.'
	@echo

# Build all images
build:
#	Build linux
	@$(MAKE) -f linux/Makefile

#	Build windows
	@$(MAKE) -f windows/Makefile

#	Build macos
	@$(MAKE) -f macos/Makefile

# Empty target do nothing
%:
	@:
