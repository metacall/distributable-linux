/*
 *	MetaCall Distributable by Parra Studios
 *	Distributable infrastructure for MetaCall.
 *
 *	Copyright (C) 2016 - 2024 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
 *
 *	Licensed under the Apache License, Version 2.0 (the "License");
 *	you may not use this file except in compliance with the License.
 *	You may obtain a copy of the License at
 *
 *		http://www.apache.org/licenses/LICENSE-2.0
 *
 *	Unless required by applicable law or agreed to in writing, software
 *	distributed under the License is distributed on an "AS IS" BASIS,
 *	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *	See the License for the specific language governing permissions and
 *	limitations under the License.
 *
 */

#include <metacall/metacall.h>
#include <stdio.h>

static int cleanup(int code)
{
	metacall_destroy();

	return code;
}

int main(int argc, char *argv[])
{
	struct metacall_log_stdio_type log_stdio = { stdout };

	printf("%s\n", metacall_print_info());

	if (metacall_log(METACALL_LOG_STDIO, (void *)&log_stdio) != 0)
	{
		return cleanup(1);
	}

	if (metacall_initialize() != 0)
	{
		return cleanup(2);
	}

	/* Backtrace */
	{
		char *segfault = (void *)&cleanup;
		*segfault = 34;
	}

	return cleanup(0);
}
