/*
 *	MetaCall Distributable by Parra Studios
 *	Distributable infrastructure for MetaCall.
 *
 *	Copyright (C) 2016 - 2019 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static int cleanup(int code)
{
	if (metacall_destroy() != 0)
	{
		return code != 0 ? -code : -255;
	}

	return code;
}

int main(int argc, char *argv[])
{
	struct metacall_log_stdio_type log_stdio = { stdout };

	printf(metacall_print_info());

	if (metacall_log(METACALL_LOG_STDIO, (void *)&log_stdio) != 0)
	{
		return cleanup(1);
	}

	if (metacall_initialize() != 0)
	{
		return cleanup(2);
	}

	/* Mock */
	{
		const char * mock_scripts[] =
		{
			"test.mock"
		};

		const enum metacall_value_id three_str_ids[] =
		{
			METACALL_STRING, METACALL_STRING, METACALL_STRING
		};

		void * ret = NULL;

		if (metacall_load_from_file("mock", mock_scripts, sizeof(mock_scripts) / sizeof(mock_scripts[0]), NULL) != 0)
		{
			return cleanup(6);
		}

		ret = metacallt("three_str", three_str_ids, "a", "b", "c");

		if (ret == NULL)
		{
			return cleanup(7);
		}

		/* Check result */
		{
			const char result[] = "Hello World";
			const char * str = metacall_value_to_string(ret);

			if (strncmp(str, result, sizeof(result)) != 0)
			{
				return cleanup(8);
			}

			printf("%s\n", str);
		}

		metacall_value_destroy(ret);
	}

	/* Python */
	{
		const char * py_scripts[] =
		{
			"/scripts/sum.py"
		};

		const enum metacall_value_id sum_ids[] =
		{
			METACALL_STRING, METACALL_STRING, METACALL_STRING
		};

		void * ret = NULL;

		if (metacall_load_from_file("py", py_scripts, sizeof(py_scripts) / sizeof(py_scripts[0]), NULL) != 0)
		{
			return cleanup(3);
		}

		ret = metacallt("sum", sum_ids, "a", "b", "c");

		if (ret == NULL)
		{
			return cleanup(4);
		}

		/* Check result */
		{
			const char result[] = "abc";
			const char * str = metacall_value_to_string(ret);

			if (strncmp(str, result, sizeof(result)) != 0)
			{
				return cleanup(5);
			}

			printf("%s\n", str);
		}

		metacall_value_destroy(ret);
	}

	/* Ruby */
	{
		const char * rb_scripts[] =
		{
			"/scripts/mult.rb"
		};

		const enum metacall_value_id mult_ids[] =
		{
			METACALL_LONG, METACALL_LONG
		};

		void * ret = NULL;

		if (metacall_load_from_file("rb", rb_scripts, sizeof(rb_scripts) / sizeof(rb_scripts[0]), NULL) != 0)
		{
			return cleanup(6);
		}

		ret = metacall("add", 324, 775);

		if (ret == NULL)
		{
			return cleanup(7);
		}

		/* Check result */
		{
			const int result = 1099;
			const int value = metacall_value_to_int(ret);

			if (result != value)
			{
				return cleanup(8);
			}


			printf("%d\n", value);
		}

		metacall_value_destroy(ret);

		/* TODO: This generates a segmentation fault, review */
		/*
		ret = metacallt("mult", mult_ids, 324L, 775L);

		if (ret == NULL)
		{
			return cleanup(7);
		}

		*//* Check result *//*
		{
			const long result = 251100L;
			const long value = metacall_value_to_long(ret);

			if (result != value)
			{
				return cleanup(8);
			}

			printf("%ld\n", value);
		}

		metacall_value_destroy(ret);
		*/
	}


	/* Inspect */
	{
		size_t size = 0;

		struct metacall_allocator_std_type std_ctx = { &malloc, &realloc, &free };

		void * allocator = metacall_allocator_create(METACALL_ALLOCATOR_STD, (void *)&std_ctx);

		char * inspect_str = metacall_inspect(&size, allocator);

		printf("Inspect:\n%s\n", inspect_str);

		metacall_allocator_free(allocator, inspect_str);

		metacall_allocator_destroy(allocator);
	}


	return cleanup(0);
}
