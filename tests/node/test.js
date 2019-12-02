const { metacall, metacall_load_from_file } = require('@METACALL_LIBRARY_PATH@');

/* TODO: Monkey-patch */

console.log(metacall_load_from_file('mock', [ 'test.mock' ]));
console.log(metacall('three_str', 'a', 'b', 'c'));
