#!/usr/bin/env node

/*
 *	MetaCall Distributable by Parra Studios
 *	Distributable infrastructure for MetaCall.
 *
 *	Copyright (C) 2016 - 2024 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
 *
 *	Licensed under the Apache License, Version 2.0 (the "License")
 */

const { metacall } = require('metacall');

function testPort() {
    console.log("Node.js port works");
    return "Node.js port works";
}

module.exports = {
    testPort
};