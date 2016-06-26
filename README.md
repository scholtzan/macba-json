# macba-json — a library for macro-based data serialization to JSON using Scala

> Note: This project was developed as part of a bachelor thesis and is only sporadically updated.

This project provides a library for serializing class instances and values to JSON.
It automatically analyses the structure of these class instances and derives the corresponding JSON representation.
The implementation is based on Scala macros.

This library also provides an API to manually create JSON objects.
In addition, it offers the possibility to generate the JSON AST representation of a class instance.
The JSON AST is based on the [json4s JSON AST](https://github.com/json4s/json4s) in order to ensure compatibility with
other libraries and reusability of existing utility functions for manipulating the JSON AST.

## Features

* Automatic serialization of values or class instances to JSON
* High serialization speed
* Streaming of generated JSON
* Type information included in the JSON string
* API for manually defining JSON
* Implicit serialization rules
* JSON AST generation
* Transformation of JSON ASTs to strings

## Compilation

[**`sbt`**](http://www.scala-sbt.org/) must be installed.

To compile the project run

    sbt compile

To use this library in other projects, it has to be published locally running

    sbt publish-local

Other projects can then use this library as a dependency in their `build.sbt` file, e.g. like this:

    libraryDependencies ++= Seq("net.scholtzan" %% "macba-json" % "0.1.0")


## Usage

A documentation of the API can be found [**here**](http://macba-json.scholtzan.net/).

The following code demonstrates basic usage and can be executed in an interactive Scala shell:

    import net.scholtzan.macba.json.auto.JsonGenerator._
    case class User(name: String, age: Int, married: Boolean)

    val user = User("Jane Doe", 42, true)

    val jsonString = toJsonString(user)
    // → {"name":"Jane Doe","age":42,"married":true}


## License

This software is licensed under the MIT License.

Copyright (c) 2016 Anna Scholtz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
