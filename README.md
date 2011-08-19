# XPC Calc
## The world's worst RPN calculator, powered by XPC and Haskell

This is actually less insane than it sounds. Haskell and XPC actually make a good fit:

- Haskell is a lazy, pure, statically-typed functional language which has a strong foreign function interface to C.
- XPC is an IPC and daemon management library for building out-of-process, stateless services which communicate via typed, asynchronous messages.

It should be fairly easy to see how these two technologies line up. 

## OK, so what is this?

XPC Calc is an [RPN calculator][rpn] which performs the requested mathematical operations out-of-process, specifically in an XPC service. Included are two XPC services -- one written in Objective-C and one written in Haskell.

[rpn]: http://en.wikipedia.org/wiki/Reverse_Polish_notation

## How does it work?

**XPC Calc** itself maintains the current stack in the main process. When the user requests an addition, subtraction, multiplication or division operations, XPC Calc sends an XPC message with the stack and the operation to an XPC service which performs the operation and returns the stack.

**The Objective-C service** has a straightforward implementation, dealing directly with the native XPC types.

**The Haskell service** is fairly primitive at this point. It has does some limited marshaling and unmarshaling of XPC types, allowing the main calculating functions to be written as pure Haskell functions, rather than in the IO monad.

## No really, the world's worst RPN calculator

All that said, XPC Calc is quite a terrible calculator. As it only supports integers, it only implements integer division, for one. Issuing `+`, for example, does not push any text in the input field onto the stack. And so on.

## Areas to improve

I'm primarily focused on improving the Haskell bits, as those are the most interesting. It would be cool if a general framework for writing XPC services in Haskell could come out of this simple project.

- The marshaling code dealing with XPC types should be extended to support a richer variety of types. At first brush, creating `XPCable` and `UnXPCable` typeclasses seems like a good place to start.
- The C bits should be reduced as much as possible. In theory, the only thing that should be necessary is glue for `xpc_connection_set_event_handler` which takes a block. (Blocks are not supported in the Haskell FFI yet, although there has been some interesting dicussion about how to add support.)
- The Xcode build system is already pretty good, but it would be nice if we instead issued a single call to `ghc`, instead of two. The additional call seems necessary "prime" the stub header files needed to build the C program.
- It appears that the `xpc_array_*` functions do not allow you to remove elements or insert elements anywhere other than the end of the list. Because of this restriction the Haskell code for implementing calculation is uglier than it should be.
- The Objective-C version should either be changed to pure C, or use some Objective-C features. (Shared marshaling code?)

## License
Copyright (c) 2011 Colin Barrett and contributors.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Additionally and informally, it would be awesome if you could display this license, or some other form of credit, in your application if you incorporate this code or find it useful. Thank you!