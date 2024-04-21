# FFI or VM Plugins, what to use?
*by jmv (Juan Vuletich)*

I've used buth alternatives over the years. I developed or fixed bugs in JPEGReadWriterPlugin2, BitBltPlugin, ExtendedClipboardPlugin, SoundPlugin, and VectorEnginePlugin. I ported FFI to 64 bit Cuis (the 32 bit FFI was inherited from Squeak), and used it for OpenCL. I don't have a favorite, and I think there shouldn't be one. So, today I spent a couple of hours writing this:


## Introduction

There are two main ways to call external code in the Cuis / Squeak world. One is FFI (what is usually used in other dynamic languages) and VM Plugins (a mechanism that is specific to the Squeak VM). There might be use cases where one of them is preferable to the other. Let's take a look at what they do, to see their pros and cons.


## FFI

FFI is a general mechanism to call external platform libraries. .so, .dll and the like. The mechanism is general (i.e. not specific for each call), so for each different function we might call, we need a way to build the native platform call stack (function arguments). This is different for each platform, and different for each function. This function+platform specific specification lives in the Smalltalk image.

FFI breaks with the idea that the VM defines the boundary between the Smalltalk world and the external world:
- Any platform specific difference in functions called via FFI is handled in the Smalltalk image
- The Smalltalk image is aware of each platform we might want to run on

For a non trivial example, evaluate `Feature require: 'OpenCL'` and browse the OpenCLPrimitiveInterface hierarchy.


## VM Plugins

VM Plugins are a Squeak VM specific way to call C code. The call mechanism is general, and handled by the VM. The C code is specific to the problem to solve, and must follow the rules for building plugins. In many cases it is not platform specific. This function specific code in plugin form lives in Slang or C code. Slang easies code development by enabling the use of the Smalltalk tools, but it is advisable to also check and understand the C code generated from it. Sometimes, straight C is preferable.

VM plugins follow the idea that the VM defines the boundary between the Smalltalk world and the external world:
- Smalltalk image is platform independent
- VM + Plugins are platform dependent

As examples of their use, you might browse all methods containing string `module: 'FloatArrayPlugin'>` or module: 'JPEGReadWriter2Plugin'>


## Use cases

Both FFI and VM Plugins can be used in a variety of scenarios, with possible different needs:
(1) Using host platform functionality, with documentation supporting dynamic linking (for instance, the Windows API)
(2) Using third party code distributed as dynamic libraries (for example, TensorFlow)
(3) Using third party code distributed as source code or static linked libraries (for example, libJPEG, stdlib.h memcpy(), math.h float stuff)
(4) Own code, written in Slang or C for performance reasons (BitBlt, FloatArray)
(5) Own code, needing to deal with Smalltalk object internals, or VM services.

Additionally, some calls need to be done with strict real time restrictions (like music / audio), or execution might be so quick and so frequent that any call overhead needs to be minimized (for example, BitBlt, LargeInteger). 


## How do they compare?

### a) Ease of development and prototyping.
For the scenarios where FFI is well suited, (1) or (2), working with FFI is usually much faster and easier. It is possible to reuse API docs and general know how from outside the Smalltalk world. On the other hand, code that needs to deal with Smalltalk object internals and VM services (5) is easier to do as a plugins. For code written in C (and not regular Smalltalk) for performance reasons (4), if you already have the required setup, writing a plugin is easier and faster, as you don't need to deal with the platform stack.

-- Bottom line: It depends.

### b) Ease of modifications, ease of updating users installations.
Modifying a plugin usually requires the developer to compile a new dll. Modifying FFI calls can be done with just Smalltalk code.

-- Bottom line: Clear win for FFI.

### c) Call Speed.
Plugin calls are almost as fast as regular numbered primitives. In Cuis and Squeak, FFI calls range from terribly slow to just plain slow, when compared to that.

-- Bottom line: Plugin wins, if you really need low call overhead.

### d) Support for callbacks
Recent implementations of FFI do support real callbacks. With VM plugins, the best we can do is to register a semaphore for the plugin to signal. This is usually safer, as the callback is done as a Smalltalk Process switch, making it easier to protect shared state. But there might be a large delay from the moment the semaphore is signaled to the moment where the "callback" is actually ran. As a side note, callbacks usually open a kind of Pandora's box. A design that doesn't use callbacks at all is usually preferable.

-- Bottom line: FFI (callbacks enabled) clear win, if you need callbacks.

### e) Access to VM services and data structures to deal with Smalltalk objects
VM plugins have a lot of services provided by available VM functions, and types provided by VM maker. Slang/C code needing to deal with the internals of Smalltalk objects is usually much easily done in plugins.

-- Bottom line: Plugin wins, if you need this.


## What is each alternative good for?

FFI is good for:
- Tight integration with host platform, like a host windowing system or native widget libraries, but only if dynamic linking is supported
- Third party code (libraries) that is meant to be linked dynamically, and everyone else calls via FFI
- Using the version of third party libraries provided by the system, making it easy to use the latest available
- Application specific stuff (where dealing with platform specific details is not a problem)
- Functionality that requires real callbacks
- Stuff that is in development, APIs that are not yet stable, experimental code

Plugins are good for:
- Own code written for performance
- Third party code (static libraries) that is meant to be linked statically
- Picking a specific version of third party libraries and sticking to it for stability
- Kernel Smalltalk functionality
- Functions that take very short time to run, and call overhead becomes dominant time
- Stable functionality that will be used by many people over a long time
