'From Cuis7.1 [latest update: #6426] on 3 June 2024 at 11:06:47 am'!
'Description Alien provides the infrastructure for FFI Callbacks, implementing the Callback hierarchy and marshalling methods within it.'!
!provides: 'Alien-Core' 1 0!
SystemOrganization addCategory: #'Alien-Core'!


!classDefinition: #Alien category: #'Alien-Core'!
ByteArray variableByteSubclass: #Alien
	instanceVariableNames: ''
	classVariableNames: 'GCMallocedAliens LoadedLibraries ProcessIoEvents WordSize'
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'Alien class' category: #'Alien-Core'!
Alien class
	instanceVariableNames: 'lifoCallbackSemaphore vmCallbackContextClass'!

!classDefinition: #AbstractVMCallbackContext category: #'Alien-Core'!
Alien variableByteSubclass: #AbstractVMCallbackContext
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'AbstractVMCallbackContext class' category: #'Alien-Core'!
AbstractVMCallbackContext class
	instanceVariableNames: ''!

!classDefinition: #VMCallbackContext32 category: #'Alien-Core'!
AbstractVMCallbackContext variableByteSubclass: #VMCallbackContext32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'VMCallbackContext32 class' category: #'Alien-Core'!
VMCallbackContext32 class
	instanceVariableNames: ''!

!classDefinition: #VMCallbackContext64 category: #'Alien-Core'!
AbstractVMCallbackContext variableByteSubclass: #VMCallbackContext64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'VMCallbackContext64 class' category: #'Alien-Core'!
VMCallbackContext64 class
	instanceVariableNames: ''!

!classDefinition: #FFICallbackThunk category: #'Alien-Core'!
Alien variableByteSubclass: #FFICallbackThunk
	instanceVariableNames: ''
	classVariableNames: 'AccessProtect AllocatedThunks ExecutablePages'
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'FFICallbackThunk class' category: #'Alien-Core'!
FFICallbackThunk class
	instanceVariableNames: 'initializer'!

!classDefinition: #LibrarySymbolNotFoundError category: #'Alien-Core'!
Error subclass: #LibrarySymbolNotFoundError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'LibrarySymbolNotFoundError class' category: #'Alien-Core'!
LibrarySymbolNotFoundError class
	instanceVariableNames: ''!

!classDefinition: #AlienWeakTable category: #'Alien-Core'!
Object subclass: #AlienWeakTable
	instanceVariableNames: 'accessProtect firstUnusedIndex lastUsedIndex weakArray strongArray owner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'AlienWeakTable class' category: #'Alien-Core'!
AlienWeakTable class
	instanceVariableNames: ''!

!classDefinition: #Callback category: #'Alien-Core'!
Object subclass: #Callback
	instanceVariableNames: 'block thunk evaluator numEvaluatorArgs argsProxyClass'
	classVariableNames: 'ABI FlushThunkToCallbackMap ThunkToCallbackMap'
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'Callback class' category: #'Alien-Core'!
Callback class
	instanceVariableNames: 'concreteClass'!

!classDefinition: #CallbackForARM32 category: #'Alien-Core'!
Callback subclass: #CallbackForARM32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'CallbackForARM32 class' category: #'Alien-Core'!
CallbackForARM32 class
	instanceVariableNames: ''!

!classDefinition: #CallbackForARMv8 category: #'Alien-Core'!
Callback subclass: #CallbackForARMv8
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'CallbackForARMv8 class' category: #'Alien-Core'!
CallbackForARMv8 class
	instanceVariableNames: ''!

!classDefinition: #CallbackForIA32 category: #'Alien-Core'!
Callback subclass: #CallbackForIA32
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'CallbackForIA32 class' category: #'Alien-Core'!
CallbackForIA32 class
	instanceVariableNames: ''!

!classDefinition: #CallbackForRiscV64 category: #'Alien-Core'!
Callback subclass: #CallbackForRiscV64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'CallbackForRiscV64 class' category: #'Alien-Core'!
CallbackForRiscV64 class
	instanceVariableNames: ''!

!classDefinition: #CallbackForWin64X64 category: #'Alien-Core'!
Callback subclass: #CallbackForWin64X64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'CallbackForWin64X64 class' category: #'Alien-Core'!
CallbackForWin64X64 class
	instanceVariableNames: ''!

!classDefinition: #CallbackForX64 category: #'Alien-Core'!
Callback subclass: #CallbackForX64
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Alien-Core'!
!classDefinition: 'CallbackForX64 class' category: #'Alien-Core'!
CallbackForX64 class
	instanceVariableNames: ''!


!Alien commentStamp: '<historical>' prior: 0!
Copyright 2008 Cadence Design Systems, Inc.
   
   Licensed under the Apache License, Version 2.0 (the ''License''); you may not use this file except in compliance with the License.  You may obtain a copy of the License at  http://www.apache.org/licenses/LICENSE-2.0

Instances of Alien represent actual parameters, return results and function pointers in FFI call-outs and call-backs and provide handles on external data.  See NewsqueakIA32ABIPlugin for the VM code that actually implements call-outs and call-backs.

See the class-side examples category for some simple example workspaces.

Aliens represent ABI (C language) data.  They can hold data directly in their bytes or indirectly by pointing to data on the C heap.  Alien instances are at least 5 bytes in length. The first 4 bytes of an Alien hold the size, as a signed integer, of the datum the instance is a proxy for.  If the size is positive then the Alien is "direct" and the actual datum resides in the object itself, starting at the 5th byte.  If the size is negative then the proxy is "indirect", is at least 8 bytes in length and the second 4 bytes hold the address of the datum, which is assumed to be on the C heap.  Any attempt to access data beyond the size will fail.  If the size is zero then the Alien is a pointer, the second 4 bytes hold a pointer, as for "indirect" Aliens, and accessing primitives indirect through the pointer to access data, but no bounds checking is performed.

When Aliens are used as parameters in FFI calls then all are "passed by value", so that e.g. a 4 byte direct alien will have its 4 bytes of data passed, and a 12-byte indirect alien will have the 12 bytes its address references passed.  Pointer aliens will have their 4 byte pointer passed.  So indirect and pointer aliens are equivalent for accessing data but different when passed as parameters, indirect Aliens passing the data and pointer Aliens passing the pointer.

Class Variables:
GCMallocedAliens <AlienWeakTable of <Alien -> Integer>> - weak collection of malloced aliens, used to free malloced memory of Aliens allocated with newGC:
LoadedLibraries <Dictionary of <String -> Alien>> - library name to library handle map

Class instance variables
lifoCallbackSemaphore <Semaphore> - The lifoCallbackSemaphore queues callbacks which cannot return because they are not the most recent callback.  Callback return must be last-in-first-out (LIFO) to avoid scrambling the C stack.  See Alien class>>invokeCallbackContext:.!

!AbstractVMCallbackContext commentStamp: '<historical>' prior: 0!
A VMCallbackContext is an Alien for the VMCallbackContext (defined in the VMMaker package), a struct that maintains all the necessary context for a callback. The callback struct looks like the following, but the sizes and alignments of fields will vary, especially between 32 and 64 bit.  Subclasses provide the concrete details for particular platforms.

typedef struct {
    void *thunkp;
    char *stackptr;
    long *intRegArgs;
    double *floatRegArgs;
    void *savedCStackPointer;
    void *savedCFramePointer;
    union {
                            long vallong;
                            struct { int low, high; } valleint64;
                            struct { int high, low; } valbeint64;
                            double valflt64;
                            struct { void *addr; long size; } valstruct;
                        }   rvs;
    jmp_buf trampoline;
 } VMCallbackContext;

Instance Variables
!

!VMCallbackContext32 commentStamp: '<historical>' prior: 0!
A VMCallbackContext32 is an Alien for the 32-bit version of VMCallbackContext (defined in the VMMaker package), a struct that maintains all the necessary context for a callback.

typedef struct {
    void *thunkp;
    char *stackptr;
    long *intRegArgs;
    double *floatRegArgs;
    void *savedCStackPointer;
    void *savedCFramePointer;
    union {
                            long vallong;
                            struct { int low, high; } valleint64;
                            struct { int high, low; } valbeint64;
                            double valflt64;
                            struct { void *addr; long size; } valstruct;
                        }   rvs;
    jmp_buf trampoline;
 } VMCallbackContext;

Instance Variables
!

!VMCallbackContext64 commentStamp: '<historical>' prior: 0!
A VMCallbackContext64 is an Alien for the 64-bit version of VMCallbackContext (defined in the VMMaker package), a struct that maintains all the necessary context for a callback.

typedef struct {
    void *thunkp;
    char *stackptr;
    long *intRegArgs;
    double *floatRegArgs;
    void *savedCStackPointer;
    void *savedCFramePointer;
    union {
                            long vallong;
                            struct { int low, high; } valleint64;
                            struct { int high, low; } valbeint64;
                            double valflt64;
                            struct { void *addr; long size; } valstruct;
                        }   rvs;
    jmp_buf trampoline;
 } VMCallbackContext;

Instance Variables
!

!FFICallbackThunk commentStamp: '<historical>' prior: 0!
An instance of FFICallbackThunk is a reference to a machine-code thunk/trampoline that calls-back into the VM.  The reference can be passed to C code which can use it as a function pointer through which to call-back into Smalltalk.  The machine-code thunk/trampoline is different for each instance, hence its address is a unique key that can be used to associate the Smalltalk side of the call-back (e.g. a block) with the thunk.  Since thunks must be executable and most OSs do not provide default execute permission on memory returned by malloc we do not use malloc directly.  Instead we rely on a primitive to provide memory that is guaranteed to be executable.  The FFICallbackThunk class>>allocateExecutablePage primitive answers an Alien that references an executable piece of memory that is some (possiby unitary) multiple of the pagesize.  Class-side code then parcels out pieces of a page to individual thunks.  These pieces are recycled when thunks are reclaimed.  Since the first byte of a thunk is non-zero we can use it as a flag indicating if the piece is in use or not.

See Callback for the higher-level construct that represents a Smalltalk block to be run in response to a callback.  Callbacks wrap instances of FFICallbackThunk and VMCallbackContext[32|64] instances that describe the stack layout and register contents for receiving callback arguments.

Class Variables
AccessProtect <Semaphore> critical section for ExecutablePages (de)allocation
AllocatedThunks <AlienWeakTable of <FFICallbackThunk -> Integer>> - weak collection of thunks, used to return thunk storage to the executable page pool.
ExecutablePages <Set of: Alien "executable page"> - collection of pages with execute permissions used to provide executable thunks!

!LibrarySymbolNotFoundError commentStamp: 'TorstenBergmann 6/14/2019 21:47' prior: 0!
A symbol within a library was not found, see Alien>>#primFindSymbol:!

!AlienWeakTable commentStamp: '<historical>' prior: 0!
This class supports simple post-mortem finalization of values associated with gc'ed objects.  An object to be finalized is registered in the table together with another object called 'the tag'. The finalizable object is held onto by the table weakly, the tag object--strongly. A table is initialized with the owner object, which is the object that performs the actual finalization. Some time after a finalizable object is garbage-collected, the owner is sent the #finalize: message with the object's tag as the argument.

Instance Variables:
	accessProtect <Semaphore>  - A mutex protecting state
	firstUnusedIndex <Integer> - The lowest index in strongArray that is empty (an invariant)
	lastUsedIndex <Integer> - The highest index in strongArray that is not empty (an invariant)
	weakArray <WeakArray> - The array of objects whose death we're interested in.
	strongArray <Array> - The array of corresponding objects that wll be passed to the owner when their corresponding element in weakArray is garbage collected.
	owner <Object> - The object that is sent finalize: with the tag of an object that has been garbage-collected.!

!Callback commentStamp: '<historical>' prior: 0!
Callbacks encapsulate callbacks from the outside world.  They allow Smalltalk blocks to be evaluated and answer their results to external (e.g. C) callees.  Callbacks are created with signature:block:, e.g.

	cb := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].

and passed through the FFI by passing their pointer, e.g.

	self qui: data ck: data size so: 8 rt: cb pointer

When the callback is made, the system arranges that the block is invoked with the arguments as defined by the signature, and the result of the block passed back, again as defined by the signature.  See methods in the signatures protocol in subclasses of Callback for signature methods that decode the C stack and registers to invoke a callback with parsed arguments.  See Callback>>valueInContext: and subclass implementations for the evaluation of the signature method that invokes the callback block with correctly parsed arguments.  See Alien class>>invokeCallbackContext: for the entry-point for callbacks into the system from the VM.

Instance Variables:
block <BlockClosure> - The Smalltalk code to be run in response to external code invoking the callback.
thunk <FFICallbackThunk> - the wrapper around the machine-code thunk that initiates the callback and whose address should be passed to C
evaluator <Symbol> - the selector of the marshalling method to use; see methods in the signatures protocol in subclasses of Callback.
numEvaluatorArgs <Integer> - the arity of evaluator
argsProxyClass <Alien subclass> - legacy; unused; the wrapper around the thunk's incoming stack pointer, used to extract arguments from the stack.

Class Variables:
ThunkToCallbackMap <Dictionary of: thunkAddress <Integer> -> callback <Callback>> - used to lookup the Callback associated with a specific thunk address on callback.  See FFICallbackThunk.
ABI <String> - the name of the current ABI

Class Instance Variables
concreteClass <Callback subclass> - the concrete class for callbacks on the current platform, or nil if one doesn't yet exist.

Implementation:
The way that it works is in two parts
- on callback the VM passes up a pointer to a structure from which all arguments, stacked and in registers (because the VM has copied any register args into the struct) can be accessed, and through which the result can be returned.
- the image level provides marshalling methods that match the signature in the callback.  Marshalling methods belong in concrete subclasses, one subclass for each ABI.

So e.g. with a callback of
		Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 | ((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign]
the marshalling methods are in one of Callback's concrete subclasses signatures protocol, for example

CallbackForIA32>>voidstarvoidstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))

where spAlien is an Alien pointing to a VMCallbackContext32.

For ARM support, where there the first four integer arguments are passed in registers, we can use

CallbackForARM32>>voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))

The selector of the method doesn't matter, providing it doesn't conflict with any other, except for the number of arguments.  What's important is the pragma which defines the signature and the ABI for which this is a valid marshalling method.  Support for callee pop callbacks (Pascal calling convention such as the Win32 stdcall: convention) are supported using the <calleepops: N> pragma which specifies how many bytes to pop.

When a callback is instantiated, Callback introspects to find the marshalling method that matches the signature for the current ABI.  If one doesn't already exist you can write one.  Hopefully we'll write an ABI compiler that will automatically generate these marshalling methods according to the platform's ABI, but for now its a manual process.; at least it's open and flexible.  When the callback is invoked the evaluator is performed with the current callbackContext and pointer(s) to the arguments.  There is a 32-bit and a 64-bit callback context, and it can have a stack pointer, integer register args and floating point register args, so it's general enough for any callback.

To pass back the result, a value is assigned into the struct via the accessor in the marshalling method and control returns to teh point where teh callback comes in, and this uses a primitive to return.  Inside the callbackContext is a jmpbuf from a setjmp.  The primitive longjmp's back to the entry point in the VM which extracts the result and the code for the kind of result and returns.  See Callback class>>invokeCallbackContext:!

!CallbackForARM32 commentStamp: 'eem 6/17/2016 13:56' prior: 0!
A CallbackForARM32 is a callback for platforms with ARMv4/v5/v6/v7 processors using the System V ABI.!

!CallbackForIA32 commentStamp: 'eem 6/17/2016 13:55' prior: 0!
A CallbackForIA32 is a callback for platforms with x86 processors using the System V ABI.!

!CallbackForWin64X64 commentStamp: 'eem 6/17/2016 13:55' prior: 0!
A CallbackForWin64X64 is a callback for platforms with x64/x86_64 processors using the Win64 ABI.!

!CallbackForX64 commentStamp: 'eem 6/17/2016 13:55' prior: 0!
A CallbackForX64 is a callback for platforms with x64/x86_64 processors using the System V ABI.!

!Alien methodsFor: 'accessing' stamp: 'eem 2/5/2008 16:27'!
address
	^self sizeField <= 0
		ifTrue: [self addressField]
		ifFalse: [self error: 'One cannot take the address of direct Alien instances; they may move']! !

!Alien methodsFor: 'accessing' stamp: 'eem 12/20/2007 17:41'!
dataSize
	^self sizeField abs! !

!Alien methodsFor: 'accessing' stamp: 'eem 4/18/2008 15:14'!
freeAfter: aBlock
	"The receiver must be a pointer to a previously allocated external C heap memory block. Invoke aBlock passing the receiver as the argument, then free the receiver. Allows automatic allocation and freeing of C artifacts using patters such as
	(Alien newCString: 'foo') freeAfter: [:cstring | ...use cstring here...]."
	
	^[aBlock value: self] ensure: [self free]! !

!Alien methodsFor: 'accessing' stamp: 'Ahe 10/3/2008 15:31'!
pointer
	"Answer a pointer to the receiver in the form of an Alien.
	 This will fail if the receiver is not indirect."
	self isPointer ifTrue:
		[self error: 'One cannot take the address of pointer Alien instances; they may move, and are already pointers'].
	^self class forPointer: self address! !

!Alien methodsFor: 'accessing' stamp: 'eem 2/15/2018 08:32'!
signedWordAtWordIndex: wordIndex
	^WordSize = 4
		ifTrue: [self signedLongAt: wordIndex - 1 * 4 + 1]
		ifFalse: [self signedLongLongAt: wordIndex - 1 * 8 + 1]! !

!Alien methodsFor: 'accessing' stamp: 'eem 2/4/2008 16:29'!
strcpy "^<ByteString>"
	^self strcpyFrom: 1! !

!Alien methodsFor: 'accessing' stamp: 'eem 5/31/2024 17:01:52'!
strcpyFrom: startIndex "<Integer> ^<ByteString>"
	| length string |
	length := self strlenStartingAt: startIndex.
	self copyInto: (string := String new: length)
		from: 1 to: length
		in: self startingAt: startIndex.
	^string! !

!Alien methodsFor: 'accessing' stamp: 'eem 2/4/2008 16:29'!
strcpyUTF8 "^<String>"
	^self strcpyUTF8From: 1! !

!Alien methodsFor: 'accessing' stamp: 'John M McIntosh 11/23/2008 23:41'!
strcpyUTF8From: startIndex "<Integer> ^<String>"
	| length bytes |
	length := self strlenStartingAt: startIndex.
	self copyInto: (bytes := ByteArray new: length)
		from: 1 to: length
		in: self startingAt: startIndex.
	^bytes utf8ToIso! !

!Alien methodsFor: 'accessing' stamp: 'Ahe 8/25/2008 20:39'!
strlen
	^self strlenStartingAt: 1! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
addressField "^<Integer>"
	<primitive: 'primAddressField' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
addressFieldPut: value "<Integer> ^<Integer>"
	<primitive: 'primAddressFieldPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 9/27/2020 16:15'!
copyFrom: start "<Integer>" to: stop "<Integer>" into: aByteObject "<Alien | indexableByteSubclass>" startingAt: destStart "<Integer>" "^<self>"
	<primitive: 'primAlienCopyInto' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
copyInto: aByteObject "<Alien | indexableByteSubclass>" from: start "<Integer>" to: stop "<Integer>" in: replacement "<Alien | indexableByteSubclass>" startingAt: repStart "<Integer>" "^<self>"
	<primitive: 'primAlienReplace' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
doubleAt: index "<Integer> ^<Float>"
	<primitive: 'primDoubleAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
doubleAt: index "<Integer>" put: value "<Float | Integer> ^<Float | Integer>"
	<primitive: 'primDoubleAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
floatAt: index "<Integer> ^<Float>"
	<primitive: 'primFloatAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
floatAt: index "<Integer>" put: value "<Float | Integer> ^<Float | Integer>"
	<primitive: 'primFloatAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 10/26/2020 20:26'!
oopAt: index "<Integer> ^<Object>"
	<primitive: 'primOopAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 10/26/2020 20:28'!
oopAt: index "<Integer>" put: value "<Object> ^<Object>"
	<primitive: 'primOopAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 7/19/2021 13:53'!
replaceFrom: start "<Integer>" to: stop "<Integer>" with: replacement "<Alien | indexableByteSubclass>" startingAt: repStart "<Integer>" "^<self>"
	"N.B. on platforms where executable memory cannot be written to unless flipped out of
	 executability, this primitive, and only this primitive, will contain code to identify an attempt
	 to write to one of the executable pages allocated via primAllocateExecutablePage, and
	 will flip protections appropriately around the replacement write."
	<primitive: 'primAlienReplace' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
signedByteAt: index "<Integer> ^<Integer>"
	<primitive: 'primSignedByteAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
signedByteAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primSignedByteAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
signedLongAt: index "<Integer> ^<Integer>"
	<primitive: 'primSignedLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
signedLongAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primSignedLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'John M McIntosh 12/5/2008 09:41'!
signedLongLongAt: index "<Integer> ^<Integer>"
	<primitive: 'primSignedLongLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'John M McIntosh 12/5/2008 09:41'!
signedLongLongAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primSignedLongLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
signedShortAt: index "<Integer> ^<Integer>"
	<primitive: 'primSignedShortAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
signedShortAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primSignedShortAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
sizeField "^<Integer>"
	<primitive: 'primSizeField' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
sizeFieldPut: size "<Integer> ^<Integer>"
	<primitive: 'primSizeFieldPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
strlenStartingAt: index "<Integer> ^<Integer>"
	<primitive: 'primStrlenFromStartIndex' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
strlenThroughPointerAt: index "<Integer> ^<Integer>"
	<primitive: 'primStrlenThroughPointerAtIndex' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
unsignedByteAt: index "<Integer> ^<Integer>"
	<primitive: 'primUnsignedByteAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
unsignedByteAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primUnsignedByteAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
unsignedLongAt: index "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
unsignedLongAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'John M McIntosh 12/5/2008 09:41'!
unsignedLongLongAt: index "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongLongAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'John M McIntosh 12/5/2008 09:41'!
unsignedLongLongAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primUnsignedLongLongAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
unsignedShortAt: index "<Integer> ^<Integer>"
	<primitive: 'primUnsignedShortAt' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-accessing' stamp: 'eem 12/5/2008 09:41'!
unsignedShortAt: index "<Integer>" put: value "<Integer> ^<Integer>"
	<primitive: 'primUnsignedShortAtPut' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'coercing' stamp: 'eem 2/15/2018 08:56'!
as: anAlienClass
	"Attempt to coerce the receiver into a different class of Alien.  Performs minimal checks!!!! Use with caution!!!!"
	| alienClassDataSize |
	"Always attempt to access anAlienClass dataSize as a run-time check for anAlienClass actually being an Alien class."
	alienClassDataSize := anAlienClass dataSize.
	self isPointer ifFalse:
		[self dataSize >= alienClassDataSize ifFalse:
			[self error: 'receiver too small to coerce']].
	anAlienClass adoptInstance: self! !

!Alien methodsFor: 'coercing' stamp: 'eem 1/10/2008 22:01'!
asAlien
	^self! !

!Alien methodsFor: 'converting' stamp: 'vb 12/6/2007 11:24'!
asSignedByte "^<Integer>"
	"Answer an Integer created from the first byte of the receiver interpreted as a signed value."
	
	^self signedByteAt: 1! !

!Alien methodsFor: 'converting' stamp: 'vb 12/6/2007 11:24'!
asSignedLong "^<Integer>"
	"Answer an Integer created from the first 4 bytes of the receiver interpreted as a signed value."
	
	^self signedLongAt: 1! !

!Alien methodsFor: 'converting' stamp: 'vb 12/6/2007 11:24'!
asSignedShort "^<Integer>"
	"Answer an Integer created from the first 2 bytes of the receiver interpreted as a signed value."
	
	^self signedShortAt: 1! !

!Alien methodsFor: 'converting' stamp: 'vb 12/6/2007 11:24'!
asUnsignedByte "^<Integer>"
	"Answer an Integer created from the first byte of the receiver interpreted as an unsigned value."
	
	^self unsignedByteAt: 1! !

!Alien methodsFor: 'converting' stamp: 'vb 12/6/2007 11:24'!
asUnsignedLong "^<Integer>"
	"Answer an Integer created from the first 4 bytes of the receiver interpreted as an unsigned value."
	
	^self unsignedLongAt: 1! !

!Alien methodsFor: 'converting' stamp: 'vb 12/6/2007 11:24'!
asUnsignedShort "^<Integer>"
	"Answer an Integer created from the first 2 bytes of the receiver interpreted as an unsigned value."
	
	^self unsignedShortAt: 1! !

!Alien methodsFor: 'memory management' stamp: 'eem 12/21/2007 15:50'!
free
	self primFree! !

!Alien methodsFor: 'instance initalization' stamp: 'eem 11/6/2007 12:09'!
initialize
	"Subclasses should redefine this method to perform initializations on instance creation"
	"Overrides ProtoObjects' method for explicitness."! !

!Alien methodsFor: 'testing' stamp: 'Ahe 10/3/2008 15:30'!
isPointer
	^self sizeField = 0! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | SmallInteger> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer> ^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" with: arg10 "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" with: arg10 "<Alien | Integer>" with: arg11 "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" with: firstArg "<Alien | Integer>" with: secondArg "<Alien | Integer>" with: thirdArg "<Alien | Integer>" with: fourthArg "<Alien | Integer>" with: fifthArg "<Alien | Integer>" with: sixthArg "<Alien | Integer>" with: arg7 "<Alien | Integer>" with: arg8 "<Alien | Integer>" with: arg9 "<Alien | Integer>" with: arg10 "<Alien | Integer>" with: arg11 "<Alien | Integer>" with: arg12 "<Alien | Integer>" "^<Alien>"
	<primitive: 'primCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-callouts' stamp: 'eem 12/5/2008 09:41'!
primFFICallResult: result "<Alien>" withArguments: args "<Array of: Alien | Integer> ^<Alien>"
	<primitive: 'primVarArgsCallOutIntegralReturn' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'primitives-library loading' stamp: 'eem 12/10/2019 20:56'!
primFindSymbol: symbolName "<String> ^<Integer>"
	"The receiver is assumed to be a proxy whose first field contans the handle of a library
	 answered by primLoadLibrary.  Attempt to find symbolName in the library and answer
	 its address, or fail if the receiver is invalid or the symbol cannot be found."
	<primitive: 'primInLibraryFindSymbol' module: 'IA32ABI' error: errorCode>
	^errorCode == #'not found'
		ifTrue: [LibrarySymbolNotFoundError signal]
		ifFalse: [self primitiveFailed]! !

!Alien methodsFor: 'primitives-memory management' stamp: 'eem 12/5/2008 09:41'!
primFree
	"Free the memory referenced by the receiver, or fail if
	 the receiver is direct or the indirect pointer is invalid.
	 Note that if successful the address and size fields are
	 zero'ed, effectively preventing double-freeing hazards."
	<primitive: 'primBoxedFree' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien methodsFor: 'printing' stamp: 'eem 12/14/2015 12:04'!
storeOn: aStream
	| sizeField |
	(sizeField := self sizeField) ~= 0 ifTrue:
		[aStream nextPut: $(].
	aStream nextPut: $(; print: self class.
	sizeField = 0
		ifTrue:
			[aStream nextPutAll: ' forPointer: '; nextPutAll: self addressField storeStringHex]
		ifFalse:
			[aStream
				nextPutAll: (sizeField < 0 ifTrue: [' newC: '] ifFalse: [' new: ']);
				print: (sizeField := sizeField abs); nextPut: $).
			 1 to: sizeField by: 4 do:
				[:i| | v |
				(v := self unsignedLongAt: i) ~= 0 ifTrue:
					[aStream
						crtab;
						nextPutAll: 'unsignedLongAt: '; print: i;
						nextPutAll: ' put: '; print: v; nextPut: $;]].
			 aStream crtab; nextPutAll: #yourself].
	aStream nextPut: $)

	"{	(Alien forPointer: 0) storeString.
		(Alien new: 8) storeString.
		(Alien newC: 8) storeString }"! !

!Alien class methodsFor: 'primitives-memory management' stamp: 'eem 12/5/2008 09:41'!
Ccalloc: byteSize "<Integer> ^<Integer>"
	<primitive: 'primCalloc' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien class methodsFor: 'primitives-memory management' stamp: 'eem 12/5/2008 09:41'!
Cmalloc: byteSize "<Integer> ^<Integer>"
	<primitive: 'primMalloc' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien class methodsFor: 'primitives-memory management' stamp: 'eem 12/5/2008 09:41'!
primFree: address "<Integer>"
	<primitive: 'primFree' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:46'!
atAddress: pointer "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: self dataSize negated;
		addressFieldPut: pointer;
		yourself! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:46'!
atAddress: address "<Integer>" dataSize: dataByteSize "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: dataByteSize asInteger abs negated;
		addressFieldPut: address;
		yourself! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/3/2008 09:50'!
dataSize "^<Integer>"
	"Answer the number of bytes required to represent an instance of the receiver.
	This excludes the size word's own bytes and is just the size of the data."
	^self subclassResponsibility! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:46'!
forPointer: address "<Integer>" "^<Alien>"
	"N.B.  This method assumes one is creating a pointer to already initialized data, hence initiaize is not sent.
	(Perhaps some alternative, eg. initializePointer, should be sent?"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: 0;
		addressFieldPut: address;
		yourself! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 2/15/2018 08:28'!
forWord "<Integer>" "^<Alien>"
	^(self basicNew: WordSize * 2)
		sizeFieldPut: WordSize;
		yourself! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 11/27/2007 10:47'!
new
	"Answer a new instance of the default size whose data resides on the Smalltalk heap directly in the instance."
	^self new: self dataSize! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:45'!
new: byteSize
	"Answer a new instance of the given byte size whose data resides on the Smalltalk heap directly in the instance."
	^(self basicNew: byteSize + WordSize)
		sizeFieldPut: byteSize;
		initialize! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 11/6/2007 12:13'!
newC
	"Answer a new instance of the default size whose data resides on the external C heap."
	^self newC: self dataSize! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:45'!
newC: byteSize
	"Answer a new instance of the given byte size whose data resides on the external C heap.
	 As with instances residing on the Smalltalk heap the data is zero-filled."
	^(self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated;
		addressFieldPut: (self Ccalloc: byteSize);
		initialize! !

!Alien class methodsFor: 'instance creation' stamp: 'TorstenBergmann 1/8/2018 19:50'!
newCString: aString "<String> ^<Alien>"
	"Answer an Alien pointing to a null-terminated copy of aString on the external C heap."

	| size |
	size := aString size.
	^(Alien rawNewC: size + 1)
		replaceFrom: 1 to: size with: aString startingAt: 1;
		unsignedByteAt: size + 1 put: 0;
		yourself! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/2/2008 12:01'!
newGC
	"Answer a new instance of the default size whose data resides on the external C heap,
	 and whose data will be reclaimed automatically when the instance is garbage collected."
	^self newGC: self dataSize! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:45'!
newGC: byteSize
	"Answer a new instance of the given byte size whose data resides on the external C heap,
	 and whose data will be reclaimed automatically when the instance is garbage collected.
	 As with instances residing on the Smalltalk heap the data is zero-filled."
	| alien address |
	(alien := self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated.
	address := self Ccalloc: byteSize.
	GCMallocedAliens add: alien finalizing: address.
	^alien
		addressFieldPut: address;
		initialize! !

!Alien class methodsFor: 'instance creation' stamp: 'eem 12/14/2015 11:45'!
rawNewC: byteSize
	"Answer a new instance of the given byte size whose data resides on the external C heap.
	 Unlike newC: the data is *not* zero-filled."
	^(self basicNew: WordSize * 2)
		sizeFieldPut: byteSize negated;
		addressFieldPut: (self Cmalloc: byteSize);
		initialize! !

!Alien class methodsFor: 'class initialization' stamp: 'eem 7/27/2022 10:04'!
ensureInSpecialObjectsArray
	"Alien must be in the specialObjectsArray to enable various primitives, FFI marshalling of
	 Aliens, and especially callbacks.  Because of the bootstrap the specialObjectsArray may
	 already have been partially initialized with the callback selector and to be large enough
	 (as part of loading immutability), but it will be missing Alien until Alien is loaded.  So check
	 if the specialObjectsArray is of the expected size before recreating and slam Alien in if the
	 specialObjectsArray is already large enough."

	| index |
	self ~~ Alien ifTrue: [^self].

	index := 53.
	((Smalltalk includesKey: #VMObjectIndices)
	 and: [((Smalltalk at: #VMObjectIndices) classPool at: #ClassAlien ifAbsent: []) ~= (index - 1)]) ifTrue:
		[self error: 'index probably wrong'].

	Smalltalk specialObjectsArray size < index ifTrue:
		[Smalltalk recreateSpecialObjectsArray].
	
	(Smalltalk specialObjectsArray size >= index
	 and: [(Smalltalk specialObjectsArray at: index) isNil
		 or: [(Smalltalk specialObjectsArray at: index) isBehavior
			and: [#(Alien AlienStub) includes: (Smalltalk specialObjectsArray at: index) name]]]) ifFalse:
		[self error: 'SystemDictionary>>recreateSpecialObjectsArray appears incompatible'].

	Smalltalk specialObjectsArray at: index put: self! !

!Alien class methodsFor: 'class initialization' stamp: 'eem 3/8/2016 14:11'!
ensureNotifiedAtStartup
	"Alien needs to be notified at system startup to clear the LoadedLibraries table
	and otherwise sanitise Aliens with dangling pointers."
	self ~~ Alien ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: Delay]! !

!Alien class methodsFor: 'class initialization' stamp: 'eem 10/17/2020 17:55'!
initialize
	"Alien initialize"
	LoadedLibraries ifNil:
		[LoadedLibraries := Dictionary new].
	GCMallocedAliens ifNil:
		[GCMallocedAliens := AlienWeakTable newForOwner: self].
	lifoCallbackSemaphore ifNil:
		[lifoCallbackSemaphore := Semaphore new].
	vmCallbackContextClass := (WordSize := Smalltalk wordSize) = 4
									ifTrue: [VMCallbackContext32]
									ifFalse: [VMCallbackContext64].
	self ensureInSpecialObjectsArray.
	self ensureNotifiedAtStartup! !

!Alien class methodsFor: 'libraries' stamp: 'eem 6/18/2016 16:33'!
ensureLoaded: aLibraryName "<String> ^<Alien>"
	^LoadedLibraries
		at: aLibraryName
		ifAbsentPut:
			[| lib alien |
			 lib := self primLoadLibrary: aLibraryName.
			 alien := Alien new: WordSize.
			 WordSize = 4
				ifTrue: [alien unsignedLongAt: 1 put: lib]
				ifFalse: [alien unsignedLongLongAt: 1 put: lib].
			 alien]! !

!Alien class methodsFor: 'libraries' stamp: 'eem 5/19/2012 13:53'!
lookup: symbol "<String>" inLibrary: libraryNameOrAlien "<String|Alien> ^<Alien>"
	^self forPointer: ((libraryNameOrAlien isString
						ifTrue: [self ensureLoaded: libraryNameOrAlien]
						ifFalse: [libraryNameOrAlien])
							primFindSymbol: symbol)! !

!Alien class methodsFor: 'libraries' stamp: 'eem 12/10/2019 20:56'!
lookupOrNil: symbol  "<String>" inLibrary: libraryName "<String> ^<Alien>"
	"Answer the address of symbol in libraryName, or nil if it is not in the library.
	Exceptions will be raised for invalid libraries, symbol names, etc."
	^[self lookup: symbol inLibrary: libraryName ]
		on: LibrarySymbolNotFoundError
		do: [:ex| nil]! !

!Alien class methodsFor: 'examples' stamp: 'eem 7/20/2021 16:08'!
exampleCqsort
	"Call the libc qsort function (which requires a callback)."
	"Alien exampleCqsort"
	"(Time microsecondsToRun: [100 timesRepeat: [Alien exampleCqsort]]) / 100.0"
	"| n t |
	 n :=t := 0.
	 { Time microsecondsToRun: [100 timesRepeat: [| tuple | tuple := Alien exampleCqsort. t := t + tuple third. n := n + tuple last]].
	    n / 100.0.
	    t / 100.0.
	    t / n asFloat}"
	| rand values nElements sizeofDouble orig callbackCount cb qsort usecs sort |
	rand := Random new.
	values := Alien newC: (nElements := 100) * (sizeofDouble := 8).
	1 to: values dataSize by: sizeofDouble do:
		[:i| values doubleAt: i put: rand next].
	orig := (1 to: values dataSize by: sizeofDouble) collect: [:i| values doubleAt: i].
	callbackCount := 0.
	cb := Callback
			signature:  #(int (*)(const void *, const void *))
			block: [ :arg1 :arg2 |
					callbackCount := callbackCount + 1.
					((arg1 doubleAt: 1) - (arg2 doubleAt: 1)) sign].
	qsort := Alien lookup: 'qsort' inLibrary: Alien libcName.
	usecs := Time microsecondsToRun: [qsort	primFFICallResult: nil
												with: values pointer
												with: nElements
												with: sizeofDouble
												with: cb thunk].
	sort := (1 to: values dataSize by: sizeofDouble) collect: [:i| values doubleAt: i].
	values free.
	^{ orig. sort. usecs. callbackCount }! !

!Alien class methodsFor: 'examples' stamp: 'eem 5/31/2024 17:39:51'!
libcName
	| platform macosVersionNumber |
	platform := Smalltalk platformName.
	platform = 'Mac OS' ifTrue:
		[macosVersionNumber := Integer readFrom: Smalltalk osVersion readStream.
		^macosVersionNumber >= 1070
			ifTrue: ['libobjc.dylib']
			ifFalse:
				[macosVersionNumber >= 1050
					ifTrue: ['libgcc_s.1.dylib']
					ifFalse: ['libc.dylib']]].
	platform = 'unix' ifTrue:
		[Smalltalk osVersion = 'linux-gnu' ifTrue:
			[^'libc.so.6'].
		 ^'libc.so'].
	platform = 'Win32' ifTrue:
		[^'msvcrt.dll'].
	^nil! !

!Alien class methodsFor: 'finalization' stamp: 'eem 12/21/2007 15:15'!
finalize: mallocTag
	self primFree: mallocTag! !

!Alien class methodsFor: 'callbacks' stamp: 'eem 10/17/2020 17:53'!
invokeCallbackContext: vmCallbackContextAddress "<Integer>"
	"The low-level entry-point for callbacks sent from the VM/IA32ABI plugin.
	 Evaluate the callback corresponding to the thunk referenced by vmCallbackContextAddress,
	 a pointer to a VMCallbackContext32 or VMCallbackContext64, set up by the VM's thunkEntry
	 routine.  Return from the Callback via primSignal:andReturnAs:fromContext:.  thisContext's
	 sender is typically an FFI call-out context and is restored as the Process's top context on return.
	 Therefore callbacks run on the process that did the call-out in which the callback occurred."
	| callbackAlien type |
	callbackAlien := vmCallbackContextClass atAddress: vmCallbackContextAddress.
	type := [Callback evaluateCallbackForContext: callbackAlien] ifCurtailed:
				[self error: 'attempt to non-local return across a callback'].
	"type determines how to return to the Callback's caller; see voidResult,
	 floatResult:, singleFloatResult:, wordResult:, in VMCallbackContext32/64."
	type ifNil: 
		[type := 1. callbackAlien wordResult: -1].
	"Now attempt to return from a Callback. This must be done in LIFO order.  The IA32ABI
	 plugin maintains a linked list of vmCallbackContextAddresses to record this order.  If
	 vmCallbackContextAddress *is* that of the most recent Callback then the return will
	 occur and the primitive will not return here.  If vmCallbackContextAddress *is not* that
	 of the most recent Callback the primitive will answer false, in which case this process
	 waits on the lifoCallbackSemaphore which will be signalled by some other attempted
	 Callback return. In any case (successful return from callback or answering false here),
	 the primtive signals the first process waiting on the semaphore (which is after this one
	 if this one was waiting), allowing the next process to attempt to return, and so on.
	 Hence all nested callbacks should eventually return, and in the right order."
	[callbackAlien primSignal: lifoCallbackSemaphore andReturnAs: type fromContext: thisContext]
		whileFalse:
			[lifoCallbackSemaphore wait]! !

!Alien class methodsFor: 'primitives-library loading' stamp: 'eem 12/5/2008 09:41'!
primInLibrary: libraryHandle "<Alien>" findSymbol: symbolName "<String> ^<Integer>"
	<primitive: 'primInLibraryFindSymbol' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien class methodsFor: 'primitives-library loading' stamp: 'eem 12/5/2008 09:41'!
primLoadLibrary: libraryName "<String> ^<Integer>"
	"Attempt to load the shared library/DLL named libraryName and
	 answer a handle which can be used in primFindSymbol:inLibrary:,
	 or fail if libraryName is invalid or libraryName cannot be found."
	<primitive: 'primLoadLibrary' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!Alien class methodsFor: 'system startup' stamp: 'eem 05/31/2024 18:32'!
startUp: resuming
	resuming ifTrue:
		["Any addresses in the finalization registry at the time the image
		 comes up in a new session MUST NOT be finalized and should
		 immediately be discarded. These addresses are no longer valid."
		 GCMallocedAliens ifNotNil:
			[:gcMallocedAliens| gcMallocedAliens removeAll].
		 "The io lock is off in the new session, so we shut down the process if it's running."
		 ProcessIoEvents := false.
		 "Makes sure signals are zero any out-of-date in progress callbacks are ignored."
		 lifoCallbackSemaphore := Semaphore new.
		 "Install the relevant VMCallbackContext class."
		 vmCallbackContextClass := (WordSize := Smalltalk wordSize) = 4
										ifTrue: [VMCallbackContext32]
										ifFalse: [VMCallbackContext64].
		 "Makes sure no stale libraries remain."
		 LoadedLibraries := Dictionary new]! !

!Alien class methodsFor: 'accessing' stamp: 'eem 2/15/2018 09:24'!
wordSize
	^WordSize! !

!Alien class methodsFor: 'primitives-callbacks' stamp: 'eem 10/17/2020 14:15'!
mostRecentCallbackContextAddress
	"Answer the address of the mostRecentCallbackContext, which will be 0 if no callback is running.
	 Does not fail (if the plugin is present and implements the primitive)."
	<primitive: 'primMostRecentCallbackContext' module: 'IA32ABI' error: ec>
	self primitiveFailed! !

!AbstractVMCallbackContext methodsFor: 'accessing' stamp: 'eem 9/27/2020 16:52'!
voidResult
	"Actually return garbage via retword"
	^1! !

!AbstractVMCallbackContext methodsFor: 'primitives' stamp: 'eem 10/17/2020 18:40'!
primSignal: aSemaphore andReturnAs: anIntegerTypeCode fromContext: aContext
	"Attempt to return from a callback. This must be done in LIFO order.  The IA32ABI
	 plugin maintains a linked list of vmCallbackContextAddresses to record this order.
	 If vmCallbackContextAddress *is* that of the most recent Callback then return from
	 the callback and do not return here.  If vmCallbackContextAddress *is not* that of
	 the most recent Callback then succeed but answer false. In either case, signal the
	 first process waiting on aSemaphore.  See Alien class>> invokeCallbackContext:
	 for a full explanation. Fail if the arguments are not of the expected type."
	<primitive: 'primReturnAsFromContextThrough' module: 'IA32ABI' error: ec>
	^self primitiveFailed! !

!AbstractVMCallbackContext class methodsFor: 'instance creation' stamp: 'eem 10/14/2010 14:01'!
dataSize
	"We only access the fields before trampoline, so this is fine."
	^32! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 4/9/2014 17:20'!
floatResult: aFloat
	self doubleAt: 25 put: aFloat.
	^3! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 10/15/2010 10:12'!
floatregargsp
	^Alien forPointer: (self unsignedLongAt: 13)! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 10/15/2010 10:12'!
intregargsp
	^Alien forPointer: (self unsignedLongAt: 9)! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 10/26/2020 20:32'!
oopResult: anObject
	"Accept any object."
	self oopAt: 25 put: anObject.
	^1! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'RonieSalgado 2/13/2017 23:18'!
singleFloatResult: aFloat
	self floatAt: 25 put: aFloat.
	^3! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 10/14/2010 21:59'!
stackp
	^Alien forPointer: (self unsignedLongAt: 5)! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 10/12/2010 17:36'!
thunkp
	^self unsignedLongAt: 1! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 6/17/2016 18:44'!
wordResult: aValue
	"Accept any value in the -2^31 to 2^32-1 range or booleans."
	| anInteger |
	anInteger := aValue isInteger
						ifTrue: [aValue]
						ifFalse:  "Will provoke a mustBeBoolean error if not a boolean."
							[aValue ifTrue: [1] ifFalse: [0]].
	anInteger >= 0
		ifTrue: [self unsignedLongAt: 25 put: anInteger]
		ifFalse: [self signedLongAt: 25 put: anInteger].
	^1! !

!VMCallbackContext32 methodsFor: 'accessing' stamp: 'eem 10/14/2010 13:47'!
wordSize
	^4! !

!VMCallbackContext32 methodsFor: 'accessing-debugging' stamp: 'eem 10/17/2020 18:30'!
savedMostRecentCallbackContext
	^self unsignedLongAt: 37! !

!VMCallbackContext32 class methodsFor: 'instance creation' stamp: 'eem 10/14/2010 14:01'!
dataSize
	"We only access the fields before trampoline, so this is fine."
	^32! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 10/15/2010 10:14'!
floatResult: aFloat
	self doubleAt: 49 put: aFloat.
	^3! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 6/17/2016 13:02'!
floatregargsp
	^Alien forPointer: (self unsignedLongLongAt: 25)! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 6/17/2016 13:02'!
intregargsp
	^Alien forPointer: (self unsignedLongLongAt: 17)! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 10/26/2020 20:33'!
oopResult: anObject
	"Accept any object."
	self oopAt: 49 put: anObject.
	^1! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'RonieSalgado 2/13/2017 23:18'!
singleFloatResult: aFloat
	self floatAt: 49 put: aFloat.
	^3! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 10/15/2010 10:13'!
stackp
	^Alien forPointer: (self unsignedLongLongAt: 9)! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 10/12/2010 17:35'!
thunkp
	^self unsignedLongLongAt: 1! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 6/17/2016 18:44'!
wordResult: aValue
	"Accept any value in the -2^63 to 2^64-1 range or booleans."
	| anInteger |
	anInteger := aValue isInteger
						ifTrue: [aValue]
						ifFalse:  "Will provoke a mustBeBoolean error if not a boolean."
							[aValue ifTrue: [1] ifFalse: [0]].
	anInteger >= 0
		ifTrue: [self unsignedLongLongAt: 49 put: anInteger]
		ifFalse: [self signedLongLongAt: 49 put: anInteger].
	^1! !

!VMCallbackContext64 methodsFor: 'accessing' stamp: 'eem 10/14/2010 13:47'!
wordSize
	^8! !

!VMCallbackContext64 methodsFor: 'accessing-debugging' stamp: 'eem 10/17/2020 18:28'!
savedMostRecentCallbackContext
	^self unsignedLongLongAt: 73! !

!VMCallbackContext64 class methodsFor: 'instance creation' stamp: 'eem 10/14/2010 14:01'!
dataSize
	"We only access the fields before trampoline, so this is fine."
	^64! !

!FFICallbackThunk methodsFor: 'accessing' stamp: 'eem 1/29/2008 13:06'!
address
	"Thunks are hacked - again they need to be indirect objects with a 0 size field."
	^self addressField! !

!FFICallbackThunk methodsFor: 'accessing' stamp: 'eem 9/28/2020 15:18'!
thunk
	"For polymorphism and coercion with Callback>>thunk, which answers an instance of me.
	 Instead of answering an error if a client sends thunk to a Callback's thunk, just answer self."
	^self! !

!FFICallbackThunk methodsFor: 'memory management' stamp: 'eem 12/21/2007 15:38'!
free
	"Recycle the memory in the executable page containing the receiver's thunk.
	 Since the receiver is a derived pointer to a block in an executable page this
	 requires returning the block to the free pool."
	AllocatedThunks remove: self.
	AccessProtect critical:
		[| address |
		address := self addressField.
		address ~= 0 ifTrue:
			[self addressFieldPut: 0.
			 self class freeBlockInExecutablePage: address]]! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 2/15/2018 10:21'!
initializeARM32
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/arm32abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:
	
		long long
		thunkEntry(long r0, long r1, long r2, long r3,
		            double d0, double d1, double d2, double d3,
		            double d4, double d5, double d6, double d7,
		            void *thunkpPlus16, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, floating-point
	register arguments, stack pointer) in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

	"0x0 <thunk+0>:		mov     r12, sp			; 0xe1a0c00d
 	 0x4 <thunk+4>:		sub     sp, sp, #16		; 0xe24dd010
 	 0x8 <thunk+8>:		str     pc, [sp, #0]		; 0xe58df000 N.B. passes thunk+16; thunkEntry compensates
 	 0xc <thunk+12>:		str     r12, [sp,#4]		; 0xe58dc004
 	 0x10 <thunk+16>:	str     lr, [sp, #12]		; 0xe58de00c
 	 0x14 <thunk+20>:	ldr     r12, [pc, #8]		; 0xe59fc008
 	 0x18 <thunk+24>:	blx     r12				; 0xe12fff3c
 	 0x1c <thunk+28>:	add     sp, sp, #12		; 0xe28dd00c
 	 0x20 <thunk+32>:	ldr     pc, [sp], #4!!		; 0xe49df004 ; pop     {pc}
 	 0x24 <thunk+36>:     .word thunkEntry"
	self
		unsignedLongAt:   1 put: 16rE1A0C00D;
		unsignedLongAt:   5 put: 16rE24DD010;
		unsignedLongAt:   9 put: 16rE58DF000;
		unsignedLongAt: 13 put: 16rE58DC004;
		unsignedLongAt: 17 put: 16rE58DE00c;
		unsignedLongAt: 21 put: 16rE59FC008;
		unsignedLongAt: 25 put: 16rE12FFF3C;
		unsignedLongAt: 29 put: 16rE28DD00C;
		unsignedLongAt: 33 put: 16rE49DF004;
		unsignedLongAt: 37 put: self primThunkEntryAddress

	"self newCCall"! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 7/19/2021 17:33'!
initializeARMv8
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/arm64abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:
	
		long long
		thunkEntry(long x0, long x1, long x2, long x3,
				       long x4, long x5, long x6, long x7,
				       double d0, double d1, double d2, double d3,
				       double d4, double d5, double d6, double d7,
				       void *thunkpPlus20, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, floating-point
	register arguments, stack pointer) in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

	| bytes |
	bytes := #["0		mov	x16, sp"			16rF0 16r03 16r00 16r91
				"4		sub	sp, sp, #0x20"			16rFF 16r83 16r00 16rD1
				"8		stp	x16, x30, [sp, #0x8]"	16rF0 16rFB 16r00 16rA9
				"c		bl		0x10"				16r01 16r00 16r00 16r94
				"10		str		x30, [sp]"			16rFE 16r03 16r00 16rF9
				"14		ldr		x16, #0x14"		16rB0 16r00 16r00 16r58
				"18		blr		x16	"				16r00 16r02 16r3F 16rD6
				 "1C	ldr		x30, [sp, #0x16]"	16rFE 16r0B 16r40 16rF9
				 "20	add		sp, sp, #0x20"		16rFF 16r83 16r00 16r91
				 "24	ret"							16rC0 16r03 16r5F 16rD6
													0 0 0 0 0 0 0 0] copy.
	bytes unsignedLongLongAt: 41 put: self primThunkEntryAddress.
	self replaceFrom: 1 to: 48 with: bytes startingAt: 1

	"self newCCall"! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'KenD 8/1/2022 11:22'!
initializeRiscV64
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/riscv64abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:
	
		long long
		thunkEntry(long x0, long x1, long x2, long x3,
				       long x4, long x5, long x6, long x7,
				       double d0, double d1, double d2, double d3,
				       double d4, double d5, double d6, double d7,
				       void *thunkpPlus20, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments 
	(thunk, integer register arguments, floating-point register arguments, stack pointer) 
	in a VMCallbackContext and invokes the callback via invokeCallbackContext:."
"
  0 :	00000297        auipc	t0,0x0    Current PC = self codeAddress (start)
  4 :	1101               	addi	sp,sp,-32   reserve 4 stack slots (1 for alignment)
  6 :	e806                sd		ra,16(sp)	push Return Address
  8 :	e40a                sd		sp,8(sp)	push current Stack Pointer
  c :	e016                sd		t0,0(sp)	push self codeAddress
  c :	02828313        t1,	t0,0x28	start+0x28 is address of &entryThunk
 10:	00033383        ld		t2,0(t1)	get &entryThunk in register
 14 :	9382               	jalr		t2			Call entryThunk()
 16 :	60c2                	ld		ra,16(sp)	Restore saved Return Address
 18 :	6105               	addi	sp,sp,32	Restore Stack Pointer
 20 :	8082               	ret					Return to Mamma
 22 :	0001               	noop				--keep prefetch happy-- 
 24 : 0001				noop
 ... 
 28:   <@entryThunk>
" 
	  "NB: upper & lower instructions swapped"
	self unsignedLongLongAt:  1 put: 16rE806110100000297.  "16r0"
	self unsignedLongLongAt:  9 put: 16r02828313E40AE016.  "16r8"
	self unsignedLongLongAt: 17 put: 16r60C2938200033383. "16r10"
	self unsignedLongLongAt: 25 put: 16r0001000180826105. "16r18"
	self unsignedLongLongAt: 33 put: 16r0001000100010001. 
	self unsignedLongLongAt: 41 put: self primThunkEntryAddress.

	self

	"self newCCall"! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 6/17/2016 10:52'!
initializeStdcall: bytes
	"Initialize the receiver with a __stdcall thunk with bytes argument bytes."
	Callback abi caseOf: {
	['IA32']		->	[self initializeX86Stdcall: bytes] }! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 9/30/2020 00:13'!
initializeX64
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/x64sysvabicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:

		long
		thunkEntry(long a0, long a1, long a2, long a3, long a4, long a5,
		            double d0, double d1, double d2, double d3,
		            double d4, double d5, double d6, double d7,
		            void *thunkp, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, floating-point
	register arguments, stack pointer) in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

	"thunk+0x0:	pushq  %rsp							54
	 thunk+0x1:	pushq  %rsp							54
	 thunk+0x4:	movabsq $thunk, %rax					48 b8 b0..b7 eight bytes of thunk address a.k.a. addressField
	 thunk+0xc:	pushq  %rax							50
	 thunk+0xd:	movabsq $thunkEntry, %rax			48 b8 b0..b7 eight bytes of the thunkEntry address
	 thunk+0x17:	callq  *%rax							ff d0
	 thunk+0x19:	addq   $0x18, %rsp					48 83 c4 18
	 thunk+0x1d:	retq									c3
	 thunk+0x1e:	nop										90
	 thunk+0x1f:	nop										90"
	self
		unsignedLongAt:		  1 put: 16rB8485454;
		unsignedLongLongAt:	  5 put: self addressField;
		unsignedLongAt:		13 put: 16r00B84850; "00 is the first byte of the 64-bit constant the movabsq/0x48 opcode moves"
		unsignedLongLongAt:	16 put: self primThunkEntryAddress;
		unsignedByteAt:		24 put: 16rFF;
		unsignedLongAt:		25 put: 16rC48348D0;
		unsignedLongAt:		29 put: 16r9090C318

	"self newCCall"! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 9/30/2020 00:13'!
initializeX64Win64
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/x64win64abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:

		long long
		thunkEntry(long long rcx, long long rdx,
		      		long long r8, long long r9,
					void *thunkp, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, integer register arguments, stack pointer)
	in a VMCallbackContext and invokes the callback via invokeCallbackContext:."

	"thunk+0x0:	pushq  %rsp							54
	 thunk+0x1:	pushq  %rsp							54
	 thunk+0x4:	movabsq $thunk, %rax					48 b8 b0..b7 eight bytes of thunk address a.k.a. addressField
	 thunk+0xc:	pushq  %rax							50
	 thunk+0xd:	subq   $0x20, %rsp						48 83 c4 e0 (this is addq -20 since the immediate is signed extended)
	 thunk+0x11:	movabsq $thunkEntry, %rax			48 b8 b0..b7 eight bytes of the thunkEntry address
	 thunk+0x1b:	callq  *%rax							ff d0
	 thunk+0x1d:	addq   $0x38, %rsp					48 83 c4 38
	 thunk+0x21:	retq									c3
	 thunk+0x22:	nop										90
	 thunk+0x23:	nop										90"
	self
		unsignedLongAt:		  1 put: 16rB8485454;
		unsignedLongLongAt:	  5 put: self addressField;
		unsignedLongAt:		13 put: 16rC4834850; 
		unsignedLongAt:		17 put: 16r00B848E0; "00 is the first byte of the 64-bit constant the movabsq/0x48 opcode moves"
		unsignedLongLongAt:	20 put: self primThunkEntryAddress;
		unsignedByteAt:		28 put: 16rFF;
		unsignedLongAt:		29 put: 16rC48348D0;
		unsignedLongAt:		33 put: 16r9090C338

	"self newCCall"! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 2/15/2018 10:19'!
initializeX86
	"Initialize the receiver with a __ccall thunk.  The thunk calls thunkEntry in the Alien/IA32ABI plugin,
	 whose source is in platforms/Cross/plugins/IA32ABI/x64win64abicc.c.  thunkEntry is the entry point
	 for Callbacks. The objective of the thunk is to call thunkEntry with all arguments to the call of the
	 thunk (registers and stack) as well as a pointer to the thunk itself.  thunkEntry is as follows:

		long
		thunkEntry(void *thunkp, sqIntptr_t *stackp)

	thunkEntry then collects the various arguments (thunk, stack pointer) in a VMCallbackContext and
	invokes the callback via invokeCallbackContext:."

	"thunk:		push   %esp				0x54							0xa1905454
	 thunk+01:	push   %esp				0x54
	 thunk+02:	nop   						0x90
	 thunk+03:	mov    $thunkEntry,%eax	0xb8 0x00 0x00 0x00 0x00	0x00000000 - entry
	 thunk+08:	nop   						0x90							0x68909090
	 thunk+09:	nop   						0x90
	 thunk+10:	nop   						0x90
	 thunk+11:	push   $thunk				0x68 0x00 0x00 0x00 0x00	0x00000000 - thunk
	 thunk+16:	call   *%eax					0xff 0xd0						0xc483d0ff
	 thunk+18:	add    $0xC,%esp			0x83 0xc4 0x0C				0x9090c30C
	 thunk+21:	ret							0xc3
	 thunk+22:	nop							0x90
	 thunk+23:	nop							0x90"
	self
		unsignedLongAt:  1 put: 16rB8905454;
		unsignedLongAt:  5 put: self primThunkEntryAddress;
		unsignedLongAt:  9 put: 16r68909090;
		unsignedLongAt: 13 put: self addressField;
		unsignedLongAt: 17 put: 16rC483D0FF;
		unsignedLongAt: 21 put: 16r9090C30C! !

!FFICallbackThunk methodsFor: 'private-instance initalization' stamp: 'eem 2/15/2018 10:08'!
initializeX86Stdcall: bytes
	"Initialize the receiver with a __stdcall thunk with bytes argument bytes. (See initializeX86 fort more info)"
	"thunk:		push   %esp				0x54							0xa1905454
	 thunk+01:	push   %esp				0x54
	 thunk+02:	nop   						0x90
	 thunk+03:	mov    $thunkEntry,%eax	0xb8 0x00 0x00 0x00 0x00	0x00000000 - entry
	 thunk+08:	nop   						0x90							0x68909090
	 thunk+09:	nop   						0x90
	 thunk+10:	nop   						0x90
	 thunk+11:	push   $thunk				0x68 0x00 0x00 0x00 0x00	0x00000000 - thunk
	 thunk+16:	call   *%eax					0xff 0xd0						0xc483d0ff
	 thunk+18:	add    $0xC,%esp			0x83 0xc4 0x0C				0xBYTSc20C
	 thunk+21:	ret    $bytes				0xc2 0xBY 0xTS"

	self
		unsignedLongAt:  1 put: 16rB8905454;
		unsignedLongAt:  5 put: self primThunkEntryAddress;
		unsignedLongAt:  9 put: 16r68909090;
		unsignedLongAt: 13 put: self addressField;
		unsignedLongAt:  17 put: 16rC483D0FF;
		unsignedShortAt: 21 put: 16rC20C;
		unsignedShortAt: 23 put: bytes! !

!FFICallbackThunk methodsFor: 'primitives-memory management' stamp: 'eem 12/21/2007 15:45'!
primFree
	self shouldNotImplement! !

!FFICallbackThunk methodsFor: 'private-primitives' stamp: 'eem 6/17/2016 13:07'!
primThunkEntryAddress "^<Integer>"
	"Answer the address of the entry-point for thunk callbacks:
		x86: long thunkEntry(void *thunkp, long *stackp);
		x64: long thunkEntry(long a, long b, long c, long d, long, e, long f,
								double d0, double d1, double d2, double d3,
								double d4, double d5, double d6, double d7,
								void *thunkp, long *stackp);
		ARM: long thunkEntry(long a, long b, long c, long d,
								double d0, double d1, double d2, double d3,
								double d4, double d5, double d6, double d7,
								void *thunkp, long *stackp);
	 etc.
	 This is the function a callback thunk/trampoline should call to initiate a callback."
	<primitive: 'primThunkEntryAddress' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!FFICallbackThunk class methodsFor: 'executable page management' stamp: 'eem 12/5/2008 09:41'!
allocateExecutablePage "^<Alien>"
	"Answer an Alien around a piece of (probably malloced) memory, of some multiple
	 of the pagesize, that has execute permissions set.  This memory can be parcelled
	 out to individual FFICallbackThunks to hold their thunk/trampoline machine code."
	<primitive: 'primAllocateExecutablePage' module: 'IA32ABI' error: errorCode>
	^self primitiveFailed! !

!FFICallbackThunk class methodsFor: 'executable page management' stamp: 'eem 7/19/2021 13:25'!
freeBlockInExecutablePage: address
	ExecutablePages do:
		[:alienPage| | alienAddress |
		alienAddress := alienPage address.
		(address >= alienAddress
		 and: [alienAddress + alienPage dataSize >= address]) ifTrue:
			[^alienPage replaceFrom: address - alienAddress + 1 to:address - alienAddress + 4 with: #[0 0 0 0] startingAt: 1]]
			! !

!FFICallbackThunk class methodsFor: 'executable page management' stamp: 'eem 7/19/2021 13:41'!
unusedBlockInExecutablePage
	"Callback thunks must be executable. This is achieved by allocating one or more executable pages
	 of memory via the allocateExecutablePage, and allocating dataSize sized chunks from them, marking
	 each used chunk with 4 non-zero bytes. We use 'used' as the used marker, until a callback thunk
	 overwrites the chunk with its own machine code, which, at least so far, does not begin with 4 0's.

	 An added complication is that on platforms such as Apple Silicon memory cannot be both executable
	 and writable at the same time.  On these platforms to write to executable memory the memory must
	 be flipped from executability, written to, and then flipped back into executablity (and currently Apple
	 Silicon appears to have no way to do this in a thread-safe manner with more than one thread !!!!). To
	 achieve this a hook exists in the Alien>>replaceFrom:to:with:startingAt: primAlienReplace primitive
	 that checks for the target being on an executable page, and performs the flips around the write.

	 Note that with e.g. a 4k page size and 40 byte callback thunks an application would have to create
	 over a hundred callbacks before it needed more than one page of executable memory."
	^AccessProtect critical:
		[ExecutablePages do:
			[:alien|
			1 to: alien dataSize - self dataSize by: self dataSize do:
				[:i|
				(alien unsignedLongAt: i) = 0 ifTrue:
					[alien replaceFrom: i to: i + 3 with: 'used' startingAt: 1.
					 ^alien address + i - 1]]].
		 (ExecutablePages add: self allocateExecutablePage)
			replaceFrom: 1 to: 4 with: 'used' startingAt: 1;
			address]! !

!FFICallbackThunk class methodsFor: 'instance creation' stamp: 'eem 7/19/2021 17:19'!
dataSize
	^48 "see FFICallbackThunk initialize & initializeStdcall:; must be big enough for the largest thunk created."! !

!FFICallbackThunk class methodsFor: 'instance creation' stamp: 'KenD 7/27/2022 10:26'!
initializerForPlatform
	"Answer the relevant initialize method for a C calling convention thunk on the current platform."
	^Callback abi caseOf: {
	['ARM32']		->	[#initializeARM32].
	['ARMv8']		->	[#initializeARMv8].
	['IA32']			->	[#initializeX86].
	['X64']			->	[#initializeX64].
	['X64Win64']	->	[#initializeX64Win64].
	['RiscV64']		-> [#initializeRiscV64]
	}! !

!FFICallbackThunk class methodsFor: 'instance creation' stamp: 'eem 9/30/2020 08:52'!
newCCall
	"Answer a new instance for the C calling-convention."
	^self privateFinalizingNew perform: (initializer ifNil: [initializer := self initializerForPlatform])! !

!FFICallbackThunk class methodsFor: 'instance creation' stamp: 'eem 12/21/2007 15:32'!
newStdCall: argBytes
	"Answer a new instance for Windows' StdCall (Pascal) convention, given the
	 size of the arguments in bytes (bytes to cut-back from the stack on return)."
	^self privateFinalizingNew initializeStdcall: argBytes! !

!FFICallbackThunk class methodsFor: 'instance creation' stamp: 'eem 1/12/2010 13:42'!
privateFinalizingNew
	"Answer a new instance that will free its page when garbage collected."
	| address |
	address := self unusedBlockInExecutablePage.
	^AllocatedThunks
		add: (self forPointer: address)
		finalizing: address ! !

!FFICallbackThunk class methodsFor: 'class initialization' stamp: 'nice 3/2/2020 10:32'!
ensureNotifiedAtStartup
	"FFICallbackThunk needs to be notified at system startup to clear the ExecutablePages table."
	self ~~ FFICallbackThunk ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil:
			[Alien ensureNotifiedAtStartup.
			Smalltalk addToStartUpList: self after: Alien]! !

!FFICallbackThunk class methodsFor: 'class initialization' stamp: 'eem 2/27/2008 16:47'!
initialize
	"FFICallbackThunk initialize"
	AccessProtect := Semaphore forMutualExclusion.
	AllocatedThunks := AlienWeakTable newForOwner: self.
	ExecutablePages := Set new.
	self ensureNotifiedAtStartup! !

!FFICallbackThunk class methodsFor: 'class initialization' stamp: 'eem 9/30/2020 08:48'!
startUp: inANewImageSession
	"Any thunks in the finalization registry at the time the image comes up in a
	 new session MUST NOT be finalized and should immediately be discarded.
	 Their thunk pointers are no longer valid."

	inANewImageSession ifTrue:
		[AllocatedThunks removeAll.
		ExecutablePages := Set new.
		initializer := nil]! !

!FFICallbackThunk class methodsFor: 'finalization' stamp: 'eem 12/21/2007 15:17'!
finalize: address
	self freeBlockInExecutablePage: address! !

!AlienWeakTable methodsFor: 'adding' stamp: 'vb 5/7/2008 08:34'!
add: anObject finalizing: postMortemFinalizationTag
	"Register a new object in the table, with the associated tag. The object is held onto weakly, the tag--strongly. The tag will be passed to the owner as the argument of #finalize: some time after the object is garbage collected."

	| size start delta |
	accessProtect critical:
		[size := strongArray size.
		 start := firstUnusedIndex].
	start to: size do:
		[:i|
		(strongArray at: i) == nil ifTrue:
			[accessProtect critical:
				[weakArray at: i put: anObject.
				 strongArray at: i put: postMortemFinalizationTag.
				 firstUnusedIndex := i + 1.
				 i > lastUsedIndex ifTrue: [lastUsedIndex := i]].
			 ^anObject]].
	delta := size min: 4096.
	accessProtect critical:
		[(weakArray := weakArray copyGrownBy: delta)
			 at: size + 1 put: anObject.
		 (strongArray := strongArray  copyGrownBy: delta)
			at: size put: postMortemFinalizationTag.
		 firstUnusedIndex := size + 1.
		 lastUsedIndex := size].
	^anObject! !

!AlienWeakTable methodsFor: 'finalization' stamp: 'vb 5/7/2008 08:35'!
finalizeValues
	"Sent periodically by the finalization loop to check for any dead objects and report their death to the owner."
	
	accessProtect critical:
		[| usedIndex value newLastUsedIndex |
		 newLastUsedIndex := 0.
		 1 to: lastUsedIndex do:
			[:i|
			(value := strongArray at: i) ifNotNil:
				[(weakArray at: i)
					ifNil:
						[strongArray at: i put: nil.
						 owner finalize: value.
						 firstUnusedIndex > i ifTrue:
							[firstUnusedIndex := i]]
					ifNotNil: [newLastUsedIndex := i]]].
		 lastUsedIndex := newLastUsedIndex]! !

!AlienWeakTable methodsFor: 'initialize-release' stamp: 'vb 5/8/2008 11:32'!
initializeWithOwner: anObject
	accessProtect := Semaphore forMutualExclusion.
	firstUnusedIndex := 1.
	lastUsedIndex := 0.
	weakArray := WeakArray new: 1024.
	strongArray := Array new: 1024.
	owner := anObject.
	WeakArray addWeakDependent: self! !

!AlienWeakTable methodsFor: 'removing' stamp: 'eem 1/11/2008 12:10'!
remove: anObject
	1 to: lastUsedIndex do:
		[:i|
		(weakArray at: i) == anObject ifTrue:
			[accessProtect critical:
				[strongArray at: i put: nil.
				 firstUnusedIndex > i ifTrue:
					[firstUnusedIndex := i].
				 i = lastUsedIndex ifTrue:
					[lastUsedIndex := i - 1]].
			 ^anObject]].
	^anObject! !

!AlienWeakTable methodsFor: 'removing' stamp: 'vb 5/8/2008 11:31'!
removeAll

	accessProtect critical:
		[1 to: lastUsedIndex do:
			[:index |
			strongArray at: index put: nil.
			weakArray at: index put: nil].
		firstUnusedIndex := 1.
		lastUsedIndex := 0]! !

!AlienWeakTable class methodsFor: 'instance creation' stamp: 'eem 12/21/2007 15:00'!
newForOwner: anOwner
	^self basicNew initializeWithOwner: anOwner! !

!Callback methodsFor: 'private' stamp: 'KenD 8/1/2022 16:05'!
addToThunkTable
	FlushThunkToCallbackMap ifTrue:
		[FlushThunkToCallbackMap := false.
		 ThunkToCallbackMap keys do:
			[:k| (ThunkToCallbackMap at: k) ifNil: [ThunkToCallbackMap removeKey: k]]].
	ThunkToCallbackMap at: thunk addressField put: self! !

!Callback methodsFor: 'private' stamp: 'eem 6/17/2016 18:51'!
evaluatorForSignature: signature "<String|Array>"
	"Search the methods marked with the signature: primtiive for those that match signature.
	 signature is typically a literal Array for the function's C signature, e.g. #(int (*)(int, char *))."
	Pragma withPragmasIn: self class do:
		[:pragma|
		 (pragma key == #signature:
		  and: [(pragma argumentAt: 1) = signature]) ifTrue:
			[^pragma method]].
	self error: 'could not find Callback signature'! !

!Callback methodsFor: 'private' stamp: 'eem 10/17/2020 14:03'!
printSignature: aLiteralArray on: aStream
	aStream nextPut: $(.
	aLiteralArray
		do:
			[:thing|
			thing isArray
				ifTrue: [self printSignature: thing on: aStream]
				ifFalse:
					[thing isString
						ifTrue: [aStream nextPutAll: thing]
						ifFalse: [aStream print: thing]]]
		separatedBy: [aStream space].
	aStream nextPut: $)! !

!Callback methodsFor: 'evaluation' stamp: 'eem 9/27/2020 16:37'!
valueInContext: callbackContext "<VMCallbackContext32|VMCallbackContext64> ^<Integer>"
	"Perform the evaluator with arguments derived from the callback context in an order most appropriate for the ABI.
	 On x86 the order is callbackContext, stackPointerAlien.
	 On ``RISCS'' it is callbackContext, integerRegsAlien, stackPointerAlien, floatRegsAlien.
	 Here we provide the RISC version; CISC subclasses override."
	numEvaluatorArgs = 2 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext intregargsp].
	numEvaluatorArgs = 3 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext intregargsp
			with: callbackContext stackp].
	numEvaluatorArgs = 4 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext intregargsp
			with: callbackContext stackp
			with: callbackContext floatregargsp].
	numEvaluatorArgs = 1 ifTrue: "legacy support"
		[^self perform: evaluator
			with: callbackContext].
	self error: 'invalid evaluator argument count'! !

!Callback methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:29'!
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	self subclassResponsibility! !

!Callback methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:29'!
intcharstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(int, char *))>
	self subclassResponsibility! !

!Callback methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:28'!
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	self subclassResponsibility! !

!Callback methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:28'!
voidstarvoidstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(const void *, const void *))>
	self subclassResponsibility! !

!Callback methodsFor: 'instance initialization' stamp: 'eem 10/15/2010 10:51'!
signature: signature "<String>" block: aBlock "<BlockClosure>"
	| method |
	method := self evaluatorForSignature: signature.
	block := aBlock.
	thunk := (method pragmaAt: #calleepops:)
				ifNil: [FFICallbackThunk newCCall]
				ifNotNil:
					[:pragma|
					FFICallbackThunk newStdCall: (pragma argumentAt: 1)].
	numEvaluatorArgs := (evaluator := method selector) numArgs.
	self addToThunkTable! !

!Callback methodsFor: 'accessing' stamp: 'eem 11/19/2007 16:54'!
thunk
	^thunk! !

!Callback methodsFor: 'printing' stamp: 'eem 10/17/2020 18:33'!
printOn: aStream
	super printOn: aStream.
	(self class lookupSelector: evaluator) ifNotNil:
		[:method|
		(method pragmaAt: #signature:) ifNotNil:
			[:pragma|
			aStream nextPutAll: ' signature: #'.
			self printSignature: pragma arguments first on: aStream]].
	block ifNotNil:
		[aStream nextPutAll: ' block: '; print: block]! !

!Callback class methodsFor: 'accessing' stamp: 'eem 6/17/2016 10:48'!
abi
	^ABI! !

!Callback class methodsFor: 'class initialization' stamp: 'nice 3/2/2020 10:31'!
ensureNotifiedAtStartup
	"Callback needs to be notified at system startup to clear the ThunkToCallbackMap and determine concreteClass."
	self ~~ Callback ifTrue: [^self].
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil:
			[FFICallbackThunk ensureNotifiedAtStartup.
			Smalltalk addToStartUpList: self after: FFICallbackThunk]! !

!Callback class methodsFor: 'class initialization' stamp: 'eem 5/31/2024 17:10:00'!
initialize
	"Callback initialize"
	self ensureNotifiedAtStartup.
	[self startUp: false] "Monticello... grrr...."
		on: Error
		do: [:ex| ].
	(Smalltalk specialObjectsArray at: 54) ~~ #invokeCallbackContext: ifTrue:
		[self inform: (Character cr asString join: #('this version of Callbacks requires the callback selector to be' '#invokeCallbackContext:.  recreate the specialObjects Array.'))]! !

!Callback class methodsFor: 'class initialization' stamp: 'eem 6/21/2016 12:57'!
startUp: isInANewSession
	"Callback startUp: false"
	| prevMap |
	concreteClass := subclasses detect: [:subclass| subclass isForCurrentPlatform] ifNone: [].
	ABI := concreteClass ifNotNil: [concreteClass abi] ifNil: ['UNKNOWN'].
	FlushThunkToCallbackMap := false.
	(WeakArray hasWeakDependent: self) ifFalse:
		[WeakArray addWeakDependent: self].
	prevMap := isInANewSession ifFalse: [ThunkToCallbackMap].
	ThunkToCallbackMap := WeakValueDictionary new.
	prevMap ifNotNil:
		[prevMap keysAndValuesDo:
			[:thunkAddress :callback|
			ThunkToCallbackMap at: thunkAddress put: callback]]! !

!Callback class methodsFor: 'evaluation' stamp: 'eem 10/17/2010 15:06'!
evaluateCallbackForContext: callbackContext "<VMCallbackContext32|VMCallbackContext64> ^<FFICallbackReturnValue>"
	(ThunkToCallbackMap
		at: callbackContext thunkp
		ifAbsent: [^self error: 'could not locate Callback instance corresponding to thunk address'])
		ifNil: [self error: 'Callback instance for this thunk address has been garbage collected']
		ifNotNil:
			[:callback|
			^callback valueInContext: callbackContext]! !

!Callback class methodsFor: 'finalization' stamp: 'eem 2/20/2008 16:11'!
finalizeValues
	FlushThunkToCallbackMap := true! !

!Callback class methodsFor: 'instance creation' stamp: 'eem 6/17/2016 13:58'!
newForPlatform
	"Answer an instance of the subclass for this platform's ABI, if any."
	^concreteClass
		ifNotNil: [:cc| cc new]
		ifNil: [self error: 'could not find a concrete Callback class for this platform']! !

!Callback class methodsFor: 'instance creation' stamp: 'eem 6/18/2016 16:35'!
signature: signature "<String>" block: aBlock "<BlockClosure> ^<Callback>"
	"Answer a Callback that evaluates aBlock whose C signature is signature 
	 and whose thunk should conform to the C calling convention."
	^self newForPlatform signature: signature block: aBlock! !

!CallbackForARM32 methodsFor: 'signatures' stamp: 'eem 2/15/2018 09:48'!
hwndlparamRetbool: callbackContext regs: regsAlien
	<signature: #(BOOL CALLBACK (*)(HWND, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (regsAlien unsignedLongAt: 5)) asAlien "i.e. coerce true to 1 and false to 0"! !

!CallbackForARM32 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:45'!
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))! !

!CallbackForARM32 methodsFor: 'signatures' stamp: 'eem 2/15/2018 09:04'!
logfontstartextmetricstardwordlparamRetint: callbackContext regs: regsAlien
	<signature: #(int CALLBACK (*)(const LOGFONT *, const TEXTMETRIC *, DWORD, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5))
			value: (regsAlien unsignedLongAt: 9)
			value: (regsAlien signedLongAt: 13))! !

!CallbackForARM32 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:26'!
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongAt: 5)))! !

!CallbackForARM32 class methodsFor: 'accessing' stamp: 'eem 6/17/2016 13:44'!
abi
	^'ARM32'! !

!CallbackForARM32 class methodsFor: 'class initialization' stamp: 'eem 05/31/2024 17:35'!
isForCurrentPlatform
	| processor |
	processor := (Smalltalk getSystemAttribute: 1003) asLowercase.
	^processor = 'arm' or: [(processor beginsWith: 'armv') and: [processor fifth <= $7]]! !

!CallbackForARMv8 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:45'!
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForARMv8 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:40'!
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForARMv8 class methodsFor: 'class initialization' stamp: 'eem 05/31/2024 17:35'!
isForCurrentPlatform
	^#('aarch64' 'arm64') includes: (Smalltalk getSystemAttribute: 1003)! !

!CallbackForARMv8 class methodsFor: 'accessing' stamp: 'eem 9/30/2020 08:17'!
abi
	^'ARMv8'! !

!CallbackForIA32 methodsFor: 'signatures' stamp: 'eem 2/15/2018 09:49'!
hwndlparamRetbool: callbackContext sp: spAlien
	<signature: #(BOOL CALLBACK (*)(HWND, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (spAlien unsignedLongAt: 5)) asAlien "i.e. coerce true to 1 and false to 0"! !

!CallbackForIA32 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:46'!
intcharstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (spAlien signedLongAt: 1)
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))! !

!CallbackForIA32 methodsFor: 'signatures' stamp: 'eem 2/15/2018 09:07'!
logfontstartextmetricstardwordlparamRetint: callbackContext sp: spAlien
	<signature: #(int CALLBACK (*)(const LOGFONT *, const TEXTMETRIC *, DWORD, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5))
			value: (spAlien unsignedLongAt: 9)
			value: (spAlien signedLongAt: 13))! !

!CallbackForIA32 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:27'!
voidstarvoidstarRetint: callbackContext sp: spAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (spAlien unsignedLongAt: 1))
			value: (Alien forPointer: (spAlien unsignedLongAt: 5)))! !

!CallbackForIA32 methodsFor: 'evaluation' stamp: 'eem 6/17/2016 18:39'!
valueInContext: callbackContext "<VMCallbackContext32|VMCallbackContext64> ^<Integer>"
	numEvaluatorArgs = 2 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext stackp].
	numEvaluatorArgs = 3 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext stackp
			with: callbackContext intregargsp].
	numEvaluatorArgs = 4 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext stackp
			with: callbackContext intregargsp
			with: callbackContext floatregargsp].
	numEvaluatorArgs = 1 ifTrue: "legacy support"
		[^self perform: evaluator
			with: callbackContext].
	self error: 'invalid evaluator argument count'! !

!CallbackForIA32 class methodsFor: 'accessing' stamp: 'eem 6/17/2016 13:45'!
abi
	^'IA32'! !

!CallbackForIA32 class methodsFor: 'class initialization' stamp: 'eem 05/31/2024 17:35'!
isForCurrentPlatform
	| processor |
	processor := (Smalltalk getSystemAttribute: 1003) asLowercase.
	^processor first = $i and: [processor = 'intel' or: ['i#86' match: processor]]! !

!CallbackForRiscV64 methodsFor: 'signatures' stamp: 'KenD 6/26/2022 14:50'!
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForRiscV64 methodsFor: 'signatures' stamp: 'KenD 6/26/2022 14:50'!
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForRiscV64 class methodsFor: 'class initialization' stamp: 'eem 05/31/2024 17:35'!
isForCurrentPlatform
	^#('riscv64' 'RiscV64') includes: (Smalltalk getSystemAttribute: 1003)! !

!CallbackForRiscV64 class methodsFor: 'accessing' stamp: 'KenD 6/26/2022 14:49'!
abi
	^'RiscV64'! !

!CallbackForWin64X64 methodsFor: 'signatures' stamp: 'eem 2/15/2018 09:47'!
hwndlparamRetbool: callbackContext regs: regsAlien
	<signature: #(BOOL CALLBACK (*)(HWND, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 1))
			value: (regsAlien unsignedLongLongAt: 9)) asAlien "i.e. coerce true to 1 and false to 0"! !

!CallbackForWin64X64 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:46'!
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForWin64X64 methodsFor: 'signatures' stamp: 'eem 2/15/2018 09:09'!
logfontstartextmetricstardwordlparamRetint: callbackContext regs: regsAlien
	<signature: #(int CALLBACK (*)(const LOGFONT *, const TEXTMETRIC *, DWORD, LPARAM))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9))
			value: (regsAlien unsignedLongAt: 17)
			value: (regsAlien signedLongLongAt: 25))! !

!CallbackForWin64X64 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:37'!
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForWin64X64 class methodsFor: 'accessing' stamp: 'nice 8/21/2016 11:59'!
abi
	^'X64Win64'! !

!CallbackForWin64X64 class methodsFor: 'class initialization' stamp: 'eem 05/31/2024 17:35'!
isForCurrentPlatform
	| processor |
	(#('Win32' 'Win64') includes: (Smalltalk getSystemAttribute: 1001)) ifFalse:
		[^false].
	processor := (Smalltalk getSystemAttribute: 1003) asLowercase.
	^processor first = $x and: [processor = 'x64' or: [('x86#64*' match: processor)]]! !

!CallbackForX64 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:45'!
intcharstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(int, char *))>
	^callbackContext wordResult:
		(block
			value: (regsAlien signedLongLongAt: 1)
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForX64 methodsFor: 'signatures' stamp: 'eem 6/17/2016 18:40'!
voidstarvoidstarRetint: callbackContext regs: regsAlien
	<signature: #(int (*)(const void *, const void *))>
	^callbackContext wordResult:
		(block
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 1))
			value: (Alien forPointer: (regsAlien unsignedLongLongAt: 9)))! !

!CallbackForX64 class methodsFor: 'accessing' stamp: 'eem 6/17/2016 13:44'!
abi
	^'X64'! !

!CallbackForX64 class methodsFor: 'class initialization' stamp: 'eem 05/31/2024 17:35'!
isForCurrentPlatform
	| processor |
	CallbackForWin64X64 isForCurrentPlatform ifTrue:
		[^false].
	processor := (Smalltalk getSystemAttribute: 1003) asLowercase.
	^processor first = $x and: [processor = 'x64' or: [('x86#64*' match: processor)]]! !

!False methodsFor: '*Alien-Core' stamp: 'John M McIntosh 11/17/2008 17:02'!
asAlien
	^ 0! !

!True methodsFor: '*Alien-Core' stamp: 'eem 1/25/2008 13:09'!
asAlien	^ 1! !

!UndefinedObject methodsFor: '*Alien-Core' stamp: 'eem 1/25/2008 13:10'!
asAlien
	^0! !

!String methodsFor: '*Alien-Core' stamp: 'eem 2/21/2008 15:45'!
asAlien
	| bs |
	^(Alien newC: (bs := self byteSize) + 1)
		replaceFrom: 1 to: bs with: self startingAt: 1! !

!WeakArray class methodsFor: '*Alien-Core' stamp: 'eem 5/31/2024 19:07:59'!
hasWeakDependent: anObject
	^FinalizationDependents includes: anObject! !
Alien initialize!
FFICallbackThunk initialize!
Callback initialize!
