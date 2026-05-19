'From Cuis7.7 [latest update: #7925] on 18 May 2026 at 3:57:08 pm'!
!classDefinition: #PrimitiveError category: #'System-Support'!
Object subclass: #PrimitiveError
	instanceVariableNames: 'errorName errorCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!PrimitiveError commentStamp: 'akg 1/8/2018 15:09' prior: 0!
A PrimitiveError is used to answer a primitive failure code that has an associated operating system/library error.

Instance Variables
	errorName:		<Symbol>
	errorCode:		<Integer>

errorName
	- typically #'operating system error'

errorCode
	- the value of the error, a signed 64-bit value, a representation imposed by the VM; specific clients must map this error value into an unsigned value as appropriate if required

Typical usage is shown in the ficticious method below:

primitiveOperation
	<primitive: 'primitiveOperation' module: 'APlugin' error: error>
	^(error isPrimitiveError)
		ifTrue: [ self processErrorCode: error errorCode ]
		ifFalse: [ self primitiveFailed ].!

!classDefinition: #ExceptionInFFICallError category: #'System-Support'!
PrimitiveError subclass: #ExceptionInFFICallError
	instanceVariableNames: 'pc'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!ExceptionInFFICallError commentStamp: 'eem 8/24/2018 15:21' prior: 0!
An ExceptionInFFICallError is used to answer the exception code and program counter at which an exceprion occurred during an FFI call.  Note that such exceptions are only delivered under certain circumstances, by default if an FFI callout method includes an error code variable.  But the VM provides command-line arguments to override this behaviour, either to always deliver or never deliver such exceptions as primitive failures.  And support appeared in VMs from VMMaker.oscog-eem.2433 or later.

Instance Variables
	pc:		<Integer>

errorCode (inherited)
	- the error code for the exception, which is platform-dependent.  e.g. on Win32 it is the ExceptionRecord->ExceptionCode, such as 16rC0000005, and on Unix it is SIGBUS, SIGILL, SIGSEGV et al.

pc
	- the program counter of the instruction that raised an exception

Here is a small FFI example to provoke such an error.  Warning: this will crash the VM if either the VM is too old, or if ExternalFunction>>#invokeWithArguments: is lacking an error code, or if the handling has been overridden by the relevant VM command line argument.

| pt handleClass elf |
pt := Parser new parse: 'function <cdecl: void ''foo'' () error: ec> self ffiCallFailedWith: ec' class: UndefinedObject.
elf := pt encoder literals first.
handleClass := elf getHandle class.
ByteArray adoptInstance: elf getHandle. "otherwise ExternalHandle access fails cuz address not in heap"
Smalltalk wordSize = 8
	ifTrue: [elf getHandle unsignedLongLongAt: 1 put: 17]
	ifFalse: [elf getHandle unsignedLongAt: 1 put: 17].
handleClass adoptInstance: elf getHandle.
elf invoke
!


!PrimitiveError methodsFor: 'accessing' stamp: 'eem 7/Dec/2017 19:32:00'!
errorCode
	
	^errorCode! !

!PrimitiveError methodsFor: 'accessing' stamp: 'akg 8/Jan/2018 15:11:00'!
errorCode: anInteger
	
	errorCode := anInteger! !

!PrimitiveError methodsFor: 'accessing' stamp: 'eem 7/Dec/2017 18:16:00'!
errorName
	
	^errorName! !

!PrimitiveError methodsFor: 'accessing' stamp: 'akg 8/Jan/2018 15:11:00'!
errorName: aSymbol
	
	errorName := aSymbol! !

!PrimitiveError methodsFor: 'testing' stamp: 'eem 24/Aug/2018 15:03:00'!
isExceptionInFFICallError
	"Answer if the receiver is an error object for an exception during an FFI call."
	^false! !

!PrimitiveError methodsFor: 'testing' stamp: 'eem 24/Aug/2018 15:10:00'!
isPrimitiveError
	"Answer if the receiver is an error object returned by a primitive."
	^true! !

!PrimitiveError methodsFor: 'printing' stamp: 'mt 17/Apr/2024 10:24:00'!
printOn: aStream

	self errorName
		ifNil: [super printOn: aStream]
		ifNotNil: [:nm | nm printOn: aStream].! !


!ExceptionInFFICallError methodsFor: 'testing' stamp: 'eem 24/Aug/2018 15:03:00'!
isExceptionInFFICallError
	"Answer if the receiver is an error object for an exception during an FFI call."
	^true! !

!ExceptionInFFICallError methodsFor: 'accessing' stamp: 'eem 24/Aug/2018 15:02:00'!
pc

	^ pc! !

!ExceptionInFFICallError methodsFor: 'accessing' stamp: 'eem 24/Aug/2018 15:02:00'!
pc: anObject

	pc := anObject.! !


!SystemDictionary methodsFor: 'special objects' stamp: 'eem 18/May/2026 15:39:18'!
recreateSpecialObjectsArray
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 60.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active processor (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: BoxedFloat64.
	newArray at: 11 put: MethodContext.
	newArray at: 11 put: (self at: #MethodContext ifAbsent: [self at: #Context]).
	newArray at: 12 put: nil. "was BlockContext."
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: ((self specialObjectsArray at: 18) ifNil: [Semaphore new]). "low space Semaphore"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopyNOWUNUSED: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	newArray at: 25 put: nil. "This is nil in Spur, which has immediate Characters."
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	newArray at: 29 put: nil. "This is nil in Spur, no compact classes."
	newArray at: 30 put: ((self specialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self specialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"

	"Entries 32 - 34 unreferenced. Previously these contained prototype instances to be copied for fast initialization.
	In Squeak these are: DoubleByteArray, WordArray, DoubleWordArray.
	Understand why."
	newArray at: 32 put: nil. "was the prototype Float"
	newArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"

	newArray at: 35 put: #cannotInterpret:.
	newArray at: 36 put: nil. "was the prototype MethodContext"
	newArray at: 37 put: BlockClosure.
	newArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self specialObjectsArray at: 41) ifNil: [LinkedList new]). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: nil).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: nil).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: nil).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: nil).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: nil).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	newArray at: 51 put: #attemptToAssign:withIndex:.
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object'
							#'object moved' #'object not pinned' #'callback error'),
						  {	PrimitiveError new errorName: #'operating system error'; yourself.
							 ExceptionInFFICallError new errorName: #'exception in FFI call'; yourself},
						  #(	#'heap compaction needed' #'operation failed'
							#'internal error' #'uninitialized'),
						  {	PrimitiveError new errorName: #'FFI marshalling error'; yourself}.
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: nil).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: nil).

	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for the foreign callback process prototype"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: nil).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray.! !

Smalltalk recreateSpecialObjectsArray!
