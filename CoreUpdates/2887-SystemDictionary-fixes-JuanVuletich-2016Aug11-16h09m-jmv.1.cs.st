'From Cuis 4.2 of 25 July 2013 [latest update: #2880] on 11 August 2016 at 4:24:42 pm'!
!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments '
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp WordSize '
	poolDictionaries: ''
	category: 'System-Support'!

!SystemDictionary methodsFor: 'image' stamp: 'fbs 7/20/2013 23:27'!
wordSize
	"Answer the size in bytes of an object pointer or word in the object memory.
	The value does not change for a given image, but may be modified by a SystemTracer
	when converting the image to another format. The value is cached in WordSize to
	avoid the performance overhead of repeatedly consulting the VM."

	"Smalltalk wordSize"

	^ WordSize ifNil: [WordSize := [self vmParameterAt: 40] on: Error do: [4]]! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/11/2016 16:14:53'!
setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring."

	Smalltalk isSpur
		ifTrue: [
			| proportion edenSize survivorSize averageObjectSize numObjects |
			proportion := 0.9. "tenure when 90% of pastSpace is full"
			edenSize := self vmParameterAt: 44.
			survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
			averageObjectSize := 8 * self wordSize. "a good approximation"
			numObjects := (proportion * survivorSize / averageObjectSize) rounded.
			self vmParameterAt: 6 put: numObjects  "tenure when more than this many objects survive the GC"
			]
		ifFalse: [
			Smalltalk vmParameterAt: 5 put: 4000.  "do an incremental GC after this many allocations"
			Smalltalk vmParameterAt: 6 put: 2000.  "tenure when more than this many objects survive the GC"
			]! !

!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 7/29/2016 15:56:38'!
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
	"This association holds the active process (a ProcessScheduler)"
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
							#blockCopy: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	"An array of the 255 Characters in ascii order.
	 Cog inlines table into machine code at: prim so do not regenerate it."
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character value: ascii])."
" 	 This is nil in Spur, which has immediate Characters."
	newArray at: 25 put: (self specialObjectsArray at: 25).
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	"An array of up to 31 classes whose instances will have compact headers; an empty array in Spur"
	newArray at: 29 put: self compactClassesArray.
	newArray at: 30 put: ((self specialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self specialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"

	"Prototype instances that can be copied for fast initialization"
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
"	newArray at: 32 put: Float new.
	newArray at: 33 put: (LargePositiveInteger new: 4).
	newArray at: 34 put: Point new."
	"Entries 32 - 34 unreferenced. Previously these contained prototype instances to be copied for fast initialization"
	newArray at: 32 put: nil. "was the prototype Float"
	newArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"

	newArray at: 35 put: #cannotInterpret:.

	"Note: This must be fixed once we start using context prototypes (yeah, right)"
	"(MethodContext new: CompiledMethod fullFrameSize)."
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	newArray at: 36 put: (self specialObjectsArray at: 36). "Is the prototype MethodContext (unused by the VM)"
	newArray at: 36 put: nil. "was the prototype MethodContext"

	newArray at: 37 put: BlockClosure.

	"(BlockContext new: CompiledMethod fullFrameSize)."
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	newArray at: 38 put: (self specialObjectsArray at: 38). "Is the prototype BlockContext (unused by the VM)"
	newArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self specialObjectsArray at: 41) ifNil: [LinkedList new]). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: []).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: []).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: []).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: []).
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
							#'object is pinned' #'primitive write beyond end of object').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	newArray at: 56 put: (self at: #WeakFinalizer ifAbsent: []).
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for foreign callback process"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: []).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray
	! !

!classDefinition: #SystemDictionary category: #'System-Support'!
IdentityDictionary subclass: #SystemDictionary
	instanceVariableNames: 'cachedClassNames cachedNonClassNames startUpScriptArguments startUpArguments'
	classVariableNames: 'EndianCache LastImageName LastQuitLogPosition LowSpaceProcess LowSpaceSemaphore ShutDownList SourceFileVersionString SpecialSelectors StartUpList StartupStamp WordSize'
	poolDictionaries: ''
	category: 'System-Support'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk recreateSpecialObjectsArray!

