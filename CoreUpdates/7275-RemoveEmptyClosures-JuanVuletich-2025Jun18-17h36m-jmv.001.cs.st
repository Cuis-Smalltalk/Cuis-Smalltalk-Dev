'From Cuis7.5 [latest update: #7272] on 18 June 2025 at 5:40:40 pm'!

!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 6/17/2025 18:53:13'!
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
							#'object is pinned' #'primitive write beyond end of object').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: nil).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: nil).

	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for foreign callback process"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: nil).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray.! !

!SystemDictionary methodsFor: 'query' stamp: 'jmv 6/17/2025 18:52:17'!
methodsWithUnboundGlobals
	"Get all methods that use undeclared global objects that are not listed in Undeclared. For a clean image the result should be empty."
	"
	self assert: Smalltalk methodsWithUnboundGlobals isEmpty
	"
	^ self allSelect: [ :m |
		m literals anySatisfy: [ :l |
			l isVariableBinding and: [
				l key isSymbol and: [
					"avoid class-side methodClass literals"
					(m methodClass bindingOf: l key)
						ifNil: [
							(Undeclared
								associationAt: l key
								ifAbsent: nil) ~~ l ]
						ifNotNil: [ :b |
							b ~~ l ]]]]]! !

