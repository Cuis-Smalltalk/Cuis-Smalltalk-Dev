'From Cuis 4.2 of 25 July 2013 [latest update: #2936] on 17 September 2016 at 7:06:37 pm'!

!SortedCollection commentStamp: 'jmv 5/11/2016 09:52' prior: 0!
I represent a collection of objects ordered by some property of the objects themselves. The ordering is specified in a BlockClosure.!


!Exception commentStamp: 'jmv 9/17/2016 18:42:51' prior: 0!
This is the main class used to implement the exception handling system (EHS).  It plays two distinct roles:  that of the exception, and that of the exception handler.  More specifically, it implements the bulk of the protocols laid out in the ANSI specification - those protocol names are reflected in the message categories.

Exception is an abstract class.  Instances should neither be created nor trapped.  In most cases, subclasses should inherit from Error or Notification rather than directly from Exception.

In implementing this EHS, The Fourth Estate Inc. incorporated some ideas and code from Craig Latta's EHS.  His insights were crucial in allowing us to implement BlockClosure>>valueUninterruptably (and by extension, #ensure: and #ifCurtailed:), and we imported the following methods with little or no modification:

ContextPart>>terminateTo:
ContextPart>>terminate
MethodContext>>receiver:
MethodContext>>answer:

Thanks, Craig!!!


!DecompilerConstructor commentStamp: '<historical>' prior: 0!
I construct the node tree for a Decompiler.
Currently unused.!


!DecompilerConstructorForClosures commentStamp: 'jmv 9/17/2016 18:41:31' prior: 0!
Superclass is never used. Only this class is used.!


!Object class methodsFor: 'documentation' stamp: 'jmv 5/11/2016 09:51'!
howToModifyPrimitives
	"You are allowed to write methods which specify primitives, but please use 
	caution.  If you make a subclass of a class which contains a primitive method, 
	the subclass inherits the primitive.  The message which is implemented 
	primitively may be overridden in the subclass (E.g., see at:put: in String's 
	subclass Symbol).  The primitive behavior can be invoked using super (see 
	Symbol string:). 
	 
	A class which attempts to mimic the behavior of another class without being 
	its subclass may or may not be able to use the primitives of the original class.  
	In general, if the instance variables read or written by a primitive have the 
	same meanings and are in the same fields in both classes, the primitive will 
	work.  

	For certain frequently used 'special selectors', the compiler emits a 
	send-special-selector bytecode instead of a send-message bytecode.  
	Special selectors were created because they offer two advantages.  Code 
	which sends special selectors compiles into fewer bytes than normal.  For 
	some pairs of receiver classes and special selectors, the interpreter jumps 
	directly to a primitive routine without looking up the method in the class.  
	This is much faster than a normal message lookup. 
	 
	A selector which is a special selector solely in order to save space has a 
	normal behavior.  Methods whose selectors are special in order to 
	gain speed contain the comment, 'No Lookup'.  When the interpreter 
	encounters a send-special-selector bytecode, it checks the class of the 
	receiver and the selector.  If the class-selector pair is a no-lookup pair, 
	then the interpreter swiftly jumps to the routine which implements the 
	corresponding primitive.  (A special selector whose receiver is not of the 
	right class to make a no-lookup pair, is looked up normally).  The pairs are 
	listed below.  No-lookup methods contain a primitive number specification, 
	<primitive: xx>, which is redundant.  Since the method is not normally looked 
	up, deleting the primitive number specification cannot prevent this 
	primitive from running.  If a no-lookup primitive fails, the method is looked 
	up normally, and the expressions in it are executed. 
	 
	No Lookup pairs of (class, selector) 
	 
	SmallInteger with any of			+ - * /  \\  bitOr: bitShift: bitAnd:  // 
	SmallInteger with any of			=  ~=  >  <  >=  <= 
	Any class with						== 
	Any class with 						@ 
	Point with either of				x y 
	BlockClosure with either of 		value value:
	"

	self error: 'comment only'! !


!ClassBuilder methodsFor: 'private' stamp: 'pb 5/6/2016 14:48'!
tooDangerousClasses
	"Return a list of class names which will not be modified in the public interface"
	^#(
		"Object will break immediately"
		ProtoObject Object
		"Contexts and their superclasses"
		InstructionStream ContextPart MethodContext BlockClosure
		"Superclasses of basic collections"
		Collection SequenceableCollection ArrayedCollection
		"Collections known to the VM"
		Array Bitmap String Symbol ByteArray CompiledMethod
		"Basic Numbers"
		Magnitude Number SmallInteger Float
		"Misc other"
		LookupKey Association Link Point Rectangle Behavior PositionableStream UndefinedObject
	)
! !


!BlockStartLocator methodsFor: 'instruction decoding' stamp: 'jmv 5/11/2016 09:35'!
send: selector super: supered numArgs: numberArguments
	nextJumpIsAroundBlock := #closureCopy:copiedValues: == selector! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'jmv 5/11/2016 09:31'!
send: selector super: supered numArgs: numberArguments
	"Print the Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

	self print: (supered ifTrue: ['superSend: '] ifFalse: ['send: ']) , selector.
	indentSpanOfFollowingJump := #closureCopy:copiedValues: = selector! !


!ContextPart methodsFor: 'system simulation' stamp: 'jmv 9/17/2016 18:18:56'!
quickStep
	"If the next instruction is a send, just perform it.
	Otherwise, do a normal step."

	self willSend ifTrue: [QuickStep _ self].
	^self step! !

!ContextPart methodsFor: 'system simulation' stamp: 'jmv 9/17/2016 18:19:05'!
stepToSendOrReturn
	"Simulate the execution of bytecodes until either sending a message or 
	 returning a value to the receiver (that is, until switching contexts)."

	| ctxt |
	[self willSend or: [self willReturn or: [self willReallyStore]]] whileFalse:
		[ctxt := self step.
		 ctxt == self ifFalse:
			[self halt. 
			 "Caused by mustBeBoolean handling"
			 ^ctxt]]! !


!Decompiler methodsFor: 'control' stamp: 'jmv 5/11/2016 09:28'!
checkForBlock: receiver selector: selector arguments: arguments
	self assert: selector == #closureCopy:copiedValues:.
	^self checkForClosureCopy: receiver arguments: arguments! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/17/2016 18:38:31'!
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/17/2016 18:38:36'!
pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
	| copiedValues |

	copiedValues := ((1 to: numCopied) collect: [:ign| stack removeLast]) reversed.
	self doClosureCopyCopiedValues: copiedValues numArgs: numArgs blockSize: blockSize! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/17/2016 18:38:41'!
pushConsArrayWithElements: numElements 
	| array |
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: stack removeLast].
	stack addLast: (constructor codeBrace: array)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/17/2016 18:38:45'!
pushNewArrayOfSize: size

	stack addLast: #pushNewArray -> (Array new: size)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/17/2016 18:38:50'!
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

	stack addLast: ((tempVars at: tempVectorIndex + 1) remoteTemps at: remoteTempIndex + 1)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/11/2016 09:30'!
send: selector super: superFlag numArgs: numArgs

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	(#closureCopy:copiedValues: == selector
	  and: [self checkForBlock: rcvr selector: selector arguments: args]) ifFalse:
		[selNode := constructor codeAnySelector: selector.
		rcvr == #CascadeFlag
			ifTrue:
				["May actually be a cascade or an ifNil: for value."
				self willJumpIfFalse
					ifTrue: "= generated by a case macro"
						[selector == #= ifTrue:
							[" = signals a case statement..."
							statements addLast: args first.
							stack addLast: rcvr. "restore #CascadeFlag"
							^ self].
						selector == #== ifTrue:
							[" == signals an ifNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: #IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]]
					ifFalse:
						[(self willJumpIfTrue and: [selector == #==]) ifTrue:
							[" == signals an ifNotNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: #IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]].
				msgNode := constructor
								codeCascadedMessage: selNode
								arguments: args.
				stack last == #CascadeFlag ifFalse:
					["Last message of a cascade"
					statements addLast: msgNode.
					messages := self popTo: stack removeLast.  "Depth saved by first dup"
					msgNode := constructor
									codeCascade: stack removeLast
									messages: messages]]
			ifFalse:
				[msgNode := constructor
							codeMessage: rcvr
							selector: selNode
							arguments: args].
		stack addLast: msgNode]! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 9/17/2016 18:38:54'!
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex

	self pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex; doStore: stack! !

!Decompiler methodsFor: 'private' stamp: 'jmv 5/11/2016 09:27'!
constructorForMethod: aMethod
	^DecompilerConstructorForClosures new! !

!Decompiler methodsFor: 'public access' stamp: 'jmv 9/17/2016 18:37:39'!
decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new: method frameSize.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new: 20.
			numLocalTemps := 0.
			super method: method pc: method initialPC.
			"skip primitive error code store if necessary"
			(method primitive ~= 0 and: [self skipCallPrimitive; willStore]) ifTrue:
				[pc := pc + (method encoderClass bytecodeSize: self firstByte).
				 tempVars := tempVars asOrderedCollection].
			block := self blockTo: method endPC + 1.
			stack isEmpty ifFalse: [self error: 'stack not empty']].
	node := constructor
				codeMethod: aSelector
				block: block
				tempVars: tempVars
				primitive: method primitive
				class: aClass.
	method primitive > 0 ifTrue:
		[node removeAndRenameLastTempIfErrorCode].
	^node preen! !


!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 5/11/2016 09:27'!
methodNode
	"Return the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
	^(aClass parserClass new
		encoderClass: EncoderForV3PlusClosures;
		parse: source class: aClass)
			sourceText: source;
			yourself! !


!SortedCollection methodsFor: 'accessing' stamp: 'jmv 5/11/2016 09:52'!
sortBlock
	"Answer the BlockClosure which is the criterion for sorting elements of 
	the receiver."

	^sortBlock! !


!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 9/17/2016 18:04:29'!
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
							#blockCopyNOWUNUSED: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	"An array of the 255 Characters in ascii order.
	 Cog inlines table into machine code at: prim so do not regenerate it."
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character numericValue: ascii])."
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


!BlockNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/11/2016 09:22'!
emitCodeForValue: stack encoder: encoder

	^self emitCodeForClosureValue: stack encoder: encoder! !

!BlockNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/11/2016 09:22'!
sizeCodeForValue: encoder
	^self sizeCodeForClosureValue: encoder! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 9/17/2016 18:06:28'!
explainChar: string
	"Does string start with a special character?"

	| char |
	char _ string at: 1.
	char = $. ifTrue: [^'Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float).'].
	char = $' ifTrue: [^'The characters between two single quotes are made into an instance of class String'].
	char = $" ifTrue: [^'Double quotes enclose a comment.  Smalltalk ignores everything between double quotes.'].
	char = $# ifTrue: [^'The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants.'].
	(char = $( or: [char = $)]) ifTrue: [^'Expressions enclosed in parenthesis are evaluated first'].
	(char = $[ or: [char = $]]) ifTrue: [^'The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockClosure and is usually passed as an argument.'].
	(char = ${ or: [char = $}]) ifTrue: [^ 'A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array'].
	(char = $< or: [char = $>]) ifTrue: [^'<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails.'].
	char = $^ ifTrue: [^'Up arrow means return from this method.  The value returned is the expression following the ^'].
	char = $| ifTrue: [^'Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code.'].
	char = $_ ifTrue: [^'Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it.'].
	char = $; ifTrue: [^'Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon.'].
	char = $: ifTrue: [^'A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon).'].
	char = $$ ifTrue: [^'The single character following a dollar sign is made into an instance of class Character'].
	char = $- ifTrue: [^'A minus sign in front of a number means a negative number.'].
	char = $e ifTrue: [^'An e in the middle of a number means that the exponent follows.'].
	char = $r ifTrue: [^'An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base.'].
	char = Character space ifTrue: [^'the space Character'].
	char = Character tab ifTrue: [^'the tab Character'].
	char = Character cr ifTrue: [^'the carriage return Character'].
	char = Character lf ifTrue: [^'newline. The line feed Character'].
	^nil! !


!Utilities class methodsFor: 'closure support' stamp: 'jmv 5/11/2016 09:39'!
initializeClosures	"Utilities initializeClosures"
	"Remove unused class vars from CompiledMethod since we can't redefine its class definition directly. Add the new BlockClosure to the specialObjectsArray"
	(#(	BlockNodeCache MethodProperties SpecialConstants) 
			intersection: CompiledMethod classPool keys asSet) 
				do:[:classVarName| CompiledMethod removeClassVarName: classVarName].
	Smalltalk recreateSpecialObjectsArray.
	"Recompile methods in ContextPart, superclasses and subclasses that access inst vars"
	ContextPart withAllSuperclasses, ContextPart allSubclasses asArray do:[:class|
		class instSize > 0 ifTrue:[
			class allInstVarNames do:[:ivn|
				(class whichSelectorsAccess: ivn) do:[:sel| class recompile: sel]]]]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 5/11/2016 09:53'!
alarmSortBlock

	^[ :alarm1 :alarm2 | alarm1 scheduledTime < alarm2 scheduledTime ]! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 5/11/2016 09:27'!
forMethod: aMethod "<CompiledMethod>" methodNode: methodNode "<MethodNode>"
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^DebuggerMethodMapForClosureCompiledMethods new
		forMethod: aMethod
		methodNode: methodNode! !

!methodRemoval: Utilities class #postRecompileCleanup!
Utilities class removeSelector: #postRecompileCleanup!
!methodRemoval: DecompilerConstructorForClosures #isForClosures!
DecompilerConstructorForClosures removeSelector: #isForClosures!
!methodRemoval: DecompilerConstructor #isForClosures!
DecompilerConstructor removeSelector: #isForClosures!
!methodRemoval: BlockNode #generateAsClosure!
BlockNode removeSelector: #generateAsClosure!
!methodRemoval: EncoderForV3 class #isRealSendAt:in:!
EncoderForV3 class removeSelector: #isRealSendAt:in:!
!methodRemoval: BytecodeEncoder class #isRealSendAt:in:!
BytecodeEncoder class removeSelector: #isRealSendAt:in:!
!methodRemoval: CompiledMethod #hasInstVarRef!
CompiledMethod removeSelector: #hasInstVarRef!
!methodRemoval: CompiledMethod #isBlueBookCompiled!
CompiledMethod removeSelector: #isBlueBookCompiled!
!methodRemoval: CompiledMethod #isClosureCompiled!
CompiledMethod removeSelector: #isClosureCompiled!
!methodRemoval: Decompiler #checkForBlockCopy:!
Decompiler removeSelector: #checkForBlockCopy:!
!methodRemoval: Decompiler #sawBlueBookBlock!
Decompiler removeSelector: #sawBlueBookBlock!
!methodRemoval: Decompiler #sawClosureBytecode!
Decompiler removeSelector: #sawClosureBytecode!
!methodRemoval: MethodContext #cachesStack!
MethodContext removeSelector: #cachesStack!
!methodRemoval: MethodContext #hasInstVarRef!
MethodContext removeSelector: #hasInstVarRef!
!methodRemoval: MethodContext #hideFromDebugger!
MethodContext removeSelector: #hideFromDebugger!
!methodRemoval: ContextPart #blockCopy:!
ContextPart removeSelector: #blockCopy:!
!methodRemoval: ContextPart #cachesStack!
ContextPart removeSelector: #cachesStack!
!methodRemoval: ContextPart #copyTo:blocks:!
ContextPart removeSelector: #copyTo:blocks:!
!methodRemoval: InstructionStream #willReallySend!
InstructionStream removeSelector: #willReallySend!
!classRemoval: #BlockContext!
Smalltalk removeClassNamed: #BlockContext!
!classRemoval: #DebuggerMethodMapForBlueBookMethods!
Smalltalk removeClassNamed: #DebuggerMethodMapForBlueBookMethods!
!classRemoval: #InstVarRefLocator!
Smalltalk removeClassNamed: #InstVarRefLocator!
