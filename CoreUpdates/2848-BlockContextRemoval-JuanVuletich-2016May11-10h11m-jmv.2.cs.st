'From Cuis 4.2 of 25 July 2013 [latest update: #2763] on 11 May 2016 at 10:20:04.726658 am'!

!SortedCollection commentStamp: 'jmv 5/11/2016 09:52' prior: 0!
I represent a collection of objects ordered by some property of the objects themselves. The ordering is specified in a BlockClosure.!


!DebuggerMethodMap commentStamp: 'jmv 5/11/2016 10:17' prior: 0!
I am a place-holder for information needed by the Debugger to inspect method activations.  I insulate the debugger from details of code generation such as exact bytecode offsets and temporary variable locations.  I have two concreate subclasses, one for methods compiled using BlueBook blocks and one for methods compiled using Closures.  These classes deal with temporary variable access. My function is to abstract the source map away from actual bytecode pcs to abstract bytecode pcs.

To reduce compilation time I try and defer as much computation to access time as possible as instances of me will be created after each compilation.

I maintain a WeakIdentityDictionary of method to DebuggerMethodMap to cache maps.  I refer to my method through a WeakArray to keep the map cache functional. If the reference from a DebuggerMethodMap to its method were strong then the method would never be dropped from the cache because the reference from its map would keep it alive.

MERGE into subclass and delete.!


!ContextPart commentStamp: '<historical>' prior: 0!
To the instruction parsing ability of InstructionStream I add the actual semantics for execution. The execution state is stored in the indexable fields of my subclasses. This includes temporary variables and a stack of values used in evaluating expressions. The actual semantics of execution can be found in my category "system simulation" and "instruction decode". These methods exactly parallel the operation of the Smalltalk machine itself.
	
The simulator is a group of my methods that do what the Smalltalk interpreter does: execute Smalltalk bytecodes. By adding code to the simulator, you may take statistics on the running of Smalltalk methods. For example,
	Transcript show: (ContextPart runSimulated: [3 factorial]) printString.
	
MERGE into subclass and remove...!


!DecompilerConstructor commentStamp: '<historical>' prior: 0!
I construct the node tree for a Decompiler.

MERGE into subclass and delete.!


!EncoderForV3 commentStamp: '<historical>' prior: 0!
I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set.  The intention is for another subclass to restrict the range of bytecodes used to long forms only, allowing the bytecode set to be redefined by avoiding using the many short forms.  The short forms may then be reassigned.

MERGE into subclass and delete.!


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


!CompiledMethod methodsFor: 'literals' stamp: 'jmv 5/11/2016 09:28'!
headerDescription
	"Answer a description containing the information about the form of the 
	receiver and the form of the context needed to run the receiver."

	| s |
	s _ String new writeStream.
	self header printOn: s.
	s newLine; nextPutAll: '"primitive: '.
	self primitive printOn: s.
	s newLine; nextPutAll: ' numArgs: '.
	self numArgs printOn: s.
	s newLine; nextPutAll: ' numTemps: '.
	self numTemps printOn: s.
	s newLine; nextPutAll: ' numLiterals: '.
	self numLiterals printOn: s.
	s newLine; nextPutAll: ' frameSize: '.
	self frameSize printOn: s.
	s newLine; nextPutAll: ' isClosureCompiled: '.
	true printOn: s.
	s nextPut: $"; newLine.
	^ s contents! !

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


!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 5/11/2016 09:52'!
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
	newArray := Array new: 56.
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
	newArray at: 10 put: Float.
	newArray at: 11 put: MethodContext.
	newArray at: 12 put: nil.
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: (self specialObjectsArray at: 18).
	"(low space Semaphore)"
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
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character value: ascii])."
	newArray at: 25 put: (self specialObjectsArray at: 25).
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	"An array of up to 31 classes whose instances will have compact headers"
	newArray at: 29 put: self compactClassesArray.
	newArray at: 30 put: (self specialObjectsArray at: 30).
	"(delay Semaphore)"
	newArray at: 31 put: (self specialObjectsArray at: 31).
	"(user interrupt Semaphore)"
	"Prototype instances that can be copied for fast initialization"
	newArray at: 32 put: Float new.
	newArray at: 33 put: (LargePositiveInteger new: 4).
	newArray at: 34 put: Point new.
	newArray at: 35 put: #cannotInterpret:.
	"Note: This must be fixed once we start using context prototypes (yeah, right)"
	"(MethodContext new: CompiledMethod fullFrameSize)."
	newArray at: 36 put: (self specialObjectsArray at: 36). "Is the prototype MethodContext (unused by the VM)"
	newArray at: 37 put: BlockClosure.
	newArray at: 38 put: nil.
	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: nil. "Reserved for a LinkedList instance for overlapped calls in CogMT"
	"finalization Semaphore"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]).
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
	"newArray at: 51 put: #attemptToAssign:withIndex:."
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallback:stack:registers:jmpbuf:.
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	newArray at: 56 put: (self at: #WeakFinalizer ifAbsent: []).

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray
	! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 5/11/2016 09:27'!
forMethod: aMethod "<CompiledMethod>" methodNode: methodNode "<MethodNode>"
	"Uncached instance creation method for private use or for tests.
	 Please consider using forMethod: instead."
	^DebuggerMethodMapForClosureCompiledMethods new
		forMethod: aMethod
		methodNode: methodNode! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'pb 5/6/2016 14:55'!
explainChar: string
	"Does string start with a special character?"

	| char |
	char _ string at: 1.
	char = $. ifTrue: [^'"Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float)."'].
	char = $' ifTrue: [^'"The characters between two single quotes are made into an instance of class String"'].
	char = $" ifTrue: [^'"Double quotes enclose a comment.  Smalltalk ignores everything between double quotes."'].
	char = $# ifTrue: [^'"The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants."'].
	(char = $( or: [char = $)]) ifTrue: [^'"Expressions enclosed in parenthesis are evaluated first"'].
	(char = $[ or: [char = $]]) ifTrue: [^'"The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockClosure and is usually passed as an argument."'].
	(char = ${ or: [char = $}]) ifTrue: [^ '"A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array"'].
	(char = $< or: [char = $>]) ifTrue: [^'"<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails."'].
	char = $^ ifTrue: [^'"Uparrow means return from this method.  The value returned is the expression following the ^"'].
	char = $| ifTrue: [^'"Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code."'].
	char = $_ ifTrue: [^'"Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it."'].
	char = $; ifTrue: [^'"Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon."'].
	char = $: ifTrue: [^'"A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon)."'].
	char = $$ ifTrue: [^'"The single character following a dollar sign is made into an instance of class Character"'].
	char = $- ifTrue: [^'"A minus sign in front of a number means a negative number."'].
	char = $e ifTrue: [^'"An e in the middle of a number means that the exponent follows."'].
	char = $r ifTrue: [^'"An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base."'].
	char = Character space ifTrue: [^'"the space Character"'].
	char = Character tab ifTrue: [^'"the tab Character"'].
	char = Character cr ifTrue: [^'"the carriage return Character"'].
	char = Character lf ifTrue: [^'"the line feed Character"'].
	^nil! !


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


!InstructionStream methodsFor: 'testing' stamp: 'jmv 5/11/2016 09:35'!
willReallySend
	"Answer whether the next bytecode is a real message-send"

	| byte |
	byte := self method at: pc.
	^byte >= 131
	  and: [byte ~= 200
	  and: [byte >= 176   "special send or short send"
		or: [byte <= 134 "long sends"	
			and: [
				"long form support demands we check the selector"
				byte = 132 ifTrue: [
					(self method at: pc + 1) // 32 > 1 ifTrue: [^false]].
				true]]]]! !


!ContextPart methodsFor: 'private' stamp: 'jmv 5/11/2016 09:50'!
copyTo: aContext
	"Copy self and my sender chain down to, but not including, aContext.  End of copied chain will have nil sender."

	| copy |
	self == aContext ifTrue: [^ nil].
	copy _ self copy.
	self sender ifNotNil: [
		copy privSender: (self sender copyTo: aContext)].
	^ copy! !

!ContextPart methodsFor: 'private' stamp: 'jmv 5/11/2016 09:41'!
doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments 
	"Simulate a primitive method whose index is primitiveIndex.  The
	 simulated receiver and arguments are given as arguments to this message.
	 Any primitive which provikes execution needs to be intercepted and simulated
	 to avoid execution running away."

	| value |
	<primitive: 19> "Simulation guard"
	"If successful, push result and return resuming context,
		else ^ PrimitiveFailToken"
	(primitiveIndex = 19) ifTrue:[
		Debugger 
			openContext: self
			label:'Code simulation error'
			contents: nil].

	primitiveIndex = 83 "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		ifTrue: [^self send: arguments first to: receiver
					with: arguments allButFirst
					super: false].
	primitiveIndex = 84 "afr 9/11/1998 19:50" "Object>>perform:withArguments:"
		ifTrue: [^self send: arguments first to: receiver
					with: (arguments at: 2)
					super: false].
	primitiveIndex = 188 ifTrue: [
		arguments size = 2 ifTrue: [ "eem 5/27/2008 11:10 Object>>withArgs:executeMethod:"
			^MethodContext
				sender: self
				receiver: receiver
				method: (arguments at: 2)
				arguments: (arguments at: 1) ].
		arguments size = 3 ifTrue: [ "CompiledMethod class >> #receiver:withArguments:executeMethod:"
			^MethodContext
				sender: self
				receiver: (arguments at: 1)
				method: (arguments at: 3)
				arguments: (arguments at: 2) ] ].
	primitiveIndex = 189 ifTrue: [ "Object >> (#with:)*executeMethod"
		^MethodContext
			sender: self
			receiver: receiver
			method: arguments last
			arguments: arguments allButLast ].

	"Closure primitives"
	(primitiveIndex = 200 and: [receiver == self]) ifTrue:
		"ContextPart>>closureCopy:copiedValues:; simulated to get startpc right"
		[^self push: (BlockClosure
						outerContext: receiver
						startpc: pc + 2
						numArgs: arguments first
						copiedValues: arguments last)].
	((primitiveIndex between: 201 and: 205)			 "BlockClosure>>value[:value:...]"
	or: [primitiveIndex between: 221 and: 222]) ifTrue: "BlockClosure>>valueNoContextSwitch[:]"
		[^receiver simulateValueWithArguments: arguments caller: self].
	primitiveIndex = 206 ifTrue:						"BlockClosure>>valueWithArguments:"
		[^receiver simulateValueWithArguments: arguments first caller: self].

	primitiveIndex = 120 ifTrue:[ "FFI method"
		value := meth literals first tryInvokeWithArguments: arguments.
	] ifFalse:[
		arguments size > 6 ifTrue: [^ContextPart primitiveFailToken].
		value := primitiveIndex = 117 "named primitives"
				ifTrue:[self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments]
				ifFalse:[receiver tryPrimitive: primitiveIndex withArgs: arguments].
	].
	^value == ContextPart primitiveFailToken
		ifTrue: [ContextPart primitiveFailToken]
		ifFalse: [self push: value]! !


!Decompiler methodsFor: 'control' stamp: 'jmv 5/11/2016 09:28'!
checkForBlock: receiver selector: selector arguments: arguments
	self assert: selector == #closureCopy:copiedValues:.
	^self checkForClosureCopy: receiver arguments: arguments! !

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

!Decompiler methodsFor: 'private' stamp: 'jmv 5/11/2016 09:27'!
constructorForMethod: aMethod
	^DecompilerConstructorForClosures new! !


!BlockNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/11/2016 09:22'!
emitCodeForValue: stack encoder: encoder

	^self emitCodeForClosureValue: stack encoder: encoder! !

!BlockNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/11/2016 09:22'!
sizeCodeForValue: encoder
	^self sizeCodeForClosureValue: encoder! !


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

!Utilities class methodsFor: 'closure support' stamp: 'pb 5/6/2016 14:50'!
postRecompileCleanup	"Utilities postRecompileCleanup"
	"Cleanup after loading closure bootstrap"
	"Before doing this, please start a new UI process (for example, by hitting alt-period and closing the debugger)."
	| unboundMethods |
	self runningWorld ifNotNil: [ :w | w removeAllKnownFailing ].
	ProcessorScheduler startUp.
	WeakArray restartFinalizationProcess.
	MethodChangeRecord allInstancesDo:[:x| x noteNewMethod: nil].
	Smalltalk cleanOutUndeclared.
	Delay startTimerEventLoop.
	EventSensor install.
	Workspace allInstancesDo:[:ws| ws initializeBindings].
	Smalltalk garbageCollect.
	Smalltalk
		at: #DebuggerMethodMap
		ifPresent: [ :dmm | dmm voidMapCache ].
	Smalltalk garbageCollect.
	unboundMethods _ CompiledMethod unboundMethods.
	unboundMethods notEmpty ifTrue: [
		unboundMethods inspectWithLabel: 'Unbound Methods'].
	unboundMethods isEmpty ifTrue:[
		self inform:'Congratulations - The bootstrap is now complete.'.
	]! !


!WorldState methodsFor: 'alarms' stamp: 'jmv 5/11/2016 09:53'!
alarmSortBlock

	^[ :alarm1 :alarm2 | alarm1 scheduledTime < alarm2 scheduledTime ]! !

!methodRemoval: BlockNode #generateAsClosure!
BlockNode removeSelector: #generateAsClosure!
!methodRemoval: Decompiler #checkForBlockCopy:!
Decompiler removeSelector: #checkForBlockCopy:!
!methodRemoval: Decompiler #sawBlueBookBlock!
Decompiler removeSelector: #sawBlueBookBlock!
!methodRemoval: MethodContext #cachesStack!
MethodContext removeSelector: #cachesStack!
!methodRemoval: MethodContext #hideFromDebugger!
MethodContext removeSelector: #hideFromDebugger!
!methodRemoval: ContextPart #blockCopy:!
ContextPart removeSelector: #blockCopy:!
!methodRemoval: ContextPart #copyTo:blocks:!
ContextPart removeSelector: #copyTo:blocks:!
!methodRemoval: CompiledMethod #isBlueBookCompiled!
CompiledMethod removeSelector: #isBlueBookCompiled!
!methodRemoval: CompiledMethod #isClosureCompiled!
CompiledMethod removeSelector: #isClosureCompiled!

!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables home method methodNode methodReturnContext receiver size tempAt: tempAt:put:)
('instruction decoding' doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' contextStack depthBelow: errorReportOn: longStack methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver:class: closureCopy:copiedValues: hasSender: jump pop push: quickSend:to:with:super: restart resume resume: resumeEvaluating: return return: return:to: runUntilErrorOrReturnFrom: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: copyTo: cut: doPrimitive:method:receiver:args: insertSender: privSender: push:fromIndexable: stackPtr stackp: tryNamedPrimitiveIn:for:withArgs: tryPrimitiveFor:receiver:args:)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('private-debugger' cachesStack)
('query' bottomContext copyStack findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isClosureContext isContext isDead secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
!

!classRemoval: #BlockContext!
Smalltalk removeClassNamed: #BlockContext!
!classRemoval: #DebuggerMethodMapForBlueBookMethods!
Smalltalk removeClassNamed: #DebuggerMethodMapForBlueBookMethods!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Smalltalk recreateSpecialObjectsArray!

