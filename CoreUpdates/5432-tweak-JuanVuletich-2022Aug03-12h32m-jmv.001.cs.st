'From Cuis 6.0 [latest update: #5431] on 3 August 2022 at 12:32:34 pm'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 12:32:03'!
openContext: aContext label: label preDebugMessage: contentsStringOrNil
	"Open a notifier (a PreDebugWindow) in response to an error, halt, or notify.
	A PreDebugWindow just shows a short view of the sender stack and provides a menu that lets the user open a full debugger.
	The context argument is in the active process stack."

	| debugger |
	<primitive: 19> "Simulation guard"

	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk logError: label inContext: aContext to: 'CuisDebug'].
	self informExistingDebugger: aContext label: label.

	debugger _ Debugger new.
	debugger 		process: Processor activeProcess context: aContext.
	debugger openWindowLabel: label usePreDebugWindow: true preDebugMessage: contentsStringOrNil.! !


!ContextPart methodsFor: 'private' stamp: 'jmv 8/3/2022 12:32:07'!
doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments
	"Simulate a primitive method whose index is primitiveIndex.  The simulated receiver and
	 arguments are given as arguments to this message. If successful, push result and return
	 resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes
	 execution needs to be intercepted and simulated to avoid execution running away."

	| value |
	"Judicious use of primitive 19 (a null primitive that doesn't do anything) prevents
	 the debugger from entering various run-away activities such as spawning a new
	 process, etc.  Injudicious use results in the debugger not being able to debug
	 interesting code, such as the debugger itself.  hence use primitive 19 with care :-)"
	"Smalltalk browseAllSelect: [:m| m primitive = 19]"
	primitiveIndex = 19 ifTrue: [
		Debugger
			openContext: self
			label:'Code simulation error'
			preDebugMessage: nil].

	((primitiveIndex between: 201 and: 222)
	 and: [(self objectClass: receiver) includesBehavior: BlockClosure]) ifTrue:
		[((primitiveIndex between: 201 and: 205)			 "BlockClosure>>value[:value:...]"
		  or: [primitiveIndex between: 221 and: 222]) ifTrue: "BlockClosure>>valueNoContextSwitch[:]"
			[^receiver simulateValueWithArguments: arguments caller: self].
		 primitiveIndex = 206 ifTrue:						"BlockClosure>>valueWithArguments:"
			[^receiver simulateValueWithArguments: arguments first caller: self]].

	primitiveIndex = 83 ifTrue: "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		[^self send: arguments first to: receiver with: arguments allButFirst super: false].
	primitiveIndex = 84 ifTrue: "afr 9/11/1998 19:50 & eem 8/18/2009 17:04" "Object>>perform:withArguments:"
		[^self send: arguments first to: receiver with: (arguments at: 2) lookupIn: (self objectClass: receiver)].
	primitiveIndex = 100 ifTrue: "eem 8/18/2009 16:57" "Object>>perform:withArguments:inSuperclass:"
		[^self send: arguments first to: receiver with: (arguments at: 2) lookupIn: (arguments at: 3)].

	"Mutex>>primitiveEnterCriticalSection
	 Mutex>>primitiveTestAndSetOwnershipOfCriticalSection"
	(primitiveIndex = 186 or: [primitiveIndex = 187]) ifTrue:
		[| effective |
		 effective := Processor activeProcess effectiveProcess.
		 "active == effective"
		 value := primitiveIndex = 186
					ifTrue: [receiver primitiveEnterCriticalSectionOnBehalfOf: effective]
					ifFalse: [receiver primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effective].
		 ^(self isPrimFailToken: value)
			ifTrue: [value]
			ifFalse: [self push: value]].

	primitiveIndex = 188 ifTrue: "eem 5/27/2008 11:10 Object>>withArgs:executeMethod:"
		[((self objectClass: (arguments at: 1)) == Array
		  and: [(self objectClass: (arguments at: 2)) includesBehavior: CompiledMethod]) ifFalse:
			[^ContextPart primitiveFailTokenFor: #'bad argument'].
		 (arguments at: 2) numArgs = (arguments at: 1) size ifFalse:
			[^ContextPart primitiveFailTokenFor: #'bad number of arguments'].
		 (arguments at: 2) primitive > 0 ifTrue:
			[(arguments at: 2) isQuick ifTrue:
				[^self push: (receiver withArgs: (arguments at: 1) executeMethod: (arguments at: 2))].
			 ^self doPrimitive: (arguments at: 2) primitive method: (arguments at: 2) receiver: receiver args: (arguments at: 1)].
		 ^MethodContext
			sender: self
			receiver: receiver
			method: (arguments at: 2)
			arguments: (arguments at: 1)].

	"Closure primitives"
	(primitiveIndex = 200 and: [self == receiver]) ifTrue:
		"ContextPart>>closureCopy:copiedValues:; simulated to get startpc right"
		[^self push: (BlockClosure
						outerContext: receiver
						startpc: pc + 2
						numArgs: arguments first
						copiedValues: arguments last)].

	primitiveIndex = 118 ifTrue: "[receiver:]tryPrimitive:withArgs:; avoid recursing in the VM"
		[(arguments size = 3
		  and: [(self objectClass: arguments second) == SmallInteger
		  and: [(self objectClass: arguments last) == Array]]) ifTrue:
			[^self doPrimitive: arguments second method: meth receiver: arguments first args: arguments last].
		 (arguments size = 2
		 and: [(self objectClass: arguments first) == SmallInteger
		 and: [(self objectClass: arguments last) == Array]]) ifFalse:
			[^ContextPart primitiveFailTokenFor: nil].
		 ^self doPrimitive: arguments first method: meth receiver: receiver args: arguments last].

	value := primitiveIndex = 120 "FFI method"
				ifTrue: [(meth literalAt: 1) tryInvokeWithArguments: arguments]
				ifFalse:
					[primitiveIndex = 117 "named primitives"
						ifTrue: [self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments]
						ifFalse:
							["should use self receiver: receiver tryPrimitive: primitiveIndex withArgs: arguments but this is only in later VMs"
							receiver tryPrimitive: primitiveIndex withArgs: arguments]].

	^(self isPrimFailToken: value)
		ifTrue: [value]
		ifFalse: [self push: value]! !


!MethodContext methodsFor: 'private' stamp: 'jmv 8/3/2022 12:32:11'!
cannotReturn: result
	closureOrNil ifNotNil: [
		^self cannotReturn: result to: sender].
	Debugger
		openContext: thisContext
		label: 'computation has been terminated'
		preDebugMessage: nil! !


!Warning methodsFor: 'exceptionDescription' stamp: 'jmv 8/3/2022 12:32:15'!
defaultAction
	"The user should be notified of the occurrence of an exceptional occurrence and given an option of continuing or aborting the computation. The description of the occurrence should include any text specified as the argument of the #signal: message."

	Debugger
		openContext: thisContext
		label: 'Warning'
		preDebugMessage: self messageText.
	self resume! !

!methodRemoval: Debugger class #openContext:label:contents: stamp: 'jmv 8/3/2022 12:32:21'!
Debugger class removeSelector: #openContext:label:contents:!
