'From Cuis 4.2 of 25 July 2013 [latest update: #2880] on 11 August 2016 at 7:14:09 pm'!

!ContextPart methodsFor: 'controlling' stamp: 'eem 8/11/2014 13:43'!
send: selector to: rcvr with: arguments lookupIn: lookupClass
	"Simulate the action of sending a message with selector and arguments
	 to rcvr. The argument, lookupClass, is the class in which to lookup the
	 message.  This is the receiver's class for normal messages, but for super
	 messages it will be some specific class related to the source method."

	| meth primIndex val ctxt |
	(meth := lookupClass lookupSelector: selector) ifNil:
		[^self send: #doesNotUnderstand:
				to: rcvr
				with: {Message selector: selector arguments: arguments}
				lookupIn: lookupClass].
	(primIndex := meth primitive) > 0 ifTrue:
		[val := self doPrimitive: primIndex method: meth receiver: rcvr args: arguments.
		 (self isPrimFailToken: val) ifFalse:
			[^val]].
	(selector == #doesNotUnderstand: and: [lookupClass == ProtoObject]) ifTrue:
		[^self error: 'Simulated message ', arguments first selector, ' not understood'].
	ctxt := MethodContext sender: self receiver: rcvr method: meth arguments: arguments.
	primIndex > 0 ifTrue:
		[ctxt failPrimitiveWith: val].
	^ctxt! !


!ContextPart class methodsFor: 'simulation' stamp: 'jmv 8/11/2016 19:13:40'!
primitiveFailTokenFor: errorCode

	"A unique object to be returned when a primitive fails during simulation"
	PrimitiveFailToken ifNil: [ PrimitiveFailToken _ Object new ].
	^{PrimitiveFailToken. errorCode}! !


!ProtoObject methodsFor: 'apply primitives' stamp: 'jmv 8/11/2016 19:12:55'!
tryPrimitive: primIndex withArgs: argumentArray
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives. See Object documentation whatIsAPrimitive."

	<primitive: 118 error: errorCode>
	^ContextPart primitiveFailTokenFor: errorCode! !


!BlockClosure methodsFor: 'system simulation' stamp: 'eem 8/11/2014 13:41'!
simulateValueWithArguments: anArray caller: aContext
	"Simulate the valueWithArguments: primitive. Fail if anArray is not an array of the right arity."
	| newContext sz |
	newContext := (MethodContext newForMethod: outerContext method)
						setSender: aContext
						receiver: outerContext receiver
						method: outerContext method
						closure: self
						startpc: startpc.
	((newContext objectClass: anArray) ~~ Array
	 or: [numArgs ~= anArray size]) ifTrue:
		[^ContextPart primitiveFailTokenFor: nil].
	sz := self basicSize.
	newContext stackp: sz + numArgs.
	1 to: numArgs do:
		[:i| newContext at: i put: (anArray at: i)].
	1 to: sz do:
		[:i| newContext at: i + numArgs put: (self at: i)].
	^newContext! !


!ContextPart methodsFor: 'controlling' stamp: 'eem 8/11/2014 13:44'!
send: selector to: rcvr with: arguments super: superFlag 
	"Simulate the action of sending a message with selector arguments
	 to rcvr. The argument, superFlag, tells whether the receiver of the
	 message was specified with 'super' in the source method."

	^self send: selector
		to: rcvr
		with: arguments
		lookupIn: (superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: rcvr])! !

!ContextPart methodsFor: 'private' stamp: 'eem 8/11/2014 12:28'!
activateReturn: aContext value: value
	"Activate 'aContext return: value' in place of self, so execution will return to aContext's sender"

	^MethodContext 
		sender: self
		receiver: aContext
		method: MethodContext theReturnMethod
		arguments: {value}! !

!ContextPart methodsFor: 'private' stamp: 'jmv 7/29/2016 16:14:39'!
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
	"SystemNavigation new browseAllSelect: [:m| m primitive = 19]"
	primitiveIndex = 19 ifTrue:
		[Debugger
			openContext: self
			label:'Code simulation error'
			contents: nil].

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

!ContextPart methodsFor: 'private' stamp: 'eem 8/11/2014 13:05'!
tryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments
	"Invoke the named primitive for aCompiledMethod, answering its result, or,
	 if the primiitve fails, answering the error code."
	<primitive: 218 error: ec>
	ec ifNotNil:
		["If ec is an integer other than -1 there was a problem with primitive 218,
		  not with the external primitive itself.  -1 indicates a generic failure (where
		  ec should be nil) but ec = nil means primitive 218 is not implemented.  So
		  interpret -1 to mean the external primitive failed with a nil error code."
		 ec isInteger ifTrue:
			[ec = -1
				ifTrue: [ec := nil]
				ifFalse: [self primitiveFailed]]].
	^self class primitiveFailTokenFor: ec! !

!methodRemoval: ContextPart class #primitiveFailToken!
ContextPart class removeSelector: #primitiveFailToken!
!methodRemoval: ContextPart #activateMethod:withArgs:receiver:class:!
ContextPart removeSelector: #activateMethod:withArgs:receiver:class:!
!methodRemoval: ContextPart #tryPrimitiveFor:receiver:args:!
ContextPart removeSelector: #tryPrimitiveFor:receiver:args:!
!methodRemoval: ProtoObject #tryNamedPrimitive!
ProtoObject removeSelector: #tryNamedPrimitive!
!methodRemoval: ProtoObject #tryNamedPrimitive:!
ProtoObject removeSelector: #tryNamedPrimitive:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:with:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:with:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:with:with:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:with:with:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:with:with:with:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:with:with:with:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:with:with:with:with:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:with:with:with:with:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:with:with:with:with:with:!
!methodRemoval: ProtoObject #tryNamedPrimitive:with:with:with:with:with:with:with:!
ProtoObject removeSelector: #tryNamedPrimitive:with:with:with:with:with:with:with:!

!BlockClosure reorganize!
('accessing' argumentCount copiedValueAt: home isBlock method numArgs numCopiedValues outerContext receiver size startpc)
('controlling' repeat repeatWithGCIf: whileFalse whileFalse: whileNil: whileNotNil: whileTrue whileTrue:)
('evaluating' bench cull:cull: durationToRun ifError: timeToRun timeToRunWithoutGC value value: value:value: value:value:value: value:value:value:value: valueAt: valueNoContextSwitch valueNoContextSwitch: valueWithArguments: valueWithExit valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and: valueWithin:onTimeout:)
('exceptions' assert ensure: ifCurtailed: on:do: on:do:on:do: on:do:on:do:on:do: onDNU:do: valueUninterruptably)
('printing' decompile printOn: printSourceOn: printStack: sourceString)
('scheduling' asContext fork forkAndWait forkAt: forkAt:named: forkNamed: newProcess newProcessWith:)
('private' asContextWithSender: copyForSaving grabProcessor grabProcessorFor:onTimeout: grabProcessorOnlyFor: reentrant valueUnpreemptively)
('testing' hasMethodReturn isClosure isDead isTrivialClosure)
('error handing' numArgsError:)
('events-support' asMinimalRepresentation isReceiverOrAnyArgumentGarbage)
('debugger access' sender)
('objects from disk' objectForDataStream:)
('services' timeProfile)
('initialization' outerContext:startpc:numArgs:copiedValues:)
('displaying' displayProgressAt:from:to:during:)
('system simulation' simulateValueWithArguments:caller:)
!

