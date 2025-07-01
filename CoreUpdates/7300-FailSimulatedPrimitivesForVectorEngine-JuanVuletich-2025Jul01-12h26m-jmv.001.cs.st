'From Cuis7.5 [latest update: #7299] on 1 July 2025 at 12:27:24 pm'!

!ContextPart methodsFor: 'private' stamp: 'jmv 7/1/2025 12:26:54'!
doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments
	"Simulate a primitive method whose index is primitiveIndex.  The simulated receiver and
	 arguments are given as arguments to this message. If successful, push result and return
	 resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes
	 execution needs to be intercepted and simulated to avoid execution running away."

	| value |
	"Test for unsimulatable side effects (that is, code that will be triggered in the image outside of the simulator range). This includes simulation guards, which are traditionally flagged using primitive 19 (a null primitive that doesn't do anything), as well as certain control primitives that might trigger code on other processes. If a side effect is detected, raise a warning to give the user/client a chance to cancel or virtualize the operation."
	"#(19 87) do: [:primitive | self systemNavigation browseAllSelect: [:m | m primitive = primitive]]"
	(primitiveIndex = 19 "simulationGuard" or: [primitiveIndex = 87 "primitiveResume"]) ifTrue:
		[[(SimulationSideEffectWarning forPrimitive: primitiveIndex)
			context: self method: meth receiver: receiver arguments: arguments;
			signalIfSkipped: [^ self]]
				ifCurtailed:
					[self push: receiver "Cheap fix of the context's internal state. Note that unwinding the receiver -- so that the next step would invoke the primitive again -- would be challenging due to to the variety of senders to this method."]].
	
	((primitiveIndex between: 201 and: 222)
	 and: [(self objectClass: receiver) includesBehavior: BlockClosure]) ifTrue:
		[(primitiveIndex = 206
		  or: [primitiveIndex = 208]) ifTrue:						"[Full]BlockClosure>>valueWithArguments:"
			[^receiver simulateValueWithArguments: arguments first caller: self].
		 ((primitiveIndex between: 201 and: 209)			 "[Full]BlockClosure>>value[:value:...]"
		  or: [primitiveIndex between: 221 and: 222]) ifTrue: "[Full]BlockClosure>>valueNoContextSwitch[:]"
			[^receiver simulateValueWithArguments: arguments caller: self]].

	primitiveIndex = 83 ifTrue: "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		[| selector |
		selector := arguments at: 1 ifAbsent:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: receiver with: arguments allButFirst].
	primitiveIndex = 84 ifTrue: "afr 9/11/1998 19:50 & eem 8/18/2009 17:04" "Object>>perform:withArguments:"
		[| selector args |
		arguments size = 2 ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		selector := arguments first.
		args := arguments second.
		args isArray ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: receiver with: args].
	primitiveIndex = 100 ifTrue: "eem 8/18/2009 16:57" "Object>>perform:withArguments:inSuperclass:"
		[| rcvr selector args superclass |
		arguments size
			caseOf: {
				[3] -> [
					rcvr := receiver.
					selector := arguments first.
					args := arguments second.
					superclass := arguments third].
				[4] -> ["mirror primitive"
					rcvr := arguments first.
					selector := arguments second.
					args := arguments third.
					superclass := arguments fourth] }
			otherwise: [^ self class primitiveFailTokenFor: #'bad number of arguments'].
		args isArray ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		((self objectClass: rcvr) includesBehavior: superclass) ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: rcvr with: args lookupIn: superclass].

	"Mutex>>primitiveEnterCriticalSectionOnBehalfOf:
	 Mutex>>primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf:"
	(primitiveIndex = 186 or: [primitiveIndex = 187]) ifTrue:
		[| effective |
		 effective := Processor activeProcess.
		 value := primitiveIndex = 186
					ifTrue: [receiver primitiveEnterCriticalSectionOnBehalfOf: effective]
					ifFalse: [receiver primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effective].
		 ^(self isPrimFailToken: value)
			ifTrue: [value]
			ifFalse: [self push: value]].
	
	(primitiveIndex = 188 or: [primitiveIndex = 189]) ifTrue:
		[| n args methodArg thisReceiver |
		 primitiveIndex caseOf:
			{[188 "primitiveExecuteMethodArgsArray"] ->
				["Object>>withArgs:executeMethod:
				  CompiledMethod class>>receiver:withArguments:executeMethod:
				  VMMirror>>ifFail:object:with:executeMethod: et al"
				 ((n := arguments size) between: 2 and: 4) ifFalse:
					[^self class primitiveFailTokenFor: #'unsupported operation'].
				 ((self objectClass: (args := arguments at: n - 1)) == Array
				  and: [(self objectClass: (methodArg := arguments at: n)) includesBehavior: CompiledMethod]) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 thisReceiver := arguments at: n - 2 ifAbsent: [receiver]].
			 [189 "primitiveExecuteMethod"] ->
				["Object>>executeMethod:
				  Object>>with:...executeMethod:"
				 (arguments size > 0) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 ((self objectClass: (methodArg := arguments atLast: 1)) includesBehavior: CompiledMethod) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 args := arguments allButLast.
				 thisReceiver := receiver]}.
		 methodArg numArgs = args size ifFalse:
			[^self class primitiveFailTokenFor: #'bad number of arguments'].
		 methodArg primitive > 0 ifTrue:
			[methodArg isQuick ifTrue:
				[^self push: (methodArg valueWithReceiver: thisReceiver arguments: args)].
			 ^self doPrimitive: methodArg primitive method: methodArg receiver: thisReceiver args: args].
		 ^self
			activateMethod: methodArg
			withArgs: args
			receiver: thisReceiver].

	primitiveIndex = 118 ifTrue: "[receiver:]tryPrimitive:withArgs:; avoid recursing in the VM"
		[(arguments size = 3
		  and: [(self objectClass: arguments second) == SmallInteger
		  and: [(self objectClass: arguments last) == Array]]) ifTrue:
			[^self doPrimitive: arguments second method: meth receiver: arguments first args: arguments last].
		 (arguments size = 2
		 and: [(self objectClass: arguments first) == SmallInteger
		 and: [(self objectClass: arguments last) == Array]]) ifFalse:
			[^self class primitiveFailTokenFor: -3].
		 ^self doPrimitive: arguments first method: meth receiver: receiver args: arguments last].

	value := primitiveIndex = 120 "FFI method"
				ifTrue: [(meth literalAt: 1) tryInvokeWithArguments: arguments]
				ifFalse:
					[primitiveIndex = 117 "named primitives"
						ifTrue: [
							"primDecl := meth literalAt: 1.
							moduleNameOrNil := primDecl at: 1.
							primitiveName := primDecl at: 2."
							(#(VectorEnginePlugin) statePointsTo: ((meth literalAt: 1) at: 1))
								ifTrue: [self class primitiveFailTokenFor: 1]
								ifFalse: [self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments].
							]
						ifFalse: "should use self receiver: receiver tryPrimitive: primitiveIndex withArgs: arguments but this is only in later VMs (and appears to be broken)"
							[receiver tryPrimitive: primitiveIndex withArgs: arguments]].

	^(self isPrimFailToken: value)
		ifTrue: [value]
		ifFalse: [self push: value]! !

