'From Cuis 5.0 [latest update: #4781] on 20 August 2021 at 10:51:59 pm'!

!ClassDescription methodsFor: 'initialization' stamp: 'jmv 8/20/2021 22:47:36'!
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	 the receiver, which is a newly changed class. Permute variables as necessary,
	 and forward old instances to new instances.. Answer nil to defeat any clients
	 that expected the old behaviour of answering the array of old instances."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| allInstances newMethod oldMethod selector |
	allInstances _ oldClass allInstances.
	allInstances notEmpty ifTrue: [
		Processor 
			processesDo: [ :p | ]
			withStackFramestDo: [ :process :context |
				(context receiver isKindOf: oldClass) ifTrue: [
					selector _ context method selector.
					oldMethod _ oldClass lookupSelector: selector.
					newMethod _ self lookupSelector: selector.
					oldMethod = newMethod ifFalse: [
						MethodInCallStackToBecomeInvalid
							signal: self class name, ' has some instance running #', selector, ' that would become invalid.' ]]]
			runningProcessSearchStart: nil.
		self updateInstances: allInstances asArray from: oldClass isMeta: self isMeta ].
	^nil

"This attempts to crash the VM by stepping off the end of an instance.
 As the doctor says, do not do this."
"	| crashingBlock class |
	class := Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar := value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock := (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock value"! !

