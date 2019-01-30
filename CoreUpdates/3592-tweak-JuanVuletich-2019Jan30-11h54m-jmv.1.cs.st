'From Cuis 5.0 of 7 November 2016 [latest update: #3591] on 30 January 2019 at 11:54:45 am'!

!ClassDescription methodsFor: 'initialization' stamp: 'jmv 1/30/2019 11:54:25'!
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as instances of 
	 the receiver, which is a newly changed class. Permute variables as necessary,
	 and forward old instances to new instances.. Answer nil to defeat any clients
	 that expected the old behaviour of answering the array of old instances."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| newMethod oldMethod selector |
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				selector _ context method selector.
				oldMethod _ oldClass lookupSelector: selector.
				newMethod _ self lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					MethodInCallStackToBecomeInvalid
						signal: self class name, ' has some instance running #', selector, ' that would become invalid.'.
					^nil ]]]
		runningProcessSearchStart: nil.
	self updateInstances: oldClass allInstances asArray from: oldClass isMeta: self isMeta.
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

