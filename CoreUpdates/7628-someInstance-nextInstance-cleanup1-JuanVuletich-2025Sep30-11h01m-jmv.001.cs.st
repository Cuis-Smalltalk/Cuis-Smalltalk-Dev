'From Cuis7.5 [latest update: #7625] on 30 September 2025 at 11:06:11 am'!

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 9/30/2025 11:04:27'!
allInstances
	"Answer all instances of the receiver."
	<primitive: 177>
	self primitiveFailed! !

!Behavior methodsFor: 'enumerating' stamp: 'jmv 9/30/2025 11:05:31'!
allInstancesDo: aBlock
	"Evaluate aBlock with each of the current instances of the receiver."
	self allInstances do: aBlock! !


!MethodContext class methodsFor: 'private' stamp: 'jmv 9/30/2025 11:05:27'!
allInstancesDo: aBlock
	"Evaluate aBlock with each of the current instances of the receiver."
	self allInstances do: aBlock! !

!MethodContext class methodsFor: 'enumerating' stamp: 'jmv 9/30/2025 11:04:34'!
allInstances
	"Answer all instances of the receiver."
	<primitive: 177>
	self primitiveFailed! !

