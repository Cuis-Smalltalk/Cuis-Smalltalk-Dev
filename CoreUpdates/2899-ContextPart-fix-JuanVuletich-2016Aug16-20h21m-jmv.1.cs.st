'From Cuis 4.2 of 25 July 2013 [latest update: #2895] on 16 August 2016 at 8:32:16 pm'!
!classDefinition: #ContextPart category: #'Kernel-Methods'!
InstructionStream subclass: #ContextPart
	instanceVariableNames: 'stackp '
	classVariableNames: 'PrimitiveFailToken QuickStep ValueIndex '
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!ContextPart class methodsFor: 'cached class state' stamp: 'jmv 8/16/2016 20:31:22'!
primitiveFailToken
	"A unique object to be returned when a primitive fails during simulation"
	PrimitiveFailToken ifNil: [ PrimitiveFailToken _ Object new ].
	^ PrimitiveFailToken! !

!ContextPart class methodsFor: 'cached class state' stamp: 'jmv 8/16/2016 20:30:19'!
valueIndex
	ValueIndex ifNil: [
		ValueIndex _ 2.
		self assert: (Association instVarNameForIndex: ValueIndex) = 'value' ].
	^ ValueIndex! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'jmv 8/16/2016 20:30:40'!
popIntoLiteralVariable: value 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into a literal variable of my method."

	self object: value instVarAt: self class valueIndex put: self pop! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'jmv 8/16/2016 20:30:43'!
pushLiteralVariable: value 
	"Simulate the action of bytecode that pushes the contents of the literal 
	variable whose index is the argument, index, on the top of the stack."

	self push: (self object: value instVarAt: self class valueIndex)! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'jmv 8/16/2016 20:30:46'!
storeIntoLiteralVariable: value 
	"Simulate the action of bytecode that stores the top of the stack into a 
	literal variable of my method."

	self object: value instVarAt: self class valueIndex put: self top! !

!ContextPart methodsFor: 'private' stamp: 'jmv 8/16/2016 20:31:42'!
isPrimFailToken: anObject
	^(self objectClass: anObject) == Array
	  and: [anObject size = 2
	  and: [anObject first == self class primitiveFailToken]]! !


!ContextPart class methodsFor: 'simulation' stamp: 'jmv 8/16/2016 20:31:30'!
primitiveFailTokenFor: errorCode

	^{self primitiveFailToken. errorCode}! !

!ContextPart class methodsFor: 'cached class state' stamp: 'jmv 8/16/2016 20:28:20'!
releaseClassCachedState

	PrimitiveFailToken _ nil.
	QuickStep _ nil.
	ValueIndex _ nil! !

!classDefinition: #ContextPart category: #'Kernel-Methods'!
InstructionStream subclass: #ContextPart
	instanceVariableNames: 'stackp'
	classVariableNames: 'PrimitiveFailToken QuickStep ValueIndex'
	poolDictionaries: ''
	category: 'Kernel-Methods'!
