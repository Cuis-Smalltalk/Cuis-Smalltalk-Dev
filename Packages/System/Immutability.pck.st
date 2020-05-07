'From Cuis 5.0 [latest update: #3826] on 12 July 2019 at 2:38:19 pm'!
'Description First version. See message #beImmutable and ImmutableTest'!
!provides: 'Immutability' 1 2!
SystemOrganization addCategory: #Immutability!


!classDefinition: #ModificationForbidden category: #Immutability!
Error subclass: #ModificationForbidden
	instanceVariableNames: 'object retrySelector fieldIndex newValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Immutability'!
!classDefinition: 'ModificationForbidden class' category: #Immutability!
ModificationForbidden class
	instanceVariableNames: ''!


!ModificationForbidden methodsFor: 'initialization' stamp: 'HAW 7/3/2019 18:48:33'!
initializeFor: anObject at: anIndex with: aValue retrySelector: aSelector

	object := anObject.
	fieldIndex := anIndex.
	newValue := aValue.
	retrySelector := aSelector ! !

!ModificationForbidden methodsFor: 'accessing' stamp: 'HAW 7/3/2019 18:48:27'!
fieldIndex
	
	^fieldIndex ! !

!ModificationForbidden methodsFor: 'accessing' stamp: 'HAW 7/3/2019 18:48:33'!
newValue
	
	^newValue ! !

!ModificationForbidden methodsFor: 'accessing' stamp: 'HAW 7/3/2019 18:28:02'!
object
	
	^object! !

!ModificationForbidden methodsFor: 'accessing' stamp: 'HAW 7/3/2019 18:28:34'!
retrySelector
	
	^retrySelector! !

!ModificationForbidden methodsFor: 'exceptionDescription' stamp: 'HAW 7/3/2019 18:45:13'!
messageText

	^self class messageText ! !

!ModificationForbidden methodsFor: 'retrying' stamp: 'HAW 7/5/2019 08:55:03'!
retryModification

	object perform: retrySelector with: fieldIndex with: newValue.
		
	self resumeUnchecked: newValue! !

!ModificationForbidden class methodsFor: 'signaling' stamp: 'HAW 7/3/2019 18:22:48'!
signalFor: anObject at: anIndex with: aValue retrySelector: aSelector

	^(self for: anObject at: anIndex with: aValue retrySelector: aSelector) signal! !

!ModificationForbidden class methodsFor: 'instance creation' stamp: 'HAW 7/3/2019 18:23:10'!
for: anObject at: anIndex with: aValue retrySelector: aSelector

	^self new initializeFor: anObject at: anIndex with: aValue retrySelector: aSelector! !

!ModificationForbidden class methodsFor: 'error message' stamp: 'HAW 7/3/2019 18:44:55'!
messageText
	
	^'Attempt to modify an immutable object'! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:01:30'!
at: index put: value 
	"Primitive. Assumes receiver is indexable. Store the argument value in 
	the indexable element of the receiver indicated by index. Fail if the 
	index is not an Integer or is out of bounds. Or fail if the value is not of 
	the right type for this kind of collection. Answer the value that was 
	stored. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 61>
	
	self isImmutable 
		ifTrue: [ ^ self modificationForbiddenFor: #at:put: index: index value: value ].

	index isInteger ifTrue:
		[self class isVariable
			ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]]
			ifFalse: [self errorNotIndexable]].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/4/2019 16:04:53'!
attemptToAssign: value withIndex: index 

	"Called by the VM when assigning an instance variable of an immutable object.
	Upon return, executing will resume *after* the inst var assignment. If the inst var mutation has to be 
	performed, do it manually here in the call back with instVarAt:put:.
	This method has to return *no* value by jumping to the context's sender
	Implementation copied from Pharo"
	
	self modificationForbiddenFor: #instVarAt:put: index: index value: value.
	
	thisContext sender jump
	"CAN'T REACH"! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:11:30'!
basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	
	self isImmutable 
		ifTrue: [ ^ self modificationForbiddenFor: #basicAt:put: index: index value: value ].

	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/12/2019 13:57:48'!
beImmutable
	
	" If the VM supports read-only objects it will not write to read-only objects.
	 An attempt to write to an instance variable of a read-only object will
	 cause the VM to send attemptToAssign:withIndex: to the read-only object.
	 An attempt to modify a read-only object in a primitive will cause the
	 primitive to fail with a #'no modification' error code
	
	Classes whose instances are composed by other objects, should redefine
	#beImmutable and #beMutable to make its parts immutable/mutable.
	See OrderedCollection>>#beImmutabe for an example"
	
	^self beImmutable: true! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 18:05:05'!
beImmutable: aBoolean

	<primitive: 164 error: ec>
	^self primitiveFailed
! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 18:10:57'!
beMutable
	
	^self beImmutable: false! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/12/2019 13:55:35'!
instVarAt: index put: anObject
	"Primitive. Store a value into a fixed variable in an object. The numbering of the
	 variables corresponds to the named instance variables, followed by the indexed
	 instance variables. Fail if the index is not an Integer or is not the index of a fixed
	 variable. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	"The classic InterpreterVMs don't support primitives 173 and 174.
	See http://forum.world.st/Some-test-where-Spur-more-slow-than-Cog-td4867810.html#a4867888
	Use primitives 73 and 74 in such case."
	Smalltalk isRunningCog ifFalse: [
		^ self instVarAtPrim74: index put: anObject ].
	
	self isImmutable 
		ifTrue: [ ^ self modificationForbiddenFor: #instVarAt:put: index: index value: anObject ].

	self primitiveFailed! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 18:06:54'!
isImmutable

	<primitive: 163 error: ec>
	^self class isImmediateClass ! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 18:21:53'!
modificationForbiddenFor: selector index: index value: value

	^ ModificationForbidden 
		signalFor: self
		at: index
		with: value
		retrySelector: selector! !

!Object methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:24:09'!
rawBasicAt: index put: value 
	"A verbatim copy of #basicAt:put:
	To be used when subclasses might redefine #basicAt:, but unaltered, raw behavior is desired.
	For example, when studying Floats, and understanding FFI, etc."
	"Primitive. Assumes receiver is indexable. Store the second argument 
	value in the indexable element of the receiver indicated by index. Fail 
	if the index is not an Integer or is out of bounds. Or fail if the value is 
	not of the right type for this kind of collection. Answer the value that 
	was stored. Essential. Do not override in a subclass. See Object 
	documentation whatIsAPrimitive."

	<primitive: 61>
	self isImmutable 
		ifTrue: [ ^ self modificationForbiddenFor: #rawBasicAt:put: index: index value: value ].

	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self rawBasicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]! !

!Object methodsFor: '*Immutability' stamp: 'HAW 6/20/2019 16:19:35'!
setIsReadOnlyObject: aBoolean
	"If the VM supports read-only objects it will not write to read-only objects.
	 An attempt to write to an instance variable of a read-only object will
	 cause the VM to send attemptToAssign:withIndex: to the read-only object.
	 An attempt to modify a read-only object in a primitive will cause the
	 primitive to fail with a #'no modification' error code.
	 This primitive sets the read-only flag of the receiver to the given
	 value and answers the previous vaue of the flag.
	 Note: Some objects can't be read-only, currently contexts and objects related
	 to process scheduling (Processor, Process instances, Semaphore instances, ...)"
	<primitive: 164 error: ec>
	^self primitiveFailed
	! !

!String methodsFor: '*Immutability' stamp: 'HAW 7/12/2019 12:46:06'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>

	self isImmutable 
		ifTrue: [ ^ self modificationForbiddenFor: #at:put: index: index value: aCharacter ].

	(aCharacter is: #Character)
		ifTrue: [	
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']! !

!OrderedCollection methodsFor: '*Immutability' stamp: 'HAW 7/4/2019 15:39:31'!
beImmutable
	
	array beImmutable.
	^super beImmutable ! !

!OrderedCollection methodsFor: '*Immutability' stamp: 'HAW 7/4/2019 15:41:10'!
beMutable
	
	array beMutable.
	^super beMutable ! !

!Set methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:58:03'!
beImmutable
	
	array beImmutable.
	^super beImmutable ! !

!Set methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:57:48'!
beMutable
	
	array beMutable.
	^super beMutable ! !

!Array2D methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:49:50'!
beImmutable
	
	elements beImmutable.
	^super beImmutable.
	! !

!Array2D methodsFor: '*Immutability' stamp: 'HAW 7/3/2019 19:50:15'!
beMutable
	
	elements beMutable.
	^super beMutable! !
