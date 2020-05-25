'From Cuis 5.0 [latest update: #4166] on 23 May 2020 at 8:15:03 pm'!

!ProtoObject methodsFor: 'testing' stamp: 'jmv 5/23/2020 19:54:19'!
isObject
	^false! !


!Object methodsFor: 'testing' stamp: 'jmv 5/23/2020 19:54:03'!
isObject
	^true! !


!Inspector methodsFor: 'accessing' stamp: 'jmv 5/23/2020 19:59:02'!
baseFieldList
	"Answer an Array consisting of 'self'
	and the instance variable names of the inspected object."

	^ (Array with: 'self' with: 'all inst vars')
			, self objectClass allInstVarNames! !

!Inspector methodsFor: 'accessing' stamp: 'jmv 5/23/2020 19:59:18'!
fieldList
	"Answer the base field list plus an abbreviated list of indices."

	self objectClass isVariable ifFalse: [^ self baseFieldList].
	^ self baseFieldList , (self indexLabelsForSize: self objectSize)! !

!Inspector methodsFor: 'accessing' stamp: 'jmv 5/23/2020 19:59:47'!
selectedClass
	"Answer the class of the receiver's current selection"

	self selectionIsUnmodifiable ifTrue: [^ self objectClass].
	^ self selection class! !

!Inspector methodsFor: 'selecting' stamp: 'jmv 5/23/2020 19:59:34'!
replaceSelectionValue: anObject 
	"The receiver has a list of variables of its inspected object. One of these 
	is selected. The value of the selected variable is set to the value, 
	anObject."
	| si instVarIndex |
	self selectionIsUnmodifiable ifTrue: [
		self toggleIndex: (si := selectionIndex).  
		self toggleIndex: si.
		^ object].
	instVarIndex := self selectionIndexNonMeta.
	instVarIndex > self objectClassInstSize
		ifFalse: [^ object instVarAt: instVarIndex put: anObject].
	self objectClass isVariable or: [self error: 'Cannot replace selection'].
	^object basicAt: self selectedObjectIndex put: anObject! !

!Inspector methodsFor: 'selecting' stamp: 'jmv 5/23/2020 20:12:32'!
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| index |
	self selectionIsMeta ifTrue: [^self selectionMeta].
	self selectionIndexNonMeta <= self objectClassInstSize
		ifTrue: [^ self object: object instVarAt: self selectionIndexNonMeta].
	index _ self selectedObjectIndex.
	^(object isObject and: [object isString])
		ifTrue: [ object at: index ]
		ifFalse: [ self object: object basicAt: index ]! !

!Inspector methodsFor: 'shout styling' stamp: 'jmv 5/23/2020 19:59:09'!
bindingNamesDo: aBlock
	self objectClass allInstVarNames do: aBlock! !

!Inspector methodsFor: 'shout styling' stamp: 'jmv 5/23/2020 19:59:25'!
hasBindingOf: aString
	^ self objectClass allInstVarNames includes: aString! !

!Inspector methodsFor: 'user interface support' stamp: 'jmv 5/23/2020 20:02:35'!
displayLabel

	| label |
	object isObject ifFalse: [^self objectClass name].
	label := [object printStringLimitedTo: 64]
		on: UnhandledError
		do: [:ex | ex return: self objectClass printString, ' (printing failed)'].
	"Note this is ambiguous: what happens with (Array with: Array)?"
	(label includesSubString: self objectClass name) ifTrue: [^label].
	^self objectClass name, ': ', label! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/23/2020 19:47:40'!
objectClass

	^self objectClass: object! !

!Inspector methodsFor: 'private' stamp: 'sqr 5/23/2020 19:47:52'!
objectSize

	^self objectSize: object! !

!methodRemoval: Inspector #object stamp: 'jmv 5/23/2020 20:14:23'!
Inspector removeSelector: #object!
