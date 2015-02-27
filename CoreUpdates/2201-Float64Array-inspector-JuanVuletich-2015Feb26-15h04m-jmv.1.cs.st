'From Cuis 4.2 of 25 July 2013 [latest update: #2200] on 26 February 2015 at 5:27:48.302272 pm'!
!classDefinition: #SequenceableCollectionInspector category: #'Tools-Inspector'!
Inspector subclass: #SequenceableCollectionInspector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!

!SequenceableCollectionInspector methodsFor: 'accessing' stamp: 'jmv 2/26/2015 15:43'!
fieldList
	^self baseFieldList,
		(1 to: object size)! !

!SequenceableCollectionInspector methodsFor: 'selecting' stamp: 'jmv 2/26/2015 15:43'!
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| basicIndex index |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ object longPrintStringLimitedTo: 20000].
	basicIndex _ selectionIndex - 2.
	index _ (object size <= (self i1 + self i2)  or: [basicIndex <= self i1])
		ifTrue: [ basicIndex ]
		ifFalse: [ object size - (self i1 + self i2) + basicIndex ].
	^object at: index! !


!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 16:20'!
length
	"Return the length of the receiver"
	^self squaredLength sqrt! !

!Float64Array methodsFor: 'accessing' stamp: 'jmv 2/26/2015 16:21'!
squaredLength
	"Return the squared length of the receiver"
	^self dot: self! !

!Float64Array methodsFor: 'inspecting' stamp: 'jmv 2/26/2015 15:43'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 2/26/2015 16:22'!
dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."

	| result |
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>"
	self flag: #Float64Primitive.

	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i)).
	].
	^result! !

!Float64Array reorganize!
('accessing' at: at:put: defaultElement floatAt: floatAt:put: length size squaredLength)
('initialization' loadFrom:)
('private' basicAt: basicAt:put: privateLoadFrom:)
('objects from disk' bytesPerElement)
('inspecting' inspectorClass)
('arithmetic' dot:)
!

