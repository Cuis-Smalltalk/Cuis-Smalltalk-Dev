'From Cuis 5.0 [latest update: #3893] on 2 October 2019 at 11:39:31 am'!
!classDefinition: #Float32SlotsObject category: #'Kernel-Objects'!
Object variableWordSubclass: #Float32SlotsObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!Float32SlotsObject commentStamp: '<historical>' prior: 0!
Abstract superclass for objects whose slots are 32 bit Floating Point values, but don't inherit from FloatArray because they are not collections, and collection protocol makes no sense on them.!


!Float32SlotsObject methodsFor: 'private' stamp: 'jmv 10/2/2019 11:38:34'!
at: index
"
'---------'print.
thisContext printStack: 10.
'======'print.
"
^self slotAt: index! !

!Float32SlotsObject methodsFor: 'private' stamp: 'jmv 10/2/2019 11:38:42'!
at: index put: stuff
"
'---------'print.
thisContext printStack: 10.
'======'print.
"
^self slotAt: index put: stuff! !

!Float32SlotsObject methodsFor: 'private' stamp: 'jmv 10/2/2019 11:21:16'!
slotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!Float32SlotsObject methodsFor: 'private' stamp: 'jmv 10/2/2019 11:21:55'!
slotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self slotAt: index put: value asFloat ].
	^value! !


!Float32SlotsObject class methodsFor: 'instance creation' stamp: 'jmv 10/2/2019 11:19:44'!
new
	"Answer a new instance of me, with size = 0, unles a specific size is used."

	^self new: self numSlots! !

!Float32SlotsObject class methodsFor: 'instance creation' stamp: 'jmv 10/2/2019 11:39:11'!
numSlots
	^0! !

