'From Cuis7.1 [latest update: #6609] on 20 August 2024 at 11:02:32 am'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 8/20/2024 11:00:34'!
subclassesSortedDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses.
	Sort by name."

	^(self subclasses sort: [:c1 :c2 | c1 name < c2 name ]) do: aBlock! !

!Behavior methodsFor: 'enumerating' stamp: 'jmv 8/20/2024 11:01:19'!
allSubclassesSortedDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's subclasses.
	At each level, sort subclasses."

	self subclassesSortedDo: [ :cl | 
		aBlock value: cl.
		cl allSubclassesSortedDo: aBlock ].! !

