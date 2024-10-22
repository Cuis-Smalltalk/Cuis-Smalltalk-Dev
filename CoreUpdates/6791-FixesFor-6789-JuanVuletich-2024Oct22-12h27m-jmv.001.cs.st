'From Cuis7.1 [latest update: #6790] on 22 October 2024 at 12:28:26 pm'!

!CodeFileBrowser methodsFor: 'class list' stamp: 'jmv 10/22/2024 11:45:23'!
classList
	"Answer a collection of the class names of the selected category. Answer an 
	empty array if no selection exists."

	(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ ^#() ].

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [(caseCodeSource classes collect: [:ea| ea name]) sort].! !


!PseudoClass methodsFor: 'accessing' stamp: 'jmv 10/22/2024 12:23:28'!
superclassName
	"Answer the name of the superclass as declared in the file.
	If no definition in the file, then answer the name of the existing superclass in the system.
	If neither, answer nil."

	self hasDefinition ifTrue: [
		^definition copyUpTo: Character space ].
	self exists ifTrue: [
		^self realClass superclass name ].
	^nil! !

