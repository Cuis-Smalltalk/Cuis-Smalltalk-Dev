'From Cuis 6.0 [latest update: #5967] on 4 August 2023 at 2:42:14 pm'!

!ProtocolBrowser methodsFor: 'class list' stamp: 'mnqpr 8/4/2023 13:51:39'!
selectedClass
	"Answer the class that is currently selected. 
	Answer base if no selection exists."
	| className class |
	className := self selectiveClassListIndex = 0
		ifTrue: [ self selectiveClassList last ]
		ifFalse: [ self selectiveClassList at: self selectiveClassListIndex ].
	class := Smalltalk at: (className findTokens: ' ') first asSymbol.
	baseClass isMeta
		ifTrue: [ ^ class class ]
		ifFalse: [ ^ class ]. ! !

