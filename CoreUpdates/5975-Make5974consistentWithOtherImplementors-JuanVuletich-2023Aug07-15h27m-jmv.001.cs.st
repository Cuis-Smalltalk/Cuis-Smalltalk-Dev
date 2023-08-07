'From Cuis 6.0 [latest update: #5974] on 7 August 2023 at 3:29:32 pm'!

!ProtocolBrowser methodsFor: 'class list' stamp: 'jmv 8/7/2023 15:27:31'!
selectedClassOrMetaClass
	"Answer the class or metaclass that is currently selected. 
	Answer base if no selection exists."
	| className |
	className := (self selectiveClassListIndex = 0) 
					ifTrue:  [self selectiveClassList last]
					ifFalse:  [self selectiveClassList at: self selectiveClassListIndex].
	^ Smalltalk at: ((className findTokens: ' ') first asSymbol)! !

!methodRemoval: ProtocolBrowser #selectedClass stamp: 'jmv 8/7/2023 15:27:41'!
ProtocolBrowser removeSelector: #selectedClass!
