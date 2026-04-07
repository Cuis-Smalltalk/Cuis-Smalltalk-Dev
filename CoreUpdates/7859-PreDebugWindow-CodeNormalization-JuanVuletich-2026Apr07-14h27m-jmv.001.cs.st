'From Cuis7.7 [latest update: #7857] on 7 April 2026 at 2:34:32 pm'!
!classDefinition: #PreDebugWindow category: #'Morphic-Tool Windows'!
CodeWindow subclass: #PreDebugWindow
	instanceVariableNames: 'messageText '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!

!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 7/Apr/2026 14:31:12'!
buildMorphicWindow
	| notifyPane |
	model expandStack.
	messageText
		ifNil: [
			notifyPane := PluggableListMorph
				withModel: model
				listGetter: #contextStackList
				indexGetter: #contextStackIndex
				indexSetter: #openFullAt: ]
		ifNotNil: [
			notifyPane := TextModelMorph withText: messageText.
			notifyPane askBeforeDiscardingEdits: false ].
	self layoutMorph
		addMorph: (self buttonRowForPreDebugWindow: model) proportionalHeight: 0.2;
		addAdjusterAndMorph: notifyPane proportionalHeight: 0.8! !

!PreDebugWindow methodsFor: 'initialization' stamp: 'jmv 7/Apr/2026 14:30:21'!
messageText: aMessageText
	messageText := aMessageText.! !


!PreDebugWindow class methodsFor: 'instance creation' stamp: 'jmv 7/Apr/2026 14:32:09'!
open: model label: aString message: otherString
	|  window |

	window := self new.
	window
		model: model;
		messageText: otherString;
		buildMorphicWindow.
	aString ifNotNil: [ window setLabel: aString ].
	window openInWorld.
	^window! !

!methodRemoval: PreDebugWindow #buildMorphicWindowMessage: stamp: 'jmv 7/Apr/2026 14:32:21'!
PreDebugWindow removeSelector: #buildMorphicWindowMessage:!
!methodRemoval: PreDebugWindow #buildMorphicWindowMessage stamp: 'jmv 7/Apr/2026 14:31:15'!
PreDebugWindow removeSelector: #buildMorphicWindowMessage!
!classDefinition: #PreDebugWindow category: #'Morphic-Tool Windows'!
CodeWindow subclass: #PreDebugWindow
	instanceVariableNames: 'messageText'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
