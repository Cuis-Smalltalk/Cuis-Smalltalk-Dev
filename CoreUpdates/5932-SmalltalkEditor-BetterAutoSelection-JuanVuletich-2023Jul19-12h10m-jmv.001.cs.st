'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 1:03:06 pm'!

!CharacterSequence methodsFor: 'testing' stamp: 'jmv 7/19/2023 12:25:44'!
isLineEndAt: anIndex
	"Answer true if a line ends at position anIndex.
	Tolerates one position behind last character:
	'hello' isLineEndAt: 6
	'hello' isLineEndAt: 5
	'hello
	world' isLineEndAt: 6
	"
	anIndex = (self size + 1) ifTrue: [ ^true ].
	^(self at: anIndex) isLineSeparator! !

!CharacterSequence methodsFor: 'testing' stamp: 'jmv 7/19/2023 12:39:39'!
isSeparators
	"Answer true if the receiver contains only separators (i.e. whitespace)."
	^ self allSatisfy: [:each| each isSeparator]! !


!Text methodsFor: 'testing' stamp: 'jmv 7/19/2023 12:27:24'!
isLineEndAt: anIndex
	"Answer true if a line ends at position anIndex.
	Tolerates one position behind last character: 	"
	^string isLineEndAt: anIndex! !

!Text methodsFor: 'testing' stamp: 'jmv 7/19/2023 12:40:57'!
isSeparators
	"Answer true if the receiver contains only separators (i.e. whitespace)."
	^string isSeparators! !


!Editor methodsFor: 'accessing-selection' stamp: 'jmv 7/19/2023 12:51:50'!
selectionIsEmptyOrSeparators
	"Answer true if no selection or selection is just separators (whitespace)"

	self subclassResponsibility! !


!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 7/19/2023 12:51:44'!
selectionIsEmptyOrSeparators
	"Answer true if no selection or selection is just separators (whitespace)"

	self hasSelection ifFalse: [ ^true ].
	^self selection isSeparators! !


!SmalltalkEditor methodsFor: 'private' stamp: 'jmv 7/19/2023 12:58:52'!
lineOrNodeSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, either
		- expand it to be the entire current line
		- or expand it to a meaningful chunk of code.
	If after that's done the selection is still empty, then evaluate the returnBlock."

	self hasSelection ifFalse: [
		(model actualContents isLineEndAt: self pointIndex)
			ifTrue: [ self selectLine ]
			ifFalse: [
				self
					withMethodNodeAndClassDo: [ :methodNode :selectedClass |
						self selectNodeUnderCursorForInspectionIn: methodNode ]
					ifErrorsParsing: [ :anError | ]]].
	self selectionIsEmptyOrSeparators ifTrue: [
		morph flash.
		^returnBlock value ].! !


!Editor methodsFor: 'private' stamp: 'jmv 7/19/2023 12:54:52'!
lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this.
	
	Do not push down this method. It is used in SimpleEditor of package Widgets-Extras
	"

	"if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [
		self selectLine ].
	self selectionIsEmptyOrSeparators ifTrue: [
		morph flash.
		^ returnBlock value ].! !

!Editor methodsFor: 'private' stamp: 'jmv 7/19/2023 12:54:34'!
wordSelectAndEmptyCheck: returnBlock
	"Ensure selecting the entire current word; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectWord.  "Select exactly a whole word"
	self selectionIsEmptyOrSeparators ifTrue: [
		morph flash.
		^ returnBlock value ].! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 13:00:42'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"

	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed (remove handler in #doItProfiling: if you prefer the debugger).' print]]
		ifFail: nil
		profiled: aBoolean.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 13:00:27'!
exploreIt

	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ ^morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:59:25'!
inspectIt

	self lineOrNodeSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ ^morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 4/3/2023 10:42:35'!
referencesToSelectedLiteral
	"Evaluate the selected text and browse methods that reference the same literal"
	[
		self
			evaluateSelectionAndDo: [ :result |
				Smalltalk
					browseMessageList: (Smalltalk allReferencesToLiteral: result)
					name: 'Users of literal: ' , result asString
					autoHighlight: self selection
					allOccurrences: true ]
			ifFail: nil
			profiled: false ]
	on: UndeclaredVariableReference , UnknownSelector
	do: [ :ex |
		morph flash ]! !

