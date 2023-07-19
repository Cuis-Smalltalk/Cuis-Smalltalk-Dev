'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 12:10:23 pm'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:10:15'!
doItProfiling: aBoolean
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"

	self lineSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result |
			Transcript finishEntry.
			[result print]
				on: UnhandledError
				do: [:ex | 'printing doIt result failed (remove handler in #doItProfiling: if you prefer the debugger).' print]]
		ifFail: nil
		profiled: aBoolean.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:09:04'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and evaluate aBlock with the result
	3 + 4
	"	

	self evaluate: self selection string andDo: aBlock ifFail: failBlock profiled: doProfile! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:10:11'!
exploreIt

	self lineSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:10:07'!
inspectIt

	self hasSelection ifFalse: [
		self
			withMethodNodeAndClassDo: [ :methodNode :selectedClass |
				self selectNodeUnderCursorForInspectionIn: methodNode ]
			ifErrorsParsing: [ :anError | ]].

	self lineSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false.
	morph convertAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 7/19/2023 12:10:01'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."

	self lineSelectAndEmptyCheck: [^ self ].
	self
		evaluateSelectionAndDo: [ :result |
			| text |
			text := model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			text := text copyReplaceAll: String lfString with: String lfString, ' '.
			text := ' ', text asText shoutDisableEmphasis, ' .'.
			self afterSelectionInsertAndSelect: (text initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 7/19/2023 12:09:56'!
referencesToSelectedLiteral
	"Evaluate the selected text and browse methods that reference the same literal"

	self lineSelectAndEmptyCheck: [^ self ].
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

