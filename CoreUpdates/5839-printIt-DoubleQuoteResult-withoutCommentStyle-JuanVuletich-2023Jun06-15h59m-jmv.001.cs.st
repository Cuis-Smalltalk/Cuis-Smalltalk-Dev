'From Cuis 6.0 [latest update: #5839] on 6 June 2023 at 4:02:58 pm'!

!Text methodsFor: 'adding emphasis' stamp: 'jmv 6/6/2023 15:11:20'!
normalEmphasis

	self addAttribute: TextEmphasis normal! !


!ShoutTextBackgroundColor methodsFor: 'testing' stamp: 'jmv 6/6/2023 15:06:55'!
dominatedByNormalEmphasis
	"TextEmphasis 0 disables Shout styling"
	^true! !


!ShoutTextColor methodsFor: 'testing' stamp: 'jmv 6/6/2023 15:12:12'!
dominatedByNormalEmphasis
	"TextEmphasis 0 disables Shout styling"
	^true! !


!ShoutTextEmphasis methodsFor: 'testing' stamp: 'jmv 6/6/2023 15:07:00'!
dominatedByNormalEmphasis
	"TextEmphasis 0 disables Shout styling"
	^true! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/6/2023 15:49:03'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	self
		evaluateSelectionAndDo: [ :result |
			| text |
			text := model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			text := text copyReplaceAll: String lfString with: String lfString, ' '.
			text := ('"', text, '"') asText normalEmphasis.
			text := text, '.'.
			self afterSelectionInsertAndSelect: (text initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false! !

