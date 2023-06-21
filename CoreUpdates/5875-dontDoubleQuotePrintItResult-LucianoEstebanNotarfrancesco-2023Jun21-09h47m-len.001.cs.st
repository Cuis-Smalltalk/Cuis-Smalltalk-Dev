'From Cuis 6.0 [latest update: #5874] on 21 June 2023 at 9:51:36 am'!

!SmalltalkEditor methodsFor: 'do-its' stamp: 'len 6/21/2023 09:49:02'!
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
			text := ' ', text asText shoutDisableEmphasis, ' .'.
			self afterSelectionInsertAndSelect: (text initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false! !

