'From Cuis 6.0 [latest update: #5852] on 14 June 2023 at 3:45:14 pm'!
!classDefinition: #ShoutDisable category: 'System-Text-Attributes'!
TextAttribute subclass: #ShoutDisable
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text-Attributes'!

!ShoutDisable commentStamp: '<historical>' prior: 0!
I'm a special Attribute. My only purpose is to disable Shout emphases and colors.!


!Text methodsFor: 'adding emphasis' stamp: 'jmv 6/14/2023 15:20:39'!
shoutDisableEmphasis

	self addAttribute: ShoutDisable new! !


!ShoutDisable methodsFor: 'comparing' stamp: 'jmv 6/14/2023 15:24:10'!
dominates: another
	"Subclasses may override condense multiple attributes"
	^ another isForShout! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/14/2023 15:43:25'!
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
			text := ('"', text, '"') asText shoutDisableEmphasis.
			text := text, '.'.
			self afterSelectionInsertAndSelect: (text initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false! !

!methodRemoval: ShoutTextEmphasis #dominatedByNormalEmphasis stamp: 'jmv 6/14/2023 15:42:01'!
ShoutTextEmphasis removeSelector: #dominatedByNormalEmphasis!
!methodRemoval: ShoutTextColor #dominatedByNormalEmphasis stamp: 'jmv 6/14/2023 15:42:01'!
ShoutTextColor removeSelector: #dominatedByNormalEmphasis!
!methodRemoval: ShoutTextBackgroundColor #dominatedByNormalEmphasis stamp: 'jmv 6/14/2023 15:41:59'!
ShoutTextBackgroundColor removeSelector: #dominatedByNormalEmphasis!
!methodRemoval: Text #normalEmphasis stamp: 'jmv 6/14/2023 15:41:57'!
Text removeSelector: #normalEmphasis!
