'From Cuis 4.2 of 25 July 2013 [latest update: #2830] on 23 June 2016 at 10:31:44.767217 am'!

!Text methodsFor: 'emphasis' stamp: 'jmv 6/23/2016 10:30'!
initialFontFrom: someAttributes
	"Apply aFont to those parts that are not already specifying one.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	| fontAttr |
	fontAttr _ someAttributes detect: [ :any | any isFont ] ifNone: [ ^ self ].
	runs mapValues: [ :attributes |
		(attributes anySatisfy: [ :attr | attr isFont ])
			ifTrue: [ attributes ]
			ifFalse: [ attributes copyWith: fontAttr ]]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 6/23/2016 10:29'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	self
		evaluateSelectionAndDo: [ :result |
			self afterSelectionInsertAndSelect: 
				((' ', result printText, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]! !

