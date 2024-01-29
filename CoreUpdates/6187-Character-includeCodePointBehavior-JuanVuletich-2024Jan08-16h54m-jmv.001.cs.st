'From Cuis6.3 [latest update: #6179] on 8 January 2024 at 5:03:37 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 16:55:13'!
isLetter
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLetter
	(Character codePoint: 16r042F) isLetter
	(Character codePoint: 16r0431) isLetter
	"
	^self isLowercase or: [ self isUppercase ]! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 16:56:03'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12 16r0085 16r2028 16r2029) statePointsTo: self codePoint! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 16:58:26'!
isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	(Character codePoint: 16r22C7) isValidInBinarySelectors
	(Character codePoint: 16r042F) isValidInBinarySelectors
	(Character codePoint: 16r0431) isValidInBinarySelectors
	(Character codePoint: 16r20D7) isValidInBinarySelectors
	"
	self isNullCharacter ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 17:01:44'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	self = $′ ifTrue: [ ^true ]. 		"Also see #xLetter"
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !

!Character methodsFor: 'fileman-testing' stamp: 'jmv 1/8/2024 16:54:29'!
isDriveLetter
	^'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' includes: self! !

