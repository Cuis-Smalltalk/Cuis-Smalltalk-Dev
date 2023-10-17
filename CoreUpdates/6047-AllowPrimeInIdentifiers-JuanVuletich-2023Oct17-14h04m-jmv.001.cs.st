'From Cuis 6.0 [latest update: #6046] on 17 October 2023 at 2:17:30 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 10/17/2023 14:15:41'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	self isNullCharacter ifTrue: [ ^false ].
	self = $′ ifTrue: [ ^true ]. 		"Also see #xLetter"
	^#(xLetter xDigit xUnderscore xDiacritical) statePointsTo: (Scanner typeTableAt: self).! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 10/17/2023 14:16:58'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore
		or: [type == #xDiacritical
		or: [ hereChar = $′ ]]]]  						"Also see #isValidInIdentifiers"
	] whileTrue: [
			"open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
				ifTrue: [DoItCharacter "doit"]
				ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~= $=]])
		ifTrue: [
			buffer nextPut: self step.
			"Allow any number of embedded colons in literal symbols"
			[(self typeTableAt: hereChar) == #xColon]
				whileTrue:
					[buffer nextPut: self step].
			#keyword]
		ifFalse: [
			#word].
	token := buffer contents.! !

