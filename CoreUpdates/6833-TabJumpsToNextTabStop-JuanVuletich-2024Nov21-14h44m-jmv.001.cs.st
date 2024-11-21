'From Cuis7.1 [latest update: #6831] on 21 November 2024 at 3:00:57 pm'!

!CharacterScanner methodsFor: 'stop conditions' stamp: 'jmv 11/21/2024 14:55:59'!
tabDestX
	"This is the basic method of adjusting destX for a tab."

	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			"Just use next tab stop"
			(destX roundDownTo: tabWidth) + tabWidth min: rightMargin ].! !

