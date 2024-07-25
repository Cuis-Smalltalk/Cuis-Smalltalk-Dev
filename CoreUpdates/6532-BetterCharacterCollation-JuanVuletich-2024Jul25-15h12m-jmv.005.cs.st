'From Cuis7.1 [latest update: #6531] on 25 July 2024 at 3:56:48 pm'!

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 15:53:31'!
basicCaseInsensitiveOrder
	"Only for the first 256 Unicode code points.
	Case insensitive
	Follow #collatesBefore:"

	| sorted answer order |
	sorted := (0 to: 255) asArray collect: [ :i | Character codePoint: i ] ::
		sort: [ :a :b |
			a asLowercase = b asLowercase
				ifTrue: [ a collatesBefore: b ]
				ifFalse: [	 	a asLowercase collatesBefore: b asLowercase ]].
	answer := Array new: 256.
	order := -1.
	sorted do: [ :c |
		c isUppercase ifFalse: [
			answer at: c codePoint + 1 put: (order := order+1)]].
	sorted do: [ :c |
		c isUppercase ifTrue: [
			answer at: c codePoint + 1 put: (answer at: c asLowercase codePoint + 1)]].
	^answer asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 15:53:34'!
basicCaseSensitiveOrder
	"Only for the first 256 Unicode code points.
	Case sensitive
	Follow #collatesBefore:"

	| sorted answer order |
	sorted := (0 to: 255) asArray collect: [ :i | Character codePoint: i ] :: sort: [ :a :b | a collatesBefore: b ].
	answer := Array new: 256.
	order := -1.
	sorted do: [ :c | answer at: c codePoint + 1 put: (order := order+1)].
	order = 255 ifFalse: [self error: 'order problem'].
	^answer asByteArray.! !

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 15:53:54'!
caseInsensitiveOrder
	^`String basicCaseInsensitiveOrder`.! !

!String class methodsFor: 'static state' stamp: 'jmv 7/25/2024 15:53:47'!
caseSensitiveOrder
	^`String basicCaseSensitiveOrder`.! !


!Character methodsFor: 'comparing' stamp: 'jmv 7/25/2024 15:54:18'!
< aCharacter
	"Compare with the standard case sensitive collation order."

	self == aCharacter ifTrue: [ ^false ].

	(self codePoint < 256 and: [ aCharacter codePoint < 256 ]) ifTrue: [
		^String does: self collateBefore: aCharacter ].

	^self collatesBefore: aCharacter.! !

