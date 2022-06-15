'From Cuis 6.0 [latest update: #5271] on 13 June 2022 at 9:04:15 am'!

!CharacterSequence methodsFor: 'copying' stamp: 'di 2/15/98 22:54'!
padded: leftOrRight to: length with: char
	leftOrRight = #left ifTrue: [
		^ (self class new: (length - self size max: 0) withAll: char) , self].
	leftOrRight = #right ifTrue: [
		^ self , (self class new: (length - self size max: 0) withAll: char)].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 08:58:52'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| kwd char keywords isAllValidInIdentifiers |
	keywords _ Array streamContents: [ :kwds |
		kwd _ self species writeStream.
		isAllValidInIdentifiers _ true.
		1
			to: self size
			do: [ :i |
				char _ self at: i.
				kwd nextPut: char.
				char = $: & isAllValidInIdentifiers
					ifTrue: [
						kwds nextPut: kwd contents.
						kwd reset.
						isAllValidInIdentifiers _ true ]
					ifFalse: [
						char isValidInIdentifiers ifFalse: [ isAllValidInIdentifiers _ false ]]].
		kwd isEmpty ifFalse: [ kwds nextPut: kwd contents ]].
	
	^ keywords.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/6/2011 10:34'!
prefixAndSuffix: aCharacter
	"Answer an array with the prefix up to the last occurrence of aCharacter, and the suffix after it.
	Answer nil if aCharacter is not in self
	'1164-ReferenceStreamFix-jmv.1.cs' prefixAndSuffix: $-
	'1164-ReferenceStreamFix-jmv.1.cs' asUtf8String prefixAndSuffix: $-
	"
	| i |
	i _ self findLast: [ :c | c = aCharacter ].
	i = 0 ifTrue: [ ^ nil ].
	^ { self copyFrom: 1 to: i-1 . self copyFrom: i+1 to: self size }! !

!CharacterSequence methodsFor: 'uCompletion' stamp: 'jmv 11/1/2011 23:20'!
separateKeywords

	^self, ' '! !


!Utf8Symbol methodsFor: 'accessing' stamp: 'jmv 6/13/2022 09:01:03'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/13/2022 09:00:44'!
isKeyword
	"Answer whether the receiver is a message keyword."

	^ self precedence = 3! !

!Utf8Symbol methodsFor: 'uCompletion' stamp: 'jmv 6/13/2022 09:00:06'!
separateKeywords

	^self isKeyword
		ifFalse: [ self, ' ' ]
		ifTrue: [
			self species streamContents: [ :strm |
				self keywords
					do: [ :keyword | strm nextPutAll: keyword ]
					separatedBy: [ strm space; space ].
				strm space]]! !


!Symbol methodsFor: 'uCompletion' stamp: 'jmv 11/1/2011 23:21'!
separateKeywords

	^self isKeyword
		ifFalse: [ self, ' ' ]
		ifTrue: [
			self species streamContents: [ :strm |
				self keywords
					do: [ :keyword | strm nextPutAll: keyword ]
					separatedBy: [ strm space; space ].
				strm space]]! !

!methodRemoval: String #separateKeywords stamp: 'jmv 6/13/2022 08:55:02'!
String removeSelector: #separateKeywords!
!methodRemoval: String #prefixAndSuffix: stamp: 'jmv 6/13/2022 08:53:47'!
String removeSelector: #prefixAndSuffix:!
!methodRemoval: String #padded:to:with: stamp: 'jmv 6/13/2022 08:53:30'!
String removeSelector: #padded:to:with:!
!methodRemoval: String #keywords stamp: 'jmv 6/13/2022 08:58:52'!
String removeSelector: #keywords!

!Utf8Symbol reorganize!
('accessing' precedence)
('comparing' =)
('converting' asPlainString asString asSymbol capitalized uncapitalized)
('testing' isInitializePvtSelector isKeyword isPvtSelector isSymbol)
('printing' print storeOn:)
('uCompletion' separateKeywords)
!

