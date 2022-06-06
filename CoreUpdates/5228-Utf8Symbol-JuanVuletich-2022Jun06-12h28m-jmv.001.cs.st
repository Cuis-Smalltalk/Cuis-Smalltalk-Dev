'From Cuis 6.0 [latest update: #5227] on 6 June 2022 at 12:29:15 pm'!
!classDefinition: #Utf8Symbol category: #'System-Text'!
Utf8String subclass: #Utf8Symbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 5/30/2022 15:13:02'!
asSymbol
	^(Utf8String with: self) asSymbol! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 6/3/2022 10:49:30'!
asSymbol
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self! !


!Utf8String class methodsFor: 'system' stamp: 'jmv 6/3/2022 10:29:49'!
symbolClassToUse
	"Answer the class to create symbols from us"
	^Utf8Symbol! !


!Utf8Symbol methodsFor: 'comparing' stamp: 'jmv 6/3/2022 10:34:35'!
= another

	"Use == between two symbols..."
	self == another ifTrue: [
		^ true].  "Was == "
	another isSymbol ifTrue: [
		^ false].  "Was not ==, therefore can't be = "

	"Otherwise use string =..."
	^ super = another! !

!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/6/2022 12:00:10'!
asPlainString
	"Answer a plain String, not a Symbol."
	^Utf8String fromUtf8Bytes: bytes.! !

!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/6/2022 12:00:21'!
asString
	^ self asPlainString! !

!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/3/2022 10:49:43'!
asSymbol! !

!Utf8Symbol methodsFor: 'testing' stamp: 'jmv 6/3/2022 10:34:29'!
isSymbol
	^ true! !


!Utf8Symbol class methodsFor: 'private' stamp: 'jmv 6/3/2022 11:09:33'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	^self basicNew bytes: aString asUtf8Bytes! !

