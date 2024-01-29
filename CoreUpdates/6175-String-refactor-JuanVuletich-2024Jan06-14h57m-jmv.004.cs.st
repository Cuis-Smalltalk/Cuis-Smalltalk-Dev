'From Cuis6.3 [latest update: #6167] on 6 January 2024 at 3:16:13 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 1/4/2024 21:28:30'!
isValidInFilenames
	"Can be part of a filename?
		$< isValidInFilenames
		$a isValidInFilenames
		$| isValidInFilenames
		$^ isValidInFilenames
		$: isValidInFilenames
	"
	self isNullCharacter ifTrue: [ ^false ].
	self isSeparator ifTrue: [ ^false ].
	^(#( $: $< $> $| $/ $\ $? $* $") statePointsTo: self) not.! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 1/6/2024 15:06:55'!
isValidInFilenames
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isValidInFilenames ].
	^true! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 1/4/2024 21:29:27'!
asFileName
	"Answer a String made up from the receiver that is an acceptable file name."

	^ self collect: [ :char |
		char isValidInFilenames
			ifFalse: [$#] 
			ifTrue: [char]].! !


!methodRemoval: CharacterSequence #indexOfAnyOf: stamp: 'jmv 1/6/2024 15:13:05'!
CharacterSequence removeSelector: #indexOfAnyOf:!

!CharacterSet reorganize!
('collection ops' add: do: includes: remove:)
('testing' isAscii)
('comparison' = hash)
('private' byteArrayMap initialize)
!

