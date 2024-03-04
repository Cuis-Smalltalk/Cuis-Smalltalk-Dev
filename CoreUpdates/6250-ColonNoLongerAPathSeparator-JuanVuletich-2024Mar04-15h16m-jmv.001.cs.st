'From Cuis6.3 [latest update: #6248] on 4 March 2024 at 3:21:51 pm'!

!CharacterSequence class methodsFor: 'fileman-constants' stamp: 'jmv 3/4/2024 15:19:36'!
pathSeparators
	^'\/'! !


!Character methodsFor: 'fileman-testing' stamp: 'jmv 3/4/2024 15:20:36'!
isPathSeparator

	^CharacterSequence pathSeparators includes: self! !


!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 3/4/2024 15:20:00'!
asPathTokens

	^self beginsWithWindowsDriveName
		ifTrue: [
			(OrderedCollection with: (self copyFrom: 1 to: 2)), 
				((self copyFrom: 3 to: self size) findTokens: CharacterSequence pathSeparators) ]
		ifFalse: [
			self findTokens: CharacterSequence pathSeparators ]! !

!CharacterSequence methodsFor: 'fileman-testing' stamp: 'jmv 3/4/2024 15:21:01'!
isPathSeparator
	^self size = 1 and: [ self first isPathSeparator ]! !

!methodRemoval: String class #pathSeparators stamp: 'jmv 3/4/2024 15:19:43'!
String class removeSelector: #pathSeparators!

!CharacterSequence class reorganize!
('instance creation' new new:withAll: numElements readFrom:)
('fileman-constants' pathSeparators)
!

