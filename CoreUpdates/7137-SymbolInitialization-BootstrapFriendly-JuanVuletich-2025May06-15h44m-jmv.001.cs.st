'From Cuis7.3 [latest update: #7126] on 6 May 2025 at 3:47:03 pm'!

!CharacterSequence class methodsFor: 'minimalKernel - Symbol support' stamp: 'jmv 5/6/2025 15:02:25'!
representsSymbols
	^false! !


!Symbol class methodsFor: 'minimalKernel - Symbol support' stamp: 'jmv 5/6/2025 14:51:13'!
estimatedCount
	^Symbols estimatedSize! !

!Symbol class methodsFor: 'minimalKernel - Symbol support' stamp: 'jmv 5/6/2025 15:02:37'!
representsSymbols
	^true! !


!Symbol class methodsFor: 'minimalKernel - Symbol support' stamp: 'jmv 5/6/2025 15:06:49'!
initialize
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[
			Symbols := WeakSet new.
			CharacterSequence allSubclassesDo: [ :cls | | sym |
				cls representsSymbols ifTrue: [
					sym := cls someInstance.
					[sym == nil] whileFalse: [
						Symbols add: sym.
						sym := sym nextInstance ]]]]! !


!StdIOWriteStream methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 4/29/2025 17:01:32'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self error: 'StdIOWriteStream [', name, '] write failed'.! !


!Character methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 5/6/2025 14:39:55'!
isLowercase
	"Answer whether the receiver is a letter."
	"
	(Character codePoint: 16r22C7) isLowercase
	(Character codePoint: 16r042F) isLowercase
	(Character codePoint: 16r0431) isLowercase
	"
	| i |
	"If no UnicodeData.txt tables, consider only ASCII"
	LowercaseLettersTable isNil ifTrue: [
		^ self codePoint between: 97 and: 122 ].
		
	i := self codePoint + 1.
	LowercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^LowercaseLettersTable bitBooleanAt: i.! !

!Character methodsFor: 'minimalKernel - Bootstrap' stamp: 'jmv 5/6/2025 14:40:28'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	"
	(Character codePoint: 16r22C7) isUppercase
	(Character codePoint: 16r042F) isUppercase
	(Character codePoint: 16r0431) isUppercase
	"
	| i |
	"If no UnicodeData.txt tables, consider only ASCII"
	UppercaseLettersTable isNil ifTrue: [
		^ self codePoint between: 65 and: 90 ].

	i := self codePoint + 1.
	UppercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^UppercaseLettersTable bitBooleanAt: i.! !

Symbol initialize!
