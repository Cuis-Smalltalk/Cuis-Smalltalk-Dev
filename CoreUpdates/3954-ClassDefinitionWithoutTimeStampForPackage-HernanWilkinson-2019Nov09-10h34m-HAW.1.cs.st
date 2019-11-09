'From Cuis 5.0 [latest update: #3953] on 9 November 2019 at 10:47:28 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 11/9/2019 10:35:24'!
definitionPreambleWithoutStamp

	^'classDefinition: ', self name printString, ' category: ', self category printString! !


!ChangeRecord methodsFor: 'testing' stamp: 'HAW 11/9/2019 10:44:49'!
isTestClassChange

	^ isTest! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 11/9/2019 10:35:36'!
definitionPreamble

	^self definitionPreambleWithoutStamp, Utilities changeStampField! !


!CodePackage methodsFor: 'saving' stamp: 'HAW 11/9/2019 10:41:04'!
write: classes classDefinitionsOn: aStream

	classes
		do: [ :class |
			aStream
				nextPut: $!!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class definition; newLine;

				nextPut: $!!; nextChunkPut: class class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class class definition; newLine;

				newLine ]
		displayingProgress: 'Saving class definitions...'.! !

