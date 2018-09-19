'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 19 September 2018 at 10:17:34 am'!

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 9/19/2018 10:17:18'!
basicDirectoryExists: fullPathName

	| result |
	result := self primLookupEntryIn: fullPathName index: 1.
	result ifNil: [ ^ false ].
 	^(result == #badDirectoryPath) not! !

