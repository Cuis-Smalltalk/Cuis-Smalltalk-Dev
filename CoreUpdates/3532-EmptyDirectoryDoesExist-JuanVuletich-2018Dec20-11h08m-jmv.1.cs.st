'From Cuis 5.0 of 7 November 2016 [latest update: #3529] on 20 December 2018 at 11:16:32 am'!

!FileIOAccessor methodsFor: 'private' stamp: 'pb 5/25/2016 00:32'!
basicDirectoryExists: fullPathName

	| result |
	result := self primLookupEntryIn: fullPathName index: 1.
 	^(result == #badDirectoryPath) not! !

