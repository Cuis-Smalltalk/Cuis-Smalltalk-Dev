'From Cuis 4.2 of 25 July 2013 [latest update: #2421] on 13 July 2015 at 6:51:53.71147 am'!

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'KenD 7/12/2015 20:08'!
formFromFileNamed: fileName
	"Answer a ColorForm stored on the file with the given name."
	| stream |
	stream _ FileStream readOnlyFileNamed: fileName.
	stream useBytes.
	^self formFromStream: stream! !

