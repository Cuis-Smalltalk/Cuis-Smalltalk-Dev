'From Cuis 6.0 [latest update: #5311] on 24 June 2022 at 11:11:11 am'!

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 11:10:04'!
findString: string
	"Fast version of #upToAll: to find a String in a file starting from the beginning.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 6/24/2022 11:10:13'!
findStringFromEnd: string
	"Fast version to find a String in a file starting from the end.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."
self flag: #unicodeNeedsImprovement.
'--------' print.
thisContext printStack: 10.
self halt.! !

!UniFileStream methodsFor: 'testing' stamp: 'jmv 6/24/2022 11:05:09'!
isDirectory
	^false! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 6/24/2022 11:04:34'!
isBinary
	^ readAdaptor class == BytesReadAdaptor! !

