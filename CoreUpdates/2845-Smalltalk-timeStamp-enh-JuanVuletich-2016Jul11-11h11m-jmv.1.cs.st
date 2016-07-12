'From Cuis 4.2 of 25 July 2013 [latest update: #2843] on 11 July 2016 at 11:21:17 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/11/2016 11:20:00'!
timeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime _ Time dateAndTimeNow.
	aStream
		nextPutAll: 'From ';
		nextPutAll: Smalltalk datedVersion;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: '] on '.
	(dateTime at: 1) printOn: aStream.
	aStream
		nextPutAll: ' at '.
	(dateTime at: 2) print24: false showSeconds: true on: aStream! !

