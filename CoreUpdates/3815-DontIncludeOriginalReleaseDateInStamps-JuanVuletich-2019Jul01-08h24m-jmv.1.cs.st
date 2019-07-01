'From Cuis 5.0 [latest update: #3814] on 1 July 2019 at 8:26:12 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/1/2019 08:24:52'!
timeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime _ DateAndTime now.
	aStream
		nextPutAll: 'From ';
		nextPutAll: Smalltalk version;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: '] on '.
	dateTime date printOn: aStream.
	aStream
		nextPutAll: ' at '.
	dateTime time print24: false showSeconds: true on: aStream! !


!SystemVersion methodsFor: 'printing' stamp: 'jmv 7/1/2019 08:25:27'!
printOn: stream
	stream
		nextPutAll: self version;
		nextPutAll: ' update ' , self highestUpdate printString! !

!methodRemoval: SystemVersion #datedVersion!
SystemVersion removeSelector: #datedVersion!
!methodRemoval: SystemDictionary #datedVersion!
SystemDictionary removeSelector: #datedVersion!
