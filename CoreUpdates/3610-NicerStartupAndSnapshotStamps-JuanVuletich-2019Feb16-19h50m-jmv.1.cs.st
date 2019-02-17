'From Cuis 5.0 of 7 November 2016 [latest update: #3609] on 16 February 2019 at 8:06:52 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 2/16/2019 20:06:00'!
snapshotMessageFor: save andQuit: quit

	| dateAndTime |
	dateAndTime _ DateAndTime now.
	^String streamContents: [ :stream |
		stream 
			nextPutAll: self tagHeader;
			nextPutAll: (self snapshotTagFor: save andQuit: quit);
			nextPutAll: self tagTail.
		stream nextPut: $(.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream nextPut: $).
		stream
			space;
			nextPutAll: self imageName asFileEntry name;
			nextPutAll: ' priorSource: ';
			print: LastQuitLogPosition ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 2/16/2019 20:02:34'!
timeStamp: aStream 
	"Writes system version and current time on stream aStream."

	| dateTime |
	dateTime _ DateAndTime now.
	aStream
		nextPutAll: 'From ';
		nextPutAll: Smalltalk datedVersion;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: '] on '.
	dateTime date printOn: aStream.
	aStream
		nextPutAll: ' at '.
	dateTime time print24: false showSeconds: true on: aStream! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 2/16/2019 20:03:15'!
setStartupStamp

	| dateAndTime |
	dateAndTime _ DateAndTime now.
	StartupStamp _ String streamContents: [ :stream |
		stream nextPutAll: '----STARTUP---- ('.
		dateAndTime date printOn: stream.
		stream space.
		dateAndTime time print24: true showSeconds: true on: stream.
		stream
			nextPutAll: ') as ';
			nextPutAll: Smalltalk imageName ] 
! !

