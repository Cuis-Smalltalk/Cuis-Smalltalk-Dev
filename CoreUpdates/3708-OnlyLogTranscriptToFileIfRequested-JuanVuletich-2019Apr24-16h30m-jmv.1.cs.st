'From Cuis 5.0 of 7 November 2016 [latest update: #3707] on 24 April 2019 at 4:30:53 pm'!

!Transcript class methodsFor: 'private' stamp: 'jmv 4/24/2019 16:30:16'!
writeToFile: anEntry

	logToFile ifTrue: [
		self filename asFileEntry appendStreamDo: [ :stream | stream nextPutAll: anEntry ]]! !

