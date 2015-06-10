'From Cuis 4.2 of 25 July 2013 [latest update: #2346] on 1 June 2015 at 9:04:14.488084 pm'!

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 6/1/2015 21:03'!
description
	^String streamContents: [ :s |
		s nextPutAll: 'Code Package File: '.
		s nextPutAll: self fullName; newLine; newLine.
		s nextPutAll: 'Provides: '.
		self provides printDetailsOn: s.
		s newLine.
		(self requires sorted: nil) do: [ :req |
			s nextPutAll: 'Requires: '.
			req printDetailsOn: s ].
		s newLine; newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ]
	]! !

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 6/1/2015 21:01'!
provides

	^featureSpec provides! !


!Feature methodsFor: 'printing' stamp: 'jmv 6/1/2015 21:02'!
printDetailsOn: aStream

	aStream
		nextPutAll: name;
		nextPut: $ .
	self printVersionRevisionOn: aStream! !


!FeatureRequirement methodsFor: 'printing' stamp: 'jmv 6/1/2015 21:02'!
printDetailsOn: aStream
	aStream
		nextPutAll: name;
		nextPut: $ .
	minVersion
		ifNil: [
			aStream nextPutAll: '*.*)'.
			^self ]
		ifNotNil: [ minVersion printOn: aStream ].
	aStream nextPut: $..
	minRevision
		ifNil: [ aStream nextPut: $* ]
		ifNotNil: [ minRevision printOn: aStream ].
	(minRevision notNil or: [ maxVersion isNil or: [maxVersion > minVersion ]]) ifTrue: [
		aStream nextPutAll: ' to '.
		maxVersion
			ifNil: [ aStream nextPut: $* ]
			ifNotNil: [ maxVersion printOn: aStream ].
		aStream nextPutAll: '.*']! !


!Feature methodsFor: 'printing' stamp: 'jmv 6/1/2015 21:02'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	self printDetailsOn: aStream.
	aStream
		nextPut: $)! !


!FeatureRequirement methodsFor: 'printing' stamp: 'jmv 6/1/2015 21:00'!
printOn: aStream
	aStream
		nextPutAll: self class name;
		nextPut: $(.
	self printDetailsOn: aStream.
	aStream
		nextPut: $)! !

