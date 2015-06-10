'From Cuis 4.2 of 25 July 2013 [latest update: #2355] on 9 June 2015 at 10:30:55.665339 am'!

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 6/9/2015 10:21'!
description
	^String streamContents: [ :s |
		s nextPutAll: 'Code Package File: '.
		s nextPutAll: self fullName; newLine; newLine.
		s nextPutAll: 'Provides: '.
		self provides printDetailsOn: s.
		s newLine.
		(self requires sorted: [:a :b | a name < b name]) do: [ :req |
			s nextPutAll: 'Requires: '.
			req printDetailsOn: s ].
		s newLine; newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ]
	]! !

