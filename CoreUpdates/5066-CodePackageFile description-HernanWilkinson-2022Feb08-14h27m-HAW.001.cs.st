'From Cuis 6.0 [latest update: #5065] on 8 February 2022 at 2:36:26 pm'!

!CodePackageFile methodsFor: 'accessing' stamp: 'HAW 2/8/2022 14:35:17'!
description
	^String streamContents: [ :s |
		s nextPutAll: 'Code Package File: '.
		s nextPutAll: self fullName; newLine; newLine.
		s nextPutAll: 'Provides: '.
		self provides printDetailsOn: s.
		s newLine.
		self requires ifNotEmpty: [ :requirements | 
			s nextPutAll: 'Requires: '.
			(requirements sorted: [:a :b | a name < b name]) do: [ :req |
				s newLine; space.
				req printDetailsOn: s ].
			s newLine ].
		s newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ]
	]! !

