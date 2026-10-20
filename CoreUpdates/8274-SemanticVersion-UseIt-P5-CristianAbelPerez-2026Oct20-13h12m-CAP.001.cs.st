'From Cuis7.9 [latest update: #0] on 20 October 2026 at 1:14:49 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:44:01 am (UTC)'!
highestUpdate

	^version patch! !

!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:44:01 am (UTC)'!
highestUpdate: anInteger lastUpdateDate: aDate

	version := SemanticVersion major: version major minor: version minor patch: anInteger.
	lastUpdateDate := aDate! !

!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:44:01 am (UTC)'!
registerUpdate: changeSetOrPackageName

	| prefix |

	prefix := SystemVersion current updatesPrefix.
	(changeSetOrPackageName beginsWith: prefix) ifTrue: [
		(changeSetOrPackageName integerOrNilAt: prefix size+1)
			ifNotNil: [ :number |
				self highestUpdate: (self highestUpdate max: number) lastUpdateDate: Date today ]]! !

!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:44:01 am (UTC)'!
versionMajor

	^version major! !

!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:44:01 am (UTC)'!
versionMinor

	^version minor! !

!SystemVersion methodsFor: 'accessing' stamp: 'CAP 19/Oct/2026, 12:45:10 am (UTC)'!
versionString
	^String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: version major;
			nextPut: $.;
			print: version minor ].! !

!SystemVersion methodsFor: 'initialization' stamp: 'CAP 19/Oct/2026, 12:45:10 am (UTC)'!
beStableRelease
	updatesPrefix := String streamContents: [ :strm |
		strm nextPutAll: '[r'; print: version major; nextPut: $.; print: version minor; nextPutAll: ']-' ].
	releaseDate := Date today.! !

!SystemVersion methodsFor: 'initialization' stamp: 'CAP 19/Oct/2026, 12:52:42 am (UTC)'!
versionMajor: aNumber versionMinor: anotherNumber

	version := SemanticVersion major: aNumber minor: anotherNumber! !

