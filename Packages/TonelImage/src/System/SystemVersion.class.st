"
SystemVersion current.

Instance variables:
	versionMajor, versionMinor
	releaseDate. If nil, not a release.
	updatesPrefix. Valid prefixes for update change sets. nil or '' -> No prefix.
	hightestUpdate. Number of last update installed, regardless of prefix.
	latestUpdateDate. Date of last update installed.
"
Class {
	#name : 'SystemVersion',
	#superclass : 'Object',
	#instVars : [
		'versionMajor',
		'versionMinor',
		'releaseDate',
		'updatesPrefix',
		'highestUpdate',
		'lastUpdateDate'
	],
	#classVars : [
		'Current'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'accessing' }
SystemVersion class >> current [
	Current ifNil: [Current := SystemVersion new].
	^Current
]

{ #category : 'instance creation' }
SystemVersion class >> newVersionMajor: aNumber versionMinor: anotherNumber [
	"
	SystemVersion newVersionMajor: 6 versionMinor: 0.
	"
	| newVersion |
	newVersion := self versionMajor: aNumber versionMinor: anotherNumber.
	newVersion
		highestUpdate: self current highestUpdate
		lastUpdateDate: self current lastUpdateDate.
	Current := newVersion

]

{ #category : 'instance creation' }
SystemVersion class >> versionMajor: aNumber versionMinor: anotherNumber [
	"
	SystemVersion versionMajor: 4 versionMinor: 1
	"
	^self basicNew versionMajor: aNumber versionMinor: anotherNumber
]

{ #category : 'initialization' }
SystemVersion >> beRollingRelease [
	"i.e. non released"
	updatesPrefix := nil.
	releaseDate := nil.
]

{ #category : 'initialization' }
SystemVersion >> beStableRelease [
	updatesPrefix := String streamContents: [ :strm |
		strm nextPutAll: '[r'; print: versionMajor; nextPut: $.; print: versionMinor; nextPutAll: ']-' ].
	releaseDate := Date today.
]

{ #category : 'accessing' }
SystemVersion >> highestUpdate [
	highestUpdate ifNil: [
		highestUpdate := 0 ].
	^ highestUpdate
]

{ #category : 'accessing' }
SystemVersion >> highestUpdate: anInteger lastUpdateDate: aDate [
	highestUpdate := anInteger.
	lastUpdateDate := aDate.
]

{ #category : 'testing' }
SystemVersion >> isCuis [
	"Sure we are!!
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!"
	^true
]

{ #category : 'testing' }
SystemVersion >> isPharo [
	"Nope. We are Cuis.
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!"
	^false
]

{ #category : 'testing' }
SystemVersion >> isSqueak [
	"Nope. We are Cuis.
	Warning: It is always better to write code that doesnt depend on platform particularities. Try to avoid calling this method!"
	^false
]

{ #category : 'testing' }
SystemVersion >> isStableRelease [
	^releaseDate notNil
]

{ #category : 'testing' }
SystemVersion >> lastUpdateDate [
	^lastUpdateDate
]

{ #category : 'printing' }
SystemVersion >> printOn: stream [
	stream
		nextPutAll: self versionString;
		nextPutAll: ' update ' , self highestUpdate printString
]

{ #category : 'accessing' }
SystemVersion >> registerUpdate: changeSetOrPackageName [

	| prefix |
	prefix := SystemVersion current updatesPrefix.
	(changeSetOrPackageName beginsWith: prefix) ifTrue: [
		(changeSetOrPackageName integerOrNilAt: prefix size+1)
			ifNotNil: [ :number |
				highestUpdate := self highestUpdate max: number.
				lastUpdateDate := Date today ]].
]

{ #category : 'testing' }
SystemVersion >> releaseDate [
	^releaseDate
]

{ #category : 'accessing' }
SystemVersion >> updatesPrefix [
	^updatesPrefix ifNil: ['']
]

{ #category : 'accessing' }
SystemVersion >> versionMajor [
	^versionMajor
]

{ #category : 'initialization' }
SystemVersion >> versionMajor: aNumber versionMinor: anotherNumber [
	versionMajor := aNumber.
	versionMinor := anotherNumber.
]

{ #category : 'accessing' }
SystemVersion >> versionMinor [
	^versionMinor
]

{ #category : 'accessing' }
SystemVersion >> versionRevision [
	"Answer an array of version.revision.
	Try to do something not completely unlike 'Semantic Versioning'.
	For example, for Cuis 4.1, updated to #1744, answer
	{ 41 . 1744 }
	SystemVersion current versionRevision
	"
	^{versionMajor * 10 + versionMinor . self highestUpdate}
]

{ #category : 'accessing' }
SystemVersion >> versionString [
	^String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: versionMajor;
			nextPut: $.;
			print: versionMinor ].
]
