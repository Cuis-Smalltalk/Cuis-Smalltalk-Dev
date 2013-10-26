'From Cuis 4.0 of 21 April 2012 [latest update: #1300] on 11 June 2012 at 12:25:31 pm'!

!FileDirectory methodsFor: 'enumeration' stamp: 'jmv 6/11/2012 12:23'!
fileAndDirectoryNames
	"FileDirectory default fileAndDirectoryNames"

	^ self entries collect: [:entry | entry name]
! !

!FileDirectory methodsFor: 'file status' stamp: 'jmv 6/11/2012 12:24'!
entryAt: fileName ifAbsent: aBlock
	"Find the entry with local name fileName and answer it.
	If not found, answer the result of evaluating aBlock."
	| comparisonBlock |
	comparisonBlock _ self isCaseSensitive
		ifTrue: [
			[ :entry |
			entry name = fileName ]]
		ifFalse: [
			[ :entry |
			entry name sameAs: fileName ]].
	^ self entries
		detect: comparisonBlock
		ifNone: aBlock! !

