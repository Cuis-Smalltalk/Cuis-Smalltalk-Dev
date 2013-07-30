'From Cuis 4.2 of 25 July 2013 [latest update: #1769] on 29 July 2013 at 12:22:23.821837 am'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/29/2013 00:21'!
placesToLookForPackagesDo: aBlock

	| base pckDir myDir |
	"Look in Cuis image folder"
	base _ FileDirectory default.
	aBlock value: base.

	"Look in the usual Packages subfolder"
	pckDir _ base directoryNamed: 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].

	"Look inside my own folder, if different"
	pathName ifNotNil: [
		myDir _ FileDirectory on: (FileDirectory dirPathFor: pathName).
		(myDir ~= base and: [ myDir ~= pckDir ]) ifTrue: [
			aBlock value: myDir ]].

	"Finally look in any sibling folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix"
	FileDirectory default containingDirectory entries do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			entry name ~= base localName ifTrue: [
				aBlock value: entry asFileDirectory ]]
		]! !

