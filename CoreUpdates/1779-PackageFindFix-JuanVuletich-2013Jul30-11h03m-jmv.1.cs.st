'From Cuis 4.2 of 25 July 2013 [latest update: #1778] on 30 July 2013 at 11:05:37.173078 am'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/30/2013 11:05'!
placesToLookForPackagesDo: aBlock

	| base pckDir myDir repoDir |
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
				repoDir _ entry asFileDirectory.
				aBlock value: repoDir.
				pckDir _ repoDir directoryNamed: 'Packages'.
				pckDir exists ifTrue: [
					aBlock value: pckDir ]]]
		]! !

