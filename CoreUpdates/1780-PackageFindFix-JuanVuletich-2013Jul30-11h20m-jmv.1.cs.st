'From Cuis 4.2 of 25 July 2013 [latest update: #1779] on 30 July 2013 at 11:33:08.582 am'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/30/2013 11:27'!
cuisAndCuisPackagesSubdirectoriesOf: aFileDirectory do: aBlock

	| repoDir pckDir |

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	aFileDirectory entries do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			repoDir _ entry asFileDirectory.
			aBlock value: repoDir.
			pckDir _ repoDir directoryNamed: 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ]]]! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/30/2013 11:25'!
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

	"Finally look in any subfolders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default do: aBlock.
	"Finally look in any sibling folders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default containingDirectory do: aBlock! !

