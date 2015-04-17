'From Cuis 4.2 of 25 July 2013 [latest update: #2240] on 16 April 2015 at 4:43:38.594827 pm'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 4/16/2015 16:43'!
placesToLookForPackagesDo: aBlock

	| base pckDir myDir parentDir |
	"Look in Cuis image folder"
	base _ FileDirectory default.
	aBlock value: base.

	"Look in the usual Packages subfolder"
	pckDir _ base directoryNamed: 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	
	"Look in parent directory. Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	parentDir _ base containingDirectory.
	aBlock value: parentDir.

	"Look inside my own folder, if different"
	pathName ifNotNil: [
		myDir _ FileDirectory on: (FileDirectory dirPathFor: pathName).
		(myDir ~= base and: [ myDir ~= pckDir ]) ifTrue: [
			aBlock value: myDir ]].

	"Finally look in any subfolders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default do: aBlock.
	"Finally look in any sibling folders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default containingDirectory do: aBlock! !

