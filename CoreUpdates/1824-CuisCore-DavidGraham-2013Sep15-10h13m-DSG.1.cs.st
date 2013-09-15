'From Cuis 4.2 of 25 July 2013 [latest update: #1823] on 15 September 2013 at 12:46:53.843133 pm'!

!FeatureRequirement methodsFor: 'private' stamp: 'DSG 9/15/2013 12:02'!
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
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default containingDirectory do: aBlock.
	"If you are working from a newImage directory,  recurse one more level into any sibling folders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default containingDirectory containingDirectory do: aBlock! !

