'From Cuis 4.2 of 25 July 2013 [latest update: #2344] on 1 June 2015 at 1:52:49.572123 pm'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 6/1/2015 13:52'!
cuisAndCuisPackagesSubdirectoriesOf: aFileDirectory do: aBlock

	| repoDir pckDir compatPckDir |

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	aFileDirectory entries do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			repoDir _ entry asFileDirectory.
			aBlock value: repoDir.
			pckDir _ repoDir directoryNamed: 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			compatPckDir _ repoDir directoryNamed: 'CompatibilityPackages'.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 6/1/2015 13:52'!
placesToLookForPackagesDo: aBlock

	| base pckDir compatPckDir myDir parentDir |
	"Look in Cuis image folder"
	base _ FileDirectory default.
	aBlock value: base.

	"Look in the usual Packages subfolders"
	pckDir _ base directoryNamed: 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	compatPckDir _ base directoryNamed: 'CompatibilityPackages'.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].
	
	"Look in parent directory. Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	parentDir _ base containingDirectory.
	aBlock value: parentDir.

	"Look inside my own folder, if different"
	pathName ifNotNil: [
		myDir _ FileDirectory on: (FileDirectory dirPathFor: pathName).
		(myDir ~= base and: [ myDir ~= pckDir and: [ myDir ~= compatPckDir ]]) ifTrue: [
			aBlock value: myDir ]].

	"Finally look in any subfolders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default do: aBlock.
	"Finally look in any sibling folders that follow the convention of naming package repositories with the 'Cuis-Smalltalk' prefix"
	self cuisAndCuisPackagesSubdirectoriesOf: FileDirectory default containingDirectory do: aBlock! !

