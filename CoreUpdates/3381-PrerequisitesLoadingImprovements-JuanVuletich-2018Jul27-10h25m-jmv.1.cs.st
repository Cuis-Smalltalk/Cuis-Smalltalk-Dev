'From Cuis 5.0 of 7 November 2016 [latest update: #3380] on 27 July 2018 at 10:26:23 am'!

!FileSystemEntry methodsFor: 'convenience' stamp: 'jmv 7/27/2018 10:18:05'!
ifExists: aBlock
	"Evaluate a block with receiver as argument if it exists on the file system. If not, do nothing."
	self exists ifTrue: [
		aBlock value: self ]! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/27/2018 10:24:05'!
withPackageSubfoldersOf: aDirectoryEntry do: aBlock

	"Look in the requested directory"
	aBlock value: aDirectoryEntry.

	"Look in the usual Packages subfolders"
	aDirectoryEntry / 'Packages' ifExists: [ :packagesFolder |
		aBlock value: packagesFolder.
		packagesFolder / 'MorphicExamples' ifExists: [ :subFolder | aBlock value: subFolder ].
		packagesFolder / 'CompatibilityPackages' ifExists: [ :subFolder | aBlock value: subFolder ]].
	aDirectoryEntry / 'M3' ifExists: [ :subFolder | aBlock value: subFolder ]! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 7/27/2018 10:16:03'!
inPackagesSubtreeOf: aDirectoryEntry do: aBlock

	"Direct subfolders"
	self withPackageSubfoldersOf: aDirectoryEntry do: aBlock.

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	aDirectoryEntry children do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			self withPackageSubfoldersOf: entry do: aBlock ]]! !

