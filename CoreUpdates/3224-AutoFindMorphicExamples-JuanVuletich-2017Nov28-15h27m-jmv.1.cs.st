'From Cuis 5.0 of 7 November 2016 [latest update: #3223] on 28 November 2017 at 3:28:20 pm'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 11/28/2017 15:27:34'!
inPackagesSubtreeOf: aDirectoryEntry do: aBlock

	| pckDir morphicExamplesPckDir compatPckDir |

	"Look in the requested directory"
	aBlock value: aDirectoryEntry.

	"Look in the usual Packages subfolders"
	pckDir _ aDirectoryEntry / 'Packages'.
	pckDir exists ifTrue: [
		aBlock value: pckDir ].
	morphicExamplesPckDir _ pckDir / 'MorphicExamples'.
	morphicExamplesPckDir exists ifTrue: [
		aBlock value: morphicExamplesPckDir ].
	compatPckDir _ aDirectoryEntry / 'CompatibilityPackages'.
	compatPckDir exists ifTrue: [
		aBlock value: compatPckDir ].

	"Finally look in folders that follow the convention of naming package repositories
	with the 'Cuis-Smalltalk' prefix, and their possible 'Packages' subdir."
	aDirectoryEntry children do: [ :entry |
		(entry isDirectory and: [ entry name beginsWith: 'Cuis-Smalltalk' ]) ifTrue: [
			aBlock value: entry.
			pckDir _ entry / 'Packages'.
			pckDir exists ifTrue: [
				aBlock value: pckDir ].
			morphicExamplesPckDir _ pckDir / 'MorphicExamples'.
			morphicExamplesPckDir exists ifTrue: [
				aBlock value: morphicExamplesPckDir ].
			compatPckDir _ entry / 'CompatibilityPackages'.
			compatPckDir exists ifTrue: [
				aBlock value: compatPckDir ]]]! !

