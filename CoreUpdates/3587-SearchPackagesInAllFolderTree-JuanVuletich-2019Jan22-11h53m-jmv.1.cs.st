'From Cuis 5.0 of 7 November 2016 [latest update: #3586] on 22 January 2019 at 11:53:37 am'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 1/22/2019 10:51:42'!
placesToLookForPackagesDo: aBlock

	| base myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ pathName asFileEntry parent.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ DirectoryEntry smalltalkImageDirectory.
	base allDirectoriesDo: aBlock.
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	base parent allDirectoriesDo: aBlock.

	"Also look in host OS current directory"
	(base ~= DirectoryEntry currentDirectory and: [base parent ~= DirectoryEntry currentDirectory])
		ifTrue: [
			base _ DirectoryEntry currentDirectory.
			base allDirectoriesDo: aBlock ]! !

!methodRemoval: FeatureRequirement #inPackagesSubtreeOf:do:!
FeatureRequirement removeSelector: #inPackagesSubtreeOf:do:!
!methodRemoval: FeatureRequirement #withPackageSubfoldersOf:do:!
FeatureRequirement removeSelector: #withPackageSubfoldersOf:do:!
