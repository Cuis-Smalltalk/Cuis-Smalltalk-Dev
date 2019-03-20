'From Cuis 5.0 of 7 November 2016 [latest update: #3683] on 20 March 2019 at 4:37:35 pm'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 3/20/2019 16:36:14'!
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
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub.
	First try directories including the word Cuis in the name. Then try others."
	base parent directoriesDo: [ :dir |
		('*Cuis*' match: dir name)
			ifTrue: [aBlock value: dir. dir allDirectoriesDo: aBlock]].
	base parent directoriesDo: [ :dir |
		('*Cuis*' match: dir name)
			ifFalse: [aBlock value: dir. dir allDirectoriesDo: aBlock]].

	"Also look in host OS current directory"
	(base ~= DirectoryEntry currentDirectory and: [base parent ~= DirectoryEntry currentDirectory])
		ifTrue: [
			base _ DirectoryEntry currentDirectory.
			base allDirectoriesDo: aBlock ]! !

