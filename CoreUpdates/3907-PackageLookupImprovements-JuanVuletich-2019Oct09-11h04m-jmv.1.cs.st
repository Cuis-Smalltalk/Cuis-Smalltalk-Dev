'From Cuis 5.0 [latest update: #3906] on 9 October 2019 at 11:38:23 am'!

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/9/2019 11:23:46'!
allRegularDirectoriesDo: aBlock
	self regularDirectoriesDo: [ :child |
		aBlock value: child.
		child allRegularDirectoriesDo: aBlock]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/9/2019 11:23:21'!
regularDirectoriesDo: aBlock
	self childrenDo: [ :each |
		each isFile ifFalse: [
			each isRegularDirectory ifTrue: [
				aBlock value: each ]]]! !

!DirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/9/2019 11:28:39'!
isRegularDirectory
	"hidden convention in Unix"
	name first = $. ifTrue: [ ^false ].
	"in MacOS, applications are actually directories, but are usually not treated as such"
	self extension = 'app' ifTrue: [ ^false ].
	"in MacOS, .bundle directories, are resource packages"
	self extension = 'bundle' ifTrue: [ ^false ].
	^true! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/9/2019 11:35:55'!
placesToLookForPackagesDo: aBlock

	| myDir base packagesDirectory |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ pathName asFileEntry parent.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ DirectoryEntry smalltalkImageDirectory.
	aBlock value: base.
	packagesDirectory _ base / 'Packages'.
	aBlock value: packagesDirectory.
	packagesDirectory allRegularDirectoriesDo: aBlock.
	base regularDirectoriesDo: [ :child |
		child = packagesDirectory ifFalse: [
			aBlock value: child.
			child allRegularDirectoriesDo: aBlock]].
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub.
	First try directories including the word Cuis in the name. Then try others."
	base parent regularDirectoriesDo: [ :dir |
		dir ~= base ifTrue: [
			('*Cuis*' match: dir name)
				ifTrue: [aBlock value: dir. dir allRegularDirectoriesDo: aBlock]]].
	base parent regularDirectoriesDo: [ :dir |
		dir ~= base ifTrue: [
			('*Cuis*' match: dir name)
				ifFalse: [aBlock value: dir. dir allRegularDirectoriesDo: aBlock]]].

	"Also look in host OS current directory"
	(base ~= DirectoryEntry currentDirectory and: [base parent ~= DirectoryEntry currentDirectory])
		ifTrue: [
			base _ DirectoryEntry currentDirectory.
			base allRegularDirectoriesDo: aBlock ]! !

