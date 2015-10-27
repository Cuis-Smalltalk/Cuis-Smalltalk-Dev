'From Cuis 4.2 of 25 July 2013 [latest update: #2555] on 26 October 2015 at 11:26:22.755629 am'!

!FeatureRequirement methodsFor: 'private' stamp: 'jmv 10/26/2015 11:26'!
placesToLookForPackagesDo: aBlock

	| base myDir |

	"Look inside my own folder"
	pathName ifNotNil: [
		myDir _ pathName asFileEntry parent.
		aBlock value: myDir ].

	"Look in Cuis image folder and reasonable subfolders"
	base _ FmDirectoryEntry smalltalkImageDirectory.
	self inPackagesSubtreeOf: base do: aBlock.
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub"
	self inPackagesSubtreeOf: base parent do: aBlock.

	"Also look in host OS current directory"
	base _ FmDirectoryEntry currentDirectory.
	self inPackagesSubtreeOf: base do: aBlock! !

