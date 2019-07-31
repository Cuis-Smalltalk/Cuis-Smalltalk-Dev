'From Cuis 5.0 [latest update: #3826] on 14 July 2019 at 3:54:15 pm'!

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 7/14/2019 15:49:59'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory

	DirectoryEntry currentDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ Smalltalk getCurrentWorkingDirectory
			ifNotNil: [ :wd | self withPathName: wd ]
			ifNil: [ (self withPathName: Smalltalk imagePath) parent ]].
	^ CurrentDirectory! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DirectoryEntry releaseClassCachedState!

