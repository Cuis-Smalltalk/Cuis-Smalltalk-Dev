'From Cuis 4.2 of 25 July 2013 [latest update: #2486] on 12 September 2015 at 11:23:17.537397 pm'!

!FmFileEntry commentStamp: 'jmv 9/12/2015 23:21' prior: 0!
I represent a single file entry (including directory).

You can write data by #fileContents: , and read the data by #fileContents.

---
mu 11/6/2006 20:21

--------------
See examples class category.

See FmDirectoryEntry.

See categories starting with '*fileman-' in String.
	Smalltalk imageName asFileEntry fileSize
	Smalltalk imageName asFileEntry parent directories do: [ :a | a print ]!


!FmDirectoryEntry commentStamp: '<historical>' prior: 0!
I represent a single file directory.
I implement various directory specific behaviors.

You can write data by #at:put: , and read the data by #at:.

---
mu 11/6/2006 20:21

--------------
Some examples:
	FmDirectoryEntry default
	FmDirectoryEntry root
	FmDirectoryEntry roots
	
See FmFileEntry!


!FmDirectoryEntry commentStamp: '<historical>' prior: 0!
I represent a single file directory.
I implement various directory specific behaviors.

You can write data by #at:put: , and read the data by #at:.

---
mu 11/6/2006 20:21

--------------
Some examples:
	FmDirectoryEntry default
	FmDirectoryEntry root
	FmDirectoryEntry roots
	
See FmFileEntry!

