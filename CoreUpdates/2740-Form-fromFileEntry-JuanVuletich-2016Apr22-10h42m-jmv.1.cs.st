'From Cuis 4.2 of 25 July 2013 [latest update: #2739] on 22 April 2016 at 11:20:42.674516 am'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 4/22/2016 10:42'!
fromFileEntry: aFileEntry
	"Read a Form or ColorForm from the given file."

	| form |
	aFileEntry readStream: [ :stream |
		form _ self fromBinaryStream: stream binary ].
	^ form! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 4/22/2016 10:45'!
fromFileNamed: filename
	"Read a Form or ColorForm from the given file."

	^ self fromFileEntry: filename asFileEntry! !

