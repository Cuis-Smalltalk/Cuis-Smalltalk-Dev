'From Cuis 4.2 of 25 July 2013 [latest update: #2540] on 20 October 2015 at 9:17:22.64945 pm'!

!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 10/20/2015 20:27'!
serviceFileIn
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'fileIn entire file'
		selector: #fileIn:
		description: 'file in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'filein')
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 10/20/2015 20:27'!
serviceInstall
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install change set'
		buttonLabel: 'install')
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!FmFileEntry methodsFor: 'accessing' stamp: 'jmv 10/20/2015 20:58'!
name
	^name ifNil: [ drive ifNil: ['']]! !

