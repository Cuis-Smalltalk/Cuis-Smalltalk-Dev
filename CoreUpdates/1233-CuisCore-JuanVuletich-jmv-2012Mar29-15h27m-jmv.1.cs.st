'From Cuis 4.0Alpha of 29 March 2012 [latest update: #1225] on 29 March 2012 at 3:40:31 pm'!

!ChangeSet commentStamp: '<historical>' prior: 0!
                ChangeSets keep track of the changes made to a system, so they can be written on a file as source code (a "fileOut").
There are separate ChangeSets for caturing changes done to the Cuis base system and changes done to Packages. Usually the ChangeSets for Packages are not important, and ignored, because Packages are saved on pck files. Change sets for the Cuis base system are the standard way of capturing changes to Cuis, and are used to evolve Cuis itself.

name - a String used to name the changeSet, and thus any associated project or fileOut.

preamble and postscript:  two strings that serve as prefix (useful for documentation) and suffix (useful for doits) to the fileout of the changeSet.

changeRecords -  Dictionary {class name -> a ClassChangeRecord}.
These classChangeRecords (qv) remember all of the system changes.

structures -    Dictionary {#Rectangle -> #(<classVersionInteger> 'origin' 'corner')}.
Of  the names of the instances variables before any changes for all classes in classChanges, and all of their superclasses.  In the same format used in SmartRefStream.  Inst var names are strings.  

superclasses -    Dictionary {#Rectangle -> #Object}.
Of all classes in classChanges, and all of their superclasses.

Structures and superclasses save the instance variable names of this class and all of its superclasses.  Later we can tell how it changed and write a conversion method.  The conversion method is used when old format objects are brought in from the disk from ImageSegment files (.extSeg) or SmartRefStream files (.obj .morph .bo .sp).

NOTE:  It should be fairly simple, by adding a bit more information to the classChangeRecords, to reconstruct the information now stored in 'structures' and 'superclasses'.  This would be a welcome simplification.
!


!ChangeSorter commentStamp: '<historical>' prior: 0!
            I display a ChangeSet.!


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 3/29/2012 15:33'!
                    installPackageStream: aStream

	| fullName pkName existing |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	existing _ CodePackage named: pkName.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		Cursor wait showWhile: [
			CodePackageFile
				installFileStream: aStream
				packageName: pkName
				fullName: fullName ]]! !

