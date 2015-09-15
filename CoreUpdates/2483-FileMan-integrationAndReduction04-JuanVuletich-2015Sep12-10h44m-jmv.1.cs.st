'From Cuis 4.2 of 25 July 2013 [latest update: #2479] on 12 September 2015 at 10:50:43.717319 am'!
!classDefinition: 'FmFileIOAccessor class' category: #'FileMan-Core'!
FmFileIOAccessor class
	instanceVariableNames: 'default librarySpecificClass dialectUtilClass '!

!FmFileIOAccessor class methodsFor: 'class initialization' stamp: 'jmv 9/12/2015 10:48'!
initialize
	"FmFileIOAccessor initialize"
	self initDefault.! !

!FmFileIOAccessor class methodsFor: 'instance creation' stamp: 'jmv 9/12/2015 10:47'!
default
	Default isNil
		ifTrue: [
			Default _ FmFileDirectoryFileIOAccessorCuis new].
	^ Default! !

!methodRemoval: FmFileIOAccessor class #dialectUtilClass:!
FmFileIOAccessor class removeSelector: #dialectUtilClass:!
!methodRemoval: FmFileIOAccessor class #initDialectUtilClass!
FmFileIOAccessor class removeSelector: #initDialectUtilClass!
!methodRemoval: FmFileIOAccessor class #initLibrarySpecificClass!
FmFileIOAccessor class removeSelector: #initLibrarySpecificClass!
!methodRemoval: FmFileIOAccessor class #librarySpecificClass!
FmFileIOAccessor class removeSelector: #librarySpecificClass!
!methodRemoval: FmFileIOAccessor class #librarySpecificClass:!
FmFileIOAccessor class removeSelector: #librarySpecificClass:!
FmFileIOAccessor initialize!
!classDefinition: 'FmFileIOAccessor class' category: #'FileMan-Core'!
FmFileIOAccessor class
	instanceVariableNames: ''!
