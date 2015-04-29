'From Cuis 4.2 of 25 July 2013 [latest update: #2257] on 19 April 2015 at 8:28:48.850912 am'!
!classDefinition: #FileDirectory category: #'System-Files'!
Object subclass: #FileDirectory
	instanceVariableNames: 'pathName '
	classVariableNames: 'DefaultDirectory DirectoryClass StandardMIMEMappings '
	poolDictionaries: ''
	category: 'System-Files'!
!classDefinition: #MacFileDirectory category: #'System-Files'!
FileDirectory subclass: #MacFileDirectory
	instanceVariableNames: ''
	classVariableNames: 'TypeToMimeMappings '
	poolDictionaries: ''
	category: 'System-Files'!
!methodRemoval: StandardFileStream #getFileType!
StandardFileStream removeSelector: #getFileType!
!methodRemoval: FileStream #mimeTypes!
FileStream removeSelector: #mimeTypes!
!methodRemoval: MacFileDirectory class #initializeTypeToMimeMappings!
MacFileDirectory class removeSelector: #initializeTypeToMimeMappings!
!methodRemoval: MacFileDirectory #mimeTypesFor:!
MacFileDirectory removeSelector: #mimeTypesFor:!
!classDefinition: #MacFileDirectory category: #'System-Files'!
FileDirectory subclass: #MacFileDirectory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
!methodRemoval: FileDirectory class #initializeStandardMIMETypes!
FileDirectory class removeSelector: #initializeStandardMIMETypes!
!methodRemoval: FileDirectory #mimeTypesFor:!
FileDirectory removeSelector: #mimeTypesFor:!
!classDefinition: #FileDirectory category: #'System-Files'!
Object subclass: #FileDirectory
	instanceVariableNames: 'pathName'
	classVariableNames: 'DefaultDirectory DirectoryClass'
	poolDictionaries: ''
	category: 'System-Files'!
