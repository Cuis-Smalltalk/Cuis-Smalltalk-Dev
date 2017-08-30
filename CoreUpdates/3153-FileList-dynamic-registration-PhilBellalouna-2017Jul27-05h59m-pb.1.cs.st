'From Cuis 5.0 of 7 November 2016 [latest update: #3130] on 27 July 2017 at 6:26:56 am'!
!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected '
	classVariableNames: 'FileReaderRegistry '
	poolDictionaries: ''
	category: 'Tools-FileList'!

!FileList class methodsFor: 'file reader registration' stamp: 'pb 7/27/2017 06:05:51'!
itemsForFile: filename
	"Answer a list of services appropriate for a file of the given name"
	| services suffix classList |
	suffix _ (FileIOAccessor default extensionFor: filename) asLowercase.
	services _ OrderedCollection new.
	"Build the list dynamically for all implementers of the appropriate class method... registration no longer required"
	classList _ (Smalltalk allClassesImplementing: #fileReaderServicesForFile:suffix:)
		collect: [ :item |
			item class == Metaclass ifTrue: [ item soleInstance ]]
		thenSelect: [ :item |
			item notNil ].
	classList do: [ :reader |
		reader ifNotNil: [
			services addAll:
				(reader
					fileReaderServicesForFile: filename
					suffix: suffix) ]].
	^ services.! !

!FileList class methodsFor: 'file reader registration' stamp: 'pb 7/27/2017 06:02:20'!
registerFileReader: aProviderClass
	"For compatibility... no longer necessary"! !

!FileList class methodsFor: 'file reader registration' stamp: 'pb 7/27/2017 06:02:32'!
unregisterFileReader: aProviderClass
	"For compatibility... no longer necessary"! !

!methodRemoval: Morph class #unload!
Morph class removeSelector: #unload!
!methodRemoval: Form class #unload!
Form class removeSelector: #unload!
!methodRemoval: FileList class #initialize!
FileList class removeSelector: #initialize!
!methodRemoval: ChangeSorter class #unload!
ChangeSorter class removeSelector: #unload!
!methodRemoval: ChangeList class #unload!
ChangeList class removeSelector: #unload!
!methodRemoval: CodeFileBrowser class #unload!
CodeFileBrowser class removeSelector: #unload!
!methodRemoval: MessageNames class #unload!
MessageNames class removeSelector: #unload!
!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!
FileList initialize!
