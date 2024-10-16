'From Haver7.0 [latest update: #6454] on 16 October 2024 at 9:14:15 pm'!

!FileList class methodsFor: 'file reader registration' stamp: 'KLG 10/16/2024 21:02:50'!
itemsForFileEntry: aFileEntry
	"Answer a list of services appropriate for a file of the given name"
	| services suffix classList |
	"
	suffix := (FileIOAccessor default extensionFor: filename) asLowercase.
	"
	suffix := aFileEntry extension asLowercase.
	"Build the list dynamically for all implementers of the appropriate class method... registration no longer required"
	classList := (Smalltalk allClassesImplementing: #fileReaderServicesForFileEntry:suffix:)
		collect: [ :item | item theNonMetaClass ].
	services := OrderedCollection new.
	classList do: [ :reader |
		services addAll:
			(reader
				fileReaderServicesForFileEntry: aFileEntry 
				suffix: suffix) ].
	services sort: [ :a :b | a sortOrder < b sortOrder ].
	^ services.! !

