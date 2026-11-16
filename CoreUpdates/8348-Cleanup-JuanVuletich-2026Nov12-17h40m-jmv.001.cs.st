'From Cuis7.9 [latest update: #8347] on 12 November 2026 at 5:41:37 pm'!

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 8:40:13 pm (UTC)'!
fileOutSystemCategory: category on: aFileStream
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes := (self superclassOrderIn: category).
	poolSet := Set new.
	classes do: [ :class |
		class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet notEmpty ifTrue: [
		tempClass := Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet := poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first := true.
	classes do: [ :class | 
		first
			ifTrue: [first := false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			initializing: false
			sortMethodsByCategory: true ].
	classes do: [ :cls | cls fileOutInitializerOn: aFileStream ].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 12/Nov/2026, 8:40:32 pm (UTC)'!
fileOutCategory: aCategoryRoot
	"FileOut all the classes in the named system category."

	DirectoryEntry fileOutsDirQuery: (aCategoryRoot asFileName , '.st') writeStreamDo: [ :fileStream |
		(self withSubCategoriesOf: aCategoryRoot) do: [ :category | 
			self fileOutSystemCategory: category on: fileStream ]].! !

!methodRemoval: SystemOrganizer #fileOutCategory:on:initializing: stamp: 'jmv 12/Nov/2026, 8:40:40 pm (UTC)'!
SystemOrganizer removeSelector: #fileOutCategory:on:initializing:!
