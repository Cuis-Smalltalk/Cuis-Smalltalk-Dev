Class {
	#name : 'TonelImageExporter',
	#superclass : 'Object',
	#category : 'Tonel',
	#package : 'Tonel'
}

{ #category : 'exporting' }
TonelImageExporter class >> exportAllTo: aDirectoryEntry [
	^ self exportClasses: (Smalltalk allClasses asArray sort: [ :left :right | left name < right name ]) to: aDirectoryEntry
]

{ #category : 'exporting' }
TonelImageExporter class >> exportClasses: classes to: aDirectoryEntry [
	| outputRoot srcRoot classCount methodCount packages |
	outputRoot := aDirectoryEntry assureExistence.
	srcRoot := (outputRoot / 'src') assureExistence.
	(srcRoot // '.properties') forceWriteStreamDo: [ :stream |
		stream nextPutAll: '{'; newLine; tab; nextPutAll: '#format : #tonel'; newLine; nextPutAll: '}'; newLine ].
	classCount := 0.
	methodCount := 0.
	packages := Set new.
	classes do: [ :class |
		| package packageDir file |
		package := self packageNameForCategory: class category.
		packages add: package.
		packageDir := (srcRoot / package) assureExistence.
		file := packageDir // (self fileNameForClass: class).
		file forceWriteStreamDo: [ :stream | TonelWriter writeClass: class on: stream ].
		classCount := classCount + 1.
		methodCount := methodCount + class selectors size + class class selectors size ].
	self
		writeManifestTo: outputRoot
		classCount: classCount
		methodCount: methodCount
		packages: packages.
	^ outputRoot
]

{ #category : 'exporting' }
TonelImageExporter class >> fileNameForClass: aClass [
	^ aClass name asFileName, '.class.st'
]

{ #category : 'exporting' }
TonelImageExporter class >> packageNameForCategory: aCategoryName [
	^ TonelWriter packageNameForCategory: aCategoryName
]

{ #category : 'exporting' }
TonelImageExporter class >> writeManifestTo: outputRoot classCount: classCount methodCount: methodCount packages: packages [
	(outputRoot // 'manifest.txt') forceWriteStreamDo: [ :stream |
		stream nextPutAll: 'Tonel image export manifest'; newLine.
		stream nextPutAll: 'Generated: '; nextPutAll: DateAndTime now printString; newLine.
		stream nextPutAll: 'Image name: '; nextPutAll: Smalltalk imageName; newLine.
		stream nextPutAll: 'Image version: '; nextPutAll: SystemVersion current versionString; newLine.
		stream nextPutAll: 'Exported class count: '; nextPutAll: classCount printString; newLine.
		stream nextPutAll: 'Exported method count: '; nextPutAll: methodCount printString; newLine.
		stream nextPutAll: 'Package directory count: '; nextPutAll: packages size printString; newLine.
		stream nextPutAll: 'Format: Tonel v3-compatible source'; newLine.
		stream newLine.
		stream nextPutAll: 'Packages:'; newLine.
		(packages asArray sort) do: [ :package |
			stream nextPutAll: '- '; nextPutAll: package; newLine ] ]
]
