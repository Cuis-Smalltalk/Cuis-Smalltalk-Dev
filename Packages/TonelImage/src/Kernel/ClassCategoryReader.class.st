"
I represent a mechanism for retrieving class descriptions stored on a file.
"
Class {
	#name : 'ClassCategoryReader',
	#superclass : 'Object',
	#instVars : [
		'class',
		'category',
		'changeStamp',
		'libraryName'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'fileIn/Out' }
ClassCategoryReader >> scanFrom: aStream [
	"File in methods from the stream, aStream.
	Convert line endings to Cuis convention."
	| methodSource |
	[
		methodSource := aStream nextChunk withCuisLineEndings.
		methodSource size > 0 ] whileTrue: [
			class
				compile: methodSource
				classified: category
				withStamp: changeStamp
				libraryName: libraryName
				notifying: nil ]
]

{ #category : 'private' }
ClassCategoryReader >> setClass: aClass category: aCategory [
	^ self setClass: aClass category: aCategory changeStamp: String new

]

{ #category : 'private' }
ClassCategoryReader >> setClass: aClass category: aCategory changeStamp: aString [

	class := aClass.
	category := aCategory.
	changeStamp := aString

]

{ #category : 'private' }
ClassCategoryReader >> setClass: aClass category: aCategory changeStamp: aString libraryName: aStringOrNil [

	class := aClass.
	category := aCategory.
	changeStamp := aString.
	libraryName := aStringOrNil.
]
