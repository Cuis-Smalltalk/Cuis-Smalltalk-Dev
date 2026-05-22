"
I am the superclass of elements of a ChangeList. My subinstances are built from fileout files.
"
Class {
	#name : 'ChangeListElement',
	#superclass : 'Object',
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'accessing' }
ChangeListElement >> changeType [

	self subclassResponsibility 
]

{ #category : 'fileIn/fileOut' }
ChangeListElement >> fileIn [

	self subclassResponsibility 
]

{ #category : 'fileIn/fileOut' }
ChangeListElement >> fileOutOn: aFileStream [

	self subclassResponsibility 
]

{ #category : 'testing' }
ChangeListElement >> isClassDeletion [
	^false
]

{ #category : 'testing' }
ChangeListElement >> isDoIt [

	^false
]

{ #category : 'testing' }
ChangeListElement >> isMethodDeletion [
	^false
]

{ #category : 'accessing' }
ChangeListElement >> methodSelector [
	^nil
]

{ #category : 'perform' }
ChangeListElement >> performOn: aCodeFile [

	 ^aCodeFile perform: (self changeType copyWith: $:) asSymbol with: self 
]

{ #category : 'accessing' }
ChangeListElement >> string [

	self subclassResponsibility 
]

{ #category : 'accessing' }
ChangeListElement >> text [
	"In Cuis, all source code is plain Strings"
	^self string
]
