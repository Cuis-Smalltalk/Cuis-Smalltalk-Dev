Class {
	#name : 'ClassRenamedChangeRecord',
	#superclass : 'ChangeListElement',
	#instVars : [
		'previousName',
		'newName',
		'stamp'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'instance creation' }
ClassRenamedChangeRecord class >> from: previousClassName to: newClassName stamp: aString [
	^self new initializeFrom: previousClassName to: newClassName stamp: aString 
]

{ #category : 'accessing' }
ClassRenamedChangeRecord >> changeClass [
	^ nil
]

{ #category : 'accessing' }
ClassRenamedChangeRecord >> changeClassName [
	^ previousName 
]

{ #category : 'accessing' }
ClassRenamedChangeRecord >> changeType [
	^ #classRenamed
]

{ #category : 'fileIn/fileOut' }
ClassRenamedChangeRecord >> fileIn [

	Smalltalk renameClassNamed: previousName as: newName
]

{ #category : 'initialization' }
ClassRenamedChangeRecord >> initializeFrom: previousClassName to: newClassName stamp: aString  [

	previousName := previousClassName.
	newName := newClassName.
	stamp := aString.
]

{ #category : 'accessing' }
ClassRenamedChangeRecord >> newClassName [
	^ newName 
]

{ #category : 'accessing' }
ClassRenamedChangeRecord >> stamp [
	^stamp
]

{ #category : 'printing' }
ClassRenamedChangeRecord >> string [
	^ 'classRenamed: #', previousName, ' as: #', newName
]
