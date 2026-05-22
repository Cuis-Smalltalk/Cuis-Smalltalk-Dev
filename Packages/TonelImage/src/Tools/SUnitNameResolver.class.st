Class {
	#name : 'SUnitNameResolver',
	#superclass : 'Object',
	#category : 'Tools-Testing',
	#package : 'Tools',
	#tag : 'Testing'
}

{ #category : 'Camp Smalltalk' }
SUnitNameResolver class >> classNamed: aSymbol [

        ^Smalltalk
                at: aSymbol
                ifAbsent: nil
]

{ #category : 'Camp Smalltalk' }
SUnitNameResolver class >> errorObject [
	^Error
]

{ #category : 'Camp Smalltalk' }
SUnitNameResolver class >> mnuExceptionObject [
	^MessageNotUnderstood new
]

{ #category : 'Camp Smalltalk' }
SUnitNameResolver class >> notificationObject [
	^Notification new
]
