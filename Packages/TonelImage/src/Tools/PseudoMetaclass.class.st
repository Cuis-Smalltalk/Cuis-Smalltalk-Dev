Class {
	#name : 'PseudoMetaclass',
	#superclass : 'PseudoClass',
	#category : 'Tools-Code File Browser',
	#package : 'Tools',
	#tag : 'Code File Browser'
}

{ #category : 'accessing' }
PseudoMetaclass >> fullName [
	^self name,' class'
]

{ #category : 'testing' }
PseudoMetaclass >> isMeta [
	^true
]

{ #category : 'accessing' }
PseudoMetaclass >> realClass [
	^super realClass class
]

{ #category : 'accessing' }
PseudoMetaclass >> theNonMetaClass [
	"Sent to a class or metaclass, always return the class"

	^self realClass theNonMetaClass
]
