"
I'm a special Attribute. My only purpose is to disable Shout emphases and colors.
"
Class {
	#name : 'ShoutDisable',
	#superclass : 'TextAttribute',
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'comparing' }
ShoutDisable >> dominates: another [
	"Subclasses may override condense multiple attributes"
	^ another isForShout
]
