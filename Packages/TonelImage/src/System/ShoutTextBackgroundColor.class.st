"
Just for code styler (Shout)
"
Class {
	#name : 'ShoutTextBackgroundColor',
	#superclass : 'TextBackgroundColor',
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'testing' }
ShoutTextBackgroundColor >> isForShout [
	"True if to be removed from code before styling"
	^true
]
