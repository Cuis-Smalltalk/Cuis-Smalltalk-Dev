Class {
	#name : 'TextAction',
	#superclass : 'TextAttribute',
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'as yet unclassified' }
TextAction class >> textActionColor [
	^ `Color r: 0.4 g: 0 b: 1.0`
]

{ #category : 'as yet unclassified' }
TextAction >> analyze: aString [
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param := self validate: aString.
		^ Array with: param with: (param size = 0 ifTrue: [nil] ifFalse: [param])].
	"Two parts"
	trim := aString withBlanksTrimmed.
	trim first == $< 
		ifTrue: [ trim last == $>
			ifTrue: ["only instructions" 
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] ifFalse: [param]]
			ifFalse: ["at the front"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [ trim last == $>
			ifTrue: ["at the end"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show := nil]].
	^ Array with: param with: show

]

{ #category : 'testing' }
TextAction >> dominatedByNormalEmphasis [
	"TextEmphasis normal (i.e. cmd-0) should turn off active text"
	^ true
]

{ #category : 'iterating' }
TextAction >> forTextActionInfoDo: aBlock [
	aBlock value: self info
]

{ #category : 'as yet unclassified' }
TextAction >> info [
	^ 'no hidden info'
]

{ #category : 'testing' }
TextAction >> isForFormatting [
	"Answer false for those attributes that are not for text formatting.
	This is, those that are not considered in #withFormattingAttributeValues:do:"
	^false
]

{ #category : 'testing' }
TextAction >> mayActOnClick [

	^ true
]

{ #category : 'as yet unclassified' }
TextAction >> validate: aString [
	"any format is OK with me"
	^ aString
]
