"
Morphic view for Transcript. See category 'GUI building'.
"
Class {
	#name : 'TranscriptWindow',
	#superclass : 'SystemWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'please-remove' }
TranscriptWindow class >> openTranscript [
	"Only required for GitHub CI unti we push updated images"
	^ Transcript open
]

{ #category : 'menu-world' }
TranscriptWindow class >> worldMenuOptions [
	^ `{{
			#submenuOf 	-> 		TheWorldMenu openLabel.
			#itemGroup 	-> 		10.
			#itemOrder 	-> 		50.
			#label 			-> 		'Transcript'.
			#object 		-> 		Transcript.
			#selector 		-> 		#open.
			#icon 			-> 		#printerIcon.
			#balloonText 	-> 		'A window showing contents of the System Transcript'.
		} asDictionary}`
]

{ #category : 'open/close' }
TranscriptWindow >> delete [
	Transcript showOnDisplay: false.
	super delete
]

{ #category : 'drawing' }
TranscriptWindow >> visible: aBoolean [
	super visible: aBoolean.
	Transcript showOnDisplay: aBoolean
]

{ #category : 'GUI building' }
TranscriptWindow >> windowColor [
	^Theme current transcript
]
