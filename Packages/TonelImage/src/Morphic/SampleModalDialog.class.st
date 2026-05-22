"
SampleModalDialog openModal. "Print-it"
SampleModalDialog open. "Nothing answered"

"
Class {
	#name : 'SampleModalDialog',
	#superclass : 'ModalDialog',
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'initialization' }
SampleModalDialog >> buildWidgets [
	"
	This is a sample implementation. Actual subclasses should reimplement #buildWidgets (without calling super!).
	self openModal
	"

	| typeRow buttonRow |
	typeRow := LinearLayoutMorph newRow.
	typeRow
		addMorph: (LabelMorph contents: 'Type');
		addMorph: (LabelMorph contents: 'So what');
		addMorph: (LabelMorph contents: 'And then').
	buttonRow := LinearLayoutMorph newRow.
	buttonRow
		padding: 10;
		gap: 20;
		addMorph: (PluggableButtonMorph model: self action: #ok label: 'Ok');
		addMorph: (PluggableButtonMorph model: self action: #cancel label: 'Cancel').
	layoutMorph
		addMorph: 
			((LabelMorph contents: 'This is a title for this Dialog' fontSize: 25)
				borderWidth: 2; borderColor: color lighter;
				padding: 20);
		addMorph: Sample01Star new;
		addMorph: typeRow;
		addMorph: buttonRow.

	"self ensureMinimumExtent."
	"To make the star somehow larger"
	self morphExtent: self minimumExtent + (0@200).
]
