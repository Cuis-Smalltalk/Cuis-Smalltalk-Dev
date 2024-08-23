'From Cuis7.1 [latest update: #6620] on 23 August 2024 at 9:56:23 am'!

!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 8/23/2024 09:52:15'!
basicClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer := selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer fullListAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn:
						(selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer isEmpty ifFalse: [
			selectedClassName := answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer! !

