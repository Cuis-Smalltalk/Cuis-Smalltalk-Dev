'From Cuis 5.0 [latest update: #4964] on 4 November 2021 at 10:18:21 am'!

!SmalltalkCompleterMorph methodsFor: 'selector documentation' stamp: 'HAW 11/4/2021 10:17:50'!
initializeSelectorDocumentation

	selectorDocumentation := TextModelMorph withText: ''.
	selectorDocumentation innerTextMorph
		acceptOnCR: false;
		crAction: [ self crPressedOnSelectorDocumentation ].

	selectorDocumentation
		wrapFlag: true;
		borderColor: Color black;
		borderWidth: 1;
		disableEditing;
		openInWorld
	! !

